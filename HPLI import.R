# SPDX-License-Identifier: GPL-3.0-or-later
# ──────────────────────────────────────────────────────────────────────────
# HPLI-EU: reading and completing the 20 metrics from a PPDB/BPDB export ####
# ──────────────────────────────────────────────────────────────────────────

#' Reading the HPLI-EU metrics out of a PPDB export
#'
#' Turns a PPDB/BPDB export into one row per substance carrying the 20
#' HPLI-EU metrics, each read from its primary PPDB field or completed from
#' a secondary one. Only observed values: no missing-data substitution
#' happens here, because that is a scoring-time policy rather than a
#' property of the data. `load_ppdb_raw_metrics()` is the entry point.
#'
#' Sourced by both "HPLI score.R" and "HPLI weights.R", so a metric is
#' derived identically whether it is about to be scored or correlated. The
#' two scripts also share the last section of this file, which identifies
#' the export in their run logs and writes the licence notice of their
#' output workbooks.
#'
#' @section Requires:
#' "HPLI parameters.R" sourced first: `hazard_direction()` resolves range
#' values here. The metric list and the PPDB columns it maps are specific
#' to HPLI-EU - a version needing other metrics needs its own sibling
#' import file, not only a different "HPLI parameters.R".
#'
#' @section Data-quality companions:
#' Every metric `m` comes back with three further columns:
#' * `m_status` - "measured" (the metric's primary PPDB field) or
#'   "completed" (derived from a secondary field or a formula).
#' * `m_bound` - "exact", "<" or ">" (a PPDB comparator), "range" (a
#'   resolved text range such as "10-20"), or "missing" whenever no bound
#'   applies at all.
#' * `m_confidence` - the PPDB's own 1-5 quality band, one level lower
#'   whenever the value was completed rather than measured.
#'
#' All three are `NA` for a genuinely missing metric. "HPLI score.R" is
#' where such a metric is given a substitution status instead. Design and
#' reasoning: "HPLI methodology.md", section "Data quality traceability".

library(readxl)
library(dplyr)
library(stringr)

# ──────────────────────────────────────────────────────────────────────────
# Machine-specific settings                                              ####
# ──────────────────────────────────────────────────────────────────────────

#' Read this machine's paths from an untracked settings file
#'
#' @param file Path to the settings file, expected next to the scripts.
#'   Default "local_paths.R".
#' @param require_dirs Character vector of setting names that the file must
#'   define, each holding the path of an existing directory. Checked in the
#'   order given.
#' @return Invisibly, the named character vector of the `require_dirs`
#'   values that were validated. Called for its side effect: everything the
#'   file assigns is created in the global environment.
#' @details
#' `file` is sourced in the global environment, so the settings it assigns
#' are visible to the calling script exactly as if they had been typed
#' there. It is listed in ".gitignore" and never committed;
#' "local_paths.example.R" is the tracked template to copy.
#'
#' Three failures stop the run, each naming the setting and the template: a
#' missing file, a setting absent from the file, and a setting holding
#' anything other than the path of a directory that exists. Every validated
#' path is echoed to the console, so a run is never ambiguous about which
#' data it read.
#'
#' Only input locations belong in that file. A script's own output paths
#' are settings of the script, declared with the rest of them, and are not
#' read from here.
load_local_paths <- function(file = "local_paths.R", require_dirs = character()) {
  if (!is.character(file) || length(file) != 1L || is.na(file)) {
    stop("file must be a single path.", call. = FALSE)
  }
  if (!is.character(require_dirs)) {
    stop("require_dirs must be a character vector of setting names.", call. = FALSE)
  }
  if (!file.exists(file)) {
    stop(
      "Machine-specific settings file not found: \"", file, "\".\n",
      "Copy \"local_paths.example.R\" to \"", file, "\" and fill in the ",
      "paths for this machine.",
      call. = FALSE
    )
  }

  source(file) # evaluated in the global environment: see @details

  validated <- character(0)
  for (setting in require_dirs) {
    if (!exists(setting, envir = globalenv(), inherits = FALSE)) {
      stop(
        "\"", file, "\" does not define `", setting, "`. See ",
        "\"local_paths.example.R\" for the expected settings.",
        call. = FALSE
      )
    }
    value <- get(setting, envir = globalenv())
    if (!is.character(value) || length(value) != 1L || is.na(value) ||
        !nzchar(trimws(value))) {
      stop(
        "`", setting, "` in \"", file, "\" must be a single non-empty path.",
        call. = FALSE
      )
    }
    if (!dir.exists(value)) {
      stop(
        "`", setting, "` in \"", file, "\" is not an existing folder: ",
        value,
        call. = FALSE
      )
    }
    message(setting, ": ", value)
    validated[setting] <- value
  }

  invisible(validated)
}

# ──────────────────────────────────────────────────────────────────────────
# Low-level parsing helpers                                              ####
# ──────────────────────────────────────────────────────────────────────────

#' Cell contents read as absent, and the pattern that marks a range
#'
#' @format `ppdb_na_strings` is a character vector of cell values treated
#'   as missing; `ppdb_range_regex` a regular expression capturing the two
#'   numbers of a range such as "10-20" (either kind of dash).
#' @details
#' Shared by `parse_ppdb_numeric()` and `parse_ppdb_bound()`, so the two
#' cannot define "this is a range" differently from one another.
ppdb_na_strings   <- c("", "NA", "N/A", "na", "n/a", "-")
ppdb_range_regex  <- "([-+]?[0-9]*\\.?[0-9]+)\\s*[-–]\\s*([-+]?[0-9]*\\.?[0-9]+)"

#' Read a PPDB numeric cell
#'
#' @param x Character vector of raw cell contents (a numeric vector is
#'   returned unchanged).
#' @param direction `1` or `-1`, as `hazard_direction()` returns: which way
#'   the hazard runs for this metric. Consulted only when a range is
#'   actually encountered; a range met without it falls back to the mean
#'   and warns.
#' @param range_policy `"worst_case"` (default) or `"mean"`: how to resolve
#'   a range into one number.
#' @return A numeric vector the same length as `x`, `NA` where nothing
#'   numeric could be read.
#' @details
#' Reads what is written and infers nothing further: HTML tags and unit
#' remnants are stripped, decimal commas accepted, and the strings of
#' `ppdb_na_strings` read as missing.
#'
#' A `<` or `>` comparator prefix is not interpreted - the number after it
#' is taken at face value, and `parse_ppdb_bound()` is what records that
#' the comparator was there.
#'
#' A range ("10-20") is resolved rather than discarded: under
#' `"worst_case"`, to whichever end is more hazardous for this metric;
#' under `"mean"`, to the average of the two ends. An imprecise value still
#' carries information a missing one does not - see
#' "HPLI methodology.md", section "Missing-data and range-value policy".
#'
#' Expects the sheet to have been read with `col_types = "text"` (see
#' `read_sheet_as_text()`): readxl's own type guessing would otherwise turn
#' a range, a comparator or a placeholder into `NA` before this function
#' ever saw it as a string.
parse_ppdb_numeric <- function(x, direction = NA_real_, range_policy = "worst_case") {
  if (is.numeric(x)) return(as.numeric(x))
  x <- as.character(x) |>
    str_replace_all("<[^>]+>", "") |>
    str_replace_all(",", ".") |>
    str_trim()
  x[x %in% ppdb_na_strings] <- NA_character_

  range_match <- str_match(x, ppdb_range_regex)
  range_lo <- suppressWarnings(as.numeric(range_match[, 2]))
  range_hi <- suppressWarnings(as.numeric(range_match[, 3]))
  is_range <- !is.na(range_lo) & !is.na(range_hi)

  value <- suppressWarnings(as.numeric(str_extract(x, "[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?")))

  if (any(is_range)) {
    if (all(is.na(direction))) {
      warning("parse_ppdb_numeric(): range value(s) found with no direction supplied - using the mean instead of a worst-case bound.", call. = FALSE)
    }
    use_mean <- identical(range_policy, "mean") || all(is.na(direction))
    resolved <- if (use_mean) {
      (range_lo + range_hi) / 2
    } else if (direction >= 0) {
      pmax(range_lo, range_hi)
    } else {
      pmin(range_lo, range_hi)
    }
    value[is_range] <- resolved[is_range]
  }
  value
}

#' Classify how precisely a PPDB cell states its value
#'
#' @param value_text Character vector of raw cell contents.
#' @param comparator_text Character vector of the same length, from the
#'   PPDB's own dedicated "<> - ..." column where the export has one.
#' @return A character vector: "exact", "<", ">", "range" or "missing".
#' @details
#' Independent of `parse_ppdb_numeric()`, which resolves the same cell to a
#' number. This function answers how well that number is pinned down, for
#' the traceability output.
#'
#' The comparator is taken from the PPDB's own column where one exists
#' (authoritative rather than inferred), and otherwise from a leading `<`
#' or `>` in the value cell itself. Both cases occur in the same export:
#' the ecotoxicity and human-toxicity files carry the dedicated column,
#' the fate file does not.
#'
#' A blank cell returns "missing" rather than `NA`, this being the one case
#' the function can detect on its own. A formula-derived value has no bound
#' to read either, but that is set by the caller, which never calls this
#' function for it.
parse_ppdb_bound <- function(value_text, comparator_text = NA_character_) {
  value_text      <- str_trim(as.character(value_text))
  comparator_text <- str_trim(as.character(comparator_text))
  from_column <- ifelse(comparator_text %in% c("<", ">"), comparator_text, NA_character_)
  from_value  <- str_extract(value_text, "^[<>]")
  is_range    <- str_detect(value_text, ppdb_range_regex)
  case_when(
    is.na(value_text) | value_text %in% ppdb_na_strings ~ "missing", # a blank cell, not NA: see @details above
    !is.na(from_column) ~ from_column,
    !is.na(from_value)  ~ from_value,
    is_range             ~ "range",
    TRUE                 ~ "exact"
  )
}

#' Cell contents that mean "not assessed" in a categorical field
#'
#' @format A character vector, compared against the lower-cased, trimmed
#'   cell.
#' @details
#' Read as missing rather than as a confirmed "no". The distinction is not
#' cosmetic: a substring match on "no" would read "No data" as a confirmed
#' absence of effect, and a metric wrongly scored 0 instead of receiving
#' the precautionary substitution.
ppdb_missing_category_strings <- c(
  "",
  "na",
  "n/a",
  "-",
  "no data",
  "not available",
  "nd",
  "nc",
  "not calculated",
  "not determined")

#' Read a PPDB categorical hazard cell
#'
#' @param x Character vector of raw cell contents.
#' @return A character vector of "yes", "possible", "no", or `NA` where the
#'   field states nothing assessable.
#' @details
#' Matches on wording rather than exact equality, since the PPDB phrases
#' these fields several ways ("known", "probable" and "likely" all read as
#' "yes"; "suspected" and "potential" as "possible"). Anything in
#' `ppdb_missing_category_strings`, and anything unrecognised, returns
#' `NA`. `score_categorical_hazard()` ("HPLI parameters.R") then maps the
#' three levels onto 0, 0.5 and 1.
parse_ppdb_category <- function(x) {
  x <- str_to_lower(str_trim(as.character(x)))
  x[x %in% ppdb_missing_category_strings] <- NA_character_
  case_when(
    is.na(x) ~ NA_character_,
    str_detect(x, "\\byes\\b|true|known|probable|likely") ~ "yes",
    str_detect(x, "possible|suspected|maybe|potential")   ~ "possible",
    str_detect(x, "\\bno\\b|unlikely|false")              ~ "no",
    TRUE ~ NA_character_
  )
}

#' Read a PPDB quality-band cell
#'
#' @param x Character vector of raw "QB - ..." cell contents.
#' @return A numeric vector: the leading digit, on the PPDB's own 1-5
#'   confidence scale, `NA` where the cell holds no digit.
parse_quality_band <- function(x) as.numeric(
  str_extract(as.character(x), "\\d"))

#' Persistence coefficient for a chronic-effect metric
#'
#' @param dt50 Numeric vector of half-lives, in days.
#' @param a Numeric scalar: the compartment's reference exposure window, in
#'   days.
#' @return A numeric vector of coefficients in (0, 1], to multiply a
#'   chronic metric's normalised score by. `1` where `dt50` is `NA`.
#' @details
#' k = (1 - 2^(-a/DT50)) / (a * ln2 / DT50), from Vandevoorde et al.
#' (2025). It attenuates a chronic effect by how fast the substance
#' degrades, so k approaches 1 as DT50 grows: a missing DT50 returning 1
#' reads as "assume persistent", which is the precautionary direction and
#' the opposite of how it may first look.
#'
#' No special case is needed at `dt50 = 0`: the formula's own limit gives
#' 0 there, cleanly, without dividing by zero.
#'
#' It lives in this file rather than with the indicator definition because
#' it is a property of how DT50 inputs are used, not of the metric,
#' threshold and weight definition - and only the scoring script applies
#' it. Where it applies and why: "HPLI methodology.md", section "Where the
#' persistence coefficient is applied".
persistence_coefficient <- function(dt50, a) {
  ifelse(is.na(dt50), 1, ((1 - 2^(-a / dt50)) / (a * log(2))) * dt50)
}

#' Complete a missing BCF from the completed KFOC
#'
#' @param bcf Numeric vector of measured bioconcentration factors.
#' @param kfoc_completed Numeric vector of KFOC values, already completed.
#' @return A numeric vector: `bcf` where it is present, otherwise the
#'   regression estimate, otherwise `NA`.
#' @details
#' A QSAR-type regression on log10(KFOC), in two segments above and below
#' log10(KFOC) = 6. A measured value always takes priority; a value
#' produced here is flagged "completed" by the caller, with its confidence
#' one level below KFOC's own.
complete_bcf <- function(bcf, kfoc_completed) {
  log_kfoc <- log10(kfoc_completed)
  case_when(
    !is.na(bcf) ~ bcf,
    log_kfoc >= 6              ~ 10^(-0.2 * log_kfoc^2 + 2.74 * log_kfoc - 4.72),
    log_kfoc > 0 & log_kfoc < 6 ~ 10^(0.85 * log_kfoc - 0.7),
    TRUE ~ NA_real_
  )
}

#' Compute the groundwater ubiquity score
#'
#' @param soil_dt50_completed Numeric vector of soil half-lives, in days,
#'   already completed.
#' @param kfoc_completed Numeric vector of KFOC values, already completed.
#' @return A numeric vector: log10(DT50) x (4 - log10(KFOC)), or `NA`
#'   wherever either input is missing or out of the formula's domain.
#' @details
#' GUS is always computed, never read from the export - see
#' "HPLI methodology.md", section "Completing soil DT50, KFOC, BCF and GUS
#' before normalisation". A missing result is left to the caller's usual
#' precautionary substitution.
compute_gus <- function(soil_dt50_completed, kfoc_completed) {
  case_when(
    soil_dt50_completed > 0 & is.finite(kfoc_completed) & kfoc_completed > 0 ~
      log10(soil_dt50_completed) * (4 - log10(kfoc_completed)),
    TRUE ~ NA_real_
  )
}

# ──────────────────────────────────────────────────────────────────────────
# Reading the four-file PPDB export                                      ####
# ──────────────────────────────────────────────────────────────────────────

#' Read one sheet of the export, every column as text
#'
#' @param path Path to the workbook.
#' @param sheet Sheet name.
#' @return A tibble with every column character, except `ID`, restored to
#'   numeric since it serves only as a join key.
#' @details
#' Reading as text is what lets the `parse_*` functions see a range, a
#' comparator or a placeholder at all: readxl's per-column type guessing
#' turns any of them into `NA` before parsing gets a chance.
read_sheet_as_text <- function(path, sheet) {
  d <- read_excel(path, sheet = sheet, col_types = "text")
  if ("ID" %in% names(d)) d$ID <- as.numeric(d$ID)
  d
}

# PPDB export version: what to change if your headers differ            ####
# ──────────────────────────────────────────────────────────────────────────
#
# The column maps below were written against the PPDB export of 2024-05-03
# and are NOT guaranteed to fit a later one. AERU revises the database
# continuously, and an export's column headers reflect the production
# pipeline that generated it as much as the database itself - this export,
# for instance, spells some fields with dots and others with spaces and
# brackets within the very same sheet (e.g. "Soil.DT50.typical...days" next
# to "Soil DT50 - Field (days)"), which is why each column is mapped
# individually rather than through one blanket rule.
#
# If read_ppdb_export() errors on a missing column, or a metric comes back
# empty for every substance, the export's headers have moved. To adapt:
#
#   1. Print the real headers of the offending file, e.g.
#        names(readxl::read_excel(file.path(ppdb_export_dir, "Fate.xlsx"), n_max = 0))
#   2. Find the equivalent column and edit the corresponding entry in the
#      map below. Each entry reads `internal name` = "name in the export":
#      the LEFT side is used throughout this script and must not change;
#      only the right-hand string follows the export.
#   3. Each numeric metric may have up to two sibling columns, mapped the
#      same way: "<> - ..." (the comparator, "<" / ">") and "QB - ..." (the
#      1-5 quality band). A metric whose export has neither still works -
#      the comparator is then scanned from the value cell itself and the
#      confidence is left NA - but check which case you are in rather than
#      assuming, since that affects the Data_quality output.
#   4. Re-run and compare against a reference run before trusting the
#      result: a silently mis-mapped column is far more likely than a hard
#      error. No reference output ships with this version of the code (see
#      "HPLI methodology.md", section "PPDB licensing and what may be
#      shared"); when one does, it will hold the HPLI and compartment scores
#      only, not the per-metric ones, so a compartment that moves points to
#      the maps feeding it, and "Data_quality" then narrows it down to the
#      metric whose status or bound changed.
#
# There are five maps, one per internal table, and each entry reads
# `internal name` = "header in the export". The internal name is what the
# rest of this file and "HPLI score.R" use; the string is what is looked
# up. Every column read is listed, including those whose header already
# matches its internal name verbatim, so the maps double as a complete
# inventory of what read_ppdb_export() depends on.
#
# Two spellings coexist in this export - some fields use dots where others
# use spaces and brackets, within the very same sheet
# ("Soil.DT50.typical...days" next to "Soil DT50 - Field (days)") - which
# is why each column is mapped one by one. GUS appears in none of the maps:
# this export has no GUS column, and the metric is always computed.
#
# The "<> - ..." and "QB - ..." sibling columns feed the traceability
# output. The ecotoxicity and human-toxicity files carry both for every
# metric HPLI-EU uses; the fate file carries quality bands but no
# comparator columns at all. That asymmetry is a property of the export,
# handled in parse_ppdb_bound(), not an omission here.

general_column_map <- c(
  ID                                 = "ID",
  Active                             = "Active",
  Reference                          = "Reference",
  `EC Regulation 1107/2009 status`   = "EC Regulation 1107/2009 status",
  `Pesticide type`                   = "Pesticide type",
  `Substance origin`                 = "Substance origin",
  # CAS registry number and chemical family, for the substance summary in
  # "HPLI visualisation.R". "CASS RN" is this export's own header spelling
  # - a typo upstream of this code, not a PPDB standard. "Substance group"
  # is PPDB's chemical-family field, e.g. "Organophosphate herbicide;
  # Phosphonoglycine herbicide" for glyphosate.
  # ("Family" in its substance-info box), confirmed by spot-checking
  # glyphosate's CAS and family string against that app's own displayed
  # values (identical).
  CAS                                = "CASS RN",
  `Substance group`                  = "Substance group"
)

fate_column_map <- c(
  ID                             = "ID",
  `Soil DT50 - Typical (days)`   = "Soil.DT50.typical...days",
  `QB - Soil DT50 - Typical`     = "QB - Soil DT50 - Typical",
  `Soil DT50 - Lab (days)`       = "Soil.DT50.lab...days",
  `QB - Soil DT50 - Lab`         = "QB - Soil DT50 - Lab",
  `Soil DT50 - Field (days)`     = "Soil DT50 - Field (days)",
  `QB - Soil DT50 - Field`       = "QB - Soil DT50 - Field",
  `Water phase only DT50 (days)` = "Water.phase.DT50...days",
  `QB - Water phase only DT50`   = "QB - Water phase only DT50",
  `Kfoc (ml/g)`                  = "Kfoc (ml/g)",
  `QB - Freundlich isotherm`     = "QB - Freundlich isotherm",
  `Koc (ml/g)`                   = "Koc (ml/g)",
  `QB - Kd & Koc`                = "QB - Kd & Koc",
  `Bioconcentration factor`      = "Bioconcentration factor",
  `QB - Bioconcentration factor` = "QB - Bioconcentration factor"
)
# GUS is deliberately absent: this export has no GUS column, and the
# metric is computed from soil DT50 and KFOC in every case - see
# compute_gus() above. Nothing to map.

# Ecotox.xlsx merges terrestrial and aquatic ecotox into one sheet; these
# two maps are applied to that same sheet, not two different files.
ecotox_terrestrial_column_map <- c(
  ID                                                   = "ID",
  `Birds - Acute LD50 (mg/kg)`                         = "Birds...Acute.LD50.mg.kg",
  `<> - Birds - Acute LD50`                             = "<> - Birds - Acute LD50",
  `QB - Birds - Acute LD50`                             = "QB - Birds - Acute LD50",
  `Earthworms - Acute 14d LC50 (mg/kg)`                 = "Earthworms...Acute.14d.LC50.mg.kg",
  `<> - Earthworms - Acute`                             = "<> - Earthworms - Acute",
  `QB - Earthworms - Acute`                             = "QB - Earthworms - Acute",
  `Honeybees - Contact acute 48hr LD50 (ug per bee)`    = "Honeybees...Contact.acute.48hr.LD50.ug.per.bee",
  `<> - Honeybees - Contact acute 48hr LD50`            = "<> - Honeybees - Contact acute 48hr LD50",
  `QB - Honeybees - Contact acute 48hr LD50`            = "QB - Honeybees - Contact acute 48hr LD50",
  `Honeybees - Oral acute 48hr LD50 (ug per bee)`       = "Honeybees...Oral.Acute.48hr.LD50.ug.per.bee",
  `<> - Honeybees - Oral acute 48hr LD50`               = "<> - Honeybees - Oral acute 48hr LD50",
  `QB - Honeybees - Oral acute 48hr LD50`               = "QB - Honeybees - Oral acute 48hr LD50",
  `Honeybees - Unknown mode acute 48hr LD50 (ug per bee)` = "Honeybees - Unknown mode acute 48hr LD50 (ug per bee)",
  `<> - Honeybees - Unknown mode acute 48hr LD50`       = "<> - Honeybees - Unknown mode acute 48hr LD50",
  `QB - Honeybees - Unknown mode acute 48hr LD50`       = "QB - Honeybees - Unknown mode acute 48hr LD50",
  `Mammals - Acute oral LD50 (mg/kg BW/day)`            = "Mammals...Acute.Oral.LD50.mg.kg.BW.day",
  `<> - Mammals - Acute oral LD50`                       = "<> - Mammals - Acute oral LD50",
  `QB - Mammals - Acute oral LD50`                       = "QB - Mammals - Acute oral LD50"
)

# Two mappings in this file needed a substantive check rather than a
# mechanical rename, and are worth flagging to anyone adapting the maps to
# another export.
#
# Algae: this export has exactly one acute-algae column, spelled
# "Algae...Acute.72hr.EC50.Growth.mg.l" in the value cell but
# "Algae - Acute 72hr EC50 growth" in its comparator and quality-band
# siblings. It is read as the freshwater algal growth-rate acute EC50 the
# indicator asks for.
#
# Aquatic invertebrates and fish: the temperate columns are the ones
# carrying NO suffix here, which is the opposite of what the headers
# suggest. Their comparator and quality-band siblings do spell out
# "- TEMPERATE", which is how the value columns were identified. An export
# that labels them differently will need this pair re-checked.
ecotox_aquatic_column_map <- c(
  ID                                                            = "ID",
  `Algae - Acute (growth rate, fresh - mg/l)`                   = "Algae...Acute.72hr.EC50.Growth.mg.l",
  `<> - Algae - Acute 72hr EC50 growth`                          = "<> - Algae - Acute 72hr EC50 growth",
  `QB - Algae - Acute 72hr EC50 growth`                          = "QB - Algae - Acute 72hr EC50 growth",
  `Aquatic invertebrates - Acute 48hr EC50 (mg/l) - TEMPERATE`  = "Aquatic.Invertebrates...Acute.48hr.EC50.mg.l",
  `<> - Aquatic invertebrates - Acute 48hr EC50 - TEMPERATE`    = "<> - Aquatic invertebrates - Acute 48hr EC50 - TEMPERATE",
  `QB - Aquatic invertebrates - Acute 48hr EC50 - TEMPERATE`    = "QB - Aquatic invertebrates - Acute 48hr EC50 - TEMPERATE",
  `Aquatic invertebrates - Chronic 21d NOEC (mg/l) - TEMPERATE` = "Aquatic.Invertebrates...Chronic.21d.NOEC.mg.l",
  `<> - Aquatic invertebrates - Chronic 21d NOEC - TEMPERATE`   = "<> - Aquatic invertebrates - Chronic 21d NOEC - TEMPERATE",
  `QB - Aquatic invertebrates - Chronic 21d NOEC - TEMPERATE`   = "QB - Aquatic invertebrates - Chronic 21d NOEC - TEMPERATE",
  `Fish - Acute 96hr LC50 (mg/l) - TEMPERATE`                   = "Fish...Acute.96hr.LC50.mg.l",
  `<> - Fish - Acute 96hr LC50 - TEMPERATE`                      = "<> - Fish - Acute 96hr LC50 - TEMPERATE",
  `QB - Fish - Acute 96hr LC50 - TEMPERATE`                      = "QB - Fish - Acute 96hr LC50 - TEMPERATE",
  `Fish - Chronic 21d NOEC (mg/l) - TEMPERATE`                  = "Fish...Chronic.21d.NOEC.mg.l",
  `<> - Fish - Chronic 21d NOEC - TEMPERATE`                     = "<> - Fish - Chronic 21d NOEC - TEMPERATE",
  `QB - Fish - Chronic 21d NOEC - TEMPERATE`                     = "QB - Fish - Chronic 21d NOEC - TEMPERATE"
)

human_column_map <- c(
  ID                                    = "ID",
  `Mammals - Dermal LD50 (mg/kg)`       = "Mammals - Dermal LD50 (mg/kg)",
  `<> - Mammals - Dermal LD50`          = "<> - Mammals - Dermal LD50",
  `QB - Mammals - Dermal LD50`          = "QB - Mammals - Dermal LD50",
  `Mammals - Inhalation LC50 (mg/l)`    = "Mammals - Inhalation LC50 (mg/l)",
  `<> - Mammals - Inhalation LC50`      = "<> - Mammals - Inhalation LC50",
  `QB - Mammals - Inhalation LC50`      = "QB - Mammals - Inhalation LC50",
  `Carcinogen?`                         = "Carcinogen?",
  `Acetyl cholinesterase inhibitor?`    = "Acetyl cholinesterase inhibitor?",
  `Neurotoxicant?`                      = "Neurotoxicant?",
  `Reproduction/development effects?`   = "Reproduction/development effects?"
)
# The four categorical human-toxicity fields have no "<> - ..." or "QB - ..."
# sibling columns in the PPDB (a yes/no/possible call has no comparator or
# confidence band to speak of) - confirmed against this export's header
# row, and matching the thesis script, which never tracked a "_dataquality"
# column for these either. Their bound/confidence are always NA - see the
# categorical_metrics loop in load_ppdb_raw_metrics() below.

#' Select and rename one sheet's columns through a column map
#'
#' @param df The sheet as read.
#' @param map A named character vector: names are the internal column
#'   names, values the headers to find them under.
#' @param source_label The file name, used in the error message.
#' @return `df` reduced to the mapped columns, renamed to their internal
#'   names, in the map's order.
#' @details
#' Stops and names every header it could not find, rather than returning a
#' silently empty column that would surface much later as a metric missing
#' for every substance. The error points at the map to edit.
apply_column_map <- function(df, map, source_label) {
  missing <- map[!map %in% names(df)]
  if (length(missing) > 0) {
    stop(
      "read_ppdb_export(): expected column(s) not found in '", source_label, "': ",
      paste(missing, collapse = "; "),
      ". The PPDB export's header names may have changed (e.g. a newer AERU ",
      "version, or a different regional/BPDB export) - update the corresponding ",
      "*_column_map near the top of \"HPLI import.R\".",
      call. = FALSE
    )
  }
  out <- df[, unname(map), drop = FALSE]
  names(out) <- names(map)
  out
}

#' Read a four-file PPDB export into five internal tables
#'
#' @param dir Folder holding General.xlsx, Fate.xlsx, Ecotox.xlsx and
#'   Human.xlsx.
#' @return A named list of five tibbles: `identification`, `fate`,
#'   `terrestrial_ecotox`, `aquatic_ecotox`, `human_tox`, with the internal
#'   column names `load_ppdb_raw_metrics()` expects.
#' @details
#' Ecotox.xlsx holds terrestrial and aquatic ecotoxicity in a single sheet,
#' so two of the five tables are two renamed views of the same data rather
#' than two files.
read_ppdb_export <- function(dir) {
  general    <- read_sheet_as_text(file.path(dir, "General.xlsx"), "General")
  fate_raw   <- read_sheet_as_text(file.path(dir, "Fate.xlsx"), "Fate")
  ecotox_raw <- read_sheet_as_text(file.path(dir, "Ecotox.xlsx"), "Ecotox")
  human_raw  <- read_sheet_as_text(file.path(dir, "Human.xlsx"), "Human")

  list(
    identification     = apply_column_map(general, general_column_map, "General.xlsx"),
    fate               = apply_column_map(fate_raw, fate_column_map, "Fate.xlsx"),
    terrestrial_ecotox = apply_column_map(ecotox_raw, ecotox_terrestrial_column_map, "Ecotox.xlsx"),
    aquatic_ecotox     = apply_column_map(ecotox_raw, ecotox_aquatic_column_map, "Ecotox.xlsx"),
    human_tox          = apply_column_map(human_raw, human_column_map, "Human.xlsx")
  )
}

# ──────────────────────────────────────────────────────────────────────────
# Public entry point                                                     ####
# ──────────────────────────────────────────────────────────────────────────

#' Read and complete the 20 HPLI-EU metrics for every substance
#'
#' @param ppdb_export_dir Folder holding the four-file PPDB export.
#'   Required; an empty or non-existent path stops the run with a message
#'   pointing at "local_paths.example.R".
#' @param range_policy Passed to `parse_ppdb_numeric()`: `"worst_case"`
#'   (default) or `"mean"`.
#' @param synthetic_only `TRUE` keeps only substances the PPDB labels as
#'   synthetic in origin. Default `FALSE`.
#' @param water_dt50_stable_assumption `TRUE` (default) reads a water DT50
#'   recorded only as the free text "stable" as 300 days; `FALSE` leaves it
#'   missing.
#' @return A tibble with one row per substance: identification columns, the
#'   20 metrics, and each metric's `_status`, `_bound` and `_confidence`
#'   companions (see the file header).
#' @details
#' The values are those the PPDB states, plus the completions described in
#' "HPLI methodology.md", section "Completing soil DT50, KFOC, BCF and GUS
#' before normalisation". Nothing is substituted for a missing metric here:
#' that is a scoring-time policy, so "HPLI score.R" applies it, while
#' "HPLI weights.R" wants the real `NA`s its pairwise correlations skip.
#'
#' Every setting arrives as an argument rather than being read from a
#' global, so the function's behaviour is determined by the call alone
#' whichever script makes it.
#'
#' `water_dt50_stable_assumption` governs the one fill-in here that assumes
#' a value never measured; every other completion reads a real PPDB field
#' or applies a published formula over such fields, and no switch affects
#' them. See "HPLI methodology.md", section "Weight calculation", for why
#' the two callers differ on it.
load_ppdb_raw_metrics <- function(
  ppdb_export_dir,
  range_policy = "worst_case",
  synthetic_only = FALSE,
  water_dt50_stable_assumption = TRUE
) {
  if (is.null(ppdb_export_dir) || !nzchar(trimws(ppdb_export_dir))) {
    stop(
      "ppdb_export_dir is not set. The PPDB export is licensed AERU material ",
      "and is not distributed with this code: point ppdb_export_dir at your ",
      "own copy of the four-file export (General.xlsx, Fate.xlsx, ",
      "Ecotox.xlsx, Human.xlsx). Copy \"local_paths.example.R\" to ",
      "\"local_paths.R\" and set it there, or edit the setting directly in ",
      "the calling script.",
      call. = FALSE
    )
  }
  if (!dir.exists(ppdb_export_dir)) {
    stop("PPDB export folder not found: ", ppdb_export_dir, call. = FALSE)
  }
  if (!is.logical(water_dt50_stable_assumption) ||
      length(water_dt50_stable_assumption) != 1L ||
      is.na(water_dt50_stable_assumption)) {
    stop("water_dt50_stable_assumption must be a single TRUE or FALSE.", call. = FALSE)
  }
  ppdb <- read_ppdb_export(ppdb_export_dir)
  identification     <- ppdb$identification
  fate               <- ppdb$fate
  terrestrial_ecotox <- ppdb$terrestrial_ecotox
  aquatic_ecotox     <- ppdb$aquatic_ecotox
  human_tox          <- ppdb$human_tox

  if (synthetic_only) {
    identification <- identification |> filter(`Substance origin` == "Synthetic")
  }

  result <- identification |>
    select(ID, Active, Reference, `EC Regulation 1107/2009 status`, `Pesticide type`,
           `Substance origin`, CAS, `Substance group`) |>
    left_join(
      fate |>
        transmute(
          ID,
          soil_dt50_typical         = parse_ppdb_numeric(`Soil DT50 - Typical (days)`, direction = hazard_direction("soil_dt50"), range_policy = range_policy),
          soil_dt50_typical_quality = parse_quality_band(`QB - Soil DT50 - Typical`),
          soil_dt50_typical_bound   = parse_ppdb_bound(`Soil DT50 - Typical (days)`),
          soil_dt50_lab             = parse_ppdb_numeric(`Soil DT50 - Lab (days)`, direction = hazard_direction("soil_dt50"), range_policy = range_policy),
          soil_dt50_lab_quality     = parse_quality_band(`QB - Soil DT50 - Lab`),
          soil_dt50_lab_bound       = parse_ppdb_bound(`Soil DT50 - Lab (days)`),
          soil_dt50_field           = parse_ppdb_numeric(`Soil DT50 - Field (days)`, direction = hazard_direction("soil_dt50"), range_policy = range_policy),
          soil_dt50_field_quality   = parse_quality_band(`QB - Soil DT50 - Field`),
          soil_dt50_field_bound     = parse_ppdb_bound(`Soil DT50 - Field (days)`),
          water_dt50_text           = as.character(`Water phase only DT50 (days)`),
          water_dt50_raw            = parse_ppdb_numeric(`Water phase only DT50 (days)`, direction = hazard_direction("water_dt50"), range_policy = range_policy),
          water_dt50_raw_quality    = parse_quality_band(`QB - Water phase only DT50`),
          water_dt50_raw_bound      = parse_ppdb_bound(`Water phase only DT50 (days)`),
          kfoc_raw                  = parse_ppdb_numeric(`Kfoc (ml/g)`, direction = hazard_direction("kfoc"), range_policy = range_policy),
          kfoc_raw_quality          = parse_quality_band(`QB - Freundlich isotherm`),
          kfoc_raw_bound            = parse_ppdb_bound(`Kfoc (ml/g)`),
          koc                       = parse_ppdb_numeric(`Koc (ml/g)`, direction = hazard_direction("kfoc"), range_policy = range_policy),
          koc_quality               = parse_quality_band(`QB - Kd & Koc`),
          koc_bound                 = parse_ppdb_bound(`Koc (ml/g)`),
          bcf_raw                   = parse_ppdb_numeric(`Bioconcentration factor`, direction = hazard_direction("bcf"), range_policy = range_policy),
          bcf_raw_quality           = parse_quality_band(`QB - Bioconcentration factor`),
          bcf_raw_bound             = parse_ppdb_bound(`Bioconcentration factor`)
        ),
      by = "ID"
    ) |>
    mutate(
      # Free text, not a measurement, hence the switch: see the argument's
      # description above.
      is_stable_in_water   = water_dt50_stable_assumption &
        (str_detect(water_dt50_text, regex("stable", ignore_case = TRUE)) |> coalesce(FALSE)),

      # Soil DT50 hierarchy: field, else whichever of lab and typical has
      # the better quality band, else whichever exists. The selector is
      # computed once and the value, status, confidence and bound are all
      # derived from it, so the four cannot drift apart.
      soil_dt50_source = case_when(
        !is.na(soil_dt50_field) ~ "field",
        !is.na(soil_dt50_lab) & !is.na(soil_dt50_typical) & soil_dt50_lab_quality > soil_dt50_typical_quality ~ "lab",
        !is.na(soil_dt50_lab) & !is.na(soil_dt50_typical) & soil_dt50_lab_quality < soil_dt50_typical_quality ~ "typical",
        !is.na(soil_dt50_lab) & !is.na(soil_dt50_typical) ~ "lab_typical_mean",
        !is.na(soil_dt50_lab)     ~ "lab",
        !is.na(soil_dt50_typical) ~ "typical",
        TRUE ~ NA_character_
      ),
      soil_dt50 = case_when(
        soil_dt50_source == "field"            ~ soil_dt50_field,
        soil_dt50_source == "lab"              ~ soil_dt50_lab,
        soil_dt50_source == "typical"          ~ soil_dt50_typical,
        soil_dt50_source == "lab_typical_mean" ~ (soil_dt50_lab + soil_dt50_typical) / 2,
        TRUE ~ NA_real_
      ),
      soil_dt50_status = case_when(
        soil_dt50_source == "field" ~ "measured",
        soil_dt50_source %in% c("lab", "typical", "lab_typical_mean") ~ "completed",
        TRUE ~ NA_character_
      ),
      soil_dt50_confidence = case_when(
        soil_dt50_source == "field"            ~ soil_dt50_field_quality,
        soil_dt50_source == "lab"              ~ soil_dt50_lab_quality - 1,
        soil_dt50_source == "typical"           ~ soil_dt50_typical_quality - 1,
        soil_dt50_source == "lab_typical_mean"  ~ pmax(soil_dt50_lab_quality, soil_dt50_typical_quality, na.rm = TRUE) - 1,
        TRUE ~ NA_real_
      ),
      soil_dt50_bound = case_when(
        soil_dt50_source == "field"   ~ soil_dt50_field_bound,
        soil_dt50_source == "lab"     ~ soil_dt50_lab_bound,
        soil_dt50_source == "typical" ~ soil_dt50_typical_bound,
        TRUE ~ "missing" # "lab_typical_mean" is a synthetic average - no single source's bound applies to it
      ),

      # KFOC: the same one-selector pattern, two-way - KFOC, else KOC. The
      # fallback costs one confidence level, as every other completion
      # does; "HPLI methodology.md", section "Data quality traceability",
      # records why this is uniform here.
      kfoc_source = case_when(
        !is.na(kfoc_raw) ~ "kfoc",
        !is.na(koc)      ~ "koc",
        TRUE ~ NA_character_
      ),
      kfoc = case_when(
        kfoc_source == "kfoc" ~ kfoc_raw,
        kfoc_source == "koc"  ~ koc,
        TRUE ~ NA_real_
      ),
      kfoc_status = case_when(
        kfoc_source == "kfoc" ~ "measured",
        kfoc_source == "koc"  ~ "completed",
        TRUE ~ NA_character_
      ),
      kfoc_confidence = case_when(
        kfoc_source == "kfoc" ~ kfoc_raw_quality,
        kfoc_source == "koc"  ~ koc_quality - 1,
        TRUE ~ NA_real_
      ),
      kfoc_bound = case_when(
        kfoc_source == "kfoc" ~ kfoc_raw_bound,
        kfoc_source == "koc"  ~ koc_bound,
        TRUE ~ NA_character_
      ),

      # BCF: one fallback only (the regression, not a second measured
      # field), so no selector is needed to keep the four quantities in
      # step.
      bcf = complete_bcf(bcf_raw, kfoc),
      bcf_status = case_when(
        !is.na(bcf_raw) ~ "measured",
        !is.na(bcf)     ~ "completed",
        TRUE ~ NA_character_
      ),
      bcf_confidence = case_when(
        !is.na(bcf_raw) ~ bcf_raw_quality,
        !is.na(bcf)     ~ kfoc_confidence - 1,
        TRUE ~ NA_real_
      ),
      bcf_bound = case_when(
        !is.na(bcf_raw) ~ bcf_raw_bound,
        TRUE ~ "missing" # a regression estimate has no PPDB comparator to inherit
      ),

      # GUS is always derived, never read from the export, so its status is
      # always "completed" - see "HPLI methodology.md", section "Completing
      # soil DT50, KFOC, BCF and GUS before normalisation".
      gus = compute_gus(soil_dt50, kfoc),
      gus_status = case_when(
        !is.na(gus) ~ "completed",
        TRUE ~ NA_character_
      ),
      gus_confidence = case_when(
        !is.na(gus) ~ pmin(soil_dt50_confidence, kfoc_confidence, na.rm = TRUE) - 1,
        TRUE ~ NA_real_
      ),
      gus_bound = "missing", # a formula-derived value has no PPDB comparator to inherit

      water_dt50 = case_when(
        !is.na(water_dt50_raw) ~ water_dt50_raw,
        is_stable_in_water      ~ 300, # the "very high load" threshold for water DT50
        TRUE ~ NA_real_
      ),
      water_dt50_status = case_when(
        !is.na(water_dt50_raw) ~ "measured",
        is_stable_in_water       ~ "completed",
        TRUE ~ NA_character_
      ),
      water_dt50_confidence = case_when(
        !is.na(water_dt50_raw) ~ water_dt50_raw_quality,
        TRUE ~ NA_real_ # the "stable in water -> 300" assumption has no PPDB quality band to inherit
      ),
      water_dt50_bound = case_when(
        !is.na(water_dt50_raw) ~ water_dt50_raw_bound,
        TRUE ~ "missing"
      )
    ) |>
    left_join(
      terrestrial_ecotox |>
        transmute(
          ID,
          birds_ld50               = parse_ppdb_numeric(`Birds - Acute LD50 (mg/kg)`, direction = hazard_direction("birds_ld50"), range_policy = range_policy),
          birds_ld50_bound         = parse_ppdb_bound(`Birds - Acute LD50 (mg/kg)`, `<> - Birds - Acute LD50`),
          birds_ld50_confidence    = parse_quality_band(`QB - Birds - Acute LD50`),
          earthworms_lc50            = parse_ppdb_numeric(`Earthworms - Acute 14d LC50 (mg/kg)`, direction = hazard_direction("earthworms_lc50"), range_policy = range_policy),
          earthworms_lc50_bound      = parse_ppdb_bound(`Earthworms - Acute 14d LC50 (mg/kg)`, `<> - Earthworms - Acute`),
          earthworms_lc50_confidence = parse_quality_band(`QB - Earthworms - Acute`),
          honeybees_ld50_contact               = parse_ppdb_numeric(`Honeybees - Contact acute 48hr LD50 (ug per bee)`, direction = hazard_direction("honeybees_ld50"), range_policy = range_policy),
          honeybees_ld50_contact_bound         = parse_ppdb_bound(`Honeybees - Contact acute 48hr LD50 (ug per bee)`, `<> - Honeybees - Contact acute 48hr LD50`),
          honeybees_ld50_contact_confidence    = parse_quality_band(`QB - Honeybees - Contact acute 48hr LD50`),
          honeybees_ld50_oral                  = parse_ppdb_numeric(`Honeybees - Oral acute 48hr LD50 (ug per bee)`, direction = hazard_direction("honeybees_ld50"), range_policy = range_policy),
          honeybees_ld50_oral_bound            = parse_ppdb_bound(`Honeybees - Oral acute 48hr LD50 (ug per bee)`, `<> - Honeybees - Oral acute 48hr LD50`),
          honeybees_ld50_oral_confidence       = parse_quality_band(`QB - Honeybees - Oral acute 48hr LD50`),
          honeybees_ld50_unknown               = parse_ppdb_numeric(`Honeybees - Unknown mode acute 48hr LD50 (ug per bee)`, direction = hazard_direction("honeybees_ld50"), range_policy = range_policy),
          honeybees_ld50_unknown_bound         = parse_ppdb_bound(`Honeybees - Unknown mode acute 48hr LD50 (ug per bee)`, `<> - Honeybees - Unknown mode acute 48hr LD50`),
          honeybees_ld50_unknown_confidence    = parse_quality_band(`QB - Honeybees - Unknown mode acute 48hr LD50`),
          mammals_ld50_oral               = parse_ppdb_numeric(`Mammals - Acute oral LD50 (mg/kg BW/day)`, direction = hazard_direction("mammals_ld50_oral"), range_policy = range_policy),
          mammals_ld50_oral_bound         = parse_ppdb_bound(`Mammals - Acute oral LD50 (mg/kg BW/day)`, `<> - Mammals - Acute oral LD50`),
          mammals_ld50_oral_confidence    = parse_quality_band(`QB - Mammals - Acute oral LD50`)
        ),
      by = "ID"
    ) |>
    rowwise() |>
    mutate(
      honeybees_ld50 = min(c(honeybees_ld50_contact, honeybees_ld50_oral, honeybees_ld50_unknown), na.rm = TRUE),
      honeybees_ld50 = ifelse(is.infinite(honeybees_ld50), NA_real_, honeybees_ld50),
      # Contact, oral and unknown-mode LD50 are equally primary
      # measurements, so taking the minimum is a selection rule and not a
      # fallback: the combined metric counts as "measured" whenever any of
      # the three is present, with no confidence penalty.
      honeybees_ld50_status = ifelse(is.na(honeybees_ld50), NA_character_, "measured"),
      honeybees_ld50_bound = case_when(
        is.na(honeybees_ld50) ~ NA_character_,
        honeybees_ld50 == honeybees_ld50_contact ~ honeybees_ld50_contact_bound,
        honeybees_ld50 == honeybees_ld50_oral    ~ honeybees_ld50_oral_bound,
        honeybees_ld50 == honeybees_ld50_unknown ~ honeybees_ld50_unknown_bound,
        TRUE ~ NA_character_
      ),
      honeybees_ld50_confidence = case_when(
        is.na(honeybees_ld50) ~ NA_real_,
        honeybees_ld50 == honeybees_ld50_contact ~ honeybees_ld50_contact_confidence,
        honeybees_ld50 == honeybees_ld50_oral    ~ honeybees_ld50_oral_confidence,
        honeybees_ld50 == honeybees_ld50_unknown ~ honeybees_ld50_unknown_confidence,
        TRUE ~ NA_real_
      )
    ) |>
    ungroup() |>
    left_join(
      aquatic_ecotox |>
        transmute(
          ID,
          algae_ec50             = parse_ppdb_numeric(`Algae - Acute (growth rate, fresh - mg/l)`, direction = hazard_direction("algae_ec50"), range_policy = range_policy),
          algae_ec50_bound       = parse_ppdb_bound(`Algae - Acute (growth rate, fresh - mg/l)`, `<> - Algae - Acute 72hr EC50 growth`),
          algae_ec50_confidence  = parse_quality_band(`QB - Algae - Acute 72hr EC50 growth`),
          aq_invertebrates_ec50            = parse_ppdb_numeric(`Aquatic invertebrates - Acute 48hr EC50 (mg/l) - TEMPERATE`, direction = hazard_direction("aq_invertebrates_ec50"), range_policy = range_policy),
          aq_invertebrates_ec50_bound      = parse_ppdb_bound(`Aquatic invertebrates - Acute 48hr EC50 (mg/l) - TEMPERATE`, `<> - Aquatic invertebrates - Acute 48hr EC50 - TEMPERATE`),
          aq_invertebrates_ec50_confidence = parse_quality_band(`QB - Aquatic invertebrates - Acute 48hr EC50 - TEMPERATE`),
          aq_invertebrates_noec            = parse_ppdb_numeric(`Aquatic invertebrates - Chronic 21d NOEC (mg/l) - TEMPERATE`, direction = hazard_direction("aq_invertebrates_noec"), range_policy = range_policy),
          aq_invertebrates_noec_bound      = parse_ppdb_bound(`Aquatic invertebrates - Chronic 21d NOEC (mg/l) - TEMPERATE`, `<> - Aquatic invertebrates - Chronic 21d NOEC - TEMPERATE`),
          aq_invertebrates_noec_confidence = parse_quality_band(`QB - Aquatic invertebrates - Chronic 21d NOEC - TEMPERATE`),
          fish_lc50               = parse_ppdb_numeric(`Fish - Acute 96hr LC50 (mg/l) - TEMPERATE`, direction = hazard_direction("fish_lc50"), range_policy = range_policy),
          fish_lc50_bound         = parse_ppdb_bound(`Fish - Acute 96hr LC50 (mg/l) - TEMPERATE`, `<> - Fish - Acute 96hr LC50 - TEMPERATE`),
          fish_lc50_confidence    = parse_quality_band(`QB - Fish - Acute 96hr LC50 - TEMPERATE`),
          fish_noec               = parse_ppdb_numeric(`Fish - Chronic 21d NOEC (mg/l) - TEMPERATE`, direction = hazard_direction("fish_noec"), range_policy = range_policy),
          fish_noec_bound         = parse_ppdb_bound(`Fish - Chronic 21d NOEC (mg/l) - TEMPERATE`, `<> - Fish - Chronic 21d NOEC - TEMPERATE`),
          fish_noec_confidence    = parse_quality_band(`QB - Fish - Chronic 21d NOEC - TEMPERATE`)
        ),
      by = "ID"
    ) |>
    left_join(
      human_tox |>
        transmute(
          ID,
          mammals_ld50_dermal        = parse_ppdb_numeric(`Mammals - Dermal LD50 (mg/kg)`, direction = hazard_direction("mammals_ld50_dermal"), range_policy = range_policy),
          mammals_ld50_dermal_bound      = parse_ppdb_bound(`Mammals - Dermal LD50 (mg/kg)`, `<> - Mammals - Dermal LD50`),
          mammals_ld50_dermal_confidence = parse_quality_band(`QB - Mammals - Dermal LD50`),
          mammals_lc50_inhalation        = parse_ppdb_numeric(`Mammals - Inhalation LC50 (mg/l)`, direction = hazard_direction("mammals_lc50_inhalation"), range_policy = range_policy),
          mammals_lc50_inhalation_bound      = parse_ppdb_bound(`Mammals - Inhalation LC50 (mg/l)`, `<> - Mammals - Inhalation LC50`),
          mammals_lc50_inhalation_confidence = parse_quality_band(`QB - Mammals - Inhalation LC50`),
          carcinogenicity            = parse_ppdb_category(`Carcinogen?`),
          cholinesterase_inhibition  = parse_ppdb_category(`Acetyl cholinesterase inhibitor?`),
          neurotoxicity              = parse_ppdb_category(`Neurotoxicant?`),
          reprotoxicity              = parse_ppdb_category(`Reproduction/development effects?`)
        ),
      by = "ID"
    )

  # Single-source metrics: no hierarchy, so "measured" whenever the value
  # is present and nothing else ever. A loop rather than ten more
  # near-identical mutate() calls.
  single_source_numeric_metrics <- c(
    "birds_ld50", "earthworms_lc50", "mammals_ld50_oral",
    "algae_ec50", "aq_invertebrates_ec50", "aq_invertebrates_noec",
    "fish_lc50", "fish_noec",
    "mammals_ld50_dermal", "mammals_lc50_inhalation"
  )
  for (m in single_source_numeric_metrics) {
    result[[paste0(m, "_status")]] <- ifelse(!is.na(result[[m]]), "measured", NA_character_)
  }

  # Categorical human-toxicity metrics: "measured" whenever present, no
  # bound or PPDB confidence band exists for a yes/no/possible call (see
  # human_column_map comment above).
  categorical_metrics <- c("carcinogenicity", "cholinesterase_inhibition", "neurotoxicity", "reprotoxicity")
  for (m in categorical_metrics) {
    result[[paste0(m, "_status")]]     <- ifelse(!is.na(result[[m]]), "measured", NA_character_)
    result[[paste0(m, "_bound")]]      <- "missing" # no PPDB comparator concept exists for a yes/no/possible field
    result[[paste0(m, "_confidence")]] <- NA_real_
  }

  # The 20 HPLI-EU metrics, after applying the completion hierarchy above,
  # plus their status/bound/confidence companions.
  result
}

# ──────────────────────────────────────────────────────────────────────────
# Output provenance and licence notice                                   ####
# ──────────────────────────────────────────────────────────────────────────

#' Identify a PPDB export without recording where it is stored
#'
#' @param ppdb_export_dir Folder holding the four-file PPDB export.
#' @return A tibble of `setting` and `value` rows, ready to bind into a
#'   `Run_log`: `ppdb_export_folder`, the folder's own name, then one
#'   `ppdb_md5_<file>` row per workbook holding its MD5 checksum.
#' @details
#' A run log travels with its workbook, so it records no local path: the
#' folder name says which export was meant, and the checksums say whether
#' two runs read byte-identical files - including whether a weights file
#' and a scoring run came from the same export. The full path is still
#' echoed to the console by `load_local_paths()`.
compute_ppdb_fingerprint <- function(ppdb_export_dir) {
  export_files <- c(general = "General.xlsx", fate = "Fate.xlsx", ecotox = "Ecotox.xlsx", human = "Human.xlsx")
  tibble(
    setting = c("ppdb_export_folder", paste0("ppdb_md5_", names(export_files))),
    value   = c(basename(ppdb_export_dir), unname(tools::md5sum(file.path(ppdb_export_dir, export_files))))
  )
}

#' Licence and citation notice for an output workbook
#'
#' @param contents Character scalar: what this workbook holds.
#' @param redistribution Character scalar: what may be done with it.
#' @return A tibble of two columns, `item` and `text`, one row per
#'   statement, written as the first sheet of the workbook.
#' @details
#' Output workbooks circulate on their own, away from the repository and
#' its README, so each one states its own terms: it is derived from the
#' PPDB, it falls under neither the code's licence nor the methodology's,
#' and it is subject to the AERU conditions of use. The sentence under
#' "Source data" is the acknowledgement the PPDB licence requires, verbatim.
#' See "HPLI methodology.md", section "PPDB licensing and what may be
#' shared".
build_output_notice <- function(contents, redistribution) {
  tibble(
    item = c(
      "Contents",
      "Source data",
      "Terms of use",
      "Redistribution",
      "Provenance",
      "Code",
      "Methodology",
      "Cite the HPLI",
      "Cite the dissertation",
      "Cite the implementation",
      "Cite the PPDB",
      "Cite the PPDB"
    ),
    text = c(
      contents,
      paste(
        "Derived from the Pesticide Properties DataBase (PPDB), Agriculture and Environment",
        "Research Unit (AERU), University of Hertfordshire. Data from the University of",
        "Hertfordshire's Pesticide Properties DataBase (PPDB) has been used, under Licence,",
        "to support this application."
      ),
      paste(
        "This workbook is covered neither by the GPL-3.0-or-later licence of the code nor by",
        "the CC BY 4.0 licence of the methodology. It is subject to the AERU terms and",
        "conditions of use of the PPDB:",
        "https://sitem.herts.ac.uk/aeru/ppdb/en/docs/Conditions_of_use.pdf"
      ),
      redistribution,
      "See the Run_log sheet: run timestamp, every setting, and the checksums of the PPDB export read.",
      paste(
        "HPLI R implementation, https://github.com/noevandevoorde/HPLI.",
        "Copyright (C) 2026 UCLouvain; author Noé Vandevoorde. Licensed under GPL-3.0-or-later."
      ),
      paste(
        "\"HPLI methodology.md\", in the same repository.",
        "Licensed under CC BY 4.0, https://creativecommons.org/licenses/by/4.0/."
      ),
      "Vandevoorde, N. et al. (2025). Environmental Research Letters. https://doi.org/10.1088/1748-9326/ae269b",
      "Vandevoorde, N. (2025). Three tools for the reduction of pesticide impacts. UCLouvain. https://hdl.handle.net/2078.5/264116",
      "HPLI R implementation. https://github.com/noevandevoorde/HPLI",
      paste(
        "Tzilivakis, J., Lewis, K.A., Green, A. and Warner, D.J. (2026). A decade of growth and",
        "impact of the Pesticide Properties Database (PPDB). Human and Ecological Risk Assessment:",
        "An International Journal, 1-26. https://doi.org/10.1080/10807039.2026.2702066"
      ),
      paste(
        "Lewis, K.A., Tzilivakis, J., Warner, D. and Green, A. (2016). An international database for",
        "pesticide risk assessments and management. Human and Ecological Risk Assessment:",
        "An International Journal, 22(4), 1050-1064. https://doi.org/10.1080/10807039.2015.1133242"
      )
    )
  )
}
