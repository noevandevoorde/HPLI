# SPDX-License-Identifier: GPL-3.0-or-later
# ──────────────────────────────────────────────────────────────────────────
# HPLI score calculation from a PPDB/BPDB export (HPLI-EU, 20 metrics)   ####
# ──────────────────────────────────────────────────────────────────────────

#' HPLI-EU score for every substance of a PPDB export
#'
#' Computes the Harmonised Pesticide Load Indicator, HPLI-EU version
#' (20 hazard metrics), for every substance of a PPDB/BPDB export: each
#' metric is normalised onto the 0-1.5 hazard scale, chronic metrics are
#' attenuated by the persistence coefficient, and the results are
#' aggregated by compartment into one score per substance.
#'
#' **This is a load score, not a risk score**: the intrinsic hazard of one
#' kg of active substance, independent of how much is applied. Comparing
#' two substances' HPLI values says which is more hazardous per kg, not
#' which contributes more hazard in practice - that needs multiplying by a
#' use quantity (annual applied quantity for the risk score, a product's
#' Maximum Authorised Dose for the Pesti-Score), which this script does
#' not do. See "HPLI methodology.md", section "Purpose and scope".
#'
#' Two sibling scripts in the same folder carry what a different HPLI
#' version would change, so that a variant means sourcing different
#' siblings rather than editing this file:
#' * "HPLI parameters.R" - the indicator definition: metrics, thresholds,
#'   compartments, default weights.
#' * "HPLI import.R" - reading and completing the metrics from the export.
#'   Shared with "HPLI weights.R", so both derive metrics identically.
#'
#' @section Settings:
#' Set in section 1 below.
#' * `ppdb_export_dir` - the export folder. Read from the untracked
#'   "local_paths.R" rather than set here.
#' * `output_file` - the publishable workbook: aggregated scores and data
#'   quality, no per-metric score.
#' * `full_output_file` - the complete workbook, per-metric scores
#'   included, kept local.
#' * `low_hazard_reference_file` - substances whose missing fate metrics
#'   mean "no such pathway" rather than "unknown". Optional.
#' * `missing_policy` - "hpli_precautionary" or "complete_only".
#' * `coverage_threshold` - minimum share of metrics with data for a
#'   substance to be scored.
#' * `range_policy` - "worst_case" or "mean".
#' * `synthetic_only` - restrict the run to synthetic-origin substances.
#' * `a_soil`, `a_water` - persistence-coefficient constants, in days.
#' * `weight_source`, `weights_file` - hardcoded weights, or weights
#'   recomputed by "HPLI weights.R".
#' * `export_raw_ppdb_values` - whether raw PPDB values reach the full
#'   workbook. Never the publishable one.
#' * `data_quality_scope` - how much of the traceability detail to export.
#'
#' @section Output:
#' Two workbooks per run, with the same five sheets:
#' * `Notice` - what the workbook holds, its terms of use (derived from the
#'   PPDB, subject to the AERU conditions of use), and how to cite it.
#' * `HPLI_results` - one row per substance: the HPLI and compartment
#'   scores, and a compact data-quality summary.
#' * `HPLI_parameters` - the indicator definition this run actually used.
#' * `Data_quality` - one row per substance and metric: what happened to
#'   each value.
#' * `Run_log` - the run's full provenance, without any local path.
#'
#' `output_file` is the publishable one, the only one meant to be shared. Its
#' `HPLI_results` sheet leaves out every per-metric score: the
#' normalisation is piecewise linear and monotone, so a per-metric score
#' can be converted back into the PPDB value it came from, except where
#' the score is floored at 0. `full_output_file` adds those scores - and,
#' with `export_raw_ppdb_values`, the raw values - and is listed in
#' ".gitignore". "HPLI visualisation.R" reads the full one. See
#' "HPLI methodology.md", section "PPDB licensing and what may be shared".
#'
#' @section Method and rationale:
#' Vandevoorde et al. (2025), and "HPLI methodology.md" in this folder for
#' every choice these scripts implement - thresholds, missing-data policy,
#' where the persistence coefficient applies, the completion hierarchy,
#' traceability, open points and planned extensions. That document is the
#' record of why; this script is the how. Keep the two in step.

library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(tibble)
library(writexl)

source("HPLI parameters.R")
source("HPLI import.R")

# ──────────────────────────────────────────────────────────────────────────
# 1. Settings                                                            ####
# ──────────────────────────────────────────────────────────────────────────

# `ppdb_export_dir` - the folder holding General.xlsx, Fate.xlsx,
# Ecotox.xlsx and Human.xlsx - comes from "local_paths.R" (untracked; copy
# "local_paths.example.R"), which is read here and must define it. A
# missing file, a missing setting or a folder that does not exist each stop
# the run with an explicit message; the folder in use is echoed to the
# console. See load_local_paths() in "HPLI import.R", and the note there on
# adapting the column maps to an export more recent than the 2024-05-03 one
# this code was written against.
load_local_paths(require_dirs = "ppdb_export_dir")

# Two workbooks per run. output_file is publishable: HPLI, compartment
# scores and data quality, no per-metric score. full_output_file holds
# everything, per-metric scores included, and is listed in ".gitignore":
# those scores can be converted back into PPDB values (see the note on
# export_raw_ppdb_values below), so it stays local.
# "HPLI visualisation.R" reads full_output_file.
output_file      <- "HPLI_results.xlsx"
full_output_file <- "HPLI_results_full.xlsx"

# Substances hand-verified as chemically inert with no plausible leaching
# or accumulation pathway, matched by PPDB ID (stable across versions,
# unlike names). For these, a missing environmental-fate metric is read as
# "this pathway does not apply" rather than "unknown, assume worst case".
# Optional: without the file every substance is treated precautionarily.
# See "HPLI methodology.md", section "Completing soil DT50, KFOC, BCF and
# GUS before normalisation".
low_hazard_reference_file <- "HPLI score - natural inert substances.csv"

# "hpli_precautionary": a missing value is replaced by the metric's
#   high-hazard threshold (the 1.00 level), not the more extreme 1.50 one.
# "complete_only": no HPLI at all while any of the 20 metrics is missing.
missing_policy <- "hpli_precautionary"

# Substances below this share of metrics with data are not scored. This is
# the substance-side bar only. The same 60% figure also selects the metrics
# themselves (see "HPLI methodology.md", section "Missing-data and
# range-value policy"), but that selection is upstream of this code and not
# performed here, so the two are not the same setting and are meant to stay
# independently adjustable.
coverage_threshold <- 0.60

# How a range value such as "10-20" becomes the single number
# normalisation needs, in parse_ppdb_numeric() ("HPLI import.R").
# "worst_case" takes the more hazardous end - the same precaution as
# missing_policy, applied to an imprecise value rather than an absent one.
# "mean" averages the two ends, for a sensitivity comparison rather than as
# an alternative default.
range_policy <- "worst_case"

# TRUE restricts the run to substances the PPDB labels synthetic in origin,
# as indicators reporting on synthetic actives only require. The published
# HPLI-EU scores natural-origin substances too, so FALSE is the default.
# Unrelated to the completion logic below, which treats every substance the
# same way whatever its origin.
synthetic_only <- FALSE

# Persistence-coefficient reference windows, in days, for the chronic
# metrics of section 7. These are the published values. An alternative pair
# derived from threshold midpoints (230 and 33) exists in the thesis and is
# listed as a planned extension in "HPLI methodology.md", section "Planned
# extensions"; it is not selectable here.
a_soil  <- 180
a_water <- 7

# Where each metric's weight comes from. "table1" uses the published
# values hardcoded in "HPLI parameters.R". "spearman_ppdb" loads
# weights_file, the output of "HPLI weights.R". The loaded file is checked
# against the active metric list before use and refused on any mismatch, and
# its provenance is carried into Run_log, so a weights file computed for a
# different HPLI version cannot be applied silently.
weight_source <- "table1"
weights_file  <- "HPLI_weights.xlsx"

# TRUE adds every substance's raw and completed PPDB values to the
# "HPLI_results" sheet of full_output_file; FALSE leaves them out. It never
# affects output_file, which carries neither raw values nor per-metric
# scores. The two are closer than they look: each normalisation is
# piecewise linear and monotone over thresholds published in
# "HPLI_parameters", so a 0-1.5 metric score converts back into its raw
# value except where the score is floored at 0.
# The full workbook is therefore licensed PPDB-derived material in either
# setting, and stays local - see "HPLI methodology.md", section "PPDB
# licensing and what may be shared".
export_raw_ppdb_values <- FALSE

# "computable_only" restricts the exported "Data_quality" sheet to the
# substances that were actually scored; an excluded substance already has
# its reason summarised in "HPLI_results". "all_substances" keeps every
# substance's per-metric detail, roughly tripling that sheet.
data_quality_scope <- "computable_only"

# ──────────────────────────────────────────────────────────────────────────
# 2. Load weights (if requested) and read PPDB/BPDB data                 ####
# ──────────────────────────────────────────────────────────────────────────

# Empty when weight_source is "table1", so section 9 appends it to Run_log
# unconditionally: a run on the hardcoded weights simply adds no rows.
weights_meta <- tibble(setting = character(0), value = character(0))

if (weight_source == "spearman_ppdb") {
  if (!file.exists(weights_file)) {
    stop(
      "weight_source is \"spearman_ppdb\" but '", weights_file, "' does not exist - ",
      "run \"HPLI weights.R\" first, or set weight_source back to \"table1\".",
      call. = FALSE
    )
  }
  computed_weights <- read_excel(weights_file, sheet = "Weights")
  weights_meta      <- read_excel(weights_file, sheet = "Run_log")

  # The weights file must cover exactly the metrics this version defines,
  # same names and same count, or it is refused: a file computed for
  # another metric set would otherwise be applied silently.
  expected_metrics <- sort(hpli_parameters$metric)
  found_metrics    <- sort(computed_weights$metric)
  if (!identical(expected_metrics, found_metrics)) {
    stop(
      "weight_source is \"spearman_ppdb\" but '", weights_file, "' does not match the current ",
      "indicator definition. Expected ", length(expected_metrics), " metrics (",
      paste(expected_metrics, collapse = ", "), "); found ", length(found_metrics), " (",
      paste(found_metrics, collapse = ", "), "). Re-run \"HPLI weights.R\" against the current ",
      "\"HPLI parameters.R\", or set weight_source back to \"table1\".",
      call. = FALSE
    )
  }

  hpli_parameters <- hpli_parameters |>
    select(-weight) |>
    left_join(computed_weights |> select(metric, weight), by = "metric")

  # Full provenance rather than just a timestamp, so a stale or
  # unexpectedly scoped weights file shows up at a glance.
  message("Loaded weights from '", weights_file, "':")
  for (i in seq_len(nrow(weights_meta))) {
    message("  ", weights_meta$setting[i], ": ", weights_meta$value[i])
  }
} else if (weight_source != "table1") {
  stop('weight_source must be "table1" or "spearman_ppdb".', call. = FALSE)
}

raw_metrics <- load_ppdb_raw_metrics(
  ppdb_export_dir = ppdb_export_dir,
  range_policy = range_policy,
  synthetic_only = synthetic_only
)

if (file.exists(low_hazard_reference_file)) {
  low_hazard_reference <- read_csv2(low_hazard_reference_file, show_col_types = FALSE)
  low_hazard_ids <- low_hazard_reference$ppdb_id[low_hazard_reference$status == "included"]
} else {
  message(
    "Note: '", low_hazard_reference_file, "' not found - every substance uses the ",
    "standard worst-case precautionary substitution (see \"HPLI methodology.md\", ",
    "natural-compound section)."
  )
  low_hazard_ids <- integer(0)
}

# ──────────────────────────────────────────────────────────────────────────
# 6. Score computation                                                   ####
# ──────────────────────────────────────────────────────────────────────────

scored <- raw_metrics |>
  mutate(
    available_metrics = rowSums(!is.na(across(all_of(metric_cols)))),
    data_coverage      = available_metrics / length(metric_cols),
    missing_metrics    = pmap_chr(across(all_of(metric_cols)), function(...) {
      vals <- list(...)
      paste(metric_cols[map_lgl(vals, is.na)], collapse = "; ")
    })
  )

if (missing_policy == "hpli_precautionary") {
  for (m in metric_cols) {
    scored[[m]] <- ifelse(is.na(scored[[m]]), high_hazard_value(m), scored[[m]])
  }
} else if (missing_policy != "complete_only") {
  stop('missing_policy must be "complete_only" or "hpli_precautionary".', call. = FALSE)
}

# water_dt50 is fully completed at this point, and is kept aside BEFORE the
# low-hazard override below, so that the persistence coefficient (section
# 7) still uses it for substances whose environmental-fate score is
# overridden to null hazard. Without this, a substance on the low-hazard
# list that has a real chronic aquatic value would see it zeroed through
# k_water along with its fate score.
water_dt50_for_k <- scored$water_dt50

# Hoisted unconditionally rather than into the branch below, since section
# 7.5 needs it whether or not low_hazard_reference_file was found.
environmental_fate_cols <- hpli_parameters$metric[hpli_parameters$compartment == "Environmental fate"]

# For the substances of low_hazard_ids, a still-missing environmental-fate
# metric means the pathway it measures - persistence, mobility,
# bioconcentration - does not apply, so it gets the null-hazard threshold
# rather than the worst case. Real data took priority above and is left
# untouched. The null-hazard value is not 0 for every metric: see
# low_hazard_value() ("HPLI parameters.R").
if (missing_policy == "hpli_precautionary" && length(low_hazard_ids) > 0) {
  is_low_hazard_row <- scored$ID %in% low_hazard_ids
  for (m in environmental_fate_cols) {
    was_missing <- is.na(raw_metrics[[m]])
    scored[[m]][is_low_hazard_row & was_missing] <- low_hazard_value(m)
  }
}

for (m in metric_cols) {
  scored[[paste0("score_", m)]] <- map_dbl(scored[[m]], score_one_metric, metric_name = m)
}

# ──────────────────────────────────────────────────────────────────────────
# 7. Persistence coefficient (chronic-effect metrics)                    ####
# ──────────────────────────────────────────────────────────────────────────

# k_water uses water_dt50_for_k (section 6), not scored$water_dt50: a
# substance on the low-hazard list keeps a normal persistence coefficient
# for its aquatic ecotoxicity. Only its environmental-fate score is treated
# as null hazard; whether that exposure pathway is persistence-related has
# not been established either way, and zeroing it would discard real data.
k_water <- persistence_coefficient(water_dt50_for_k, a_water)
k_soil  <- persistence_coefficient(scored$soil_dt50, a_soil)
k_human <- rowMeans(cbind(k_water, k_soil), na.rm = TRUE)

# Human chronic toxicity only: for these substances the documented chronic
# hazard comes from repeated inhalation, not from environmental
# persistence, so k_human is forced to 0 rather than left to the
# k_soil/k_water average - which would only be partly reduced, k_water
# being deliberately untouched above.
k_human[scored$ID %in% low_hazard_ids] <- 0

scored$score_aq_invertebrates_noec <- scored$score_aq_invertebrates_noec * k_water
scored$score_fish_noec             <- scored$score_fish_noec * k_water

scored$score_carcinogenicity           <- scored$score_carcinogenicity * k_human
scored$score_cholinesterase_inhibition <- scored$score_cholinesterase_inhibition * k_human
scored$score_neurotoxicity             <- scored$score_neurotoxicity * k_human
scored$score_reprotoxicity             <- scored$score_reprotoxicity * k_human

# ──────────────────────────────────────────────────────────────────────────
# 7.5 Data quality traceability                                          ####
# ──────────────────────────────────────────────────────────────────────────
#
# Records, per substance and metric, what happened to that value, where
# missing_metrics (section 6) only lists which ones were absent. Design and
# reasoning: "HPLI methodology.md", section "Data quality traceability".
#
# Three independent fields per metric:
#   - status: "measured" (the primary PPDB field), "completed" (from a
#     secondary field or a formula, both in "HPLI import.R"),
#     "substituted_precautionary" or "substituted_null_hazard" (filled in
#     at scoring time by sections 6 and 7), or "missing" (reachable only
#     under missing_policy == "complete_only", where nothing is
#     substituted). "imputed_related_substance" and "imputed_family_mean"
#     are reserved for planned extensions and produced by no code path
#     today; they are named here so the schema will not need revisiting.
#   - bound: "exact", "<", ">", "range", or "missing" for every case where
#     no bound applies at all.
#   - confidence: the PPDB's 1-5 quality band, one level lower when the
#     value was completed, NA when it was substituted and so never
#     measured.
#
# The value itself is not repeated here - it is already in "HPLI_results".
# This sheet is about provenance.
#
# The comparison works because raw_metrics still holds a real NA for every
# missing metric: the substitution of section 6 wrote into scored, not into
# raw_metrics, so the two together say cell by cell whether a substitution
# happened and which one.
data_quality <- map_dfr(metric_cols, function(m) {
  raw_value         <- raw_metrics[[m]]
  was_missing       <- is.na(raw_value)
  is_low_hazard_row <- scored$ID %in% low_hazard_ids & m %in% environmental_fate_cols

  status <- case_when(
    !was_missing ~ raw_metrics[[paste0(m, "_status")]],
    was_missing & is_low_hazard_row & missing_policy == "hpli_precautionary" ~ "substituted_null_hazard",
    was_missing & missing_policy == "hpli_precautionary" ~ "substituted_precautionary",
    TRUE ~ "missing" # missing_policy == "complete_only": left missing, substance excluded downstream (section 8)
  )

  tibble(
    ID         = scored$ID,
    Active     = scored$Active,
    metric     = m,
    status     = status,
    bound      = if_else(was_missing, "missing", raw_metrics[[paste0(m, "_bound")]]),
    confidence = if_else(was_missing, NA_real_, raw_metrics[[paste0(m, "_confidence")]])
  )
}) |>
  mutate(metric = factor(metric, levels = metric_cols)) |>
  arrange(ID, metric) |>
  mutate(metric = as.character(metric))

# Compact per-substance counts, one row per ID, so the main results sheet
# can be filtered and sorted without opening Data_quality. The per-metric
# detail stays in that sheet rather than becoming some 60 extra columns
# here.
data_quality_summary <- data_quality |>
  group_by(ID) |>
  summarise(
    n_measured                  = sum(status == "measured", na.rm = TRUE),
    n_completed                 = sum(status == "completed", na.rm = TRUE),
    n_substituted_precautionary = sum(status == "substituted_precautionary", na.rm = TRUE),
    n_substituted_null_hazard   = sum(status == "substituted_null_hazard", na.rm = TRUE),
    n_imputed                   = sum(status %in% c("imputed_related_substance", "imputed_family_mean"), na.rm = TRUE),
    n_unbounded                 = sum(bound %in% c("<", ">"), na.rm = TRUE),
    n_range                     = sum(bound == "range", na.rm = TRUE),
    .groups = "drop"
  )

# ──────────────────────────────────────────────────────────────────────────
# 8. Aggregation                                                         ####
# ──────────────────────────────────────────────────────────────────────────

score_cols <- paste0("score_", metric_cols)
weight_vec <- setNames(hpli_parameters$weight, paste0("score_", hpli_parameters$metric))
compartments <- unique(hpli_parameters$compartment)

results <- scored |>
  rowwise() |>
  mutate(
    can_calculate = if_else(
      missing_policy == "complete_only",
      all(!is.na(c_across(all_of(score_cols)))),
      data_coverage >= coverage_threshold
    ),
    HPLI = if_else(
      can_calculate,
      sum(c_across(all_of(score_cols)) * weight_vec[score_cols], na.rm = FALSE),
      NA_real_
    )
  ) |>
  ungroup()

for (comp in compartments) {
  comp_cols <- score_cols[hpli_parameters$compartment == comp]
  comp_name <- paste0("HPLI_", str_replace_all(str_to_lower(comp), "[^a-z]+", "_"))
  results[[comp_name]] <- rowSums(
    sweep(as.matrix(results[comp_cols]), 2, weight_vec[comp_cols], `*`),
    na.rm = FALSE
  )
  results[[comp_name]][!results$can_calculate] <- NA_real_
}

results <- results |> arrange(desc(can_calculate), desc(HPLI))

# ──────────────────────────────────────────────────────────────────────────
# 8.5 Substituted/completed contribution shares                          ####
# ──────────────────────────────────────────────────────────────────────────
#
# What share of the HPLI, and of each compartment score, comes from a
# metric that was not measured. Substituted and completed values are
# reported as two separate shares rather than one: a completed value is
# still real PPDB-derived data, qualitatively different from a worst-case
# fill-in.
#
# Built from data_quality (section 7.5, still covering every substance at
# this point), joined against each metric's weight and compartment and
# against its score after the persistence coefficient of section 7 - the
# same values the HPLI columns above are built from.
metric_scores_long <- results |>
  select(ID, all_of(score_cols)) |>
  pivot_longer(-ID, names_to = "metric", values_to = "score") |>
  mutate(metric = str_remove(metric, "^score_"))

contributions <- data_quality |>
  select(ID, metric, status) |>
  left_join(hpli_parameters |> select(metric, compartment, weight), by = "metric") |>
  left_join(metric_scores_long, by = c("ID", "metric")) |>
  mutate(weighted_score = weight * score)

#' Share of a score attributable to values that were not measured
#'
#' @param df The per-substance, per-metric contributions: one row per
#'   substance and metric, with `status`, `weight` and `weighted_score`.
#' @param ... Grouping columns, passed on to `group_by()`: `ID` for the
#'   overall share, `ID` and `compartment` for the per-compartment ones.
#' @return A tibble with one row per group and two columns beside the
#'   grouping ones: `pct_substituted` and `pct_completed`, each the
#'   percentage of the group's total weighted score contributed by metrics
#'   of that provenance.
#' @details
#' Weighted by each metric's own weight and score, so the shares say how
#' much of the number actually rests on values that were not measured -
#' not merely how many metrics were missing.
substitution_share <- function(df, ...) {
  df |>
    group_by(...) |>
    summarise(
      total_weighted       = sum(weighted_score, na.rm = TRUE),
      substituted_weighted = sum(weighted_score[status %in% c("substituted_precautionary", "substituted_null_hazard")], na.rm = TRUE),
      completed_weighted   = sum(weighted_score[status == "completed"], na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      pct_substituted = 100 * substituted_weighted / total_weighted,
      pct_completed   = 100 * completed_weighted / total_weighted
    ) |>
    select(-total_weighted, -substituted_weighted, -completed_weighted)
}

overall_shares <- substitution_share(contributions, ID)

compartment_shares <- substitution_share(contributions, ID, compartment) |>
  mutate(comp_slug = str_replace_all(str_to_lower(compartment), "[^a-z]+", "_")) |>
  select(-compartment) |>
  pivot_wider(
    names_from = comp_slug,
    values_from = c(pct_substituted, pct_completed),
    names_glue = "{.value}_{comp_slug}"
  )

# NA for an excluded substance, matching the HPLI columns: a share of a
# score that was not itself reported is not meaningful, even though the
# underlying sums do still compute.
substitution_shares <- overall_shares |>
  left_join(compartment_shares, by = "ID") |>
  left_join(results |> select(ID, can_calculate), by = "ID") |>
  mutate(across(-c(ID, can_calculate), ~ if_else(can_calculate, .x, NA_real_))) |>
  select(-can_calculate)

results_main <- results |>
  left_join(data_quality_summary, by = "ID") |>
  left_join(substitution_shares, by = "ID") |>
  select(
    ID, Active, Reference, CAS, `Substance origin`, `Pesticide type`, `Substance group`,
    `EC Regulation 1107/2009 status`,
    can_calculate, data_coverage, available_metrics, missing_metrics,
    n_measured, n_completed, n_substituted_precautionary, n_substituted_null_hazard,
    n_imputed, n_unbounded, n_range,
    HPLI, starts_with("HPLI_"),
    pct_substituted, pct_completed, starts_with("pct_substituted_"), starts_with("pct_completed_"),
    if (export_raw_ppdb_values) all_of(metric_cols) else NULL,
    all_of(score_cols)
  )

# The publishable results: the full ones without any per-metric score, and
# without the raw values whatever export_raw_ppdb_values says (section 1).
results_published <- results_main |>
  select(-all_of(score_cols), -any_of(metric_cols))

# ──────────────────────────────────────────────────────────────────────────
# 9. Export                                                              ####
# ──────────────────────────────────────────────────────────────────────────

# No local path: the export is identified by its folder name and the
# checksums of its four workbooks (compute_ppdb_fingerprint(),
# "HPLI import.R"), and the output files by name only. weights_meta (empty
# under weight_source "table1") is prefixed "weights_" and appended, so a
# loaded weights file's full provenance is recorded in this run's own
# output and not only in the console.
log_summary <- bind_rows(
  tibble(
    setting = c("run_timestamp", "output_file", "full_output_file"),
    value   = c(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), basename(output_file), basename(full_output_file))
  ),
  compute_ppdb_fingerprint(ppdb_export_dir),
  tibble(
    setting = c("missing_policy", "range_policy",
                "coverage_threshold", "weight_source", "export_raw_ppdb_values", "data_quality_scope",
                "n_rows", "n_calculated", "n_not_calculated"),
    value = c(missing_policy, range_policy, as.character(coverage_threshold),
              weight_source, as.character(export_raw_ppdb_values), data_quality_scope,
              as.character(nrow(results_main)), as.character(sum(results_main$can_calculate)), as.character(sum(!results_main$can_calculate)))
  ),
  weights_meta |> mutate(setting = paste0("weights_", setting))
)

# The two workbooks share one run log and differ by a first "file_role"
# row, so either file says on its own which of the two it is.
file_role_published <- "publishable: HPLI and compartment scores with data quality, no per-metric score"
file_role_full <- paste0(
  "full, local only: per-metric scores",
  if (export_raw_ppdb_values) " and raw PPDB values" else "",
  " included - licensed PPDB-derived material, not to be shared"
)
log_published <- bind_rows(tibble(setting = "file_role", value = file_role_published), log_summary)
log_full      <- bind_rows(tibble(setting = "file_role", value = file_role_full), log_summary)

# data_quality still covers every substance at this point, as section 8.5
# needs. data_quality_scope governs only what is exported here.
data_quality_sheet <- if (identical(data_quality_scope, "computable_only")) {
  data_quality |> semi_join(results_main |> filter(can_calculate), by = "ID")
} else if (identical(data_quality_scope, "all_substances")) {
  data_quality
} else {
  stop('data_quality_scope must be "computable_only" or "all_substances".', call. = FALSE)
}

parameters_out <- hpli_parameters |>
  left_join(
    hazard_breakpoints |>
      mutate(
        threshold_x     = map_chr(x, ~ paste(.x, collapse = "; ")),
        threshold_score = map_chr(y, ~ paste(.x, collapse = "; "))
      ) |>
      select(metric, threshold_x, threshold_score),
    by = "metric"
  ) |>
  # Categorical metrics have no thresholds to interpolate between, so the
  # join leaves their threshold columns blank. Filled in here for
  # readability only: both columns are already character.
  mutate(
    threshold_x     = if_else(metric_type == "categorical", "no; possible; yes", threshold_x),
    threshold_score = if_else(metric_type == "categorical", "0; 0.5; 1", threshold_score)
  ) |>
  # hazard_direction() recorded per metric: which end of a range counts as
  # worse for parse_ppdb_numeric(). NA for categorical metrics, and note
  # the call must not even be evaluated for those - if_else() and
  # case_when() would evaluate it on every row whatever the condition, so
  # the numeric metrics are selected first.
  mutate(
    direction = map2_dbl(metric, metric_type, \(m, t) if (t == "numeric") hazard_direction(m) else NA_real_),
    direction_label = case_when(
      metric_type != "numeric" ~ "score increases with likeliness (categorical)",
      direction > 0             ~ "score increases with raw value",
      direction < 0             ~ "score decreases with raw value"
    )
  )

# Each workbook opens on its own licence notice (build_output_notice(),
# "HPLI import.R"), since either may circulate without the repository.
notice_published <- build_output_notice(
  contents = paste(
    "Output of \"HPLI score.R\": the HPLI and its four compartment scores, with their",
    "data-quality indicators, one row per substance. Per-metric scores are left out: each",
    "normalisation is piecewise linear and monotone, so a per-metric score could be converted",
    "back into the PPDB value it was computed from."
  ),
  redistribution = paste(
    "The workbook meant to be shared, as a reference against which a run on another PPDB",
    "export can be compared. Sharing and use remain subject to the AERU conditions above."
  )
)
notice_full <- build_output_notice(
  contents = paste0(
    "Full output of \"HPLI score.R\": the HPLI, its compartment scores, every metric's ",
    "normalised score",
    if (export_raw_ppdb_values) ", the raw and completed PPDB values" else "",
    " and the data-quality detail, one row per substance."
  ),
  redistribution = paste(
    "Keep local; do not share or commit. A per-metric score can be converted back into the",
    "PPDB value it was computed from, so this workbook is licensed PPDB-derived material."
  )
)

write_xlsx(
  list(
    Notice          = notice_published,
    HPLI_results    = results_published,
    HPLI_parameters = parameters_out,
    Data_quality    = data_quality_sheet,
    Run_log         = log_published
  ),
  path = output_file
)

write_xlsx(
  list(
    Notice          = notice_full,
    HPLI_results    = results_main,
    HPLI_parameters = parameters_out,
    Data_quality    = data_quality_sheet,
    Run_log         = log_full
  ),
  path = full_output_file
)

message("Done. Publishable results written to: ", output_file)
message("Full results (local only, per-metric scores) written to: ", full_output_file)
message("Substances with a computable HPLI: ", sum(results_main$can_calculate), " / ", nrow(results_main))
