# SPDX-License-Identifier: GPL-3.0-or-later
# ──────────────────────────────────────────────────────────────────────────
# HPLI-EU rose plot visualisation, from a saved HPLI_results_full.xlsx  ####
# ──────────────────────────────────────────────────────────────────────────

#' Rose diagrams of a substance's HPLI-EU metric scores
#'
#' Draws one wedge per metric: angular width proportional to the metric's
#' aggregation weight, radial height its normalised hazard score (0-1.5),
#' with the data-quality traceability drawn over it. `draw_hpli_rose()` is
#' the entry point; `build_hpli_score_table()` and `print_substance_info()`
#' give the same substance's detail as a table and a console summary.
#'
#' @section Input:
#' A saved "HPLI_results_full.xlsx" written by "HPLI score.R", read by
#' `load_hpli_results()`. It must be the full workbook: the publishable
#' "HPLI_results.xlsx" leaves out the per-metric scores the wedges are
#' drawn from (see "HPLI methodology.md", section "PPDB licensing and what
#' may be shared"). Nothing else: this script sources none of its
#' sibling scripts and needs no PPDB access. Every quantity the plot needs
#' - the metric list, compartments and weights - comes from that
#' workbook's own "HPLI_parameters" sheet, so a plot's wedge widths match
#' the weights that actually produced the run being plotted rather than
#' whatever "HPLI parameters.R" defines now.
#'
#' @section Reference:
#' "HPLI methodology.md", sections "Rose plot visualisation" and "Data
#' quality traceability".

library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(ggplot2)
library(ggnewscale)
library(ggpattern)
library(patchwork)
library(gridExtra)

# ──────────────────────────────────────────────────────────────────────────
# 1. Load a saved HPLI_results_full.xlsx                                 ####
# ──────────────────────────────────────────────────────────────────────────

#' Read one sheet, explaining the one error that is easy to hit
#'
#' @param results_file Path to the workbook.
#' @param sheet Sheet name.
#' @return The sheet, as `read_excel()` returns it.
#' @details
#' A workbook open in Excel cannot be read: Excel holds a lock and readxl
#' reports it as a cryptic "zip file cannot be opened". This wrapper turns
#' that into a message saying to close the file.
read_sheet_or_explain <- function(results_file, sheet) {
  tryCatch(
    read_excel(results_file, sheet = sheet),
    error = function(e) {
      stop(
        "Could not read '", results_file, "' (sheet '", sheet, "'). ",
        "If this file is currently open in Excel (or a backup is still syncing it), ",
        "close it and try again: an open workbook keeps R from reading it.\n",
        "Original error: ", conditionMessage(e),
        call. = FALSE
      )
    }
  )
}

#' Load a scored dataset from a saved workbook
#'
#' @param results_file Path to the full workbook written by
#'   "HPLI score.R", "HPLI_results_full.xlsx" by default.
#' @return A named list of the four sheets: `results`, `parameters`,
#'   `data_quality`, `run_log`. Everything else in this file takes this
#'   list as its `hpli_data` argument.
#' @details
#' Prints the run's timestamp on load, since nothing here re-derives the
#' data: a plot reflects whichever scoring run produced that file, and the
#' file may have been regenerated between sessions.
#'
#' Stops with an explicit message when given the publishable workbook,
#' which has no per-metric score to draw.
load_hpli_results <- function(results_file = "HPLI_results_full.xlsx") {
  if (!file.exists(results_file)) {
    stop("'", results_file, "' not found - run \"HPLI score.R\" first.", call. = FALSE)
  }
  results      <- read_sheet_or_explain(results_file, "HPLI_results")
  data_quality <- read_sheet_or_explain(results_file, "Data_quality")
  parameters   <- read_sheet_or_explain(results_file, "HPLI_parameters")
  run_log      <- read_sheet_or_explain(results_file, "Run_log")

  if (!any(startsWith(names(results), "score_"))) {
    stop(
      "'", results_file, "' has no per-metric scores: it is the publishable workbook, ",
      "which leaves them out. Point load_hpli_results() at the full one written by ",
      "\"HPLI score.R\" (its full_output_file setting, \"HPLI_results_full.xlsx\" by default).",
      call. = FALSE
    )
  }

  run_timestamp <- run_log$value[run_log$setting == "run_timestamp"]
  weight_source <- run_log$value[run_log$setting == "weight_source"]
  message(
    "Loaded '", results_file, "' - scores computed ", run_timestamp,
    " (weight_source: ", weight_source, ")."
  )

  list(results = results, data_quality = data_quality, parameters = parameters, run_log = run_log)
}

# ──────────────────────────────────────────────────────────────────────────
# 2. Presentation constants (not part of the indicator definition itself) ####
# ──────────────────────────────────────────────────────────────────────────

#' Human-readable label per metric
#'
#' @format A named character vector: internal metric name -> the label
#'   shown on a plot or in a table.
#' @details
#' Covers the metrics of the current indicator definition. A version with
#' further metrics can either extend this map or leave them out: an
#' unmapped name falls back to itself, via `format_metric_label()`.
metric_labels <- c(
  soil_dt50                 = "Soil persistence (DT50 soil)",
  water_dt50                = "Water persistence (DT50 water)",
  kfoc                      = "Surface water transfer (Kfoc)",
  gus                       = "Groundwater transfer (GUS)",
  bcf                       = "Aquatic biome transfer (BCF)",
  birds_ld50                = "Birds (acute oral)",
  earthworms_lc50           = "Earthworms (acute soil)",
  honeybees_ld50            = "Honeybees (acute oral/contact/other)",
  mammals_ld50_oral         = "Mammals (acute oral)",
  algae_ec50                = "Algae (acute aqueous)",
  aq_invertebrates_ec50     = "Aquatic invertebrates (acute aq.)",
  aq_invertebrates_noec     = "Aquatic invertebrates (chronic aq.)",
  fish_lc50                 = "Fish (acute aqueous)",
  fish_noec                 = "Fish (chronic aqueous)",
  mammals_ld50_dermal       = "Mammals (acute dermal)",
  mammals_lc50_inhalation   = "Mammals (acute inhalation)",
  carcinogenicity           = "Humans (carcinogenicity)",
  cholinesterase_inhibition = "Humans (cholinesterase inhibition)",
  neurotoxicity             = "Humans (neurotoxicity)",
  reprotoxicity             = "Humans (reprotoxicity)"
)

#' Label a metric for display
#'
#' @param metric Character vector of internal metric names.
#' @return The matching labels from `metric_labels`, falling back to the
#'   metric name itself where the map has no entry.
format_metric_label <- function(metric) coalesce(metric_labels[metric], metric)

#' Light and dark ends of each compartment's colour gradient
#'
#' @format A named list, one element per compartment, each a character
#'   vector of two hex colours.
#' @details
#' One gradient per compartment, interpolated to however many metrics that
#' compartment holds (`build_metric_colors()`), rather than one hand-picked
#' colour per metric: adding or removing a metric then needs no palette
#' upkeep.
compartment_gradient_extremes <- list(
  "Environmental fate"        = c("#ffffcc", "#006837"),
  "Ecotoxicity (terrestrial)" = c("#feedde", "#d94701"),
  "Ecotoxicity (aquatic)"     = c("#eff3ff", "#08519c"),
  "Human toxicity"            = c("#feebe2", "#7a0177")
)

# ──────────────────────────────────────────────────────────────────────────
# 3. Per-metric colour and wedge-position tables (derived from the loaded ####
#    "HPLI_parameters" sheet - parameters$metric/$compartment/$weight)
# ──────────────────────────────────────────────────────────────────────────

#' One colour per metric, as a gradient within each compartment
#'
#' @param parameters The loaded "HPLI_parameters" sheet: needs `metric` and
#'   `compartment`.
#' @param gradient_extremes Named list of two-colour endpoints per
#'   compartment. Defaults to `compartment_gradient_extremes`.
#' @return A named character vector, metric -> hex colour.
#' @details
#' Each compartment's gradient runs light to dark in that compartment's
#' definition order, so neighbouring wedges of the same compartment read as
#' a family. `group_modify()` preserves within-group row order, so no sort
#' step is needed.
build_metric_colors <- function(parameters, gradient_extremes = compartment_gradient_extremes) {
  coloured <- parameters |>
    select(metric, compartment) |>
    group_by(compartment) |>
    group_modify(~ tibble(
      metric = .x$metric,
      colour = colorRampPalette(gradient_extremes[[.y$compartment]])(nrow(.x))
    )) |>
    ungroup()
  setNames(coloured$colour, coloured$metric)
}

#' Angular position of each metric's wedge
#'
#' @param parameters The loaded "HPLI_parameters" sheet: needs `metric`,
#'   `compartment` and `weight`.
#' @return A list of two elements: `wedges`, the sheet with `xmin`, `xmax`
#'   and `xmid` added on a 0-1 scale (one full turn of the plot), and
#'   `segment_x`, the compartment boundary positions for the radial
#'   dividers.
#' @details
#' A wedge's angular width is the metric's aggregation weight: the whole
#' turn is the indicator, and each metric occupies its own share of it.
#' Positions are therefore the cumulative weight, taken in the order the
#' sheet lists metrics in, which is compartment by compartment.
#'
#' The compartment boundaries fall out of that same cumulative sum rather
#' than being read from a second copy of `compartment_weights`, which this
#' script could not reach anyway since it sources nothing. They land at 0,
#' 1/3, 1/2 and 2/3, as the fixed compartment shares require - to within
#' the rounding of the published weights.
build_metric_wedges <- function(parameters) {
  wedges <- parameters |>
    mutate(
      xmax = cumsum(weight),
      xmin = lag(xmax, default = 0),
      xmid = (xmin + xmax) / 2
    ) |>
    select(metric, compartment, weight, xmin, xmax, xmid)

  compartment_bounds <- wedges |>
    group_by(compartment) |>
    summarise(xmax = max(xmax), .groups = "drop") |>
    arrange(xmax) |>
    pull(xmax)
  segment_x <- c(0, head(compartment_bounds, -1)) # drop the last (=1): a segment at the full circle's closing point is redundant

  list(wedges = wedges, segment_x = segment_x)
}

# ──────────────────────────────────────────────────────────────────────────
# 4. Per-substance plotting data                                         ####
# ──────────────────────────────────────────────────────────────────────────

#' Assemble one long table of everything a rose plot needs
#'
#' @param substances Character vector of substance names.
#' @param hpli_data The list from `load_hpli_results()`.
#' @param trunk Radial value at which a wedge is truncated for drawing;
#'   the untruncated score is kept alongside.
#' @return A tibble with one row per substance and metric, carrying the
#'   score, the data-quality status, bound and confidence, and the wedge
#'   position.
#' @details
#' Scores come from "HPLI_results" and are already multiplied by the
#' persistence coefficient. The data-quality columns are `NA` for a
#' substance the scoring run's `data_quality_scope` left out of its
#' "Data_quality" sheet - a substance below the coverage threshold, under
#' the default scope.
prepare_rose_plot_data <- function(substances, hpli_data, trunk = 1.5) {
  parameters <- hpli_data$parameters
  score_cols <- paste0("score_", parameters$metric)
  wedge_info <- build_metric_wedges(parameters)

  missing_substances <- setdiff(substances, hpli_data$results$Active)
  if (length(missing_substances) > 0) {
    stop(
      "Substance(s) not found in the loaded HPLI_results: ",
      paste(missing_substances, collapse = ", "),
      " - check spelling (Active, case-sensitive).",
      call. = FALSE
    )
  }

  scores_long <- hpli_data$results |>
    filter(Active %in% substances) |>
    select(ID, Active, all_of(score_cols)) |>
    pivot_longer(-c(ID, Active), names_to = "metric", values_to = "score") |>
    mutate(metric = str_remove(metric, "^score_"))

  plot_data <- scores_long |>
    left_join(hpli_data$data_quality |> select(ID, metric, status, bound, confidence), by = c("ID", "metric")) |>
    left_join(wedge_info$wedges, by = "metric") |>
    mutate(
      metric        = factor(metric, levels = parameters$metric),
      truncated     = pmin(score, trunk),
      is_truncated  = score > trunk,
      value_label   = if_else(is_truncated, as.character(round(score, 1)), NA_character_),
      is_substituted = status %in% c("substituted_precautionary", "substituted_null_hazard"),
      is_completed   = status == "completed"
    )

  no_data_quality <- plot_data |>
    group_by(Active) |>
    summarise(all_na = all(is.na(status)), .groups = "drop") |>
    filter(all_na) |>
    pull(Active)
  if (length(no_data_quality) > 0) {
    message(
      "Note: no Data_quality rows for ", paste(no_data_quality, collapse = ", "),
      " (excluded by data_quality_scope, or can_calculate == FALSE) - ",
      "scores are still plotted, but with no hatching/bound/confidence overlay."
    )
  }

  list(plot_data = plot_data, segment_x = wedge_info$segment_x)
}

# ──────────────────────────────────────────────────────────────────────────
# 5. Rose plot for one substance                                         ####
# ──────────────────────────────────────────────────────────────────────────

#' Build the rose plot for one substance
#'
#' @param d One substance's rows from `prepare_rose_plot_data()`.
#' @param substance_name Used for the plot title.
#' @param segment_x Compartment boundary positions, from
#'   `build_metric_wedges()`.
#' @param metric_colors Named colour vector, from `build_metric_colors()`.
#' @param show_data_quality `TRUE` adds the bound and confidence
#'   annotations. The hatching of substituted metrics is drawn either way.
#' @param trunk Radial truncation value.
#' @return A ggplot object.
build_rose_ggplot <- function(d, substance_name, segment_x, metric_colors, show_data_quality, trunk) {
  background <- tibble(
    xmin = 0, xmax = 1,
    ymin = c(0, 0.5, 1.0),
    ymax = c(0.5, 1.0, 1.5),
    band = factor(c("Low to moderate", "Moderate to high", "High to very high"),
                  levels = c("Low to moderate", "Moderate to high", "High to very high"))
  )

  p <- ggplot(d, aes(x = 0, y = truncated, fill = metric)) +
    # Concentric hazard-level bands
    geom_rect(
      data = background,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = band),
      alpha = 0.5, inherit.aes = FALSE
    ) +
    scale_fill_manual(
      name = "Load",
      values = c("Low to moderate" = "white", "Moderate to high" = "gray85", "High to very high" = "gray70"),
      guide = guide_legend(
        override.aes = list(color = "gray70", linewidth = 0.5),
        keywidth = unit(0.35, "cm"), keyheight = unit(0.35, "cm"), order = 1
      )
    ) +
    # Compartment boundaries
    geom_segment(
      data = tibble(x = segment_x),
      aes(x = x, xend = x, y = 0, yend = 1.5),
      colour = "gray65", linewidth = 0.5, inherit.aes = FALSE
    ) +
    ggnewscale::new_scale_fill() +
    # Metric wedges (truncated at `trunk`)
    geom_rect(
      aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = truncated, fill = metric),
      color = "black", inherit.aes = FALSE
    )

  if (any(d$is_truncated)) {
    p <- p +
      geom_rect(
        data = filter(d, is_truncated),
        aes(xmin = xmin, xmax = xmax, ymin = trunk, ymax = trunk + 0.075),
        fill = "red", inherit.aes = FALSE
      ) +
      geom_text(
        data = filter(d, is_truncated),
        aes(x = xmid, y = trunk - 0.2, label = value_label),
        size = 3.5, color = "#8B0000", fontface = "bold", inherit.aes = FALSE
      )
  }

  # Hatching marks a substituted metric and is drawn whatever
  # show_data_quality says: it is the traceability signal this plot exists
  # for. Completed metrics are deliberately not hatched - they are real
  # PPDB-derived data, and are instead marked by an italicised confidence
  # label when the finer annotations are switched on.
  if (any(d$is_substituted, na.rm = TRUE)) {
    p <- p +
      ggpattern::geom_rect_pattern(
        data = filter(d, is_substituted),
        aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = truncated),
        fill = "transparent", color = NA, pattern_fill = "black",
        pattern_density = 0.025, pattern_spacing = 0.02, pattern_angle = 60,
        pattern = "stripe", inherit.aes = FALSE
      )
  }

  p <- p +
    scale_fill_manual(
      values = metric_colors, labels = format_metric_label,
      guide = guide_legend(ncol = 1, keywidth = unit(0.35, "cm"), keyheight = unit(0.35, "cm"), order = 2)
    ) +
    labs(title = str_to_sentence(substance_name), x = NULL, y = NULL, fill = "Metric") +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.title = element_text(face = "bold", size = 9),
      legend.text = element_text(size = 7),
      legend.spacing.y = unit(0.05, "cm"),
      plot.title = element_text(face = "bold", size = 15, colour = "#1b4332", hjust = 0.5, margin = margin(b = 8)),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    ) +
    scale_x_continuous(limits = c(0, 1)) +
    coord_polar(start = 0)

  if (show_data_quality) {
    bound_labels <- d |> filter(!is.na(bound) & !bound %in% c("exact", "missing"))
    if (nrow(bound_labels) > 0) {
      p <- p + geom_text(
        data = bound_labels, aes(x = xmid, y = truncated + 0.09, label = bound),
        size = 3, color = "black", inherit.aes = FALSE
      )
    }
    # Confidence band, italicised when the value was completed and plain
    # when it was measured. Two geom_text() layers rather than one: a
    # single text geom cannot mix font faces within one aes() mapping.
    conf_measured <- d |> filter(!is.na(confidence), status == "measured")
    conf_completed <- d |> filter(!is.na(confidence), status == "completed")
    if (nrow(conf_measured) > 0) {
      p <- p + geom_text(
        data = conf_measured, aes(x = xmid, y = truncated + 0.22, label = confidence),
        size = 2.8, color = "black", fontface = "plain", inherit.aes = FALSE
      )
    }
    if (nrow(conf_completed) > 0) {
      p <- p + geom_text(
        data = conf_completed, aes(x = xmid, y = truncated + 0.22, label = confidence),
        size = 2.8, color = "black", fontface = "italic", inherit.aes = FALSE
      )
    }
  }

  p
}

# ──────────────────────────────────────────────────────────────────────────
# 6. Full per-metric detail (console/View() only - not attached to a plot) ####
# ──────────────────────────────────────────────────────────────────────────

#' The full per-metric detail behind one substance's plot
#'
#' @param substance One substance name.
#' @param hpli_data The list from `load_hpli_results()`.
#' @param trunk Radial truncation value, passed through.
#' @return A tibble with one row per metric: compartment, label, score,
#'   status, bound and confidence.
#' @details
#' For the console or `View()`. It is deliberately not attached to the
#' plot, where a table this size does not read well next to the diagram;
#' `print_substance_info()` is the compact summary meant to accompany one.
build_hpli_score_table <- function(substance, hpli_data, trunk = 1.5) {
  d <- prepare_rose_plot_data(substance, hpli_data, trunk)$plot_data
  d |>
    arrange(metric) |>
    transmute(
      Compartment = compartment,
      Metric      = format_metric_label(as.character(metric)),
      Score       = round(score, 2),
      Status      = status,
      Bound       = bound,
      Confidence  = confidence
    )
}

# ──────────────────────────────────────────────────────────────────────────
# 7. Compact substance info (printed to the console)                     ####
# ──────────────────────────────────────────────────────────────────────────

#' Print a compact identity summary for one substance
#'
#' @param substance_name One substance name.
#' @param hpli_data The list from `load_hpli_results()`.
#' @return Invisibly `NULL`; called for the console output.
#' @details
#' CAS number, chemical family and origin are carried through from
#' "HPLI_results"; the main and sub type are derived here for display only,
#' and are not part of the scored dataset. Printed rather than attached to
#' the plot, and callable on its own as well as through
#' `draw_hpli_rose()`.
print_substance_info <- function(substance_name, hpli_data) {
  results_row <- hpli_data$results |> filter(Active == substance_name) |> slice(1)
  if (nrow(results_row) == 0) {
    warning("No HPLI_results row for '", substance_name, "' - skipping info.", call. = FALSE)
    return(invisible(NULL))
  }
  fmt <- function(x, digits = NULL, suffix = "") {
    if (length(x) == 0 || is.na(x)) return("-")
    if (!is.null(digits)) return(paste0(formatC(x, digits = digits, format = "f"), suffix))
    as.character(x)
  }
  # Main and sub type: "Pesticide type" split on its first comma, so
  # mepiquat's "Plant Growth Regulator, Herbicide" reads as a main type of
  # "Plant Growth Regulator" and a sub type of "Herbicide". The main type
  # is prefixed with the substance's origin, e.g. "Synthetic Herbicide".
  pesticide_type <- results_row$`Pesticide type`
  has_comma <- !is.na(pesticide_type) && str_detect(pesticide_type, ",")
  main_type <- if (has_comma) str_trim(str_extract(pesticide_type, "^[^,]+")) else pesticide_type
  sub_type  <- if (has_comma) str_trim(str_remove(pesticide_type, "^[^,]+,\\s*")) else NA_character_

  rows <- c(
    Substance              = fmt(results_row$Active),
    CAS                    = fmt(results_row$CAS),
    `Main type`            = fmt(paste(na.omit(c(results_row$`Substance origin`, main_type)), collapse = " ")),
    `Sub type`             = fmt(sub_type),
    Family                 = fmt(results_row$`Substance group`),
    `EC 1107/2009 status`  = fmt(results_row$`EC Regulation 1107/2009 status`),
    `HPLI (load score)`    = fmt(results_row$HPLI, 3),
    `Data coverage`        = fmt(results_row$data_coverage * 100, 0, "%"),
    `% substituted`        = fmt(results_row$pct_substituted, 1, "%"),
    `% completed`          = fmt(results_row$pct_completed, 1, "%")
  )
  cat("\n", strrep("-", 40), "\n", sep = "")
  cat(sprintf("%-20s: %s\n", names(rows), rows))
  cat(strrep("-", 40), "\n", sep = "")
  invisible(results_row)
}

# ──────────────────────────────────────────────────────────────────────────
# 8. Public entry point                                                  ####
# ──────────────────────────────────────────────────────────────────────────

#' Draw the rose diagram for one or several substances
#'
#' @param substances One substance name, or a vector of several - one plot
#'   each, assembled into a patchwork grid sharing a single legend.
#' @param hpli_data The list from `load_hpli_results()`.
#' @param show_data_quality `TRUE` annotates each wedge with its bound
#'   ("<", ">", "range") and confidence band, the latter italicised where
#'   the value was completed. Hatching of substituted metrics is drawn
#'   either way. Default `FALSE`.
#' @param show_info `TRUE` (default) also prints each substance's compact
#'   info to the console. It is not attached to the plot, so it works the
#'   same for one substance or several. For the per-metric detail, call
#'   `build_hpli_score_table()`.
#' @param trunk Radial value at which a wedge is truncated, so that one
#'   extreme score does not flatten the rest of the diagram. Default 1.5.
#' @return A ggplot object, or a patchwork of them for several substances.
draw_hpli_rose <- function(
  substances,
  hpli_data,
  show_data_quality = FALSE,
  show_info = TRUE,
  trunk = 1.5
) {
  if (show_info) walk(substances, print_substance_info, hpli_data = hpli_data)

  prepared <- prepare_rose_plot_data(substances, hpli_data, trunk)
  metric_colors <- build_metric_colors(hpli_data$parameters)

  make_one <- function(substance_name) {
    d <- prepared$plot_data |> filter(Active == substance_name)
    build_rose_ggplot(d, substance_name, prepared$segment_x, metric_colors, show_data_quality, trunk)
  }

  plots <- map(substances, make_one)
  if (length(plots) == 1) {
    plots[[1]]
  } else {
    patchwork::wrap_plots(plots) + patchwork::plot_layout(guides = "collect")
  }
}

# ──────────────────────────────────────────────────────────────────────────
# Example usage (not run automatically)                                  ####
# ──────────────────────────────────────────────────────────────────────────

hpli_data <- load_hpli_results("HPLI_results_full.xlsx")
draw_hpli_rose("glyphosate", hpli_data)
draw_hpli_rose("glyphosate", hpli_data, show_data_quality = TRUE)
draw_hpli_rose("glyphosate", hpli_data, show_data_quality = TRUE, show_info = FALSE)
draw_hpli_rose(c("glyphosate", "diquat", "asulam", "mepiquat"), hpli_data)
build_hpli_score_table("asulam", hpli_data) |> View()
