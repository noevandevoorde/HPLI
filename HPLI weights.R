# SPDX-License-Identifier: GPL-3.0-or-later
# ──────────────────────────────────────────────────────────────────────────
# HPLI-EU weight calculation: inverse Spearman-correlation weighting     ####
# ──────────────────────────────────────────────────────────────────────────

#' Per-metric aggregation weights computed from a PPDB export
#'
#' 1. Computes one weight per metric of the indicator definition currently
#' loaded from "HPLI parameters.R", by inverse-correlation weighting within
#' each compartment: the more a metric correlates with its compartment
#' peers, the more redundant it is, and the lower its weight.
#' 2. Replaces the hardcoded table-1 defaults of "HPLI parameters.R" with
#' data-driven ones.
#'
#' Correlations are computed only among the metrics the active
#' "HPLI parameters.R" defines, so sourcing a different indicator
#' definition yields weights self-consistent with it and needs no change
#' here.
#'
#' Metric extraction reuses `load_ppdb_raw_metrics()` from "HPLI import.R"
#' (i.e. the same code "HPLI score.R" scores with), without missing values
#' substitution. Missing values therefore stay `NA` and `cor()` skips them
#' pairwise, so a weight reflects observed data only.
#'
#' @section Input:
#' A four-file AERU PPDB export: licensed material, not distributed with
#' this code - see "HPLI methodology.md", section "PPDB licensing and what
#' may be shared".
#'
#' @section Settings:
#' Set in section 1 below.
#' * `ppdb_export_dir` - folder holding the export (General.xlsx,
#'   Fate.xlsx, Ecotox.xlsx, Human.xlsx). Not set here: it is read from the
#'   untracked "local_paths.R", since it differs from one machine to the
#'   next. A correlation needs many substances to run over, so this is
#'   always a full export, never a single-substance extract.
#' * `range_policy` - how a PPDB range value (e.g. "10-20") is resolved
#'   into the single number a correlation needs: "worst_case" (default) or
#'   "mean". Same meaning as in "HPLI score.R", but a separate setting, so
#'   a sensitivity run here does not disturb the scoring run.
#' * `weights_file` - path the workbook is written to.
#'
#' @section Output:
#' `weights_file` (default "HPLI_weights.xlsx"), three sheets:
#' * `Notice` - what the workbook holds, its terms of use (derived from the
#'   PPDB, subject to the AERU conditions of use), and how to cite it.
#' * `Weights` - one row per metric: `compartment`, `metric`, `weight`
#'   (share of the whole indicator; sums to 1 across all metrics) and
#'   `weight_within_compartment` (sums to 1 within each compartment).
#' * `Run_log` - computation timestamp, the export's folder name and the
#'   checksums of its four workbooks (no local path), `range_policy`,
#'   number of substances read, metric count, metric list.
#'
#' "HPLI score.R" loads this file when its `weight_source` setting is
#' "spearman_ppdb", refuses it unless the metrics of its `Weights` sheet
#' match the active indicator definition exactly, and copies its `Run_log`
#' into its own with a "weights_" prefix.
#'
#' @section Method and rationale:
#' Vandevoorde et al. (2025); "HPLI methodology.md", section "Weight
#' calculation". The output is expected to differ from the weights
#' published in that paper's appendix; that section documents each source
#' of difference.

library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(writexl)

source("HPLI parameters.R")
source("HPLI import.R")

# ──────────────────────────────────────────────────────────────────────────
# 1. Settings                                                            ####
# ──────────────────────────────────────────────────────────────────────────

# `ppdb_export_dir` comes from "local_paths.R" (untracked; copy
# "local_paths.example.R"), which is read here and must define it. A
# missing file, a missing setting or a folder that does not exist each stop
# the run with an explicit message; the folder in use is echoed to the
# console. See load_local_paths() in "HPLI import.R".
load_local_paths(require_dirs = "ppdb_export_dir")

range_policy <- "worst_case"

weights_file <- "HPLI_weights.xlsx"

# ──────────────────────────────────────────────────────────────────────────
# 2. Read the PPDB and prepare metrics for correlation                   ####
# ──────────────────────────────────────────────────────────────────────────

# Completions that read another real PPDB field (soil DT50 lab/typical,
# KFOC<-KOC) or apply a published formula over such fields (BCF, GUS) are
# kept. The "stable in water -> 300 days" fill-in is not: see
# "HPLI methodology.md", section "Weight calculation".
raw_metrics <- load_ppdb_raw_metrics(
  ppdb_export_dir = ppdb_export_dir,
  range_policy = range_policy,
  water_dt50_stable_assumption = FALSE
)

# Categorical metrics arrive as "no"/"possible"/"yes" strings and are coded
# to 0/0.5/1 by `score_categorical_hazard()` - the same mapping the
# indicator scores them with. `metric_cols` keeps only the active
# definition's metrics, in its own order.
categorical_metrics <- hpli_parameters$metric[hpli_parameters$metric_type == "categorical"]
metrics_data <- raw_metrics |>
  mutate(across(all_of(categorical_metrics), score_categorical_hazard)) |>
  select(all_of(metric_cols))

# ──────────────────────────────────────────────────────────────────────────
# 3. Inverse Spearman-correlation weighting, per compartment             ####
# ──────────────────────────────────────────────────────────────────────────

#' Inverse-correlation weights within one compartment
#'
#' @param compartment_name Character scalar. The compartment's name, copied
#'   into the returned tibble.
#' @param metrics Character vector. Names of the columns of `data` holding
#'   this compartment's metrics.
#' @param data Data frame containing at least the `metrics` columns, all
#'   numeric, with `NA` for missing values.
#' @return A tibble of `length(metrics)` rows and three columns:
#'   `compartment`, `metric`, `weight_within_compartment` - the last
#'   summing to 1.
#' @details
#' Each weight is proportional to the inverse of the sum of that metric's
#' absolute Spearman correlations with the other metrics of the same
#' compartment, then normalised to sum to 1 within the compartment.
#' Correlations use `use = "pairwise.complete.obs"`, so each pair is
#' computed on the substances where both metrics have a value.
#'
#' A compartment holding one single metric returns weight 1 for it without
#' computing any correlation.
#'
#' A metric whose absolute correlations sum to exactly 0 - too few
#' overlapping observations to correlate with any peer - raises a warning
#' and makes the compartment's weights unusable: that metric's weight comes
#' out `NaN` and every peer's comes out 0. Check the metric's data coverage
#' rather than the arithmetic.
compute_compartment_weights <- function(compartment_name, metrics, data) {
  if (!is.character(compartment_name) || length(compartment_name) != 1L) {
    stop("compartment_name must be a single character string.", call. = FALSE)
  }
  if (!is.character(metrics) || !length(metrics)) {
    stop("metrics must be a non-empty character vector.", call. = FALSE)
  }
  missing_cols <- setdiff(metrics, names(data))
  if (length(missing_cols)) {
    stop(
      "compute_compartment_weights(): column(s) absent from data: ",
      paste(missing_cols, collapse = ", "), ".", call. = FALSE
    )
  }

  if (length(metrics) == 1) {
    return(tibble(compartment = compartment_name, metric = metrics, weight_within_compartment = 1))
  }

  df <- data |> select(all_of(metrics))
  cor_matrix <- cor(df, method = "spearman", use = "pairwise.complete.obs")
  abs_cor_matrix <- abs(cor_matrix)
  diag(abs_cor_matrix) <- NA
  s_i <- colSums(abs_cor_matrix, na.rm = TRUE)

  if (any(s_i == 0)) {
    warning(
      "compute_compartment_weights(): metric(s) with zero total correlation in '",
      compartment_name, "' (", paste(names(s_i)[s_i == 0], collapse = ", "),
      ") - too little overlapping data with its compartment peers to weight ",
      "meaningfully. Check data coverage for that metric before trusting this run.",
      call. = FALSE
    )
  }

  inv_s_i <- 1 / s_i
  weight_within_compartment <- inv_s_i / sum(inv_s_i)

  tibble(
    compartment = compartment_name,
    metric = names(weight_within_compartment),
    weight_within_compartment = as.numeric(weight_within_compartment)
  )
}

compartment_metrics <- split(hpli_parameters$metric, hpli_parameters$compartment)

weights_within <- imap_dfr(
  compartment_metrics,
  ~ compute_compartment_weights(.y, .x, metrics_data)
)

# Scale each compartment's internally-normalised weights down to that
# compartment's fixed share of the indicator (`compartment_weights`,
# "HPLI parameters.R"): the 5 environmental-fate metrics sum to 1/3, not 1.
# Rows are returned in the order of the active indicator definition.
weights_out <- weights_within |>
  mutate(weight = weight_within_compartment * compartment_weights[compartment]) |>
  select(compartment, metric, weight, weight_within_compartment) |>
  arrange(match(metric, hpli_parameters$metric))

# ──────────────────────────────────────────────────────────────────────────
# 4. Export                                                              ####
# ──────────────────────────────────────────────────────────────────────────

# `Run_log` dates a given set of weights, and "HPLI score.R" copies it into
# its own run log. No local path: the export is identified by its folder
# name and the checksums of its four workbooks (compute_ppdb_fingerprint(),
# "HPLI import.R"), so a scoring run can tell whether its weights came from
# the same export as its scores.
run_log <- bind_rows(
  tibble(setting = "computed_at", value = format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  compute_ppdb_fingerprint(ppdb_export_dir),
  tibble(
    setting = c("range_policy", "n_substances_read", "n_metrics", "metrics"),
    value = c(
      range_policy,
      as.character(nrow(raw_metrics)),
      as.character(length(metric_cols)),
      paste(sort(metric_cols), collapse = "; ")
    )
  )
)

# The workbook opens on its own licence notice (build_output_notice(),
# "HPLI import.R"), since it may circulate without the repository.
notice <- build_output_notice(
  contents = paste(
    "Output of \"HPLI weights.R\": one aggregation weight per metric, computed by",
    "inverse-correlation weighting from Spearman correlations over a PPDB export."
  ),
  redistribution = paste(
    "Meant to be shared as a reference against which weights recomputed from another PPDB",
    "export can be compared. Sharing and use remain subject to the AERU conditions above."
  )
)

write_xlsx(
  list(
    Notice  = notice,
    Weights = weights_out,
    Run_log = run_log
  ),
  path = weights_file
)

message("Done. Weights written to: ", weights_file)
print(weights_out |> mutate(weight = round(weight * 100, 2)) |> select(compartment, metric, `weight (%)` = weight))
