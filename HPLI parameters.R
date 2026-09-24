# SPDX-License-Identifier: GPL-3.0-or-later
# ──────────────────────────────────────────────────────────────────────────
# HPLI-EU indicator definition: metrics, thresholds, compartments, weights ####
# ──────────────────────────────────────────────────────────────────────────

#' HPLI-EU indicator definition
#'
#' Defines what the indicator is - its metrics, their normalisation
#' thresholds, their compartments and their default weights - as opposed to
#' how a substance is scored against it ("HPLI score.R") or how its metrics
#' are read out of a PPDB export ("HPLI import.R"). Sourced by both, and
#' close to data-only by design.
#'
#' A sibling version - HPLI-Wal's 27 metrics, or any other metric set - is
#' a new file defining the same object names. Nothing in the scoring or
#' import logic needs to change for it.
#'
#' @section Objects defined:
#' * `hpli_thresholds` - normalisation thresholds, one row per level, one
#'   column per numeric metric.
#' * `hpli_parameters` - one row per metric: name, type, compartment,
#'   default weight.
#' * `metric_cols` - the metric names, in definition order.
#' * `compartment_weights` - each compartment's fixed share of the total.
#' * `hazard_breakpoints` - `hpli_thresholds` reshaped into the (x, y)
#'   pairs the normalisation functions consume.
#'
#' @section Functions defined:
#' `hazard_direction()`, `score_categorical_hazard()`,
#' `interpolate_hazard_score()`, `score_one_metric()`,
#' `high_hazard_value()`, `low_hazard_value()`.
#'
#' @section Reference:
#' Every threshold, weight and rule below comes from Vandevoorde et al.
#' (2025). The reasoning behind each is recorded in
#' "HPLI methodology.md", chiefly sections "Data source and thresholds"
#' and "Missing-data and range-value policy".

library(dplyr)
library(tidyr)
library(purrr)

#' Normalisation thresholds per numeric metric
#'
#' @format A tibble with one row per threshold level and one column per
#'   numeric metric. `threshold_level` holds the HPLI score (0.00 to 1.50)
#'   and each metric column the raw value at which that score is reached;
#'   `NA` where a metric does not define that level. Levels are the same
#'   for every metric, so the table is sparse by construction.
#' @details
#' Laid out to mirror table 1 of Vandevoorde et al. (2025) - one column per
#' metric, one row per level - so it can be checked against the published
#' table directly, column by column, rather than reconstructed from a
#' differently shaped encoding.
hpli_thresholds <- tribble(
  ~threshold_level, ~soil_dt50, ~water_dt50, ~kfoc, ~gus, ~bcf, ~birds_ld50, ~earthworms_lc50, ~honeybees_ld50, ~mammals_ld50_oral, ~algae_ec50, ~aq_invertebrates_ec50, ~aq_invertebrates_noec, ~fish_lc50, ~fish_noec, ~mammals_ld50_dermal, ~mammals_lc50_inhalation,
  0.00,             0,          0,           10000,  -18,  0,    20000,       10000,             1000,            20000,              100,         1000,                   100,                     1000,       100,        50000,                100,
  0.10,             NA,         1,           NA,     0,    NA,   NA,          NA,                NA,              NA,                 NA,          NA,                     NA,                      NA,         NA,         NA,                   NA,
  0.25,             20,         NA,          1000,   1.8,  NA,   2000,        1000,              100,             2000,               10,          100,                    10,                      100,        10,         5000,                 10,
  0.33,             NA,         NA,          500,    NA,   NA,   NA,          NA,                NA,              NA,                 NA,          NA,                     NA,                      NA,         NA,         NA,                   NA,
  0.50,             60,         14,          NA,     NA,   100,  NA,          NA,                NA,              NA,                 NA,          NA,                     NA,                      NA,         NA,         2000,                 4,
  0.67,             NA,         NA,          75,     NA,   NA,   NA,          NA,                NA,              NA,                 NA,          NA,                     NA,                      NA,         NA,         NA,                   NA,
  1.00,             180,        30,          15,     2.8,  5000, 100,         10,                1,               100,                0.01,        0.1,                    0.01,                    0.1,        0.01,       200,                  0.1,
  1.50,             1800,       300,         0,      28,   50000,0,           0,                 0,               0,                  0,           0,                      0,                       0,          0,          0,                    0
)

#' The indicator's metrics, with their type, compartment and weight
#'
#' @format A tibble with one row per metric and four columns:
#'   * `metric` - the internal name, used as a column name throughout.
#'   * `metric_type` - "numeric" (normalised by interpolation between
#'     thresholds) or "categorical" (mapped by
#'     `score_categorical_hazard()`).
#'   * `compartment` - one of the names of `compartment_weights`.
#'   * `weight` - the metric's share of the whole indicator, summing to 1
#'     over all rows.
#' @details
#' `weight` holds the published table-1 weights, renormalised to sum to
#' exactly 1 (the published percentages are rounded, and sum to about
#' 100.2%). "HPLI score.R" can replace them at run time with weights
#' recomputed from the PPDB by "HPLI weights.R", which both computes over
#' this table's metric list and is checked against it when its output is
#' loaded back in.
#'
#' This table is also what makes the file swappable: changing the metric
#' list here is what defines a different HPLI version.
hpli_parameters <- tribble(
  ~metric,                     ~metric_type,   ~compartment,                ~weight,
  "soil_dt50",                 "numeric",      "Environmental fate",         0.105,
  "water_dt50",                "numeric",      "Environmental fate",         0.066,
  "kfoc",                      "numeric",      "Environmental fate",         0.052,
  "gus",                       "numeric",      "Environmental fate",         0.051,
  "bcf",                       "numeric",      "Environmental fate",         0.059,
  "birds_ld50",                "numeric",      "Ecotoxicity (terrestrial)",  0.029,
  "earthworms_lc50",           "numeric",      "Ecotoxicity (terrestrial)",  0.032,
  "honeybees_ld50",            "numeric",      "Ecotoxicity (terrestrial)",  0.075,
  "mammals_ld50_oral",         "numeric",      "Ecotoxicity (terrestrial)",  0.031,
  "algae_ec50",                "numeric",      "Ecotoxicity (aquatic)",      0.046,
  "aq_invertebrates_ec50",     "numeric",      "Ecotoxicity (aquatic)",      0.029,
  "aq_invertebrates_noec",     "numeric",      "Ecotoxicity (aquatic)",      0.031,
  "fish_lc50",                 "numeric",      "Ecotoxicity (aquatic)",      0.029,
  "fish_noec",                 "numeric",      "Ecotoxicity (aquatic)",      0.031,
  "mammals_ld50_dermal",       "numeric",      "Human toxicity",             0.051,
  "mammals_lc50_inhalation",   "numeric",      "Human toxicity",             0.052,
  "carcinogenicity",           "categorical",  "Human toxicity",             0.091,
  "cholinesterase_inhibition", "categorical",  "Human toxicity",             0.029,
  "neurotoxicity",             "categorical",  "Human toxicity",             0.035,
  "reprotoxicity",             "categorical",  "Human toxicity",             0.074
) |>
  mutate(weight = weight / sum(weight)) # published percentages are rounded and sum to ~100.2%

#' The metric names, in the order this file defines them
#'
#' @format A character vector, used to select and order metric columns
#'   consistently across the scoring, weighting and import code.

metric_cols <- hpli_parameters$metric

#' Each compartment's fixed share of the indicator
#'
#' @format A named numeric vector summing to 1, one element per
#'   compartment of `hpli_parameters`.
#' @details
#' Environmental fate and human toxicity each carry a third of the total;
#' the remaining third is split evenly between terrestrial and aquatic
#' ecotoxicity. Unlike the per-metric weights, these shares are fixed
#' rather than data-driven: "HPLI weights.R" rescales each compartment's
#' internally-normalised weights down to the share given here.
compartment_weights <- c(
  "Environmental fate"         = 1 / 3,
  "Ecotoxicity (terrestrial)"  = 1 / 6,
  "Ecotoxicity (aquatic)"      = 1 / 6,
  "Human toxicity"             = 1 / 3
)

#' Threshold breakpoints per numeric metric, as (x, y) pairs
#'
#' @format A tibble with one row per numeric metric and three columns:
#'   `metric`, `x` (list of raw values, ascending) and `y` (list of the
#'   matching HPLI scores). Only the levels a metric actually defines are
#'   kept, so the lists vary in length between metrics.
#' @details
#' Derived from `hpli_thresholds`, never edited directly.
#' `interpolate_hazard_score()` interpolates over these pairs;
#' `hazard_direction()`, `high_hazard_value()` and `low_hazard_value()`
#' read them to answer, respectively, which way the hazard runs and which
#' raw value sits at a given score.
hazard_breakpoints <- hpli_thresholds |>
  pivot_longer(-threshold_level, names_to = "metric", values_to = "value") |>
  filter(!is.na(value)) |>
  arrange(metric, value) |>
  group_by(metric) |>
  summarise(x = list(value), y = list(threshold_level), .groups = "drop")

#' Which way the hazard runs for a numeric metric
#'
#' @param metric_name Character scalar. A metric name present in
#'   `hazard_breakpoints`.
#' @return `1` if the HPLI score increases with the raw value (soil DT50:
#'   longer persistence is more hazardous), `-1` if it decreases (KFOC:
#'   lower retention means more mobility, so more hazard).
#' @details
#' Read from `hazard_breakpoints` rather than from a separate hardcoded
#' list, so it cannot drift out of step with the thresholds that already
#' define "worse" everywhere else. Every metric is monotonic in
#' `hpli_thresholds` by construction, so comparing the first and last
#' breakpoint settles the direction.
#'
#' Used by `parse_ppdb_numeric()` ("HPLI import.R") to resolve a range
#' value to its more hazardous end - see "HPLI methodology.md", section
#' "Missing-data and range-value policy".
hazard_direction <- function(metric_name) {
  bp <- filter(hazard_breakpoints, metric == metric_name)
  y <- bp$y[[1]]
  if (y[length(y)] >= y[1]) 1 else -1
}

# Printed once per source(): the direction each numeric metric resolves to.
# It drives a range resolution that leaves no other trace in the output, so
# this line is the chance to notice a wrong sign before it silently affects
# a value. The same information is written per metric to the
# "HPLI_parameters" sheet of the output workbook.
message("Hazard direction per numeric metric (score vs. raw value):")
for (m in hpli_parameters$metric[hpli_parameters$metric_type == "numeric"]) {
  d <- hazard_direction(m)
  message("  ", m, ": ", if (d > 0) "+1 (score increases with raw value)" else "-1 (score decreases with raw value)")
}

#' Normalise a categorical hazard metric
#'
#' @param value Character vector of PPDB categorical values, expected as
#'   "no", "possible" or "yes" (lower case, as `parse_ppdb_category()`
#'   returns them).
#' @return A numeric vector the same length as `value`: 0, 0.5 and 1
#'   respectively, and `NA` for anything else, missing values included.
#' @details
#' The normalisation of the four categorical human-toxicity metrics.
#' "HPLI weights.R" also applies it to numeric-code those metrics before
#' correlating them, so their correlation structure sits on the same scale
#' as the score it will weight.
score_categorical_hazard <- function(value) {
  case_when(
    is.na(value)        ~ NA_real_,
    value == "no"       ~ 0,
    value == "possible" ~ 0.5,
    value == "yes"      ~ 1,
    TRUE ~ NA_real_
  )
}

#' Normalise one raw value by interpolating between thresholds
#'
#' @param value Numeric scalar. The raw metric value to normalise.
#' @param x Numeric vector of raw values (a metric's breakpoints); need
#'   not be sorted.
#' @param y Numeric vector of the same length, the HPLI scores matching
#'   `x`.
#' @return A numeric scalar: the interpolated score, floored at 0 and
#'   unbounded above. `NA` if `value` is `NA` or infinite.
#' @details
#' Piecewise-linear between the breakpoints. Beyond the first or last
#' breakpoint the value is extrapolated along that end segment's slope, so
#' the scale has no ceiling: a soil DT50 of 2345 days scores 2.6 rather
#' than being capped at 1.50.
#'
#' Extrapolation is deliberately asymmetric. Below the null-hazard end it
#' can reach a negative score - a KFOC far above the null-hazard threshold
#' extrapolates to about -30 - which is floored at 0. Only the high-hazard
#' side is left open. See "HPLI methodology.md", section "Data source and
#' thresholds".
#'
#' An infinite `value` returns `NA` rather than extrapolating. No code path
#' produces one today; the guard is kept so that a future one cannot score
#' a substance off an infinity.
interpolate_hazard_score <- function(value, x, y) {
  if (is.na(value) || is.infinite(value)) return(NA_real_)
  ord <- order(x)
  x <- x[ord]; y <- y[ord]
  score <- if (value >= min(x) && value <= max(x)) {
    approx(x = x, y = y, xout = value, ties = "ordered")$y
  } else if (value < min(x)) {
    slope <- (y[2] - y[1]) / (x[2] - x[1])
    y[1] + slope * (value - x[1])
  } else {
    n <- length(x)
    slope <- (y[n] - y[n - 1]) / (x[n] - x[n - 1])
    y[n] + slope * (value - x[n])
  }
  max(score, 0)
}

#' Normalise one value, dispatching on the metric's type
#'
#' @param value The raw value: numeric for a numeric metric, character for
#'   a categorical one.
#' @param metric_name Character scalar. A metric name present in
#'   `hpli_parameters`.
#' @return A numeric scalar, the metric's HPLI score.
#' @details
#' The single entry point "HPLI score.R" scores through: it reads the
#' metric's type from `hpli_parameters` and routes to
#' `score_categorical_hazard()` or `interpolate_hazard_score()`
#' accordingly, so no caller has to know which kind of metric it holds.
score_one_metric <- function(value, metric_name) {
  if (hpli_parameters$metric_type[hpli_parameters$metric == metric_name] == "categorical") {
    return(score_categorical_hazard(value))
  }
  bp <- filter(hazard_breakpoints, metric == metric_name)
  interpolate_hazard_score(value, bp$x[[1]], bp$y[[1]])
}

#' The raw value at a metric's high-hazard threshold
#'
#' @param metric_name Character scalar. A metric name present in
#'   `hpli_parameters`.
#' @return The raw value whose score is closest to 1.00: a numeric scalar
#'   for a numeric metric, the string "yes" for a categorical one.
#' @details
#' What the precautionary policy substitutes for a missing value - the
#' high-hazard level, not the more extreme 1.50 outlier level. See
#' "HPLI methodology.md", section "Missing-data and range-value policy".
high_hazard_value <- function(metric_name) {
  if (hpli_parameters$metric_type[hpli_parameters$metric == metric_name] == "categorical") return("yes")
  bp <- filter(hazard_breakpoints, metric == metric_name)
  bp$x[[1]][which.min(abs(bp$y[[1]] - 1.00))]
}

#' The raw value at a metric's null-hazard threshold
#'
#' @param metric_name Character scalar. A numeric metric name present in
#'   `hazard_breakpoints`.
#' @return A numeric scalar: the raw value whose score is closest to 0.00.
#' @details
#' The mirror of `high_hazard_value()`, used only for the substances listed
#' in `low_hazard_reference_file` ("HPLI score.R"), where a missing metric
#' means the hazard pathway does not apply rather than that the value is
#' unknown.
#'
#' The null-hazard value is not 0 for every metric: it is 0 for soil and
#' water DT50 and for BCF, but a *high* KFOC (well retained) and a very
#' negative GUS (non-leaching). Hence the lookup rather than a constant.
#' See "HPLI methodology.md", section "Completing soil DT50, KFOC, BCF and
#' GUS before normalisation".
low_hazard_value <- function(metric_name) {
  bp <- filter(hazard_breakpoints, metric == metric_name)
  bp$x[[1]][which.min(abs(bp$y[[1]] - 0.00))]
}
