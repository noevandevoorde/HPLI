# HPLI — Harmonised Pesticide Load Indicator

An R implementation of the HPLI, a composite hazard indicator for pesticide active substances. It transforms the physico-chemical and (eco)toxicological properties of a substance into a unitless **load score**, aggregated over four compartments: environmental fate, terrestrial ecotoxicity, aquatic ecotoxicity and human toxicity.

The indicator is described in [Vandevoorde et al. (2025)](https://iopscience.iop.org/article/10.1088/1748-9326/ae269b), *Environmental Research Letters*, and in more detail in the appendices D to I of the dissertation it comes from, [Vandevoorde (2025)](https://hdl.handle.net/2078.5/264116), *Three tools for the reduction of pesticide impacts* (UCLouvain).

This repository implements **HPLI-EU**, the 20-metric version. `HPLI methodology.md` records every methodological decision behind it, including where this implementation deliberately departs from the published values — see also `CHANGELOG.md`.

> **A load score, not a risk score.** The HPLI is the intrinsic hazard of one kg of active substance, independent of how much is applied. Comparing two substances' scores says which is more hazardous per kg, not which contributes more hazard in practice. That requires multiplying by a use quantity, which this code does not do.

---

## Requirements

**R** with the following packages:

```r
install.packages(c(
  "readxl", "readr", "dplyr", "tidyr", "purrr",
  "stringr", "tibble", "writexl"                      # scoring and weights
))
install.packages(c(
  "ggplot2", "ggnewscale", "ggpattern",
  "patchwork", "gridExtra"                            # visualisation only
))
```

**A PPDB export.** The Pesticide Properties DataBase is a licensed product of the AERU at the University of Hertfordshire. **It is not distributed with this code**, and you need your own access to it. The scripts expect an export laid out as four workbooks in one folder:

```
General.xlsx    Fate.xlsx    Ecotox.xlsx    Human.xlsx
```

This code was written against the export of **3 May 2024**, which is also the snapshot behind every figure quoted in the documentation. The database is revised continuously — properties and quality flags, not only new substances — so a more recent export will not reproduce those figures exactly, and its column headers may differ (see [Adapting to another PPDB export](#adapting-to-another-ppdb-export)).

## Setup

Copy the settings template and fill in the path to your own export:

```r
file.copy("local_paths.example.R", "local_paths.R")
# then edit local_paths.R:
#   ppdb_export_dir <- "C:/path/to/your/PPDB"
```

`local_paths.R` is listed in `.gitignore` and is never committed, so a working local path stays local. It holds **input locations only**; each script's own output paths are settings of that script.

If it is missing, if `ppdb_export_dir` is absent from it, or if the folder does not exist, the run stops with a message naming the problem. On success the folder in use is echoed to the console, so a run is never ambiguous about which data it read.

## Running

Set the working directory to this folder — the scripts source each other by relative path — and run:

```r
source("HPLI weights.R")   # optional: recompute weights from the PPDB
source("HPLI score.R")     # the main run
```

`HPLI score.R` sources `HPLI parameters.R` and `HPLI import.R` itself. It uses the published table-1 weights by default, so `HPLI weights.R` is only needed if you want weights recomputed from your own export (see `weight_source` below).

To plot a substance from a saved run, load the full results workbook (see [Output](#output)):

```r
source("HPLI visualisation.R")
hpli_data <- load_hpli_results("HPLI_results_full.xlsx")

draw_hpli_rose("glyphosate", hpli_data)
draw_hpli_rose(c("glyphosate", "diquat", "asulam"), hpli_data, show_data_quality = TRUE)
build_hpli_score_table("asulam", hpli_data) |> View()
```

Note that the workbook must not be open in Excel while R reads it; Excel holds a lock, and the scripts report this explicitly rather than failing cryptically.

## Files

| File | Role |
|---|---|
| `HPLI parameters.R` | The indicator's definition: metrics, normalisation thresholds, compartments, default weights. **This is the file a different HPLI version replaces.** |
| `HPLI import.R` | Reads and completes the metrics from a PPDB export. Shared by the scoring and the weighting, so a metric is derived identically either way. |
| `HPLI score.R` | Settings, scoring, persistence coefficient, aggregation, output. The main entry point. |
| `HPLI weights.R` | Inverse-correlation weights computed from the export. |
| `HPLI visualisation.R` | Per-substance rose diagrams, from a saved full results workbook. Sources nothing and needs no PPDB access. |
| `local_paths.example.R` | Template for the untracked `local_paths.R`. |
| `HPLI score - natural inert substances.csv` | Hand-verified list of chemically inert substances. Optional; without it every substance is treated precautionarily. |
| `HPLI methodology.md` | Every methodological decision, its justification and its source. |
| `CHANGELOG.md` | Versions, and the differences from the published implementation. |
| `LICENSE` | The GNU General Public License, version 3. |

## Output

`HPLI score.R` writes two workbooks on every run, with the same sheets:

- `HPLI_results.xlsx`, the **publishable** one, the only one meant to be shared;
- `HPLI_results_full.xlsx`, the **full** one, kept local. It adds each metric's normalised score to the `HPLI_results` sheet (and the raw PPDB values, with `export_raw_ppdb_values = TRUE`). `HPLI visualisation.R` reads this one.

| Sheet | Contents |
|---|---|
| `Notice` | What the workbook holds, its terms of use, and how to cite the PPDB and the HPLI. |
| `HPLI_results` | One row per substance: the HPLI, the four compartment scores, a compact data-quality summary, and the share of the score attributable to values that were not measured. In the full workbook only, each metric's normalised score. |
| `HPLI_parameters` | The indicator definition this run actually used — including which weights. |
| `Data_quality` | One row per substance and metric: what happened to that value (measured, completed, substituted), how precisely it was stated, and its confidence band. |
| `Run_log` | The run's full provenance: which of the two files this is, timestamp, the export's folder name and the checksums of its four workbooks (never a local path), every setting, and the weights' own vintage. |

`HPLI weights.R` writes `HPLI_weights.xlsx` (a notice, the weights, and their own run log).

No output workbook is included in this version of the repository, and all of them are listed in `.gitignore`.

**On sharing the results.** Only the publishable workbook is meant to leave your machine. The full workbook is licensed PPDB-derived material. See [License](#license) and `HPLI methodology.md`, section "PPDB licensing and what may be shared".

## Settings

All in section 1 of `HPLI score.R`, and recorded in `Run_log` for every run.

| Setting | Default | What it does |
|---|---|---|
| `ppdb_export_dir` | *(from `local_paths.R`)* | The four-file PPDB export folder. |
| `output_file` | `"HPLI_results.xlsx"` | The publishable workbook: aggregated scores and data quality. |
| `full_output_file` | `"HPLI_results_full.xlsx"` | The full workbook, per-metric scores included. Kept local. |
| `low_hazard_reference_file` | the CSV above | Substances whose missing fate metrics mean "no such pathway" rather than "unknown". |
| `missing_policy` | `"hpli_precautionary"` | Substitute a missing value with the metric's high-hazard threshold, or (`"complete_only"`) score nothing while any metric is missing. |
| `coverage_threshold` | `0.60` | Minimum share of metrics with data for a substance to be scored. |
| `range_policy` | `"worst_case"` | How a range such as `"10-20"` resolves: the more hazardous end, or (`"mean"`) the average. |
| `synthetic_only` | `FALSE` | Restrict the run to substances the PPDB labels synthetic in origin. |
| `a_soil`, `a_water` | `180`, `7` | Persistence-coefficient reference windows, in days. |
| `weight_source` | `"table1"` | Published weights, or (`"spearman_ppdb"`) weights recomputed by `HPLI weights.R`. |
| `export_raw_ppdb_values` | `FALSE` | Whether raw PPDB values reach the full workbook. Never the publishable one. |
| `data_quality_scope` | `"computable_only"` | Whether the `Data_quality` sheet covers only the substances that were scored, or all of them. |

`HPLI weights.R` has its own `range_policy` and `weights_file`, deliberately separate so that a sensitivity run there does not disturb the scoring run.

## Adapting to another PPDB export

The column maps at the top of `HPLI import.R` translate each PPDB export header into the internal name the code uses. They were written against the 3 May 2024 PPDB export, whose headers reflect the pipeline that produced it — some fields spell out spaces and brackets, others use dots, within the very same sheet — which is why each column is mapped individually rather than by a blanket rule.

If a run stops on a missing column, or a metric comes back empty for every substance, the headers have moved. `HPLI import.R` documents the procedure in place: print the real headers and edit the right-hand side of the corresponding map entry (additionnaly compare the result against a reference run before trusting it, since a silently mis-mapped column is far more likely than a hard error).

## Adapting to another metric set

A different version of the indicator — the 27-metric Walloon set, or one adapted to another region/country — is a sibling `HPLI parameters.R`, not a fork of the calculation. Everything else iterates over whatever the active definition contains.

Two things to know before starting: a version needs a sibling import file if its extra metrics draw on PPDB columns the current maps do not cover; and **a version must compute its own weights**, since the inverse-correlation weighting is defined within a compartment and over the metrics that compartment contains, so adding or removing one metric moves every other weight in that compartment. `HPLI methodology.md`, section "Adapting the indicator to another metric set", sets this out — including why the metric list itself depends on which substances the indicator is meant to cover.

## Conventions

- Objects in `snake_case`; functions lead with a verb (`compute_`, `build_`, `parse_`, `load_`, `score_`, `draw_`). A function that *is* the value it returns keeps a noun — `persistence_coefficient()`, `hazard_direction()`, `high_hazard_value()`.
- Comments follow `roxygen2` (`#'`) wherever there is an object to document — a file header, a function, a data object. Plain `#` elsewhere. The `####` banners feed the RStudio outline.
- The *what* and *how* live in the scripts; the *why* lives in `HPLI methodology.md`.
- Scripts cross-reference the methodology **by section title**.

## Known divergences from the published values

This code presents three divergences from the published values in the *ERL* paper, all deliberate and all documented in `CHANGELOG.md` and the methodology. In short: the persistence coefficient is applied to the normalised score rather than to the raw value, which changes the two chronic aquatic metrics; correlation-based weights are computed over the active metric set only; and the "stable in water" reading is excluded from the weight calculation while kept for scoring.

## Citing

Please cite the article for the indicator, and this repository for the implementation (GitHub's "Cite this repository" button reads the same references from `CITATION.cff`):

> Vandevoorde, N., Kudsk, P., Agnan, Y. and Baret, P. V. (2025). Five methodological updates of the Danish Pesticide Load Indicator to support EU-wide pesticide risk reduction policies. *Environmental Research Letters*, 20, 124070. https://doi.org/10.1088/1748-9326/ae269b
>
> Vandevoorde, N. (2026). *HPLI: an R implementation of the Harmonised Pesticide Load Indicator* (version 0.1.0) [Computer software]. UCLouvain. https://github.com/noevandevoorde/HPLI

The method is set out in full in appendices D to I of the dissertation:

> Vandevoorde, N. (2025). *Three tools for the reduction of pesticide impacts* [Doctoral dissertation, UCLouvain]. https://hdl.handle.net/2078.5/264116

Work using the PPDB data behind the scores must also cite the PPDB, as set out under [License](#license).

## License

- **Code** — the R scripts, `local_paths.example.R` and the rest of this repository unless listed below. Copyright (C) 2026 UCLouvain; author Noé Vandevoorde. Licensed under the GNU General Public License, version 3 or (at your option) any later version: see `LICENSE`. Each script carries the identifier `SPDX-License-Identifier: GPL-3.0-or-later`.
- **Methodology** — `HPLI methodology.md` and `HPLI score - natural inert substances.csv`. Licensed under [Creative Commons Attribution 4.0 International (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/). The few PPDB values quoted in the comments of the CSV remain PPDB data, cited below.
- **Results and weights workbooks** — any workbook these scripts write. None is included in this repository. They are derived from the PPDB and covered by **neither** of the two licences above. They are subject to the AERU [terms and conditions of use of the PPDB](https://sitem.herts.ac.uk/aeru/ppdb/en/docs/Conditions_of_use.pdf), and each carries a `Notice` sheet saying so.

Data from the University of Hertfordshire's Pesticide Properties DataBase (PPDB) has been used, under Licence, to support this application. Cite the PPDB as:

> Tzilivakis, J., Lewis, K.A., Green, A. and Warner, D.J. (2026). A decade of growth and impact of the Pesticide Properties Database (PPDB). *Human and Ecological Risk Assessment: An International Journal*, 1–26. https://doi.org/10.1080/10807039.2026.2702066
>
> Lewis, K.A., Tzilivakis, J., Warner, D. and Green, A. (2016). An international database for pesticide risk assessments and management. *Human and Ecological Risk Assessment: An International Journal*, 22(4), 1050–1064. https://doi.org/10.1080/10807039.2015.1133242
