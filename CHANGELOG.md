# Changelog

All notable changes to this implementation are recorded here. The format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and versions follow [semantic versioning](https://semver.org/) — with the `0.x` series signalling that the interface is not yet stable and that a dedicated R package is a later step.

Every version records what it changes **and whether that changes the numbers**, since a run's comparability matters more here than its feature set. Where a change makes this implementation depart from the values published in [Vandevoorde et al. (2025)](https://iopscience.iop.org/article/10.1088/1748-9326/ae269b), that is stated as such.

---

## [0.1.0] — 2026-09-24

First “packaged” version: the four scoring scripts, the visualisation, and the methodological record.

### Scope

HPLI-EU, the 20-metric version of the indicator, computed for every substance of a PPDB export. Validated against the PPDB export of 3 May 2024: 2711 substances read, 890 passing the 60% coverage threshold and scored.

### Added

- **Weight calculation from the PPDB** (`HPLI weights.R`): inverse-correlation weights, computed within each compartment from Spearman correlations over the active metric set, and loadable by the scoring script in place of the published defaults. The loaded file is refused unless its metric list matches the active indicator definition exactly.
- **Data-quality traceability**: every value carries its provenance — measured, completed from a secondary field, or substituted at scoring time — along with how precisely the source stated it (`exact`, `<`, `>`, `range`) and the PPDB's own confidence band, degraded by one level whenever the value was completed rather than measured. Exported as a `Data_quality` sheet, with compact per-substance counts in the main results.
- **Contribution shares**: the percentage of the HPLI, and of each compartment score, attributable to values that were not measured — substituted and completed reported separately. Counting missing metrics says little about how much of a score rests on them; this weights each by its own contribution.
- **Rose plot visualisation** (`HPLI visualisation.R`): per-substance diagrams with one wedge per metric, angular width proportional to its aggregation weight, and the traceability drawn over it. Reads a saved results workbook and nothing else, so a figure always reflects the run it was drawn from.
- **Hand-verified inert-substance list**: for substances that are chemically inert with no plausible leaching mechanism, a missing environmental-fate metric is read as "this pathway does not apply" and substituted with the null-hazard threshold rather than the worst case.
- **Two results workbooks per run.** `HPLI_results.xlsx` is the publishable one: the HPLI, the four compartment scores and the data-quality indicators, and no per-metric score. `HPLI_results_full.xlsx` adds each metric's normalised score and stays local; the rose plots are drawn from it. Neither is included in the repository. The split follows from the normalisation itself: being piecewise linear and monotone over published thresholds, it lets a per-metric score be converted back into the PPDB value it came from. `export_raw_ppdb_values` (default `FALSE`) governs only whether the raw and completed PPDB values are added to the full workbook. *(Methodology, section "PPDB licensing and what may be shared".)*
- **A `Notice` sheet in every output workbook**, weights included: what the workbook holds, that it is derived from the PPDB and subject to the AERU terms of use rather than to the licences of the code or the methodology, and how to cite the PPDB and the HPLI.
- **Run provenance without local paths**: every run records every setting, the vintage of the weights it used, and the PPDB export it read, identified by the folder's name and the MD5 checksums of its four workbooks. The full path is echoed to the console only.
- **Licences**: the code under GPL-3.0-or-later (`LICENSE`, and an SPDX identifier at the top of each script); the methodology and the inert-substance list under CC BY 4.0.

### Changed — corrections to the published implementation

Three updates correct errors in the implementation behind the published figures, not alternative conventions. An independent reimplementation should expect exactly these differences, and no other difference from these causes.

**One affects the scores.**

1. **The persistence coefficient multiplies the normalised score**, where the published implementation divides the raw value before normalising. The two are not equivalent under a non-linear normalisation, so the **chronic aquatic invertebrate and fish NOEC scores will not reproduce the published ones** for any substance with a defined water DT50. Dividing the raw value rescales the measurement as though degradation changed the toxicity endpoint, when what degradation attenuates is the exposure; it also cannot be applied at all to the categorical human-toxicity metrics, which have no raw value to divide. *(Methodology, section "Where the persistence coefficient is applied".)*

**Two affect the weights**, and neither reaches a score under the default settings: `weight_source` is `"table1"` (from the published *ERL* paper), so a run uses the published weights unless it is explicitly pointed at recomputed ones. Both errors have the same shape — an upstream step imposing on the weight calculation a scope it did not choose.

2. **The correlation matrix is built on the active metric set.** The published implementation computed each compartment's matrix over the alternative 27-metric Walloon superset and then dropped the 7 metrics outside the EU version without recomputing, so metrics the 20-metric EU version never scores still shaped every weight it kept. The effect tracks how many metrics a compartment loses: honeybees acute moves from 7.5% to 4.6%, while human toxicity barely moves. *(Methodology, section "Weight calculation".)*
3. **The "stable in water" fill-in is kept out of the weight calculation**, and kept for scoring. Reading a free-text water DT50 as 300 days belongs to metric extraction, which scoring and weighting share, so the published weight calculation received that constant among the real values with no way to tell them apart — manufacturing agreement between water DT50 and whatever else those substances have in common. Extraction now takes the assumption as a parameter, which the weight calculation turns off; the completions that read another real PPDB field, or apply a published formula over one, are kept. Affects 12 substances of 2711; no weight moves by more than 0.19 percentage points, and only within environmental fate. *(Methodology, section "Weight calculation".)*

### Changed — behaviour

- **Missing DT50 in the persistence coefficient** is precautionary-substituted like any other missing metric, then used. The published implementation skips the coefficient entirely when *either* DT50 is missing, discarding the other even when it is known.
- **Ranges are resolved rather than discarded.** A cell holding `"10-20"` states an imprecise value, not an unknown one; it resolves to its more hazardous end by default. One such value exists in the 3 May 2024 PPDB export.
- **KFOC's confidence is degraded when falling back to KOC**, consistent with every other completion. The earlier implementation degraded its soil DT50, BCF and GUS fallbacks but not this one.
- **No fate value is fabricated from a natural-origin flag.** Automatic detection of chemically inert substances was attempted and rejected — no field or wording in the database separates them from accumulation-relevant compounds reliably — in favour of the hand-verified list.

### Known limitations

- The metric list is an **input**, not a computation. Which metrics clear the 60% coverage bar depends on the substance scope and on the data available at the time, and both move; re-deriving it is not implemented in the current version of the code.
- Only HPLI-EU is available. Locally derived versions, and a customisation of parameters and coefficients, are planned for future versions of the code.
- Missing-data imputation from a chemically related substance, or from the mean of a pesticide functional group (cf. the UK-PLI, [Tzilivakis et al. (2026)](https://doi.org/10.1080/03601234.2025.2610127)), is not implemented yet.
- The implementation computes a load score only. Risk scores and the Pesti-Score need use data this code does not read.
- No reference output is included.

---

## Reference implementation

[Vandevoorde et al. (2025)](https://iopscience.iop.org/article/10.1088/1748-9326/ae269b), and appendices D to I of [Vandevoorde (2025)](https://hdl.handle.net/2078.5/264116), describe the indicator and are the source of the thresholds and weights. The values published alongside them were produced by an earlier implementation; the differences from it are listed under 0.1.0 above.
