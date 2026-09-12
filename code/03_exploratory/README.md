# Exploratory code

Nothing in the paper depends on this directory. It is committed so that the
specification search behind the reported results is visible rather than hidden.

| File | What it was for |
|---|---|
| `grid_did_specifications.R` | Grid over jump definitions, thresholds, estimation windows and outcome transformations for the DID. Writes `did_jump_grid_results.csv` and a top-50 ranking. |
| `grid_sa_panelmatch.R` | Wider grid crossing the Sun–Abraham estimator with PanelMatch, over lags, outcome variants and sample restrictions. |
| `scm_candidate_mining.R` | Screens the panel for country-years with a clean enough pre-period to support a synthetic control. |
| `scm_case_egypt.R` | Synthetic control for Egypt, with a sweep over treatment-year definitions. |
| `scm_case_venezuela.R` | Synthetic control for Venezuela. |
| `qa_geo_first_entry_panel.R` | Consistency checks on the GEO first-entry panel: duplicate country-years, unmapped COW codes, at-risk and first-entry-year contradictions, entry counts below entry indicators. Writes problem files. |
| `qa_panel_v2_1.R` | The same style of checks one merge stage later. |

**Why the SCM work is not in the paper.** The donor pool for a country entering GEO is
thin and the pre-treatment fit was poor for every candidate except a handful of cases; the
gap plots were not stable across reasonable choices of treatment year. The threshold sweep
in `scm_case_egypt.R` is the clearest illustration of that instability. The DID design
replaced it.

**Reading the grid files honestly.** `grid_did_specifications.R` and
`grid_sa_panelmatch.R` evaluate hundreds of specifications. The specification reported in
the paper — jump relative to a 3-year mean, threshold 0.2 pp, 1970–2020, `log(1 + entries)` —
was chosen for the substantive reasons given in Section 3.3, and the threshold sensitivity
figure (`code/02_analysis/23_robustness.R`) shows how the estimate moves across thresholds.
These grid scripts are not a multiple-testing correction and should not be read as one.

The QA scripts also serve a second purpose: they document what the two missing build steps
were expected to produce. See `data/README.md`.
