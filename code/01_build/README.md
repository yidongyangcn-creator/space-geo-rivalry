# Panel construction

Run these in numeric order from the repository root, after placing the third-party
sources in `data/raw/` as described in `data/README.md`. Intermediate files land in
`data/interim/` (git-ignored).

| Script | In | Out |
|---|---|---|
| `11_merge_rivalry.R` | `1.0_geo_first_entry_panel.csv`, strategic rivalry list | `1.1_*_plus_rivals5yr.csv` — active-rival counts and rivals' 5-year payload totals |
| `12_rival_share.R` | `1.1_*` | `1.2_*_plus_rivalshare.csv` — rivals' share of global 5-year payloads |
| `13_payload_owner_counts.R` | `2.1.csv`, `launchlog.tsv` | `2.1.csv` (in place) — payload counts by satellite-owner state |
| `14_payload_agency_counts.R` | `2.1.csv`, `launchlog.tsv`, `orgs.tsv` | `2.1.csv` (in place) — the same counts under agency-based attribution |
| `15_merge_population.R` | `2.3.csv`, WDI population | `2.3_with_pop.csv` — population merged on harmonised COW codes |

Note that scripts 13 and 14 **overwrite their input in place**. They are two alternative
attribution rules for the same underlying launches — owner-state versus operating-agency —
and the paper uses the owner-state version (13). Running 14 after 13 replaces those
columns. Keep a copy of `2.1.csv` before running either.

## The gap

Two steps in the chain have no surviving script:

1. **GCAT → `1.0_geo_first_entry_panel.csv`.** Parsing `geotab.tsv` and `launchlog.tsv`
   into country-year GEO entry counts, first-entry years and the at-risk indicator.
2. **`2.1_clean.csv` → `2.2.csv` → `2.3.csv`.** Merging V-Dem `v2x_polyarchy` and the
   remaining WDI indicators.

Both were done interactively and the code was not saved. The QA scripts in
`code/03_exploratory/` check exactly what those steps should produce — duplicate
country-years, unmapped codes, at-risk and first-entry-year consistency, entry counts
against entry indicators — so the expected output is specified even though the
transformation is not reproducible from source.

Earlier iterations of this project (a space-command outcome, then a UCS Satellite
Database version) exist outside this repository. They use different sources and a
different country key, so they do not fill these two gaps.

This is why `data/derived/panel_country_year.csv` is tracked in the repository rather than
rebuilt. Everything downstream of it is fully reproducible from a fresh clone.
