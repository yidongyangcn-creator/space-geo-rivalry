# Data

`derived/panel_country_year.csv` is tracked in this repository and is all you need to
reproduce the results. This file documents where it came from, so the build can be
re-run from scratch if needed.

## What is tracked

| Path | Contents |
|---|---|
| `derived/panel_country_year.csv` | 10,175 country-years, 176 countries, 1963–2024. The estimation panel. |
| `derived/CODEBOOK.md` | Every column, its source and its coverage. |

`raw/` and `interim/` are git-ignored. `raw/` holds third-party source files, none of
which are redistributed here — some are large, some carry their own licence terms.
`interim/` holds the numbered by-products of the build (`1.0_*`, `2.1`–`2.4`), which are
fully determined by `raw/` plus the build scripts.

## Sources to download into `raw/`

| Source | Files | Where | Notes |
|---|---|---|---|
| **GCAT** — General Catalog of Artificial Space Objects, J. McDowell | `geotab.tsv`, `launchlog.tsv`, `orgs.tsv` | <https://planet4589.org/space/gcat/> | `geotab` = 1,927 objects in the GEO belt; `launchlog` = 28,662 launches, 1957–2026; `orgs` = organisation-to-state mapping. Tab-separated with a `#`-prefixed header line and a second comment line — the readers in `code/01_build/` handle this. |
| **Strategic Rivalry data**, Colaresi, Rasler & Thompson (2008) | `strategic_rivalry_data_list_of_rivalries_by_type.csv` | Replication archive for *Strategic Rivalries in World Politics* | Dyadic rivalry list with start and end years. Mapped to COW country codes. |
| **V-Dem v15** | `V-Dem-CY-Full+Others-v15.dta` | <https://v-dem.net/data/the-v-dem-dataset/> | 1.1 GB. Only `v2x_polyarchy` is used; extract a country-year slice rather than committing the `.dta`. |
| **World Bank WDI** | `wdi/API_NY.GDP.MKTP.KD_*.csv`, `wdi/API_SP.POP.TOTL_*.csv`, `wdi/API_MS.MIL.XPND.CD_*.csv`, `wdi/API_MS.MIL.XPND.GD.ZS_*.csv`, `wdi/API_NE.EXP.GNFS.ZS_*.csv`, `wdi/API_NV.IND.TOTL.ZS_*.csv` | <https://data.worldbank.org/> | Standard wide-format WDI exports (4 header rows). Put them all in `raw/wdi/`. |
| **Correlates of War**, Formal Alliances v4.1 | `alliance_v4.1_by_*.csv` | <https://correlatesofwar.org/data-sets/formal-alliances/> | Used only in exploratory work on alliance substitution; not needed for the reported results. |

Country identifiers are harmonised to **COW country codes** throughout, via
`countrycode`, with manual overrides for Germany (255), Vietnam (816), Zanzibar → Tanzania
(511), Serbia (345), Kosovo (347) and the USSR (365). Those overrides live in
`code/01_build/15_merge_population.R`.

## Rebuilding the panel

Run the `code/01_build/` scripts in numeric order, from the repository root, once the
files above are in place:

```bash
Rscript code/01_build/11_merge_rivalry.R          # rivalry dyads -> active-rival counts
Rscript code/01_build/12_rival_share.R            # rivals' share of global 5-year GEO payloads
Rscript code/01_build/13_payload_owner_counts.R   # owner-attributed payload and launch counts
Rscript code/01_build/14_payload_agency_counts.R  # agency-attributed counts (alternative attribution)
Rscript code/01_build/15_merge_population.R       # WDI population, with COW harmonisation
```

**Two build steps have no surviving script.** The GCAT-to-panel step that produces
`1.0_geo_first_entry_panel.csv` (first-entry years, annual GEO entry counts, the at-risk
indicator) and the merge step that folds in V-Dem and the remaining WDI indicators were
done interactively and the code was not kept. The QA scripts in `code/03_exploratory/`
(`qa_geo_first_entry_panel.R`, `qa_panel_v2_1.R`) document what those steps were expected
to produce and check the result, so the gap is auditable even though it is not executable.
Anyone rebuilding from raw GCAT should read those two files first.

The project's earlier iterations survive in a separate working folder, but they do not
reconstruct these two steps: the first iteration's outcome was the establishment of a
national space command, and the second used the UCS Satellite Database rather than GCAT,
keyed on V-Dem country ids rather than COW codes. They document how the design arrived at
GEO entries as the outcome; they are not the missing build code.

This is why `derived/panel_country_year.csv` is committed rather than treated as a build
artefact: it is the earliest point in the pipeline that is fully reproducible.
