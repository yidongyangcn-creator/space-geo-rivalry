# Codebook — `panel_country_year.csv`

10,175 rows. One row per country-year. 176 countries, 1963–2024. Unbalanced: countries
enter as they become independent and exit with data availability. `ccode` × `year`
uniquely identifies a row.

Coverage is the share of the 10,175 rows that are non-missing.

## Identifiers

| Column | Type | Coverage | Description |
|---|---|---|---|
| `country_name` | chr | 100% | Country name as carried through the merge. |
| `ccode` | int | 100% | Correlates of War country code. Primary key with `year`. |
| `cowcode` | int | 100% | COW code as supplied by the source merge; equals `ccode` after harmonisation. |
| `year` | int | 100% | Calendar year, 1963–2024. |

## Outcome variables

| Column | Type | Coverage | Description |
|---|---|---|---|
| `n_geo_entries` | int | 100% | **Main dependent variable.** New GEO entries attributed to the country in that year, by satellite-owner state code. Mean 0.090, SD 0.712, max 18. 97.4% of rows are zero. |
| `geo_entry_any` | 0/1 | 100% | 1 if `n_geo_entries > 0`. Mean 0.032. |
| `n_geo_launches` | int | 100% | Launch events carrying that country's GEO payloads. |
| `first_entry_year` | int | 21.1% | Year of the country's first GEO entry. Missing for the 140 countries that never enter. |
| `event` | 0/1 | 100% | Failure indicator for the Cox model: 1 in the year of first GEO entry. |
| `at_risk` | 0/1 | 100% | 1 while the country is still in the risk set (at or before first entry). Defines the Cox sample. |

## Space-activity measures

| Column | Type | Coverage | Description |
|---|---|---|---|
| `n_payloads_owner` | int | 100% | Payloads launched with this country as satellite-owner state, that year. |
| `n_launch_events_owner` | int | 100% | Distinct launch events including this country's payloads. |
| `payloads_5yr_owner` | int | 100% | Rolling 5-year sum of `n_payloads_owner` (t−4 … t). The capacity measure. |
| `world_payloads_5yr` | int | 100% | Global 5-year payload total, the denominator for the share measures. |
| `payload_share_5yr_owner` | num | 100% | `payloads_5yr_owner / world_payloads_5yr`, in [0, 1]. |

## Rivalry measures

| Column | Type | Coverage | Description |
|---|---|---|---|
| `rival_count_active` | int | 100% | Number of strategic rivals active that year. Mean 0.736, max 8. 102 of 176 countries have at least one rivalry at some point. |
| `rivals_payloads_5yr_owner_sum` | int | 100% | 5-year payload total summed across the country's active rivals. |
| `rivals_share_5yr_owner` | num | 100% | **Main treatment variable.** Rivals' share of all global GEO payloads acquired over the previous five years, in [0, 1]. Mean 0.014, SD 0.087. The paper uses it in percentage points: `R_pct = 100 × rivals_share_5yr_owner`. |

**Treatment construction.** The jump is `J_it = R_pct_it − mean(R_pct_{i,t−1}, R_pct_{i,t−2}, R_pct_{i,t−3})`, and the treatment year `T_i` is the first year with `J_it ≥ 0.2`. Restricted to 1970–2020 with at least 5 pre- and 5 post-treatment years, this gives 29 treated countries and 143 never-treated controls. The function `build_did_sample()` in `code/00_setup.R` is the only implementation of this; every analysis script calls it.

## Controls

| Column | Type | Coverage | Source | Description |
|---|---|---|---|---|
| `v2x_polyarchy` | num | 100% | V-Dem v15 | Electoral democracy index, [0, 1]. Mean 0.426. |
| `gdp_kd` | num | 91.2% | WDI `NY.GDP.MKTP.KD` | GDP, constant USD. Used as `log(gdp_kd)`. |
| `pop_total` | num | 98.8% | WDI `SP.POP.TOTL` | Population. Used as `log(pop_total)`. |
| `mil_exp_cd` | num | 77.2% | WDI `MS.MIL.XPND.CD` (SIPRI) | Military expenditure, current USD. The paper reports it in billions. |
| `mil_exp_gdp_pct` | num | 74.1% | WDI `MS.MIL.XPND.GD.ZS` | Military expenditure as % of GDP. Mean 2.66. |
| `industry_gdp_pct` | num | 74.4% | WDI `NV.IND.TOTL.ZS` | Industry value added as % of GDP. Mean 27.5. The capacity control in the Cox models. |
| `exports_gdp_pct` | num | 75.6% | WDI `NE.EXP.GNFS.ZS` | Exports as % of GDP. Not used in the reported specifications. |

Coverage is thinner for the composition-of-the-economy variables, concentrated in small
states and conflict zones in the earlier decades. The OLS table holds the sample fixed
across columns so that the incremental specifications stay comparable.
