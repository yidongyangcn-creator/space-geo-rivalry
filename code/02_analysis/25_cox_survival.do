* ---------------------------------------------------------------------------
* Run from the repository root:  do code/02_analysis/<this file>
global proj "`c(pwd)'"
global panel "$proj/data/derived/panel_country_year.csv"
global outtab "$proj/output/tables"
* ---------------------------------------------------------------------------

clear all
set more off

import delimited using "$panel", clear varnames(1)

* Construct variables
gen rivals_share_pct = 100 * rivals_share_5yr_owner
gen ln_pop          = ln(pop_total)
gen ln_gdp          = ln(gdp_kd)
gen rivals_asinh    = asinh(rivals_share_pct)

* Survival setup
stset year, id(ccode) failure(event==1)

* Core models
* Model 1: bivariate
stcox rivals_share_5yr_owner, vce(cluster ccode)

* Model 2: add baseline development control
stcox rivals_share_5yr_owner ln_gdp, vce(cluster ccode)

* Model 3: main preferred scale + capacity controls
stcox rivals_share_pct industry_gdp_pct ln_pop, vce(cluster ccode)

* Model 4: add military spending control
stcox rivals_share_pct mil_exp_gdp_pct industry_gdp_pct ln_pop, vce(cluster ccode)

* Functional form check (quadratic in raw % scale)
stcox c.rivals_share_pct##c.rivals_share_pct industry_gdp_pct ln_pop, vce(cluster ccode) nohr

* Distribution check
sum rivals_share_pct if e(sample), detail

* Sensitivity check: transformed exposure
stcox c.rivals_asinh##c.rivals_asinh industry_gdp_pct ln_pop, vce(cluster ccode) nohr

xtset ccode year
reg n_geo_entries rivals_share_5yr_owner ln_gdp ln_pop mil_exp_cd i.year, vce(cluster ccode)
outreg2 using "$outtab/cox_ols_companion.doc", replace

gen mil_exp_bil = mil_exp_cd/1e9
reg n_geo_entries rivals_share_5yr_owner ln_gdp ln_pop mil_exp_bil i.year i.ccode, vce(cluster ccode)
outreg2 using "$outtab/cox_ols_companion.doc", append
