* ---------------------------------------------------------------------------
* Run from the repository root:  do code/02_analysis/<this file>
global proj "`c(pwd)'"
global panel "$proj/data/derived/panel_country_year.csv"
global outtab "$proj/output/tables"
* ---------------------------------------------------------------------------

clear all
import delimited "$panel", clear

* 基本处理
gen ln_gdp_kd    = ln(gdp_kd) if gdp_kd > 0
gen ln_pop_total = ln(pop_total) if pop_total > 0
gen mil_exp_bil  = mil_exp_cd / 1000000000

* 为了让各列可比，固定同一批样本
gen ols_sample = !missing(n_geo_entries, rivals_share_5yr_owner, ///
                          ln_gdp_kd, ln_pop_total, mil_exp_bil, ///
                          year, ccode)

* 变量标签（让表格好看一点）
label var rivals_share_5yr_owner "Rival share (5yr)"
label var ln_gdp_kd              "Log GDP"
label var ln_pop_total           "Log population"
label var mil_exp_bil            "Military expenditure (bn USD)"

* 如果没装 esttab/eststo
cap which esttab
if _rc ssc install estout

eststo clear

* (1) 只放 rivalry
quietly reg n_geo_entries rivals_share_5yr_owner i.year ///
    if ols_sample == 1, vce(cluster ccode)
eststo m1
estadd local YearFE "Yes"

* (2) + GDP
quietly reg n_geo_entries rivals_share_5yr_owner ln_gdp_kd i.year ///
    if ols_sample == 1, vce(cluster ccode)
eststo m2
estadd local YearFE "Yes"

* (3) + Population
quietly reg n_geo_entries rivals_share_5yr_owner ln_gdp_kd ln_pop_total i.year ///
    if ols_sample == 1, vce(cluster ccode)
eststo m3
estadd local YearFE "Yes"

* (4) + Military expenditure
quietly reg n_geo_entries rivals_share_5yr_owner ln_gdp_kd ln_pop_total mil_exp_bil i.year ///
    if ols_sample == 1, vce(cluster ccode)
eststo m4
estadd local YearFE "Yes"

* 如果你想再多一列，可以把下面打开
quietly reg n_geo_entries rivals_share_5yr_owner ln_gdp_kd ln_pop_total ///
    mil_exp_bil v2x_polyarchy i.year if ols_sample == 1, vce(cluster ccode)
eststo m5
estadd local YearFE "Yes"

* 导出 LaTeX 表
esttab m1 m2 m3 m4 using "$outtab/ols_incremental.tex", replace ///
    booktabs label se star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("(1)" "(2)" "(3)" "(4)") ///
    keep(rivals_share_5yr_owner ln_gdp_kd ln_pop_total mil_exp_bil) ///
    stats(YearFE N r2, labels("Year FE" "N" "R^2")) ///
    nonumbers compress

* 如果你想先直接在 Stata 里看，不导出文件，就用：
* esttab m1 m2 m3 m4, booktabs label se star(* 0.10 ** 0.05 *** 0.01) ///
*     mtitles("(1)" "(2)" "(3)" "(4)") ///
*     keep(rivals_share_5yr_owner ln_gdp_kd ln_pop_total mil_exp_bil) ///
*     stats(YearFE N r2, labels("Year FE" "N" "R^2")) ///
*     nonumbers compress
