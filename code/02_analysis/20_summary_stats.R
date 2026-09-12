## ---------------------------------------------------------------------------
## 20_summary_stats.R — Table 1 of the paper (summary statistics).
## Writes output/tables/summary_statistics.{csv,tex}
## ---------------------------------------------------------------------------

if (!exists("PROJ")) source("code/00_setup.R")

panel <- read_csv(PANEL, show_col_types = FALSE)

vars <- tribble(
  ~name,                 ~label,                              ~transform,
  "n_geo_entries",       "Annual GEO entries",                 identity,
  "geo_entry_any",       "Any GEO entry",                      identity,
  "rivals_share_5yr_owner", "Rival GEO share",                 identity,
  "rival_count_active",  "Active rival count",                 identity,
  "v2x_polyarchy",       "V-Dem polyarchy",                    identity,
  "gdp_kd",              "ln(GDP), constant USD",              log,
  "pop_total",           "ln(Population)",                     log,
  "mil_exp_gdp_pct",     "Military expenditure / GDP (%)",     identity,
  "industry_gdp_pct",    "Industry / GDP (%)",                 identity
)

summ <- purrr::pmap_dfr(vars, function(name, label, transform) {
  x <- transform(panel[[name]])
  x <- x[is.finite(x)]
  tibble(Variable = label, N = length(x), Mean = mean(x), SD = sd(x),
         Min = min(x), Max = max(x))
})

cat("\n=== Table 1: Summary statistics ===\n")
cat(sprintf("Panel: %d country-years, %d countries, %d-%d\n\n",
            nrow(panel), dplyr::n_distinct(panel$ccode),
            min(panel$year), max(panel$year)))
print(as.data.frame(summ), digits = 4, row.names = FALSE)

write_csv(summ, file.path(OUT_TAB, "summary_statistics.csv"))

## LaTeX (booktabs) version, to be \input{} from the paper
tex <- c(
  "\\begin{tabular}{lrrrrr}", "\\toprule",
  "Variable & $N$ & Mean & Std. Dev. & Min & Max \\\\", "\\midrule",
  sprintf("%s & %s & %.3f & %.3f & %.3f & %.3f \\\\",
          summ$Variable, format(summ$N, big.mark = ","),
          summ$Mean, summ$SD, summ$Min, summ$Max),
  "\\bottomrule", "\\end{tabular}")
writeLines(tex, file.path(OUT_TAB, "summary_statistics.tex"))

cat("\nWrote output/tables/summary_statistics.{csv,tex}\n")
