## ---------------------------------------------------------------------------
## 23_robustness.R — Section 5 of the paper (sensitivity and robustness).
##
## Regenerates, from data/derived/panel_country_year.csv alone:
##   cohort_distribution.png    Fig. 2   treatment cohorts over time
##   bacon_decomposition.png    App. C   Goodman-Bacon (2021) weight decomposition
##   placebo_permutation.png    Fig. 3   1,000 random re-assignments of treatment
##   heterogeneity_capacity.png Fig. 4   H2: effect split by pre-treatment capacity
##   leave_one_cohort_out.png   Fig. 6   drop each cohort in turn
##   threshold_sensitivity.png  Fig. 7   vary the jump threshold J >= delta
##
## Reference values this file should reproduce (paper, Section 4-5):
##   baseline TWFE beta          = 0.0611 (SE 0.0261, p = 0.0205)
##   space-active subsample beta ~ 0.19-0.20
##   placebo permutation p-value ~ 0.02
##
## Usage:  Rscript code/02_analysis/23_robustness.R      (from the repo root)
## ---------------------------------------------------------------------------

if (!exists("PROJ")) source("code/00_setup.R")

suppressPackageStartupMessages({
  library(fixest)
  library(slider)
})
HAS_BACON <- requireNamespace("bacondecomp", quietly = TRUE)

set.seed(20260101)

d <- build_did_sample()

fit_did <- function(dat) feols(y_log1p ~ D | ccode + year, data = dat, cluster = ~ccode)

m_base  <- fit_did(d)
ct_base <- coeftable(m_base)["D", ]
b_base  <- ct_base[["Estimate"]]

cat("\n=== Baseline TWFE DID (paper Eq. 5) ===\n")
cat(sprintf("  beta = %.4f  SE = %.4f  p = %.4f   N = %d\n",
            b_base, ct_base[["Std. Error"]], ct_base[["Pr(>|t|)"]], nobs(m_base)))


## ---------------------------------------------------------------------------
## 1) Cohort distribution
## ---------------------------------------------------------------------------

cohort_tab <- d |>
  filter(ok) |> distinct(ccode, cohort) |>
  count(cohort, name = "n_countries")

cat("\n=== Treatment cohorts ===\n"); print(cohort_tab)

p_cohort <- ggplot(cohort_tab, aes(cohort, n_countries)) +
  geom_col(width = 1.6, fill = "grey35") +
  scale_x_continuous(breaks = seq(1970, 2020, 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(title    = "Distribution of rival-pressure shock cohorts",
       subtitle = sprintf("Treatment year T_i = first year with a jump of >= %.1f pp; %d treated countries",
                          JUMP_THR, sum(cohort_tab$n_countries)),
       x = "Treatment year", y = "Number of countries")
save_fig(p_cohort, "cohort_distribution", width = 8, height = 4.5)


## ---------------------------------------------------------------------------
## 2) Goodman-Bacon decomposition
##    Needs a balanced panel, so restrict to countries observed in every year.
## ---------------------------------------------------------------------------

if (HAS_BACON) {
  bal <- d |> count(ccode) |> filter(n == max(n)) |> pull(ccode)
  d_bal <- d |> filter(ccode %in% bal)

  bacon_out <- try(
    bacondecomp::bacon(y_log1p ~ D, data = as.data.frame(d_bal),
                       id_var = "ccode", time_var = "year"),
    silent = TRUE)

  if (!inherits(bacon_out, "try-error")) {
    bc <- if (is.list(bacon_out) && !is.data.frame(bacon_out)) bacon_out$two_by_twos else bacon_out
    wavg <- sum(bc$estimate * bc$weight) / sum(bc$weight)

    cat("\n=== Goodman-Bacon decomposition ===\n")
    print(bc |> group_by(type) |>
            summarise(weight = sum(weight), estimate = weighted.mean(estimate, weight),
                      .groups = "drop"))
    cat(sprintf("  weighted average of 2x2s = %.4f (TWFE beta = %.4f)\n", wavg, b_base))

    p_bacon <- ggplot(bc, aes(weight, estimate, colour = type, shape = type)) +
      geom_hline(yintercept = b_base, linetype = "dashed", colour = "grey40") +
      geom_point(size = 2.4, alpha = .85) +
      labs(title    = "Goodman-Bacon decomposition of the TWFE estimate",
           subtitle = sprintf("Dashed line = overall TWFE beta (%.4f); balanced sub-panel", b_base),
           x = "Weight", y = "2x2 DID estimate", colour = NULL, shape = NULL)
    save_fig(p_bacon, "bacon_decomposition", width = 8, height = 5)
  } else {
    message("bacondecomp::bacon() failed — skipping the decomposition figure.")
  }
} else {
  message("Package 'bacondecomp' not installed — skipping the decomposition figure.")
}


## ---------------------------------------------------------------------------
## 3) Placebo permutation test
##    Randomly re-assign the observed cohort years to random countries,
##    holding the cohort-year distribution fixed. 1,000 draws.
## ---------------------------------------------------------------------------

N_PERM <- 1000
obs_cohorts <- d |> filter(ok) |> distinct(ccode, cohort) |> pull(cohort)
all_ccodes  <- unique(d$ccode)
d_lean      <- d |> select(ccode, year, y_log1p)

placebo <- vapply(seq_len(N_PERM), function(i) {
  picked <- sample(all_ccodes, length(obs_cohorts))
  fake   <- tibble(ccode = picked, fake_cohort = sample(obs_cohorts))
  dd <- d_lean |> left_join(fake, by = "ccode") |>
    mutate(D = as.integer(!is.na(fake_cohort) & year >= fake_cohort))
  coef(feols(y_log1p ~ D | ccode + year, data = dd, notes = FALSE))[["D"]]
}, numeric(1))

p_perm <- mean(placebo >= b_base)
cat("\n=== Placebo permutation test ===\n")
cat(sprintf("  draws = %d | placebo mean = %.4f | SD = %.4f\n",
            N_PERM, mean(placebo), sd(placebo)))
cat(sprintf("  %d of %d placebo betas >= observed %.4f  ->  permutation p = %.3f\n",
            sum(placebo >= b_base), N_PERM, b_base, p_perm))

p_placebo <- ggplot(tibble(b = placebo), aes(b)) +
  geom_histogram(bins = 40, fill = "grey70", colour = "white") +
  geom_vline(xintercept = b_base, colour = "firebrick", linewidth = .9) +
  annotate("text", x = b_base, y = Inf, label = sprintf("  observed = %.4f", b_base),
           hjust = 0, vjust = 1.8, colour = "firebrick", size = 3.6) +
  labs(title    = "Placebo distribution from 1,000 random treatment assignments",
       subtitle = sprintf("Permutation p-value = %.3f", p_perm),
       x = "Placebo DID coefficient", y = "Count")
save_fig(p_placebo, "placebo_permutation", width = 8, height = 4.8)


## ---------------------------------------------------------------------------
## 4) Heterogeneity by pre-treatment capacity (H2)
##    Two capacity splits:
##      space  — any GEO entry before treatment (before 1990 for controls)
##      gdp    — above-median average GDP over the estimation window
## ---------------------------------------------------------------------------

ref_year <- ifelse(d$ok, d$cohort, 1990L)
pre_activity <- d |> mutate(ref = ref_year) |> filter(year < ref) |>
  group_by(ccode) |> summarise(pre_entries = sum(n_geo_entries, na.rm = TRUE), .groups = "drop")

gdp_mean <- d |> group_by(ccode) |>
  summarise(gdp_avg = mean(gdp_kd, na.rm = TRUE), .groups = "drop")

d <- d |>
  left_join(pre_activity, by = "ccode") |>
  left_join(gdp_mean, by = "ccode") |>
  mutate(
    cap_space = as.integer(coalesce(pre_entries, 0) > 0),
    cap_gdp   = as.integer(!is.na(gdp_avg) & gdp_avg >= median(gdp_mean$gdp_avg, na.rm = TRUE))
  )

het_rows <- bind_rows(lapply(
  list(c("cap_space", "Prior GEO activity"), c("cap_gdp", "Above-median GDP")),
  function(v) {
    var <- v[1]; lab <- v[2]
    bind_rows(lapply(c(1, 0), function(g) {
      sub <- d[d[[var]] == g, ]
      m   <- fit_did(sub); ct <- coeftable(m)["D", ]
      tibble(split = lab,
             group = if (g == 1) "High capacity" else "Low capacity",
             est = ct[["Estimate"]], se = ct[["Std. Error"]],
             n = nobs(m), k = n_distinct(sub$ccode))
    }))
  }))

cat("\n=== Heterogeneity by pre-treatment capacity (split samples) ===\n")
print(het_rows)

# Interacted specification, reported alongside the splits
cat("\nInteracted specification:\n")
print(coeftable(feols(y_log1p ~ D + D:cap_space | ccode + year, data = d, cluster = ~ccode)))

p_het <- ggplot(het_rows, aes(est, group, colour = group)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = est - 1.96 * se, xmax = est + 1.96 * se), height = .16) +
  geom_point(size = 2.8) +
  facet_wrap(~split, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = c("High capacity" = "firebrick", "Low capacity" = "steelblue")) +
  guides(colour = "none") +
  labs(title    = "Treatment effect by pre-treatment capacity",
       subtitle = "Separate TWFE DID on each subsample; 95% CI, clustered by country",
       x = "DID coefficient on log(1 + GEO entries)", y = NULL)
save_fig(p_het, "heterogeneity_capacity", width = 8, height = 5)


## ---------------------------------------------------------------------------
## 5) Leave-one-cohort-out
## ---------------------------------------------------------------------------

loco <- bind_rows(lapply(sort(unique(d$cohort[d$ok])), function(cyr) {
  sub <- d |> filter(!(ok & cohort == cyr))
  ct  <- coeftable(fit_did(sub))["D", ]
  tibble(dropped = cyr, est = ct[["Estimate"]], se = ct[["Std. Error"]])
}))

cat("\n=== Leave-one-cohort-out ===\n"); print(loco)

p_loco <- ggplot(loco, aes(factor(dropped), est)) +
  geom_hline(yintercept = b_base, linetype = "dashed", colour = "firebrick") +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_errorbar(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), width = .2) +
  geom_point(size = 2.2) +
  labs(title    = "Leave-one-cohort-out estimates",
       subtitle = sprintf("Dashed line = full-sample beta (%.4f); 95%% CI, clustered by country", b_base),
       x = "Cohort dropped", y = "DID coefficient") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_fig(p_loco, "leave_one_cohort_out", width = 8.5, height = 5)


## ---------------------------------------------------------------------------
## 6) Threshold sensitivity
## ---------------------------------------------------------------------------

thresholds <- c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.50, 1.00)

thr_tab <- bind_rows(lapply(thresholds, function(thr) {
  dt <- build_did_sample(thr = thr)
  ct <- coeftable(fit_did(dt))["D", ]
  tibble(threshold = thr,
         n_treated = n_distinct(dt$ccode[dt$ok]),
         est = ct[["Estimate"]], se = ct[["Std. Error"]], p = ct[["Pr(>|t|)"]])
}))

cat("\n=== Threshold sensitivity ===\n"); print(thr_tab)

p_thr <- ggplot(thr_tab, aes(threshold, est)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_ribbon(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se),
              fill = "steelblue", alpha = .18) +
  geom_line(colour = "steelblue") +
  geom_point(size = 2.2, colour = "steelblue") +
  geom_vline(xintercept = JUMP_THR, linetype = "dotted", colour = "firebrick") +
  geom_text(aes(label = n_treated), vjust = -1.4, size = 3, colour = "grey30") +
  labs(title    = "Sensitivity to the treatment threshold",
       subtitle = "Point labels = number of treated countries; dotted line = threshold used in the paper",
       x = expression("Jump threshold " * delta * " (percentage points)"),
       y = "DID coefficient")
save_fig(p_thr, "threshold_sensitivity", width = 8, height = 5)


## ---------------------------------------------------------------------------
## 7) Save every number behind the figures
## ---------------------------------------------------------------------------

write_csv(cohort_tab, file.path(OUT_TAB, "cohort_distribution.csv"))
write_csv(het_rows,   file.path(OUT_TAB, "heterogeneity_capacity.csv"))
write_csv(loco,       file.path(OUT_TAB, "leave_one_cohort_out.csv"))
write_csv(thr_tab,    file.path(OUT_TAB, "threshold_sensitivity.csv"))
write_csv(tibble(draw = seq_along(placebo), beta = placebo),
          file.path(OUT_TAB, "placebo_permutation_draws.csv"))

cat("\nDone. Figures in output/figures, numbers in output/tables.\n")
