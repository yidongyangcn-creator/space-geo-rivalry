## ---------------------------------------------------------------------------
## 26_cox_survival.R — Appendix E: the extensive margin (H3).
## R port of code/02_analysis/25_cox_survival.do, plus the appendix figures.
##
## Question: does rivalry explain *first* entry into GEO, or is first entry
## driven by capacity? The paper's answer is the latter (H3).
##
## Produces:
##   km_survival_rivalry_full.png   Kaplan-Meier, high vs low rival pressure
##   schoenfeld_residuals.png       proportional-hazards diagnostic
##   cox_predicted_survival.png     predicted survival at capacity quartiles
##   cox_stratified_capacity.png    coefficients by capacity stratum
##   cox_model_fit.png              likelihood-ratio / concordance comparison
##   output/tables/cox_models.csv   the coefficient table
##
## Usage:  Rscript code/02_analysis/26_cox_survival.R
## ---------------------------------------------------------------------------

if (!exists("PROJ")) source("code/00_setup.R")

suppressPackageStartupMessages({
  library(survival)
  library(broom)
})

raw <- read_csv(PANEL, show_col_types = FALSE)

## ---- Counting-process format over the at-risk spell ------------------------
## at_risk == 1 marks country-years before (and including) first GEO entry.
surv <- raw |>
  filter(at_risk == 1) |>
  arrange(ccode, year) |>
  group_by(ccode) |>
  mutate(tstart = year - min(year), tstop = tstart + 1) |>
  ungroup() |>
  mutate(
    rivals_share_pct = 100 * rivals_share_5yr_owner,
    rivals_asinh     = asinh(rivals_share_pct),
    ln_pop           = log(pop_total),
    ln_gdp           = log(gdp_kd)
  )

base_vars <- c("rivals_share_pct", "industry_gdp_pct", "ln_pop",
               "event", "tstart", "tstop", "ccode", "gdp_kd")
cc  <- surv |> filter(if_all(all_of(base_vars), ~ !is.na(.)))
mil <- cc   |> filter(!is.na(mil_exp_gdp_pct))

cat(sprintf("\nRisk set: %d country-years, %d countries, %d first-entry events\n",
            nrow(cc), n_distinct(cc$ccode), sum(cc$event)))


## ---------------------------------------------------------------------------
## 1) Cox models (mirrors the Stata do-file)
## ---------------------------------------------------------------------------

m1 <- coxph(Surv(tstart, tstop, event) ~ rivals_share_5yr_owner,
            data = cc, cluster = ccode)
m2 <- coxph(Surv(tstart, tstop, event) ~ rivals_share_5yr_owner + ln_gdp,
            data = cc |> filter(!is.na(ln_gdp)), cluster = ccode)
m3 <- coxph(Surv(tstart, tstop, event) ~ rivals_share_pct + industry_gdp_pct + ln_pop,
            data = cc, cluster = ccode)
m4 <- coxph(Surv(tstart, tstop, event) ~ rivals_share_pct + mil_exp_gdp_pct +
                                         industry_gdp_pct + ln_pop,
            data = mil, cluster = ccode)
m_quad <- coxph(Surv(tstart, tstop, event) ~ rivals_share_pct + I(rivals_share_pct^2) +
                                             industry_gdp_pct + ln_pop,
                data = cc, cluster = ccode)
m_asinh <- coxph(Surv(tstart, tstop, event) ~ rivals_asinh + I(rivals_asinh^2) +
                                              industry_gdp_pct + ln_pop,
                 data = cc, cluster = ccode)

models <- list("(1) Rivalry only" = m1, "(2) + ln GDP" = m2,
               "(3) + capacity" = m3, "(4) + military burden" = m4,
               "(5) quadratic" = m_quad, "(6) asinh" = m_asinh)

cox_tab <- bind_rows(lapply(names(models), function(nm)
  tidy(models[[nm]], conf.int = TRUE, exponentiate = FALSE) |> mutate(model = nm)))

cat("\n=== Cox proportional-hazards models ===\n")
for (nm in names(models)) { cat("\n--", nm, "--\n"); print(summary(models[[nm]])$coefficients) }
write_csv(cox_tab, file.path(OUT_TAB, "cox_models.csv"))


## ---------------------------------------------------------------------------
## 2) Kaplan-Meier: high vs low rival pressure
## ---------------------------------------------------------------------------

km_dat <- cc |>
  group_by(ccode) |>
  mutate(rival_group = if_else(mean(rivals_share_pct, na.rm = TRUE) >
                                 median(cc$rivals_share_pct, na.rm = TRUE),
                               "High rival pressure", "Low rival pressure")) |>
  ungroup()

km <- survfit(Surv(tstart, tstop, event) ~ rival_group, data = km_dat)
km_df <- broom::tidy(km) |>
  mutate(strata = sub("rival_group=", "", strata))

lr <- survdiff(Surv(tstart, tstop, event) ~ rival_group, data = km_dat)
lr_p <- pchisq(lr$chisq, df = length(lr$n) - 1, lower.tail = FALSE)
cat(sprintf("\nLog-rank test, high vs low rival pressure: chi2 = %.2f, p = %.3f\n",
            lr$chisq, lr_p))

p_km <- ggplot(km_df, aes(time, estimate, colour = strata, fill = strata)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .15, colour = NA) +
  geom_step(linewidth = .8) +
  scale_colour_manual(values = c("High rival pressure" = "firebrick",
                                 "Low rival pressure"  = "steelblue")) +
  scale_fill_manual(values   = c("High rival pressure" = "firebrick",
                                 "Low rival pressure"  = "steelblue")) +
  labs(title    = "Time to first GEO entry, by rival pressure",
       subtitle = sprintf("Kaplan-Meier survival with 95%% CI; log-rank p = %.3f", lr_p),
       x = "Years in the risk set", y = "Share not yet entered GEO",
       colour = NULL, fill = NULL)
save_fig(p_km, "km_survival_rivalry_full", width = 8, height = 5)


## ---------------------------------------------------------------------------
## 3) Proportional-hazards diagnostic (Schoenfeld residuals)
## ---------------------------------------------------------------------------

zph <- cox.zph(m3)
cat("\n=== Schoenfeld residual test (model 3) ===\n"); print(zph)

sch <- as.data.frame(zph$y)
sch$time <- zph$x
sch_long <- sch |>
  pivot_longer(-time, names_to = "term", values_to = "residual") |>
  mutate(term = recode(term,
                       rivals_share_pct = "Rivals' share (%)",
                       industry_gdp_pct = "Industry / GDP (%)",
                       ln_pop           = "ln(population)"))

p_sch <- ggplot(sch_long, aes(time, residual)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(alpha = .35, size = .9) +
  geom_smooth(method = "loess", formula = y ~ x, se = TRUE, colour = "firebrick") +
  facet_wrap(~term, scales = "free_y") +
  labs(title    = "Schoenfeld residuals",
       subtitle = "A slope in time indicates a violation of proportional hazards",
       x = "Time", y = "Scaled Schoenfeld residual")
save_fig(p_sch, "schoenfeld_residuals", width = 9, height = 4.5)


## ---------------------------------------------------------------------------
## 4) Predicted survival at capacity quartiles
## ---------------------------------------------------------------------------

qs <- quantile(cc$industry_gdp_pct, c(.10, .50, .90), na.rm = TRUE)
newd <- data.frame(
  rivals_share_pct = mean(cc$rivals_share_pct, na.rm = TRUE),
  industry_gdp_pct = as.numeric(qs),
  ln_pop           = mean(cc$ln_pop, na.rm = TRUE)
)
sf <- survfit(m3, newdata = newd)
pred <- bind_rows(lapply(seq_len(nrow(newd)), function(i)
  tibble(time = sf$time, surv = sf$surv[, i],
         lo = sf$lower[, i], hi = sf$upper[, i],
         group = sprintf("Industry/GDP = %.1f%% (p%d)",
                         newd$industry_gdp_pct[i], c(10, 50, 90)[i]))))

p_pred <- ggplot(pred, aes(time, surv, colour = group, fill = group)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = .12, colour = NA) +
  geom_step(linewidth = .8) +
  labs(title    = "Predicted time to first GEO entry, by industrial capacity",
       subtitle = "Cox model (3), rival pressure and population held at their means",
       x = "Years in the risk set", y = "Predicted survival",
       colour = NULL, fill = NULL)
save_fig(p_pred, "cox_predicted_survival", width = 8, height = 5)


## ---------------------------------------------------------------------------
## 5) Coefficients stratified by capacity
## ---------------------------------------------------------------------------

cc <- cc |> mutate(cap_group = if_else(industry_gdp_pct >=
                                         median(industry_gdp_pct, na.rm = TRUE),
                                       "High industrial capacity",
                                       "Low industrial capacity"))

strat <- bind_rows(lapply(unique(cc$cap_group), function(g) {
  sub <- cc |> filter(cap_group == g)
  fit <- coxph(Surv(tstart, tstop, event) ~ rivals_share_pct + ln_pop,
               data = sub, cluster = ccode)
  tidy(fit, conf.int = TRUE) |> mutate(stratum = g)
})) |>
  mutate(term = recode(term, rivals_share_pct = "Rivals' share (%)",
                       ln_pop = "ln(population)"))

cat("\n=== Cox coefficients by capacity stratum ===\n"); print(strat)

p_strat <- ggplot(strat, aes(estimate, term, colour = stratum)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = .15, position = position_dodge(.5)) +
  geom_point(size = 2.6, position = position_dodge(.5)) +
  scale_colour_manual(values = c("High industrial capacity" = "#1f77b4",
                                 "Low industrial capacity"  = "#d62728")) +
  labs(title    = "Cox coefficients by industrial-capacity stratum",
       subtitle = "Log hazard ratios with 95% CI, clustered by country",
       x = "Log hazard ratio", y = NULL, colour = NULL)
save_fig(p_strat, "cox_stratified_capacity", width = 8, height = 4.5)


## ---------------------------------------------------------------------------
## 6) Model fit: does rivalry add anything beyond capacity? (H3)
## ---------------------------------------------------------------------------

m_cap_only <- coxph(Surv(tstart, tstop, event) ~ industry_gdp_pct + ln_pop,
                    data = cc, cluster = ccode)

fit_tab <- tibble(
  model = c("Rivalry only", "Capacity only", "Rivalry + capacity", "+ Military burden"),
  concordance = c(summary(m1)$concordance[1], summary(m_cap_only)$concordance[1],
                  summary(m3)$concordance[1], summary(m4)$concordance[1]),
  se          = c(summary(m1)$concordance[2], summary(m_cap_only)$concordance[2],
                  summary(m3)$concordance[2], summary(m4)$concordance[2]),
  loglik      = c(logLik(m1), logLik(m_cap_only), logLik(m3), logLik(m4))
) |> mutate(model = factor(model, levels = model))

lrt <- anova(m_cap_only, m3)
cat("\n=== H3 test: does rivalry add explanatory power over capacity? ===\n")
print(fit_tab); print(lrt)

p_fit <- ggplot(fit_tab, aes(model, concordance)) +
  geom_col(width = .6, fill = "grey35") +
  geom_errorbar(aes(ymin = concordance - 1.96 * se, ymax = concordance + 1.96 * se),
                width = .15) +
  geom_hline(yintercept = .5, linetype = "dashed", colour = "firebrick") +
  geom_text(aes(label = sprintf("%.3f", concordance)), vjust = -0.8, size = 3.5) +
  coord_cartesian(ylim = c(0.45, 0.95)) +
  labs(title    = "Discriminative ability of alternative Cox models",
       subtitle = "Harrell's C with 95% CI; dashed line = chance",
       x = NULL, y = "Harrell's C")
save_fig(p_fit, "cox_model_fit", width = 8, height = 4.8)

write_csv(fit_tab, file.path(OUT_TAB, "cox_model_fit.csv"))
write_csv(strat,   file.path(OUT_TAB, "cox_stratified_capacity.csv"))

cat("\nDone. Figures in output/figures, tables in output/tables.\n")
