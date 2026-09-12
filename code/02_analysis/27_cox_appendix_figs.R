## Paths are defined in code/00_setup.R (run from the repository root).
if (!exists("PROJ")) source("code/00_setup.R")

# =============================================================================
# make_cox_figs.R
# Produces the two missing appendix figures from 2.5.csv:
#   (1) cox_concordance.png   -- Harrell's C across alternative Cox specs
#   (2) cox_gdp_strat.png     -- Cox coefficients split by GDP subsample
#
# Run from the project root where 2.5.csv (or 2.0/2.5.csv) lives:
#   Rscript make_cox_figs.R
# =============================================================================

suppressPackageStartupMessages({
  library(survival); library(ggplot2); library(dplyr)
  library(readr);    library(broom)
})

# ---- 1. Locate and load the panel -------------------------------------------
panel_path <- PANEL
message("Reading: ", panel_path)

raw <- read_csv(panel_path, show_col_types = FALSE)

# ---- 2. Build the analysis sample -------------------------------------------
# Keep country-years where the country is still in the risk set (pre first-entry)
# and construct counting-process survival format.
df <- raw %>%
  filter(at_risk == 1) %>%
  arrange(ccode, year) %>%
  group_by(ccode) %>%
  mutate(tstart = year - min(year),
         tstop  = tstart + 1) %>%
  ungroup() %>%
  mutate(
    rivals_share_pct = rivals_share_5yr_owner * 100,
    ln_pop           = log(pop_total),
    gdp              = gdp_kd
  )

# Complete-case sample used for models 1–3 (no military burden)
base_vars <- c("rivals_share_pct", "industry_gdp_pct", "ln_pop", "event",
               "tstart", "tstop", "ccode", "gdp")
df_cc <- df %>% filter(if_all(all_of(base_vars), ~ !is.na(.)))

# Sample for model 4 (also requires mil_exp_gdp_pct)
df_mil <- df_cc %>% filter(!is.na(mil_exp_gdp_pct))

cat(sprintf("\nPanel after filtering: %d country-years, %d countries, %d events\n",
            nrow(df_cc), dplyr::n_distinct(df_cc$ccode), sum(df_cc$event)))

# ---- 3. Fit alternative Cox specifications ----------------------------------
m_riv  <- coxph(Surv(tstart, tstop, event) ~ rivals_share_pct,
                data = df_cc, cluster = ccode)

m_cap  <- coxph(Surv(tstart, tstop, event) ~ industry_gdp_pct + ln_pop,
                data = df_cc, cluster = ccode)

m_full <- coxph(Surv(tstart, tstop, event) ~ rivals_share_pct +
                                            industry_gdp_pct + ln_pop,
                data = df_cc, cluster = ccode)

m_mil  <- coxph(Surv(tstart, tstop, event) ~ rivals_share_pct +
                                            industry_gdp_pct + ln_pop +
                                            mil_exp_gdp_pct,
                data = df_mil, cluster = ccode)

# ---- 4. Figure 1: concordance comparison ------------------------------------
c_tab <- tibble(
  model = c("Rivalry only",
            "Capacity only\n(industry share, ln pop)",
            "Rivalry + Capacity",
            "+ Military burden"),
  C  = c(summary(m_riv)$concordance[1],
         summary(m_cap)$concordance[1],
         summary(m_full)$concordance[1],
         summary(m_mil)$concordance[1]),
  SE = c(summary(m_riv)$concordance[2],
         summary(m_cap)$concordance[2],
         summary(m_full)$concordance[2],
         summary(m_mil)$concordance[2])
) %>% mutate(model = factor(model, levels = model))

p1 <- ggplot(c_tab, aes(x = model, y = C)) +
  geom_col(width = 0.6, fill = "grey35") +
  geom_errorbar(aes(ymin = C - 1.96 * SE, ymax = C + 1.96 * SE),
                width = 0.15) +
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "red") +
  geom_text(aes(label = sprintf("%.3f", C)),
            vjust = -0.6, size = 3.6) +
  coord_cartesian(ylim = c(0.45, 0.90)) +
  labs(x = NULL, y = "Harrell's C-statistic",
       title = "Discriminative ability of alternative Cox models",
       subtitle = "Dashed line = chance (C = 0.5); whiskers are 95% CI") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(OUT_FIG, "cox_concordance.png"), p1, width = 7, height = 4.8, dpi = 300)
message("Wrote cox_concordance.png")
print(c_tab)

# ---- 5. Figure 2: GDP-stratified coefficients -------------------------------
gdp_cut <- median(df_cc$gdp, na.rm = TRUE)

df_cc <- df_cc %>%
  mutate(gdp_group = if_else(gdp >= gdp_cut, "High-GDP states",
                                              "Low-GDP states"))

fit_sub <- function(dat) {
  coxph(Surv(tstart, tstop, event) ~ rivals_share_pct +
                                    industry_gdp_pct + ln_pop,
        data = dat, cluster = ccode)
}

fits <- df_cc %>%
  group_by(gdp_group) %>%
  group_modify(~ tidy(fit_sub(.x), conf.int = TRUE)) %>%
  ungroup() %>%
  mutate(term = recode(term,
                       rivals_share_pct = "Rivals' share (%)",
                       industry_gdp_pct = "Industry / GDP (%)",
                       ln_pop           = "ln(population)"))

p2 <- ggplot(fits, aes(x = estimate, y = term, colour = gdp_group)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_point(position = position_dodge(width = 0.5), size = 2.6) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.5), height = 0.15) +
  scale_colour_manual(values = c("High-GDP states" = "#1f77b4",
                                 "Low-GDP states"  = "#d62728")) +
  labs(x = "Cox log hazard ratio (95% CI)", y = NULL, colour = NULL,
       title = "Cox coefficients by GDP subsample",
       subtitle = sprintf("GDP split at the country-year median (%.2e, constant USD)",
                          gdp_cut)) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom")

ggsave(file.path(OUT_FIG, "cox_gdp_strat.png"), p2, width = 7.5, height = 5.0, dpi = 300)
message("Wrote cox_gdp_strat.png")
print(fits)

message("\nDone. Both figures written to: ", getwd())
