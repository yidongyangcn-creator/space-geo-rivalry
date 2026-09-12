## Paths are defined in code/00_setup.R (run from the repository root).
if (!exists("PROJ")) source("code/00_setup.R")

###############################################################################
#  Sun & Abraham (2021) Robustness Check — Single Main Spec
#  ---------------------------------------------------------
#  Matches your thesis main spec exactly:
#    - Jump def: diff from 3-year mean of rivals_pct
#    - Threshold: 0.2 percentage points
#    - Window: 1970–2020
#    - Outcome: log(1 + n_geo_entries)
#
#  Outputs:
#    1. TWFE vs Sun-Abraham static ATT (console print)
#    2. TWFE vs SA event-study overlay plot (PDF + PNG)
#    3. SA cohort-specific ATT decomposition
#    4. Joint pre-trend test for SA estimator
#
#  Requirements: R >= 4.1, fixest >= 0.11
###############################################################################
## (workspace is not cleared: paths come from code/00_setup.R)
library(readr)
library(dplyr)
library(tidyr)
library(slider)
library(fixest)
library(ggplot2)

# ── 0) LOAD DATA ────────────────────────────────────────────────────────────

path <- PANEL

df0 <- read_csv(path, show_col_types = FALSE) %>%
  mutate(
    year  = as.integer(year),
    ccode = as.integer(ccode),
    rivals_pct    = 100 * as.numeric(rivals_share_5yr_owner),
    n_geo_entries = as.numeric(n_geo_entries),
    y_log1p       = log1p(pmax(n_geo_entries, 0))
  ) %>%
  arrange(ccode, year)


# ── 1) CONSTRUCT TREATMENT COHORT ───────────────────────────────────────────
#  jump_it = rivals_pct_it - mean(rivals_pct_{i,t-1}, ..., rivals_pct_{i,t-3})
#  T_i = first year where jump >= 0.2

df <- df0 %>%
  group_by(ccode) %>%
  arrange(year) %>%
  mutate(
    base_mean = slide_dbl(lag(rivals_pct, 1),
                          ~mean(.x, na.rm = TRUE),
                          .before = 2, .complete = TRUE),
    jump = rivals_pct - base_mean
  ) %>%
  ungroup()

cohort_df <- df %>%
  group_by(ccode) %>%
  summarise(
    cohort = suppressWarnings(
      min(year[!is.na(jump) & jump >= 0.2], na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>%
  mutate(cohort = ifelse(is.infinite(cohort), 10000L, as.integer(cohort)))

df <- df %>% left_join(cohort_df, by = "ccode")


# ── 2) SAMPLE CONSTRUCTION ─────────────────────────────────────────────────
#  Window: 1970–2020
#  Keep treated units with >= 5 years pre and >= 5 years post
#  Keep all never-treated units

START <- 1970
END   <- 2020

d <- df %>% filter(year >= START, year <= END)

unit_info <- d %>%
  distinct(ccode, cohort) %>%
  mutate(
    treated = cohort < 10000,
    ok      = treated & (cohort - START >= 5) & (END - cohort >= 5)
  )

cat("── Sample Info ──\n")
cat(sprintf("  Treated units (with support): %d\n", sum(unit_info$ok)))
cat(sprintf("  Never-treated units:          %d\n", sum(!unit_info$treated)))
cat(sprintf("  Cohort years: %s\n",
            paste(sort(unique(cohort_df$cohort[cohort_df$cohort < 10000])),
                  collapse = ", ")))

keep_ids <- unit_info %>% filter(ok | !treated) %>% pull(ccode)
d <- d %>%
  filter(ccode %in% keep_ids) %>%
  left_join(unit_info %>% select(ccode, ok, treated), by = "ccode") %>%
  mutate(
    treated_flag = ifelse(ok, 1L, 0L),
    post_flag    = ifelse(ok & year >= cohort, 1L, 0L)
  )


# ══════════════════════════════════════════════════════════════════════════════
# 3) ESTIMATION
# ══════════════════════════════════════════════════════════════════════════════

cat("\n══════════════════════════════════════════════════════════════\n")
cat("  A) STATIC DID: TWFE vs Sun-Abraham ATT\n")
cat("══════════════════════════════════════════════════════════════\n\n")

# ── A1) Baseline TWFE ──
m_twfe <- feols(y_log1p ~ treated_flag:post_flag | ccode + year,
                data = d, cluster = "ccode")
cat("TWFE Static DID:\n")
print(coeftable(m_twfe))

# ── A2) Sun & Abraham ──
m_sa <- feols(y_log1p ~ sunab(cohort, year) | ccode + year,
              data = d, cluster = "ccode")

# Aggregated ATT across all post-treatment periods
sa_att <- summary(m_sa, agg = "ATT")
cat("\nSun-Abraham Aggregated ATT:\n")
print(coeftable(sa_att))

# Side-by-side comparison
ct_twfe <- coeftable(m_twfe)
ct_sa   <- coeftable(sa_att)
rn_twfe <- grep("treated_flag.*post_flag|post_flag.*treated_flag",
                rownames(ct_twfe), value = TRUE)[1]

cat("\n┌─────────────────────────────────────────────────────┐\n")
cat("│         TWFE vs Sun-Abraham: Main Spec              │\n")
cat("├─────────────┬──────────┬──────────┬─────────────────┤\n")
cat("│ Estimator   │    β     │   SE     │   p-value       │\n")
cat("├─────────────┼──────────┼──────────┼─────────────────┤\n")
cat(sprintf("│ TWFE        │ %7.4f  │ %7.4f  │ %7.4f          │\n",
            ct_twfe[rn_twfe, 1], ct_twfe[rn_twfe, 2], ct_twfe[rn_twfe, 4]))
cat(sprintf("│ Sun-Abraham │ %7.4f  │ %7.4f  │ %7.4f          │\n",
            ct_sa[1, 1], ct_sa[1, 2], ct_sa[1, 4]))
cat("└─────────────┴──────────┴──────────┴─────────────────┘\n")
cat(sprintf("  N = %d  |  Treated units = %d\n", nobs(m_sa), sum(unit_info$ok)))


# ══════════════════════════════════════════════════════════════════════════════

cat("\n══════════════════════════════════════════════════════════════\n")
cat("  B) EVENT-STUDY: TWFE vs Sun-Abraham\n")
cat("══════════════════════════════════════════════════════════════\n\n")

# ── B1) TWFE event-study ──
d <- d %>%
  mutate(rel_time = ifelse(ok, year - cohort, NA_integer_))

m_twfe_es <- feols(y_log1p ~ i(rel_time, ref = -1) | ccode + year,
                   data = d, cluster = "ccode")

# ── B2) Sun-Abraham event-study (aggregated by period) ──
sa_es <- summary(m_sa, agg = "period")

# ── Extract coefficients for plotting ──
extract_es <- function(model, method_label, is_sunab = FALSE) {
  ct <- coeftable(model)
  idx <- grep("rel_time::|period::", rownames(ct))
  if (length(idx) == 0) idx <- seq_len(nrow(ct))

  tibble(
    rel_time = as.integer(gsub(".*::", "", rownames(ct)[idx])),
    estimate = ct[idx, "Estimate"],
    se       = ct[idx, "Std. Error"],
    ci_lo    = estimate - 1.96 * se,
    ci_hi    = estimate + 1.96 * se,
    method   = method_label
  )
}

es_twfe <- extract_es(m_twfe_es, "TWFE")
es_sa   <- extract_es(sa_es, "Sun-Abraham")

es_data <- bind_rows(es_twfe, es_sa) %>%
  filter(rel_time >= -5, rel_time <= 5)

# ── Plot ──
p <- ggplot(es_data, aes(x = rel_time, y = estimate,
                         color = method, shape = method)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "grey70") +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi),
                width = 0.2, position = position_dodge(0.3)) +
  geom_point(size = 2.5, position = position_dodge(0.3)) +
  scale_color_manual(values = c("TWFE" = "steelblue", "Sun-Abraham" = "firebrick")) +
  labs(
    title    = "Event-Study: TWFE vs Sun-Abraham (IW Estimator)",
    subtitle = "Outcome: log(1 + GEO entries)  |  ref = t\u22121  |  95% CI, clustered SE",
    x = expression("Event time (year " - ~ T[i] ~ ")"),
    y = "Coefficient estimate",
    color = "Estimator", shape = "Estimator"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(file.path(OUT_FIG, "event_study_twfe_vs_sa.pdf"), p, width = 8, height = 5)
ggsave(file.path(OUT_FIG, "event_study_twfe_vs_sa.png"), p, width = 8, height = 5, dpi = 300)
cat("  Saved: event_study_twfe_vs_sa.pdf / .png\n")


# ══════════════════════════════════════════════════════════════════════════════

cat("\n══════════════════════════════════════════════════════════════\n")
cat("  C) PRE-TREND DIAGNOSTICS (Sun-Abraham)\n")
cat("══════════════════════════════════════════════════════════════\n\n")

# Joint test of pre-treatment coefficients = 0
# (SA event-study, periods -5 to -2, with ref = -1)
pre_coefs <- es_sa %>% filter(rel_time >= -5, rel_time <= -2)
cat("Sun-Abraham pre-treatment coefficients:\n")
print(pre_coefs %>% select(rel_time, estimate, se))

# Wald test via the full SA model
# sunab stores the pre-treatment coefficients; we test them jointly
pre_test <- tryCatch({
  # Get the coefficient names for pre-treatment periods
  all_names <- names(coef(m_sa))
  pre_names <- all_names[grep("::-[2-9]|::-[1-9][0-9]", all_names)]
  # Keep only -5 to -2
  pre_names_keep <- pre_names[sapply(pre_names, function(nm) {
    rt <- as.integer(gsub(".*::", "", nm))
    rt >= -5 & rt <= -2
  })]

  if (length(pre_names_keep) > 0) {
    wt <- wald(m_sa, keep = pre_names_keep)
    wt
  } else {
    NULL
  }
}, error = function(e) { cat("  Wald test error:", e$message, "\n"); NULL })

if (!is.null(pre_test)) {
  cat("\nJoint Wald test H0: all pre-treatment SA coefficients = 0\n")
  print(pre_test)
}


# ══════════════════════════════════════════════════════════════════════════════

cat("\n══════════════════════════════════════════════════════════════\n")
cat("  D) COHORT-SPECIFIC ATT DECOMPOSITION\n")
cat("══════════════════════════════════════════════════════════════\n\n")

# Shows which cohorts drive the aggregate result
sa_cohort <- tryCatch({
  summary(m_sa, agg = "cohort")
}, error = function(e) NULL)

if (!is.null(sa_cohort)) {
  cat("Sun-Abraham ATT by treatment cohort:\n")
  print(coeftable(sa_cohort))
}


cat("\n\nDone.\n")
