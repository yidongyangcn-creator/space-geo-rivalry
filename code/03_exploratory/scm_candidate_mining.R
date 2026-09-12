## Paths are defined in code/00_setup.R (run from the repository root).
if (!exists("PROJ")) source("code/00_setup.R")

# =========================
# SCM candidate mining on 2.2.csv
# =========================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
  library(purrr)
})

# ---------- paths ----------
in_path  <- file.path(DATA_INTERIM, "2.2.csv")
out_dir  <- dirname(in_path)
out_cand <- file.path(out_dir, "2.2_scm_candidates.csv")
out_sum  <- file.path(out_dir, "2.2_scm_summary.csv")

# ---------- knobs (you should tweak if you want) ----------
yr_min <- 1963L
yr_max <- 2024L

# outcome you might use for SCM plots
OUTCOME <- "payload_share_5yr_owner"   # or "payloads_5yr_owner" (raw counts)

# treatment variable
TREATVAR <- "rivals_share_5yr_owner"

# SCM-ish minimum data requirements
min_pre_years  <- 12L   # pre-treatment years needed
min_post_years <- 8L    # post-treatment years needed
min_donors     <- 25L   # donor pool size (after exclusions)
min_outcome_sd_pre <- 1e-4  # outcome should have some variation pre

# ---------- load ----------
df <- read_csv(in_path, show_col_types = FALSE) %>%
  mutate(
    year  = as.integer(year),
    ccode = as.integer(ccode)
  ) %>%
  filter(year >= yr_min, year <= yr_max)

stopifnot(all(c("ccode","year", OUTCOME, TREATVAR) %in% names(df)))

# ---------- helper: build candidate events from a binary treated indicator ----------
build_candidates <- function(dat, treated_col, spec_id, thr_label, k_persist) {
  # treated_col: string name of a 0/1 column in dat
  # Find first year where treated becomes 1 and stays 1 for k_persist years.
  # We set treat_year = that first year.
  treated_sym <- rlang::sym(treated_col)
  
  cand <- dat %>%
    arrange(ccode, year) %>%
    group_by(ccode) %>%
    mutate(
      tr = as.integer(!!treated_sym),
      # streak length starting at t: sum of next k years all == 1
      # We'll compute by rolling window in a simple way:
      tr_next = purrr::map_int(seq_along(tr), function(i){
        if (i + k_persist - 1 > length(tr)) return(0L)
        as.integer(all(tr[i:(i + k_persist - 1)] == 1L))
      })
    ) %>%
    ungroup()
  
  # treat_year = first year where tr_next == 1
  ty <- cand %>%
    group_by(ccode) %>%
    summarise(
      treat_year = ifelse(any(tr_next == 1L), min(year[tr_next == 1L]), NA_integer_),
      .groups="drop"
    ) %>%
    filter(!is.na(treat_year))
  
  if (nrow(ty) == 0) {
    return(tibble())
  }
  
  # ---------- SCM feasibility coarse checks ----------
  # For each candidate, compute:
  # - pre/post length
  # - pre sd of outcome
  # - donor size: countries with full pre window available and NOT treated before candidate year
  # - stability of treatvar pre (avoid slowly trending "fake break")
  outcome_sym <- rlang::sym(OUTCOME)
  treatvar_sym <- rlang::sym(TREATVAR)
  
  # pre stability metrics for treatvar
  # We'll compute mean absolute year-to-year change in treatvar pre.
  dat2 <- dat %>%
    arrange(ccode, year) %>%
    group_by(ccode) %>%
    mutate(
      treatvar_lag = dplyr::lag(!!treatvar_sym),
      treatvar_d1  = abs((!!treatvar_sym) - treatvar_lag)
    ) %>%
    ungroup()
  
  # create a quick “ever-treated” flag for this spec to exclude donors (pre-treatment contamination)
  # ever treated year for each ccode under this treated indicator
  ever_tr <- dat2 %>%
    group_by(ccode) %>%
    summarise(
      first_treated_year = ifelse(any(!!treated_sym == 1L), min(year[!!treated_sym == 1L]), NA_integer_),
      .groups="drop"
    )
  
  # main eval
  eval_one <- function(cc, ty0) {
    dd <- dat2 %>% filter(ccode == cc)
    
    # pre/post windows (simple counts)
    pre  <- dd %>% filter(year < ty0)
    post <- dd %>% filter(year >= ty0)
    
    pre_years  <- n_distinct(pre$year)
    post_years <- n_distinct(post$year)
    
    # outcome variation pre
    pre_sd <- sd(pre[[OUTCOME]], na.rm=TRUE)
    pre_mean <- mean(pre[[OUTCOME]], na.rm=TRUE)
    
    # treatvar stability pre (mean abs diff)
    tv_stab <- pre %>% summarise(mad_d1 = mean(treatvar_d1, na.rm=TRUE)) %>% pull(mad_d1)
    if (is.nan(tv_stab)) tv_stab <- NA_real_
    
    # donor pool criteria:
    # donor must have at least min_pre_years before ty0 and must NOT be treated before ty0
    donors <- dat2 %>%
      group_by(ccode) %>%
      summarise(
        pre_years = sum(year < ty0),
        .groups="drop"
      ) %>%
      left_join(ever_tr, by="ccode") %>%
      filter(
        ccode != cc,
        pre_years >= min_pre_years,
        is.na(first_treated_year) | first_treated_year >= ty0
      )
    
    donor_n <- nrow(donors)
    
    # pass/fail
    pass <- (pre_years >= min_pre_years) &&
      (post_years >= min_post_years) &&
      (!is.na(pre_sd) && pre_sd >= min_outcome_sd_pre) &&
      (donor_n >= min_donors)
    
    tibble(
      ccode = cc,
      treat_year = ty0,
      pre_years = pre_years,
      post_years = post_years,
      outcome_pre_sd = pre_sd,
      outcome_pre_mean = pre_mean,
      treatvar_pre_mad_d1 = tv_stab,
      donor_n = donor_n,
      pass_basic = pass
    )
  }
  
  eval_tbl <- purrr::pmap_dfr(list(ty$ccode, ty$treat_year), eval_one)
  
  # merge country name for readability
  name_map <- dat %>% distinct(ccode, country_name)
  out <- eval_tbl %>%
    left_join(name_map, by="ccode") %>%
    mutate(
      spec_id = spec_id,
      treat_def = treated_col,
      threshold = thr_label,
      k_persist = k_persist
    ) %>%
    relocate(spec_id, treat_def, threshold, k_persist, country_name, ccode, treat_year)
  
  out
}

# ---------- create multiple treatment specs ----------
# We'll create several treated indicators inside df, then call build_candidates()

df <- df %>%
  group_by(ccode) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    # year-to-year change in rivals share (for "jump" specs)
    r_d1 = (!!rlang::sym(TREATVAR)) - dplyr::lag(!!rlang::sym(TREATVAR)),
    r_d1 = ifelse(is.na(r_d1), 0, r_d1),
    # within-country zscore
    r_mean = mean(!!rlang::sym(TREATVAR), na.rm=TRUE),
    r_sd   = sd(!!rlang::sym(TREATVAR), na.rm=TRUE),
    r_z    = ifelse(is.na(r_sd) | r_sd == 0, 0, ((!!rlang::sym(TREATVAR)) - r_mean) / r_sd)
  ) %>%
  ungroup()

# Global percentile thresholds for rivals_share
p_list <- c(0.80, 0.90, 0.95)
p_thr  <- quantile(df[[TREATVAR]], probs = p_list, na.rm = TRUE)

# Absolute thresholds (keep a few; adjust if you want)
abs_thr <- c(0.05, 0.10, 0.20, 0.30, 0.50)

# Jump thresholds (delta)
jump_thr <- c(0.02, 0.05, 0.10)

# Zscore thresholds
z_thr <- c(1.0, 1.5, 2.0)

# persistence requirements
k_list <- c(1L, 2L, 3L)  # require treatment stays on for k years

# Create treated indicator columns (binary)
# 1) absolute
for (t in abs_thr) {
  nm <- paste0("tr_abs_", str_replace_all(as.character(t), "\\.", "p"))
  df[[nm]] <- as.integer(df[[TREATVAR]] >= t)
}

# 2) percentile
for (i in seq_along(p_list)) {
  p <- p_list[i]; t <- as.numeric(p_thr[i])
  nm <- paste0("tr_p", as.integer(p*100))
  df[[nm]] <- as.integer(df[[TREATVAR]] >= t)
}

# 3) jump in one year
for (t in jump_thr) {
  nm <- paste0("tr_jump_", str_replace_all(as.character(t), "\\.", "p"))
  df[[nm]] <- as.integer(df$r_d1 >= t)
}

# 4) within-country zscore high
for (t in z_thr) {
  nm <- paste0("tr_z_", str_replace_all(as.character(t), "\\.", "p"))
  df[[nm]] <- as.integer(df$r_z >= t)
}

treated_cols <- names(df)[str_detect(names(df), "^tr_")]

# ---------- mining loop ----------
all_cands <- list()
spec_id <- 0L

for (tc in treated_cols) {
  # figure label
  thr_label <- tc
  for (k in k_list) {
    spec_id <- spec_id + 1L
    cand <- build_candidates(
      dat = df,
      treated_col = tc,
      spec_id = spec_id,
      thr_label = thr_label,
      k_persist = k
    )
    if (nrow(cand) > 0) all_cands[[length(all_cands)+1]] <- cand
  }
}

cands <- bind_rows(all_cands)

if (nrow(cands) == 0) {
  stop("No candidates found under these specs. Loosen thresholds or persistence, or lower min_pre/min_post.")
}

# ---------- extra scoring (rank the useful ones) ----------
# We want: pass_basic==TRUE, high treatvar level after treatment, and clear break.
# We'll compute:
# - avg treatvar in [t0, t0+4] minus avg in [t0-5, t0-1]
calc_break <- function(cc, ty0) {
  d <- df %>% filter(ccode == cc)
  pre  <- d %>% filter(year >= (ty0-5), year <= (ty0-1)) %>% summarise(m = mean(.data[[TREATVAR]], na.rm=TRUE)) %>% pull(m)
  post <- d %>% filter(year >= ty0, year <= (ty0+4)) %>% summarise(m = mean(.data[[TREATVAR]], na.rm=TRUE)) %>% pull(m)
  if (is.nan(pre)) pre <- NA_real_
  if (is.nan(post)) post <- NA_real_
  tibble(treatvar_pre5 = pre, treatvar_post5 = post, treatvar_break = post - pre)
}

break_tbl <- pmap_dfr(list(cands$ccode, cands$treat_year), calc_break)

cands2 <- bind_cols(cands, break_tbl) %>%
  mutate(
    # simple score: donor size + break size - instability penalty
    score = (donor_n / 10) + (pmax(treatvar_break, 0, na.rm=TRUE) * 20) - (coalesce(treatvar_pre_mad_d1, 0) * 50),
    score = ifelse(is.na(score), -Inf, score)
  ) %>%
  arrange(desc(pass_basic), desc(score))

# ---------- summaries ----------
sum_tbl <- cands2 %>%
  group_by(treat_def, threshold, k_persist) %>%
  summarise(
    n_candidates = n(),
    n_pass = sum(pass_basic),
    pass_rate = mean(pass_basic),
    median_donor = median(donor_n),
    median_break = median(treatvar_break, na.rm=TRUE),
    .groups="drop"
  ) %>%
  arrange(desc(n_pass), desc(pass_rate), desc(median_break))

# ---------- write outputs ----------
write_csv(cands2, out_cand)
write_csv(sum_tbl, out_sum)

cat("Wrote candidates:", out_cand, "\n")
cat("Wrote summary   :", out_sum, "\n")

# ---------- quick console view ----------
cat("\nTop 20 PASSING candidates:\n")
print(
  cands2 %>%
    filter(pass_basic) %>%
    select(country_name, ccode, treat_year, treat_def, k_persist, donor_n,
           treatvar_break, outcome_pre_sd, treatvar_pre_mad_d1, score) %>%
    slice_head(n=20),
  n=20
)

cat("\nBest spec configs by #pass:\n")
print(sum_tbl %>% slice_head(n=15), n=15)
