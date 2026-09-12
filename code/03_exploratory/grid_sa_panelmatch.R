## Paths are defined in code/00_setup.R (run from the repository root).
if (!exists("PROJ")) source("code/00_setup.R")
## (workspace is not cleared: paths come from code/00_setup.R)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(slider)
library(fixest)
library(PanelMatch)

# =============================
# 0) PATH + GLOBAL SETTINGS
# =============================
path <- PANEL

# expected sign of treatment effect on outcome
EXPECTED_SIGN <- +1   # want positive

# PanelMatch bootstrap (keep modest so it finishes)
BOOT_PM <- 120

# horizons
POST_K <- 0:5

# Minimal treated units threshold (keep low to "retain events")
MIN_TREATED_UNITS <- 20
MIN_POST_YEARS <- 5   # require at least 5 post years in window

# =============================
# 1) LOAD + BUILD OUTCOME VARIANTS
# =============================
df0 <- read_csv(path, show_col_types = FALSE) %>%
  mutate(
    year  = as.integer(year),
    ccode = as.integer(ccode),
    
    rivals_pct = 100 * as.numeric(rivals_share_5yr_owner),
    
    n_geo_entries = as.numeric(n_geo_entries),
    log1p_entries = log1p(pmax(n_geo_entries, 0)),
    asinh_entries = asinh(pmax(n_geo_entries, 0)),
    
    ln_gdp = log1p(as.numeric(gdp_kd)),
    ln_pop = log(as.numeric(pop_total)),
    industry_gdp_pct = as.numeric(industry_gdp_pct),
    mil_exp_gdp_pct  = as.numeric(mil_exp_gdp_pct),
    v2x_polyarchy    = as.numeric(v2x_polyarchy)
  ) %>%
  arrange(ccode, year)

# 5-year rolling sum of entries (more signal, less sparsity)
df0 <- df0 %>%
  group_by(ccode) %>%
  arrange(year) %>%
  mutate(
    entries_5yr_sum = slide_dbl(n_geo_entries, ~sum(.x, na.rm=TRUE), .before = 4, .complete = TRUE),
    log1p_entries_5yr_sum = log1p(pmax(entries_5yr_sum, 0))
  ) %>%
  ungroup()

OUTCOMES <- c("n_geo_entries", "log1p_entries", "asinh_entries",
              "entries_5yr_sum", "log1p_entries_5yr_sum")

# =============================
# 2) TREATMENT: JUMP BUILDERS
# =============================
make_jump <- function(df, base_k=3, method=c("diff_mean","diff_median","z_mean"), z_k=10){
  method <- match.arg(method)
  
  df %>%
    group_by(ccode) %>%
    arrange(year) %>%
    mutate(
      x = rivals_pct,
      base_mean = slide_dbl(lag(x,1), ~mean(.x, na.rm=TRUE), .before=base_k-1, .complete=TRUE),
      base_med  = slide_dbl(lag(x,1), ~median(.x, na.rm=TRUE), .before=base_k-1, .complete=TRUE),
      sd_hist   = slide_dbl(lag(x,1), ~sd(.x, na.rm=TRUE), .before=z_k-1, .complete=TRUE),
      jump = case_when(
        method=="diff_mean"   ~ (x - base_mean),
        method=="diff_median" ~ (x - base_med),
        method=="z_mean"      ~ (x - base_mean) / sd_hist
      )
    ) %>%
    ungroup()
}

make_T_clean <- function(d, delta, start, end, L){
  Ti <- d %>%
    group_by(ccode) %>%
    summarise(
      T_raw = suppressWarnings(min(year[!is.na(jump) & jump >= delta], na.rm=TRUE)),
      .groups="drop"
    ) %>%
    mutate(
      # robust NA handling
      T_raw = dplyr::if_else(is.infinite(T_raw), as.integer(NA), as.integer(T_raw)),
      ok = !is.na(T_raw) & (T_raw - start >= L) & (end - T_raw >= MIN_POST_YEARS),
      T  = if_else(ok, T_raw, as.integer(NA))
    )
  Ti
}

# =============================
# 3) SUN–ABRAHAM RUN + EXTRACT
# =============================
parse_k <- function(term){
  # term like "year::5" or "year::-3"
  if(!grepl("^year::", term)) return(NA_integer_)
  suppressWarnings(as.integer(sub("^year::", "", term)))
}

run_sa_one <- function(d, outcome, start, end, L, delta){
  dj <- d %>% filter(year >= start, year <= end)
  Ti <- make_T_clean(dj, delta, start, end, L)
  
  treated_units <- sum(Ti$ok, na.rm=TRUE)
  any_T <- sum(!is.na(Ti$T_raw))
  if(treated_units < MIN_TREATED_UNITS){
    return(tibble(ok=FALSE, reason=paste0("treated_units<",MIN_TREATED_UNITS),
                  treated_units=treated_units, any_T=any_T,
                  sa_post_min_p=NA_real_, sa_pre_joint_p=NA_real_, sa_post_avg_b=NA_real_,
                  nobs=NA_integer_))
  }
  
  # keep ok treated + never-treated controls (T_raw NA)
  keep_ids <- Ti %>% filter(ok | is.na(T_raw)) %>% pull(ccode)
  
  dat <- dj %>%
    left_join(Ti %>% select(ccode, T, ok, T_raw), by="ccode") %>%
    filter(ccode %in% keep_ids)
  
  fml <- as.formula(paste0(outcome, " ~ sunab(T, year, ref.p=-1) | ccode + year"))
  m <- tryCatch(feols(fml, data=dat, cluster="ccode"), error=function(e) NULL)
  if(is.null(m)){
    return(tibble(ok=FALSE, reason="SA feols failed",
                  treated_units=treated_units, any_T=any_T,
                  sa_post_min_p=NA_real_, sa_pre_joint_p=NA_real_, sa_post_avg_b=NA_real_,
                  nobs=NA_integer_))
  }
  
  ct <- as.data.frame(coeftable(m))
  ct$term <- rownames(ct)
  
  # aggregate event-time terms look like year::k (WITHOUT cohort::)
  tab <- ct %>%
    filter(grepl("^year::", term) & !grepl("cohort::", term)) %>%
    mutate(k = vapply(term, parse_k, integer(1)))
  
  if(nrow(tab)==0){
    return(tibble(ok=FALSE, reason="no year::k terms",
                  treated_units=treated_units, any_T=any_T,
                  sa_post_min_p=NA_real_, sa_pre_joint_p=NA_real_, sa_post_avg_b=NA_real_,
                  nobs=nobs(m)))
  }
  
  post_tab <- tab %>% filter(k %in% POST_K)
  if(nrow(post_tab)==0){
    return(tibble(ok=FALSE, reason="no post terms 0..5",
                  treated_units=treated_units, any_T=any_T,
                  sa_post_min_p=NA_real_, sa_pre_joint_p=NA_real_, sa_post_avg_b=NA_real_,
                  nobs=nobs(m)))
  }
  
  sa_post_min_p <- min(post_tab$`Pr(>|t|)`, na.rm=TRUE)
  sa_post_avg_b <- mean(post_tab$Estimate, na.rm=TRUE)
  
  # pretrend joint test on leads k in {-L..-2}
  lead_ks <- (-L):-2
  lead_terms <- tab %>% filter(k %in% lead_ks) %>% pull(term)
  
  sa_pre_joint_p <- NA_real_
  if(length(lead_terms) >= 2){
    hyps <- paste0("`", lead_terms, "` = 0")
    wt <- tryCatch(wald(m, hyps), error=function(e) NULL)
    if(!is.null(wt)){
      # wald() may return a list, a named numeric, or a matrix depending on fixest version
      if(is.list(wt) && !is.null(wt$p.value)){
        sa_pre_joint_p <- as.numeric(wt$p.value)
      } else if(is.numeric(wt) && length(wt) == 1){
        sa_pre_joint_p <- as.numeric(wt)
      } else if(is.numeric(wt) && !is.null(names(wt)) && any(grepl("p", names(wt), ignore.case=TRUE))){
        sa_pre_joint_p <- as.numeric(wt[grep("p", names(wt), ignore.case=TRUE)[1]])
      } else if(is.matrix(wt) || is.data.frame(wt)){
        # try common column names
        cn <- tolower(colnames(wt))
        if(any(cn %in% c("p.value","pvalue","p"))){
          sa_pre_joint_p <- as.numeric(wt[1, which(cn %in% c("p.value","pvalue","p"))[1]])
        } else {
          sa_pre_joint_p <- NA_real_
        }
      } else {
        sa_pre_joint_p <- NA_real_
      }
    }
  }
  
  tibble(ok=TRUE, reason=NA_character_,
         treated_units=treated_units, any_T=any_T,
         sa_post_min_p=sa_post_min_p, sa_pre_joint_p=sa_pre_joint_p, sa_post_avg_b=sa_post_avg_b,
         nobs=nobs(m))
}

# =============================
# 4) PANELMATCH RUN + EXTRACT (lead 0 p-value)
# =============================
pm_lead0 <- function(est){
  s <- tryCatch(summary(est), error=function(e) NULL)
  if(is.null(s)) return(list(ok=FALSE, b0=NA_real_, p0=NA_real_, note="summary failed"))
  
  if(is.data.frame(s)){
    nm <- tolower(names(s))
    pcol <- names(s)[nm %in% c("p.value","p","pval","p_value")][1]
    lcol <- names(s)[nm %in% c("lead","time","t","f","k")][1]
    ecol <- names(s)[nm %in% c("estimate","att","tau","effect")][1]
    if(!is.na(pcol) && !is.na(lcol)){
      s0 <- s[s[[lcol]]==0, , drop=FALSE]
      if(nrow(s0)>=1){
        p0 <- s0[[pcol]][1]
        b0 <- if(!is.na(ecol)) s0[[ecol]][1] else NA_real_
        return(list(ok=TRUE, b0=b0, p0=p0, note=NA_character_))
      }
    }
  }
  list(ok=FALSE, b0=NA_real_, p0=NA_real_, note="could not parse p0")
}

run_pm_one <- function(dj, outcome, start, end, L, delta){
  d <- dj %>% filter(year >= start, year <= end)
  Ti <- make_T_clean(d, delta, start, end, L)
  
  treated_units <- sum(Ti$ok, na.rm=TRUE)
  if(treated_units < MIN_TREATED_UNITS){
    return(tibble(pm_ok=FALSE, pm_note="treated_units<min", pm_b0=NA_real_, pm_p0=NA_real_))
  }
  
  keep_ids <- Ti %>% filter(ok | is.na(T_raw)) %>% pull(ccode)
  
  dat <- d %>%
    left_join(Ti %>% select(ccode, T, ok, T_raw), by="ccode") %>%
    filter(ccode %in% keep_ids) %>%
    mutate(
      treated = as.integer(!is.na(T)),
      D = as.integer(treated==1 & year >= T)
    )
  
  # If too few treated-years, PM gets unstable
  if(sum(dat$D, na.rm=TRUE) < 50){
    return(tibble(pm_ok=FALSE, pm_note="too few treated-years", pm_b0=NA_real_, pm_p0=NA_real_))
  }
  
  sets <- tryCatch(
    PanelMatch(
      lag = L,
      time.id = "year",
      unit.id = "ccode",
      treatment = "D",
      outcome.var = outcome,
      lead = 0:5,
      qoi = "att",
      refinement.method = "mahalanobis",
      match.missing = TRUE,
      size.match = 10,
      data = dat,
      covs.formula =
        as.formula(paste0("~ ln_gdp + ln_pop + industry_gdp_pct + mil_exp_gdp_pct + v2x_polyarchy + ",
                          "lag(ln_gdp, 1:",L,") + lag(ln_pop, 1:",L,") + ",
                          "lag(industry_gdp_pct, 1:",L,") + lag(v2x_polyarchy, 1:",L,") + ",
                          "lag(", outcome, ", 1:",L,")"))
    ),
    error=function(e) NULL
  )
  if(is.null(sets)){
    return(tibble(pm_ok=FALSE, pm_note="PanelMatch failed", pm_b0=NA_real_, pm_p0=NA_real_))
  }
  
  est <- tryCatch(
    PanelEstimate(sets=sets, data=dat, se.method="bootstrap", number.iterations=BOOT_PM),
    error=function(e) NULL
  )
  if(is.null(est)){
    return(tibble(pm_ok=FALSE, pm_note="PanelEstimate failed", pm_b0=NA_real_, pm_p0=NA_real_))
  }
  
  s0 <- pm_lead0(est)
  tibble(pm_ok=TRUE, pm_note=s0$note, pm_b0=s0$b0, pm_p0=s0$p0)
}

# =============================
# 5) GRID: windows × outcomes × L × jump method × delta × sample_set
# =============================
# lags to try
L_LIST <- c(1, 3, 5)

# sample sets
SAMPLE_SETS <- c("all", "active_only")

# jump methods
JUMP_METHODS <- c("diff_mean","diff_median","z_mean")

# outcomes
OUTCOMES <- c("n_geo_entries", "log1p_entries", "asinh_entries",
              "entries_5yr_sum", "log1p_entries_5yr_sum")


WINDOWS <- tibble::tribble(
  ~wname, ~start, ~end,
  "1970_2020", 1970, 2020,
  "1980_2020", 1980, 2020,
  "1990_2022", 1990, 2022,
  "2000_2022", 2000, 2022
)

# sample sets: keep more events, less deletion
# all: full sample
# active_only: keep countries with at least 1 entry in window (reduces pure zeros)
SAMPLE_SETS <- c("all", "active_only")

JUMP_METHODS <- c("diff_mean","diff_median","z_mean")

DELTA_GRID <- tibble::tribble(
  ~method, ~delta,
  "diff_mean",   0.1,
  "diff_mean",   0.2,
  "diff_mean",   0.3,
  "diff_mean",   0.5,
  "diff_median", 0.1,
  "diff_median", 0.2,
  "diff_median", 0.3,
  "diff_median", 0.5,
  "z_mean",      1.0,
  "z_mean",      1.5,
  "z_mean",      2.0
)

grid <- tidyr::expand_grid(
  outcome = OUTCOMES,
  w = seq_len(nrow(WINDOWS)),
  L = L_LIST,
  sample_set = SAMPLE_SETS,
  method = JUMP_METHODS
) %>%
  left_join(DELTA_GRID, by="method") %>%
  mutate(
    window = WINDOWS$wname[w],
    start  = WINDOWS$start[w],
    end    = WINDOWS$end[w]
  ) %>%
  select(-w)

# =============================
# 6) RUN GRID
# =============================
res_list <- vector("list", nrow(grid))

for(i in seq_len(nrow(grid))){
  g <- grid[i,]
  
  # build jump once per row (simple, yes slower; you can memoize later)
  dj <- make_jump(df0, base_k=g$L, method=g$method, z_k=10) %>%
    filter(year >= g$start, year <= g$end)
  
  # sample set filter (keep more events but less deletion)
  if(g$sample_set == "active_only"){
    active_ids <- dj %>%
      group_by(ccode) %>%
      summarise(any_entry = any(n_geo_entries > 0, na.rm=TRUE), .groups="drop") %>%
      filter(any_entry) %>%
      pull(ccode)
    dj <- dj %>% filter(ccode %in% active_ids)
  }
  
  # SA
  sa <- run_sa_one(dj, outcome=g$outcome, start=g$start, end=g$end, L=g$L, delta=g$delta)
  
  # PM (only if SA ok; otherwise skip to save time)
  pm <- if(isTRUE(sa$ok)){
    run_pm_one(dj, outcome=g$outcome, start=g$start, end=g$end, L=g$L, delta=g$delta)
  } else {
    tibble(pm_ok=FALSE, pm_note="skipped (SA failed)", pm_b0=NA_real_, pm_p0=NA_real_)
  }
  
  # scoring
  p_sa_post <- ifelse(is.na(sa$sa_post_min_p), 1, sa$sa_post_min_p)
  p_sa_pre  <- ifelse(is.na(sa$sa_pre_joint_p), 1, sa$sa_pre_joint_p)
  p_pm0     <- ifelse(is.na(pm$pm_p0), 1, pm$pm_p0)
  
  score <- p_sa_post + p_sa_pre + p_pm0
  
  # penalties
  if(!is.na(sa$sa_post_avg_b) && sign(sa$sa_post_avg_b) != EXPECTED_SIGN) score <- score + 10
  if(!is.na(sa$sa_pre_joint_p) && sa$sa_pre_joint_p < 0.10) score <- score + 10
  
  res_list[[i]] <- tibble(
    outcome = g$outcome,
    sample_set = g$sample_set,
    window = g$window, start=g$start, end=g$end,
    L = g$L,
    method = g$method,
    delta = g$delta,
    
    treated_units = sa$treated_units,
    any_T = sa$any_T,
    nobs = sa$nobs,
    
    sa_ok = sa$ok,
    sa_post_avg_b = sa$sa_post_avg_b,
    sa_post_min_p = sa$sa_post_min_p,
    sa_pre_joint_p = sa$sa_pre_joint_p,
    sa_reason = sa$reason,
    
    pm_ok = pm$pm_ok,
    pm_b0 = pm$pm_b0,
    pm_p0 = pm$pm_p0,
    pm_note = pm$pm_note,
    
    score = score
  )
  
  cat("done", i, "/", nrow(grid), " | score=", round(score,4), "\n")
}

res <- bind_rows(res_list)

# keep only rows where SA ran
res_ok <- res %>% filter(sa_ok) %>% arrange(score)

top20 <- res_ok %>% slice(1:20)

write_csv(res, file.path(OUT_TAB, "grid_all.csv"))
write_csv(top20, file.path(OUT_TAB, "grid_top20.csv"))

print(top20)
cat("\nSaved files in:\n", getwd(), "\n- grid_all.csv\n- grid_top20.csv\n", sep="")