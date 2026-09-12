## Paths are defined in code/00_setup.R (run from the repository root).
if (!exists("PROJ")) source("code/00_setup.R")
## (workspace is not cleared: paths come from code/00_setup.R)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(slider)
library(fixest)

# -----------------------------
# 0) LOAD DATA (use full 176-country panel; DO NOT filter at_risk)
# -----------------------------
path <- PANEL

df0 <- read_csv(path, show_col_types = FALSE) %>%
  mutate(
    year  = as.integer(year),
    ccode = as.integer(ccode),
    
    # rivals pressure
    rivals_share = as.numeric(rivals_share_5yr_owner),
    rivals_pct   = 100 * as.numeric(rivals_share_5yr_owner),
    
    # outcomes
    n_geo_entries = as.numeric(n_geo_entries),
    geo_entry_any = as.integer(geo_entry_any),
    event         = as.integer(event),
    payloads_5yr_owner = as.numeric(payloads_5yr_owner),
    payload_share_5yr_owner = as.numeric(payload_share_5yr_owner),
    
    # controls (optional robustness; baseline DID below uses only FE)
    ln_gdp = log1p(as.numeric(gdp_kd)),
    ln_pop = log(as.numeric(pop_total)),
    industry_gdp_pct = as.numeric(industry_gdp_pct),
    mil_exp_gdp_pct  = as.numeric(mil_exp_gdp_pct),
    v2x_polyarchy    = as.numeric(v2x_polyarchy),
    
    # transformed outcomes (optional)
    y_log1p_entries = log1p(pmax(n_geo_entries, 0)),
    y_asinh_payload = asinh(pmax(payloads_5yr_owner, 0))
  ) %>%
  arrange(ccode, year)

# -----------------------------
# 1) JUMP DEFINITIONS
# -----------------------------
make_jump <- function(df, x = "rivals_pct",
                      base_fun = c("mean","median"),
                      base_k = 3,
                      diff_fun = c("diff","z"),
                      z_k = 10) {
  base_fun <- match.arg(base_fun)
  diff_fun <- match.arg(diff_fun)
  
  df %>%
    group_by(ccode) %>%
    arrange(year) %>%
    mutate(
      x_raw  = .data[[x]],
      x_base = if (base_fun=="mean") {
        slide_dbl(lag(x_raw, 1),
                  ~mean(.x, na.rm=TRUE),
                  .before = base_k-1, .complete = TRUE)
      } else {
        slide_dbl(lag(x_raw, 1),
                  ~median(.x, na.rm=TRUE),
                  .before = base_k-1, .complete = TRUE)
      },
      x_sd = slide_dbl(lag(x_raw, 1),
                       ~sd(.x, na.rm=TRUE),
                       .before = z_k-1, .complete = TRUE),
      jump = case_when(
        diff_fun=="diff" ~ (x_raw - x_base),
        diff_fun=="z"    ~ (x_raw - x_base) / x_sd
      )
    ) %>%
    ungroup()
}

# Convert jump into a single adoption year T_i (first time jump >= delta)
make_T <- function(df, delta){
  df %>%
    group_by(ccode) %>%
    summarise(
      T = suppressWarnings(min(year[!is.na(jump) & jump >= delta], na.rm = TRUE)),
      .groups="drop"
    ) %>%
    mutate(T = ifelse(is.infinite(T), NA_integer_, as.integer(T)))
}

# Run a simple staggered DID: y_it ~ treated_i * post_it + FE(i,t)
run_did <- function(df, outcome, start, end, min_pre=5, min_post=5){
  d <- df %>%
    filter(year >= start, year <= end)
  
  # require enough pre/post support for treated units
  info <- d %>%
    distinct(ccode, T) %>%
    mutate(
      ok = !is.na(T) & (T - start >= min_pre) & (end - T >= min_post)
    )
  
  treated_units <- sum(info$ok, na.rm=TRUE)
  if(treated_units < 20) {
    return(list(ok=FALSE, error=paste0("treated_units<20 (", treated_units, ")"), p=NA_real_, b=NA_real_, se=NA_real_))
  }
  
  d <- d %>%
    left_join(info %>% select(ccode, ok), by="ccode") %>%
    mutate(
      treated = ifelse(ok, 1L, 0L),
      post    = ifelse(treated==1L & year >= T, 1L, 0L)
    )
  
  # Only keep: valid treated units + never-treated units (cleaner baseline)
  keep_ids <- info %>% filter(ok | is.na(T)) %>% pull(ccode)
  d <- d %>% filter(ccode %in% keep_ids)
  
  # If outcome is missing too much, skip
  if(sum(!is.na(d[[outcome]])) < 2000) {
    return(list(ok=FALSE, error="too many missing outcomes", p=NA_real_, b=NA_real_, se=NA_real_))
  }
  
  fml <- as.formula(paste0(outcome, " ~ treated:post | ccode + year"))
  
  m <- tryCatch(
    feols(fml, data=d, cluster="ccode"),
    error=function(e) NULL
  )
  if(is.null(m)) return(list(ok=FALSE, error="feols failed", p=NA_real_, b=NA_real_, se=NA_real_))
  
  ct <- coeftable(m)
  # term name in fixest will be "treated:post"
  if(!("treated:post" %in% rownames(ct))) {
    return(list(ok=FALSE, error="coef not found (collinearity?)", p=NA_real_, b=NA_real_, se=NA_real_))
  }
  
  b  <- ct["treated:post","Estimate"]
  se <- ct["treated:post","Std. Error"]
  p  <- ct["treated:post","Pr(>|t|)"]
  
  list(ok=TRUE, error=NA_character_, p=p, b=b, se=se, treated_units=treated_units, n=nobs(m))
}

# -----------------------------
# 2) GRID: jump defs × outcomes × windows × lags
# -----------------------------
windows <- tribble(
  ~wname, ~start, ~end,
  "1970_2020", 1970, 2020,
  "1980_2020", 1980, 2020,
  "1990_2022", 1990, 2022,
  "2000_2022", 2000, 2022
)

outcomes <- c(
  "n_geo_entries",
  "y_log1p_entries",
  "geo_entry_any",
  "event",
  "y_asinh_payload",
  "payload_share_5yr_owner"
)

# L here controls BOTH (i) baseline lookback window for jump (base_k) and (ii) min_pre requirement
L_list <- c(3,5)

jump_defs <- tribble(
  ~x, ~base_fun, ~diff_fun, ~z_k, ~jname,
  "rivals_pct", "mean",   "diff", 10, "diff_mean",
  "rivals_pct", "median", "diff", 10, "diff_median",
  "rivals_pct", "mean",   "z",    10, "z_mean"
)

# thresholds for each diff_fun
delta_grid <- tribble(
  ~diff_fun, ~delta,
  "diff", 0.1,
  "diff", 0.2,
  "diff", 0.3,
  "diff", 0.5,
  "diff", 1.0,
  "z",    1.0,
  "z",    1.5,
  "z",    2.0
)

results <- list()
k <- 1

for(L in L_list){
  for(j in seq_len(nrow(jump_defs))){
    jd <- jump_defs[j,]
    
    # compute jump for this definition and lag length L
    dj <- make_jump(df0, x=jd$x, base_fun=jd$base_fun, base_k=L, diff_fun=jd$diff_fun, z_k=jd$z_k)
    
    # loop over deltas compatible with diff_fun
    deltas <- delta_grid %>% filter(diff_fun == jd$diff_fun) %>% pull(delta)
    
    for(delta in deltas){
      Ti <- make_T(dj, delta=delta)
      djt <- dj %>% left_join(Ti, by="ccode")
      
      for(wi in seq_len(nrow(windows))){
        w <- windows[wi,]
        
        for(outcome in outcomes){
          cat("RUN", k, "| L", L, "| jump", jd$jname, "| delta", delta, "| window", w$wname, "| outcome", outcome, "\n")
          
          res <- run_did(djt, outcome=outcome, start=w$start, end=w$end, min_pre=L, min_post=5)
          
          results[[k]] <- tibble(
            ok = res$ok,
            p  = res$p,
            b  = res$b,
            se = res$se,
            n  = res$n %||% NA_integer_,
            treated_units = res$treated_units %||% NA_integer_,
            error = res$error,
            
            L = L,
            jump_def = jd$jname,
            base_fun = jd$base_fun,
            diff_fun = jd$diff_fun,
            delta = delta,
            
            window = w$wname,
            start = w$start,
            end = w$end,
            
            outcome = outcome
          )
          
          k <- k + 1
        }
      }
    }
  }
}

out <- bind_rows(results)

# sort by p-value (smallest first), only successful runs
out_ok <- out %>% filter(ok, !is.na(p)) %>% arrange(p)

# save
write_csv(out, file.path(OUT_TAB, "did_jump_grid_results.csv"))
write_csv(out_ok %>% slice(1:50), file.path(OUT_TAB, "did_jump_grid_top50.csv"))

print(out_ok %>% slice(1:20))
cat("\nSaved:\n  did_jump_grid_results.csv\n  did_jump_grid_top50.csv\nin: ", getwd(), "\n", sep="")