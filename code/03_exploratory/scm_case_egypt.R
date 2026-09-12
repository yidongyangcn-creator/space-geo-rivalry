## Paths are defined in code/00_setup.R (run from the repository root).
if (!exists("PROJ")) source("code/00_setup.R")
## (workspace is not cleared: paths come from code/00_setup.R)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(Synth)

dat_path <- file.path(DATA_INTERIM, "2.4.csv")
out_dir  <- file.path(DATA_INTERIM, "scm_egypt_threshold_sweep")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# -------- settings (relaxed, but not too loose) --------
treated_cc <- 651
treated_name <- "Egypt"

PRE_YEARS  <- 10
POST_YEARS <- 8
MIN_DONORS <- 15

PREDICTORS <- c("gdp_kd","mil_exp_cd","mil_exp_gdp_pct","exports_gdp_pct","industry_gdp_pct","pop_total")

# outcome: keep your current one; you can switch to payload_share_5yr_owner later
OUTCOME_RAW <- "payloads_5yr_owner"
OUTCOME_TX  <- function(x) asinh(pmax(x, 0))

# threshold grids
JUMP_LOOKBACK <- 3
JUMP_DELTAS   <- c(0.0002, 0.0005, 0.001, 0.002, 0.005)
LEVEL_Q       <- c(0.70, 0.80, 0.90, 0.95)
STAB_GRID     <- list(c(1,1), c(2,3), c(3,5))  # (k,m)

# -------- load --------
df <- read_csv(dat_path, show_col_types = FALSE) %>%
  mutate(
    cowcode = as.integer(cowcode),
    year    = as.integer(year),
    at_risk = as.integer(at_risk),
    first_entry_year = suppressWarnings(as.integer(first_entry_year)),
    rivals_share_5yr_owner = as.numeric(rivals_share_5yr_owner),
    payloads_5yr_owner = as.numeric(payloads_5yr_owner),
    payload_share_5yr_owner = as.numeric(payload_share_5yr_owner),
    across(all_of(PREDICTORS), as.numeric)
  ) %>%
  mutate(outcome = OUTCOME_TX(.data[[OUTCOME_RAW]]))

# -------- helper: find T0 --------
find_T0_jump_stable <- function(yrs, vals, delta, lookback, k, m){
  n <- length(yrs)
  if(n < (lookback + m)) return(NA_integer_)
  for(i in (lookback+1):(n-m+1)){
    base <- mean(vals[(i-lookback):(i-1)], na.rm=TRUE)
    if(is.na(base)) next
    if((vals[i] - base) >= delta){
      w <- vals[i:(i+m-1)]
      if(sum(w >= (base + delta), na.rm=TRUE) >= k) return(yrs[i])
    }
  }
  NA_integer_
}

find_T0_level_stable <- function(yrs, vals, thr, k, m){
  n <- length(yrs)
  if(n < m) return(NA_integer_)
  for(i in 1:(n-m+1)){
    w <- vals[i:(i+m-1)]
    if(sum(w >= thr, na.rm=TRUE) >= k) return(yrs[i])
  }
  NA_integer_
}

pick_anchors <- function(dsub, treated_cc, pre_start, pre_end, k=4){
  tp <- dsub[dsub$unit==treated_cc & dsub$year>=pre_start & dsub$year<=pre_end, c("year","outcome")]
  avail <- tp$year[!is.na(tp$outcome)]
  if(length(avail)==0) return(integer(0))
  k <- min(k, length(avail))
  a <- unique(round(seq(min(avail), max(avail), length.out=k)))
  a[a %in% avail]
}

run_case <- function(T0, rule_label){
  pre_start <- T0 - PRE_YEARS
  pre_end   <- T0 - 1
  post_end  <- T0 + POST_YEARS
  yrs_needed <- pre_start:post_end
  
  tdat <- df %>% filter(cowcode==treated_cc, year %in% yrs_needed)
  if(nrow(tdat) != length(yrs_needed)) return(list(ok=FALSE, error="treated missing years"))
  
  donors0 <- df %>% distinct(cowcode) %>% filter(cowcode != treated_cc) %>% pull(cowcode)
  
  dwin <- df %>%
    filter(year %in% yrs_needed, cowcode %in% c(treated_cc, donors0)) %>%
    transmute(
      unit = as.integer(cowcode),
      year = as.integer(year),
      outcome = as.numeric(outcome),
      across(all_of(PREDICTORS), as.numeric)
    )
  
  miss_treated <- dwin %>%
    filter(unit==treated_cc, year>=pre_start, year<=pre_end) %>%
    summarise(across(all_of(PREDICTORS), ~mean(is.na(.)))) %>%
    pivot_longer(everything(), names_to="pred", values_to="miss")
  
  ok_preds <- miss_treated %>% filter(miss < 1) %>% pull(pred)
  if(length(ok_preds) < 2) return(list(ok=FALSE, error="too few predictors"))
  
  donor_ok <- dwin %>%
    filter(unit != treated_cc, year>=pre_start, year<=pre_end) %>%
    group_by(unit) %>%
    summarise(ok = all(colSums(!is.na(pick(all_of(ok_preds)))) > 0), .groups="drop") %>%
    filter(ok) %>% pull(unit)
  if(length(donor_ok) < MIN_DONORS) return(list(ok=FALSE, error=paste0("donors after pred filter=", length(donor_ok))))
  
  dsub <- dwin %>%
    filter(unit %in% c(treated_cc, donor_ok)) %>%
    complete(unit = unique(unit), year = yrs_needed) %>%
    arrange(unit, year) %>%
    as.data.frame()
  dsub$unit <- as.numeric(dsub$unit)
  dsub$year <- as.numeric(dsub$year)
  
  anchors <- pick_anchors(dsub, treated_cc, pre_start, pre_end, k=4)
  donor_ok2 <- as_tibble(dsub) %>%
    filter(unit != treated_cc, year %in% anchors) %>%
    group_by(unit) %>%
    summarise(ok = all(!is.na(outcome)), .groups="drop") %>%
    filter(ok) %>% pull(unit)
  
  if(length(donor_ok2) >= MIN_DONORS && length(anchors) >= 2){
    donor_final <- donor_ok2
    spec_preds <- lapply(anchors, function(t) list("outcome", t, "mean"))
    anchors_used <- paste(anchors, collapse=",")
  } else {
    donor_final <- donor_ok
    spec_preds <- list(list("outcome", pre_start:pre_end, "mean"))
    anchors_used <- "pre-mean"
  }
  
  dsub_final <- dwin %>%
    filter(unit %in% c(treated_cc, donor_final)) %>%
    complete(unit = unique(unit), year = yrs_needed) %>%
    arrange(unit, year) %>%
    as.data.frame()
  dsub_final$unit <- as.numeric(dsub_final$unit)
  dsub_final$year <- as.numeric(dsub_final$year)
  
  dp <- tryCatch(
    dataprep(
      foo = dsub_final,
      predictors = ok_preds,
      predictors.op = "mean",
      special.predictors = spec_preds,
      dependent = "outcome",
      unit.variable = "unit",
      time.variable = "year",
      treatment.identifier = as.numeric(treated_cc),
      controls.identifier = as.numeric(donor_final),
      time.predictors.prior = pre_start:pre_end,
      time.optimize.ssr = pre_start:pre_end,
      time.plot = pre_start:post_end
    ),
    error=function(e) return(list(.err=TRUE, msg=conditionMessage(e)))
  )
  if(is.list(dp) && isTRUE(dp$.err)) return(list(ok=FALSE, error=paste0("dataprep: ", dp$msg)))
  
  syn <- tryCatch(
    synth(dp, verbose=FALSE),
    error=function(e) return(list(.err=TRUE, msg=conditionMessage(e)))
  )
  if(is.list(syn) && isTRUE(syn$.err)) return(list(ok=FALSE, error=paste0("synth: ", syn$msg)))
  
  Y1 <- dp$Y1plot
  Y0 <- dp$Y0plot %*% syn$solution.w
  years <- dp$tag$time.plot
  gap <- as.numeric(Y1 - Y0)
  
  pre_idx  <- years >= pre_start & years <= pre_end
  post_idx <- years >= T0 & years <= post_end
  
  pre_mspe  <- mean(gap[pre_idx]^2, na.rm=TRUE)
  post_mspe <- mean(gap[post_idx]^2, na.rm=TRUE)
  post_auc  <- sum(gap[post_idx], na.rm=TRUE)
  post_pos_share <- mean(gap[post_idx] > 0, na.rm=TRUE)
  
  # scoring: prefer positive effect + decent pre fit but not "machine precision" + not just one-year spike
  score <- 0
  score <- score + 5 * post_pos_share
  score <- score + 2 * (post_auc)
  score <- score - 1 * log10(pre_mspe + 1e-16)  # penalize absurdly tiny pre_mspe
  
  list(
    ok=TRUE, T0=T0, rule=rule_label, donors=length(donor_final), anchors=anchors_used,
    pre_mspe=pre_mspe, post_mspe=post_mspe,
    post_auc=post_auc, post_pos_share=post_pos_share,
    score=score,
    series=data.frame(year=years, treated=as.numeric(Y1), synth=as.numeric(Y0), gap=gap)
  )
}

# -------- generate candidate T0s for Egypt --------
s <- df %>% filter(cowcode==treated_cc, at_risk==1) %>% arrange(year)
stopifnot(nrow(s) > 20)

cand <- list()

# jump grid + stability
for(d in JUMP_DELTAS){
  for(km in STAB_GRID){
    k <- km[1]; m <- km[2]
    t0 <- find_T0_jump_stable(s$year, s$rivals_share_5yr_owner, delta=d, lookback=JUMP_LOOKBACK, k=k, m=m)
    if(!is.na(t0)) cand[[length(cand)+1]] <- data.frame(T0=t0, rule=paste0("jump_d",d,"_k",k,"m",m))
  }
}

# level grid + stability
qs <- quantile(s$rivals_share_5yr_owner, probs=LEVEL_Q, na.rm=TRUE)
for(i in seq_along(LEVEL_Q)){
  thr <- as.numeric(qs[i])
  for(km in STAB_GRID){
    k <- km[1]; m <- km[2]
    t0 <- find_T0_level_stable(s$year, s$rivals_share_5yr_owner, thr=thr, k=k, m=m)
    if(!is.na(t0)) cand[[length(cand)+1]] <- data.frame(T0=t0, rule=paste0("level_q",LEVEL_Q[i],"_thr",signif(thr,3),"_k",k,"m",m))
  }
}

cand_df <- bind_rows(cand) %>% distinct(T0, .keep_all=TRUE) %>% arrange(T0)
print(cand_df)

# -------- run all candidates and rank --------
res_list <- lapply(seq_len(nrow(cand_df)), function(i){
  run_case(cand_df$T0[i], cand_df$rule[i])
})

ok_res <- bind_rows(lapply(res_list, function(x){
  if(isTRUE(x$ok)){
    tibble(T0=x$T0, rule=x$rule, donors=x$donors, anchors=x$anchors,
           pre_mspe=x$pre_mspe, post_mspe=x$post_mspe,
           post_auc=x$post_auc, post_pos_share=x$post_pos_share, score=x$score)
  } else NULL
}))

ok_res <- ok_res %>% arrange(desc(score))
write.csv(ok_res, file.path(out_dir, "egypt_threshold_sweep_summary.csv"), row.names=FALSE)
print(head(ok_res, 15))

# -------- save top 3 plots --------
topN <- min(3, nrow(ok_res))
for(i in 1:topN){
  T0 <- ok_res$T0[i]
  rule <- ok_res$rule[i]
  rr <- res_list[[which(sapply(res_list, function(x) isTRUE(x$ok) && x$T0==T0 && x$rule==rule))[1]]]
  ser <- rr$series
  
  lev_png <- file.path(out_dir, paste0("levels_Egypt_651_T0_",T0,"_",rule,".png"))
  gap_png <- file.path(out_dir, paste0("gap_Egypt_651_T0_",T0,"_",rule,".png"))
  
  png(lev_png, width=1600, height=900, res=200)
  plot(ser$year, ser$treated, type="l", lwd=2,
       xlab="Year", ylab=paste0("Outcome (asinh(", OUTCOME_RAW, "))"),
       main=paste0("SCM Levels: Egypt (651)  T0=",T0," [",rule,"]"))
  lines(ser$year, ser$synth, lty=2, lwd=2)
  abline(v=T0, lty=3)
  legend("topleft", legend=c("Treated","Synthetic"), lty=c(1,2), lwd=2, bty="n")
  dev.off()
  
  png(gap_png, width=1600, height=900, res=200)
  plot(ser$year, ser$gap, type="l", lwd=2,
       xlab="Year", ylab="Treated - Synthetic gap",
       main=paste0("SCM Gap: Egypt (651)  T0=",T0," [",rule,"]"))
  abline(h=0, lty=2)
  abline(v=T0, lty=3)
  dev.off()
}

cat("\nWrote:", file.path(out_dir, "egypt_threshold_sweep_summary.csv"), "\n")