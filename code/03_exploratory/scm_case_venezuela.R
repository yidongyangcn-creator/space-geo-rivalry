## Paths are defined in code/00_setup.R (run from the repository root).
if (!exists("PROJ")) source("code/00_setup.R")
## (workspace is not cleared: paths come from code/00_setup.R)
library(readr)
library(dplyr)
library(tidyr)
library(Synth)

# ----------------------------
# SETTINGS
# ----------------------------
dat_path <- file.path(DATA_INTERIM, "2.4.csv")

treated_cc <- 101
T0 <- 2007

PRE_YEARS  <- 15
POST_YEARS <- 10
MIN_DONORS <- 20

PREDICTORS <- c("gdp_kd","mil_exp_cd","mil_exp_gdp_pct","exports_gdp_pct","industry_gdp_pct","pop_total")
OUTCOME_RAW <- "payloads_5yr_owner"

# ----------------------------
# READ + OUTCOME
# ----------------------------
df <- read_csv(dat_path, show_col_types = FALSE) %>%
  mutate(
    cowcode = as.integer(cowcode),
    year    = as.integer(year),
    payloads_5yr_owner = as.numeric(payloads_5yr_owner),
    across(all_of(PREDICTORS), as.numeric)
  ) %>%
  mutate(outcome = asinh(pmax(.data[[OUTCOME_RAW]], 0)))

pre_start <- T0 - PRE_YEARS
pre_end   <- T0 - 1
post_end  <- T0 + POST_YEARS
yrs_needed <- pre_start:post_end

# treated window
stopifnot(nrow(df %>% filter(cowcode==treated_cc, year %in% yrs_needed)) == length(yrs_needed))

# base donors
donors0 <- df %>% distinct(cowcode) %>% filter(cowcode != treated_cc) %>% pull(cowcode)

# window data
dwin <- df %>%
  filter(year %in% yrs_needed, cowcode %in% c(treated_cc, donors0)) %>%
  transmute(
    unit = as.integer(cowcode),
    year = as.integer(year),
    outcome = as.numeric(outcome),
    across(all_of(PREDICTORS), as.numeric)
  )

# usable predictors for treated
miss_treated <- dwin %>%
  filter(unit==treated_cc, year>=pre_start, year<=pre_end) %>%
  summarise(across(all_of(PREDICTORS), ~mean(is.na(.)))) %>%
  pivot_longer(everything(), names_to="pred", values_to="miss") %>%
  arrange(desc(miss))

ok_preds <- miss_treated %>% filter(miss < 1) %>% pull(pred)
stopifnot(length(ok_preds) >= 2)

# donor NA filter for predictors (at least one obs in pre for each pred)
donor_ok <- dwin %>%
  filter(unit != treated_cc, year>=pre_start, year<=pre_end) %>%
  group_by(unit) %>%
  summarise(ok = all(colSums(!is.na(pick(all_of(ok_preds)))) > 0), .groups="drop") %>%
  filter(ok) %>%
  pull(unit)

stopifnot(length(donor_ok) >= MIN_DONORS)

# balance panel
dsub <- dwin %>%
  filter(unit %in% c(treated_cc, donor_ok)) %>%
  tidyr::complete(unit = unique(unit), year = yrs_needed) %>%
  arrange(unit, year) %>%
  as.data.frame()

dsub$unit <- as.numeric(dsub$unit)
dsub$year <- as.numeric(dsub$year)

# ----------------------------
# DYNAMIC ANCHORS (Route 1)
# ----------------------------
treated_pre <- dsub[dsub$unit==treated_cc & dsub$year>=pre_start & dsub$year<=pre_end, c("year","outcome")]
avail_years <- treated_pre$year[!is.na(treated_pre$outcome)]

if(length(avail_years) == 0) stop("treated has outcome all-NA in pre window")

# pick up to 4 anchors spread out
anchor_n <- min(4, length(avail_years))
anchors <- unique(round(seq(min(avail_years), max(avail_years), length.out = anchor_n)))
anchors <- anchors[anchors %in% avail_years]

cat("Anchors chosen:", paste(anchors, collapse=", "), "\n")

# filter donors: must have outcome non-NA at ALL anchors
donor_ok2 <- dsub %>%
  dplyr::as_tibble() %>%
  filter(unit != treated_cc, year %in% anchors) %>%
  group_by(unit) %>%
  summarise(ok = all(!is.na(outcome)), .groups="drop") %>%
  filter(ok) %>%
  pull(unit)

cat("Donors after anchor outcome filter:", length(donor_ok2), "\n")

# If donors collapse too much, fall back to Route 2 (pre mean only)
use_route2 <- length(donor_ok2) < MIN_DONORS

if(use_route2){
  cat("Fallback: use pre-mean only (no single-year anchors)\n")
  donor_final <- donor_ok
  spec_preds <- list(list("outcome", pre_start:pre_end, "mean"))
} else {
  donor_final <- donor_ok2
  spec_preds <- lapply(anchors, function(t) list("outcome", t, "mean"))
}

# final dsub (rebalance for final donor set)
dsub_final <- dwin %>%
  filter(unit %in% c(treated_cc, donor_final)) %>%
  tidyr::complete(unit = unique(unit), year = yrs_needed) %>%
  arrange(unit, year) %>%
  as.data.frame()

dsub_final$unit <- as.numeric(dsub_final$unit)
dsub_final$year <- as.numeric(dsub_final$year)

# ----------------------------
# DATAPREP + SYNTH
# ----------------------------
dp <- dataprep(
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
)

syn <- synth(dp, verbose = FALSE)

Y1 <- dp$Y1plot
Y0 <- dp$Y0plot %*% syn$solution.w
gap <- as.numeric(Y1 - Y0)
years <- dp$tag$time.plot

pre_idx  <- years >= pre_start & years <= pre_end
post_idx <- years >= T0 & years <= post_end

cat("\nSUCCESS\n")
cat("donors used:", length(donor_final), "\n")
cat("pre MSPE :", mean(gap[pre_idx]^2, na.rm=TRUE), "\n")
cat("post MSPE:", mean(gap[post_idx]^2, na.rm=TRUE), "\n")
cat("post/pre :", mean(gap[post_idx]^2, na.rm=TRUE) / mean(gap[pre_idx]^2, na.rm=TRUE), "\n")
cat("post gap AUC:", sum(gap[post_idx], na.rm=TRUE), "\n")