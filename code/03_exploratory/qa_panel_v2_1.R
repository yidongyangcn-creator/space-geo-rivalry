## Paths are defined in code/00_setup.R (run from the repository root).
if (!exists("PROJ")) source("code/00_setup.R")

library(readr)
library(dplyr)
library(stringr)

path <- file.path(DATA_INTERIM, "2.1_clean.csv")

df <- read_csv(path, show_col_types = FALSE)

cat("=====================================\n")
cat("SANITY CHECK: 2.1_clean.csv\n")
cat("=====================================\n\n")

# ------------------ 0) expected columns ------------------
expected <- c(
  "country_name","year","cowcode","v2x_polyarchy","ccode",
  "first_entry_year","event","at_risk","n_geo_entries","geo_entry_any","n_geo_launches",
  "n_payloads_owner","n_launch_events_owner",
  "payloads_5yr_owner","world_payloads_5yr","payload_share_5yr_owner",
  "rival_count_active","rivals_payloads_5yr_owner_sum","rivals_share_5yr_owner",
  "gdp_kd"
)

cat("[0] COLUMN CHECK\n")
present <- intersect(expected, names(df))
missing <- setdiff(expected, names(df))
extra   <- setdiff(names(df), expected)

cat("Present:", length(present), "/", length(expected), "\n")
if (length(missing) > 0) cat("Missing:", paste(missing, collapse=", "), "\n")
if (length(extra)   > 0) cat("Extra cols:", paste(extra, collapse=", "), "\n")
cat("\n")

stopifnot(all(c("ccode","year","country_name") %in% names(df)))

# ------------------ 1) basic structure & types ------------------
cat("[1] BASIC STRUCTURE\n")
cat("Rows:", nrow(df), "\n")
cat("Cols:", ncol(df), "\n")
cat("Year range:", min(df$year, na.rm=TRUE), "to", max(df$year, na.rm=TRUE), "\n")
cat("Unique ccodes:", n_distinct(df$ccode, na.rm=TRUE), "\n")
cat("Missing ccode:", sum(is.na(df$ccode)), "\n")
cat("Missing year :", sum(is.na(df$year)), "\n\n")

cat("Type snapshot (key vars):\n")
type_vars <- intersect(expected, names(df))
print(sapply(df[type_vars], class))
cat("\n")

# ------------------ 2) key uniqueness ------------------
cat("[2] KEY UNIQUENESS: (ccode, year)\n")
dup_key <- df %>% count(ccode, year, name="n") %>% filter(n > 1)
cat("Duplicate key rows:", nrow(dup_key), "\n")
if (nrow(dup_key) > 0) print(head(dup_key, 50))
cat("\n")

# ------------------ 3) panel completeness (within observed span) ------------------
cat("[3] PANEL COMPLETENESS (per ccode)\n")
comp <- df %>%
  group_by(ccode) %>%
  summarise(
    min_year = min(year, na.rm=TRUE),
    max_year = max(year, na.rm=TRUE),
    n_years  = n_distinct(year),
    expected = (max_year - min_year + 1),
    gaps     = expected - n_years,
    .groups="drop"
  ) %>%
  arrange(desc(gaps), ccode)

cat("Countries with gaps inside their own min-max span:", sum(comp$gaps > 0), "\n")
print(comp %>% filter(gaps > 0) %>% slice_head(n=25))
cat("\n")

# ------------------ 4) missingness profile ------------------
cat("[4] MISSINGNESS PROFILE\n")
miss_tbl <- df %>%
  summarise(across(all_of(type_vars), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to="var", values_to="n_missing") %>%
  mutate(p_missing = n_missing / nrow(df)) %>%
  arrange(desc(n_missing))
print(miss_tbl, n = Inf)
cat("\n")

# ------------------ 5) event / at-risk logic ------------------
cat("[5] EVENT / AT_RISK LOGIC\n")

if (all(c("event","at_risk") %in% names(df))) {
  cat("Event counts:\n")
  print(df %>% count(event, sort=TRUE))
  cat("\nAt_risk counts:\n")
  print(df %>% count(at_risk, sort=TRUE))
  
  bad_event <- df %>% filter(!is.na(event) & !(event %in% c(0,1)))
  cat("\nEvent non-binary rows:", nrow(bad_event), "\n")
  
  bad_risk <- df %>% filter(!is.na(at_risk) & !(at_risk %in% c(0,1)))
  cat("At_risk non-binary rows:", nrow(bad_risk), "\n")
  
  # common survival design check: event implies at_risk==1 (usually)
  viol <- df %>% filter(event == 1 & at_risk != 1)
  cat("Violations: event==1 but at_risk!=1 :", nrow(viol), "\n")
  if (nrow(viol) > 0) print(viol %>% select(country_name, ccode, year, event, at_risk) %>% head(20))
  
  # single failure per subject (if that’s how you coded it)
  ev_per_ccode <- df %>% filter(event==1) %>% count(ccode, name="n_event") %>% arrange(desc(n_event))
  cat("\nMax events per ccode:", ifelse(nrow(ev_per_ccode)>0, max(ev_per_ccode$n_event), 0), "\n")
  if (nrow(ev_per_ccode)>0 && max(ev_per_ccode$n_event) > 1) {
    cat("ccodes with multiple events (show first 20):\n")
    print(ev_per_ccode %>% filter(n_event > 1) %>% head(20))
  }
}
cat("\n")

# ------------------ 6) GEO variables logic ------------------
cat("[6] GEO VARIABLES LOGIC\n")
geo_vars <- intersect(c("first_entry_year","n_geo_entries","geo_entry_any","n_geo_launches"), names(df))
if (length(geo_vars) > 0) {
  print(df %>% summarise(across(all_of(geo_vars), ~ sum(is.na(.)))))
  
  # constraints: geo_entry_any should be 0/1 and consistent with n_geo_entries
  if ("geo_entry_any" %in% names(df)) {
    bad_geo_any <- df %>% filter(!is.na(geo_entry_any) & !(geo_entry_any %in% c(0,1)))
    cat("geo_entry_any non-binary rows:", nrow(bad_geo_any), "\n")
  }
  if (all(c("geo_entry_any","n_geo_entries") %in% names(df))) {
    viol1 <- df %>% filter(!is.na(n_geo_entries), !is.na(geo_entry_any),
                           (n_geo_entries > 0 & geo_entry_any != 1) |
                             (n_geo_entries == 0 & geo_entry_any != 0))
    cat("Violations: n_geo_entries vs geo_entry_any inconsistency:", nrow(viol1), "\n")
    if (nrow(viol1) > 0) print(viol1 %>% select(country_name, ccode, year, n_geo_entries, geo_entry_any) %>% head(20))
  }
  if ("first_entry_year" %in% names(df)) {
    fe_bad <- df %>% filter(!is.na(first_entry_year) & (first_entry_year < 1900 | first_entry_year > max(df$year, na.rm=TRUE)))
    cat("first_entry_year out-of-range rows:", nrow(fe_bad), "\n")
    if (nrow(fe_bad) > 0) print(fe_bad %>% select(country_name, ccode, year, first_entry_year) %>% head(20))
  }
}
cat("\n")

# ------------------ 7) payload/event counts sanity ------------------
cat("[7] PAYLOAD / EVENT COUNTS\n")
if (all(c("n_payloads_owner","n_launch_events_owner") %in% names(df))) {
  s <- df %>% summarise(
    miss_payload = sum(is.na(n_payloads_owner)),
    miss_events  = sum(is.na(n_launch_events_owner)),
    zero_payload = sum(n_payloads_owner == 0, na.rm=TRUE),
    zero_events  = sum(n_launch_events_owner == 0, na.rm=TRUE),
    pos_payload  = sum(n_payloads_owner > 0, na.rm=TRUE),
    pos_events   = sum(n_launch_events_owner > 0, na.rm=TRUE),
    max_payload  = max(n_payloads_owner, na.rm=TRUE),
    p99_payload  = as.numeric(quantile(n_payloads_owner, 0.99, na.rm=TRUE)),
    max_events   = max(n_launch_events_owner, na.rm=TRUE),
    p99_events   = as.numeric(quantile(n_launch_events_owner, 0.99, na.rm=TRUE))
  )
  print(s)
  
  # logical: payloads >= events typically (since one launch can have many payloads)
  viol <- df %>% filter(!is.na(n_payloads_owner), !is.na(n_launch_events_owner),
                        n_payloads_owner < n_launch_events_owner)
  cat("Violations: payloads < launch_events:", nrow(viol), "\n")
  if (nrow(viol) > 0) print(viol %>% select(country_name, ccode, year, n_payloads_owner, n_launch_events_owner) %>% head(30))
  
  cat("\nTop 20 country-years by n_payloads_owner:\n")
  print(df %>% arrange(desc(n_payloads_owner)) %>%
          select(country_name, ccode, year, n_payloads_owner, n_launch_events_owner) %>%
          slice_head(n=20))
  
  cat("\nTop 20 country-years by n_launch_events_owner:\n")
  print(df %>% arrange(desc(n_launch_events_owner)) %>%
          select(country_name, ccode, year, n_payloads_owner, n_launch_events_owner) %>%
          slice_head(n=20))
}
cat("\n")

# ------------------ 8) 5-year rolling and shares checks ------------------
cat("[8] 5-YEAR ROLLING + SHARE CHECKS\n")
if (all(c("payloads_5yr_owner","world_payloads_5yr","payload_share_5yr_owner") %in% names(df))) {
  # should be within [0,1] unless world==0
  bad_share <- df %>% filter(!is.na(payload_share_5yr_owner) &
                               (payload_share_5yr_owner < -1e-9 | payload_share_5yr_owner > 1 + 1e-9))
  cat("payload_share_5yr_owner out of [0,1] rows:", nrow(bad_share), "\n")
  if (nrow(bad_share) > 0) print(bad_share %>% select(country_name, ccode, year, payloads_5yr_owner, world_payloads_5yr, payload_share_5yr_owner) %>% head(30))
  
  # consistency: share ~= payloads/world when world>0
  chk <- df %>%
    filter(!is.na(payloads_5yr_owner), !is.na(world_payloads_5yr), world_payloads_5yr > 0) %>%
    mutate(share_calc = payloads_5yr_owner / world_payloads_5yr,
           diff = abs(share_calc - payload_share_5yr_owner)) %>%
    summarise(
      n = n(),
      max_diff = max(diff, na.rm=TRUE),
      p99_diff = as.numeric(quantile(diff, 0.99, na.rm=TRUE)),
      mean_diff = mean(diff, na.rm=TRUE)
    )
  cat("Share consistency (calc vs stored):\n")
  print(chk)
  
  # world totals by year (sanity trend)
  yearly <- df %>%
    group_by(year) %>%
    summarise(
      world_payloads_5yr = max(world_payloads_5yr, na.rm=TRUE),
      sum_country_payloads_5yr = sum(payloads_5yr_owner, na.rm=TRUE),
      .groups="drop"
    ) %>%
    arrange(year)
  
  cat("\nWorld_payloads_5yr (first 10 years):\n")
  print(yearly %>% slice_head(n=10))
  cat("\nWorld_payloads_5yr (last 10 years):\n")
  print(yearly %>% slice_tail(n=10))
}
cat("\n")

# ------------------ 9) rivals variables checks ------------------
cat("[9] RIVALS CHECKS\n")
if (all(c("rival_count_active","rivals_payloads_5yr_owner_sum","rivals_share_5yr_owner","world_payloads_5yr") %in% names(df))) {
  
  # rival_count should be >=0 integer-ish
  bad_rc <- df %>% filter(!is.na(rival_count_active) & rival_count_active < 0)
  cat("Negative rival_count_active rows:", nrow(bad_rc), "\n")
  
  # rivals_share should be in [0,1] (allow tiny numerical eps)
  bad_rs <- df %>% filter(!is.na(rivals_share_5yr_owner) &
                            (rivals_share_5yr_owner < -1e-9 | rivals_share_5yr_owner > 1 + 1e-9))
  cat("rivals_share_5yr_owner out of [0,1] rows:", nrow(bad_rs), "\n")
  if (nrow(bad_rs) > 0) print(bad_rs %>% select(country_name, ccode, year, rival_count_active, rivals_payloads_5yr_owner_sum, rivals_share_5yr_owner) %>% head(30))
  
  # consistency: rivals_share ~= rivals_payloads/world when world>0
  chk2 <- df %>%
    filter(!is.na(rivals_payloads_5yr_owner_sum), !is.na(world_payloads_5yr), world_payloads_5yr > 0) %>%
    mutate(share_calc = rivals_payloads_5yr_owner_sum / world_payloads_5yr,
           diff = abs(share_calc - rivals_share_5yr_owner)) %>%
    summarise(
      n = n(),
      max_diff = max(diff, na.rm=TRUE),
      p99_diff = as.numeric(quantile(diff, 0.99, na.rm=TRUE)),
      mean_diff = mean(diff, na.rm=TRUE)
    )
  cat("Rivals share consistency (calc vs stored):\n")
  print(chk2)
  
  # top rivals share in latest year
  yr_max <- max(df$year, na.rm=TRUE)
  cat("\nTop 15 rivals_share_5yr_owner in latest year =", yr_max, "\n")
  print(df %>% filter(year == yr_max) %>%
          arrange(desc(rivals_share_5yr_owner)) %>%
          select(country_name, ccode, year, rival_count_active, rivals_payloads_5yr_owner_sum, rivals_share_5yr_owner) %>%
          slice_head(n=15))
}
cat("\n")

# ------------------ 10) democracy variable checks ------------------
cat("[10] VDEM VAR CHECK\n")
if ("v2x_polyarchy" %in% names(df)) {
  cat("v2x_polyarchy summary:\n")
  print(summary(df$v2x_polyarchy))
  bad_poly <- df %>% filter(!is.na(v2x_polyarchy) & (v2x_polyarchy < -1e-6 | v2x_polyarchy > 1 + 1e-6))
  cat("v2x_polyarchy out-of-[0,1] rows:", nrow(bad_poly), "\n")
  if (nrow(bad_poly) > 0) print(bad_poly %>% select(country_name, ccode, year, v2x_polyarchy) %>% head(20))
}
cat("\n")

# ------------------ 11) GDP checks ------------------
cat("[11] GDP CHECK\n")
if ("gdp_kd" %in% names(df)) {
  cat("GDP missing rows:", sum(is.na(df$gdp_kd)), "\n")
  cat("GDP nonpositive rows:", sum(df$gdp_kd <= 0, na.rm=TRUE), "\n")
  cat("GDP summary (non-missing):\n")
  print(summary(df$gdp_kd))
  
  # countries with zero observed GDP
  gdp_cov <- df %>%
    group_by(ccode, country_name) %>%
    summarise(
      n_nonmiss = sum(!is.na(gdp_kd)),
      min_year  = min(year),
      max_year  = max(year),
      .groups="drop"
    ) %>%
    arrange(n_nonmiss)
  
  cat("\nBottom 30 countries by GDP coverage:\n")
  print(gdp_cov %>% slice_head(n=30), n=30)
  
  # spot check for known weird cases
  cat("\nGDP coverage spot check (Venezuela, North Korea, Taiwan if present):\n")
  print(gdp_cov %>% filter(country_name %in% c("Venezuela","North Korea","Taiwan")) , n=20)
}
cat("\n")

cat("=== DONE ===\n")
