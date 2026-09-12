## Paths are defined in code/00_setup.R (run from the repository root).
if (!exists("PROJ")) source("code/00_setup.R")
## (workspace is not cleared: paths come from code/00_setup.R)
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(janitor)

# Optional but very useful
if (!requireNamespace("countrycode", quietly = TRUE)) install.packages("countrycode")
library(countrycode)

# ============================================================
# 0) Paths
# ============================================================
base_dir  <- DATA_INTERIM
panel_path <- file.path(base_dir, "geo_first_entry_panel_ccode.csv")

# ============================================================
# 1) Load + basic structure check
# ============================================================
panel0 <- read_csv(panel_path, show_col_types = FALSE) %>% clean_names()

cat("\n====================\n1) BASIC STRUCTURE\n====================\n")
cat("Dim:", dim(panel0), "\n")
cat("Cols:\n"); print(names(panel0))

required <- c("ccode","year","first_entry_year","event","at_risk",
              "n_geo_entries","geo_entry_any","n_geo_launches")
missing_required <- setdiff(required, names(panel0))
if (length(missing_required) > 0) {
  stop("Missing required columns: ", paste(missing_required, collapse=", "))
}

panel <- panel0 %>%
  mutate(across(all_of(required), ~as.numeric(.))) %>%
  mutate(
    ccode = as.integer(ccode),
    year  = as.integer(year),
    first_entry_year = as.integer(first_entry_year),
    event = as.integer(event),
    at_risk = as.integer(at_risk),
    n_geo_entries = as.integer(n_geo_entries),
    geo_entry_any = as.integer(geo_entry_any),
    n_geo_launches = as.integer(n_geo_launches)
  )

# ============================================================
# 2) Basic NA / type / range checks
# ============================================================
cat("\n====================\n2) NA + RANGE CHECKS\n====================\n")

na_share <- panel %>%
  summarise(across(all_of(required), ~mean(is.na(.)))) %>%
  pivot_longer(everything(), names_to="var", values_to="na_share") %>%
  arrange(desc(na_share))
print(na_share, n = 50)

range_summary <- panel %>%
  summarise(
    year_min = min(year, na.rm=TRUE),
    year_max = max(year, na.rm=TRUE),
    ccode_n  = n_distinct(ccode),
    rows     = n()
  )
print(range_summary)

# year sanity
bad_year <- panel %>%
  filter(is.na(year) | year < 1900 | year > 2100) %>%
  distinct(ccode, year)
if (nrow(bad_year) > 0) {
  cat("\n[!] BAD YEAR VALUES found:\n")
  print(bad_year, n=200)
}

# negative counts sanity
neg_counts <- panel %>%
  filter(n_geo_entries < 0 | n_geo_launches < 0)
if (nrow(neg_counts) > 0) {
  cat("\n[!] NEGATIVE COUNTS found:\n")
  print(neg_counts, n=200)
}

# ============================================================
# 3) Key uniqueness: ccode-year should be unique
# ============================================================
cat("\n====================\n3) KEY UNIQUENESS\n====================\n")
dup_key <- panel %>%
  count(ccode, year) %>%
  filter(n > 1)
if (nrow(dup_key) == 0) {
  cat("OK: (ccode,year) unique.\n")
} else {
  cat("\n[!] DUPLICATE (ccode,year) rows:\n")
  print(dup_key, n=200)
}

# ============================================================
# 4) Event logic consistency checks
# ============================================================
cat("\n====================\n4) EVENT LOGIC CHECKS\n====================\n")

# 4.1 event must be 0/1, at_risk must be 0/1
bad_binary <- panel %>%
  filter(!event %in% c(0,1) | !at_risk %in% c(0,1) | !geo_entry_any %in% c(0,1))
if (nrow(bad_binary) > 0) {
  cat("\n[!] NON-BINARY flags found (event/at_risk/geo_entry_any):\n")
  print(bad_binary %>% select(ccode, year, event, at_risk, geo_entry_any) %>% head(200), n=200)
} else {
  cat("OK: event/at_risk/geo_entry_any are binary.\n")
}

# 4.2 event implies geo_entry_any=1 and n_geo_entries>=1
bad_event_implies <- panel %>%
  filter(event == 1 & (geo_entry_any != 1 | n_geo_entries < 1))
if (nrow(bad_event_implies) > 0) {
  cat("\n[!] EVENT rows without entry evidence:\n")
  print(bad_event_implies %>% select(ccode, year, event, n_geo_entries, geo_entry_any, n_geo_launches), n=200)
} else {
  cat("OK: event implies entry evidence.\n")
}

# 4.3 geo_entry_any should equal (n_geo_entries>0)
bad_entry_any <- panel %>%
  filter(geo_entry_any != as.integer(n_geo_entries > 0))
if (nrow(bad_entry_any) > 0) {
  cat("\n[!] geo_entry_any mismatch with n_geo_entries:\n")
  print(bad_entry_any %>% select(ccode, year, n_geo_entries, geo_entry_any), n=200)
} else {
  cat("OK: geo_entry_any matches n_geo_entries>0.\n")
}

# 4.4 each ccode should have at most one event==1 (first-entry event)
multi_event <- panel %>%
  group_by(ccode) %>%
  summarise(n_event = sum(event, na.rm=TRUE), .groups="drop") %>%
  filter(n_event > 1)
if (nrow(multi_event) > 0) {
  cat("\n[!] MULTIPLE EVENTS for same ccode:\n")
  print(multi_event, n=200)
} else {
  cat("OK: at most one event per ccode.\n")
}

# 4.5 first_entry_year consistency:
# for each ccode:
# - if any event==1 then first_entry_year should equal the year where event==1
# - if no event==1 then first_entry_year should be NA (or some sentinel you set)
fe_consistency <- panel %>%
  group_by(ccode) %>%
  summarise(
    fe_unique = n_distinct(first_entry_year[!is.na(first_entry_year)]),
    fe_min = suppressWarnings(min(first_entry_year, na.rm=TRUE)),
    fe_max = suppressWarnings(max(first_entry_year, na.rm=TRUE)),
    event_year = suppressWarnings(min(year[event==1], na.rm=TRUE)),
    has_event = any(event==1, na.rm=TRUE),
    .groups="drop"
  ) %>%
  mutate(
    fe_min = ifelse(is.infinite(fe_min), NA, fe_min),
    fe_max = ifelse(is.infinite(fe_max), NA, fe_max),
    event_year = ifelse(is.infinite(event_year), NA, event_year),
    bad_fe_multiple = fe_unique > 1,
    bad_fe_mismatch = has_event & (!is.na(event_year)) & (fe_min != event_year),
    bad_fe_missing  = has_event & is.na(fe_min)
  )

bad_fe <- fe_consistency %>%
  filter(bad_fe_multiple | bad_fe_mismatch | bad_fe_missing)

if (nrow(bad_fe) > 0) {
  cat("\n[!] first_entry_year inconsistency by ccode:\n")
  print(bad_fe, n=200)
} else {
  cat("OK: first_entry_year consistent with event year (within panel).\n")
}

# ============================================================
# 5) Risk set logic: at_risk should be 1 before event, 0 after
# ============================================================
cat("\n====================\n5) RISK SET CHECKS\n====================\n")

risk_check <- panel %>%
  arrange(ccode, year) %>%
  group_by(ccode) %>%
  mutate(
    cum_event = cumsum(event),
    should_at_risk = as.integer(cum_event == 0),  # at risk until event occurs (exclusive)
    # if you want at_risk==1 on the event year itself, adjust to (cum_event <= 1 & lag(cum_event,0)==0) etc.
    mismatch = at_risk != should_at_risk
  ) %>%
  ungroup()

risk_bad <- risk_check %>%
  filter(mismatch)

if (nrow(risk_bad) > 0) {
  cat("\n[!] at_risk mismatches (first 200 rows):\n")
  print(risk_bad %>% select(ccode, year, event, at_risk, should_at_risk) %>% head(200), n=200)
  cat("\nNOTE: This check assumes at_risk becomes 0 starting the event year.\n")
  cat("If you define at_risk=1 on the event year, tell me and I’ll change the logic.\n")
} else {
  cat("OK: at_risk matches 'no prior event' rule.\n")
}

# ============================================================
# 6) Count logic: launch counts vs entry counts (should be >= most years)
# ============================================================
cat("\n====================\n6) COUNT LOGIC CHECKS\n====================\n")

# Usually: n_geo_launches >= n_geo_entries if you count launches of those entrants in same year.
# But there are legit exceptions (multi-payload launches, co-mingled, catalog quirks).
count_weird <- panel %>%
  filter(n_geo_launches < n_geo_entries) %>%
  arrange(desc(n_geo_entries - n_geo_launches))

if (nrow(count_weird) > 0) {
  cat("\n[!] Years with n_geo_launches < n_geo_entries (inspect):\n")
  print(count_weird %>% select(ccode, year, n_geo_entries, n_geo_launches) %>% head(200), n=200)
} else {
  cat("OK: n_geo_launches >= n_geo_entries everywhere (nice).\n")
}

# Huge spikes: entries/launches above p99
p99_entries <- quantile(panel$n_geo_entries, 0.99, na.rm=TRUE)
p99_launch  <- quantile(panel$n_geo_launches, 0.99, na.rm=TRUE)

spikes <- panel %>%
  filter(n_geo_entries >= p99_entries | n_geo_launches >= p99_launch) %>%
  arrange(desc(n_geo_entries), desc(n_geo_launches))

cat("\nTop spikes (p99+):\n")
print(spikes %>% select(ccode, year, n_geo_entries, n_geo_launches) %>% head(100), n=100)

# ============================================================
# 7) Country labels (human-readable), plus suspicious small-country checks
# ============================================================
cat("\n====================\n7) COUNTRY LABELS + BASIC PLAUSIBILITY\n====================\n")

ccode_names <- panel %>%
  distinct(ccode) %>%
  mutate(country = countrycode(ccode, "cown", "country.name"))

if (any(is.na(ccode_names$country))) {
  cat("\n[!] Some ccodes could not be translated to country.name:\n")
  print(ccode_names %>% filter(is.na(country)), n=200)
}

# Show event list with country names
event_list <- panel %>%
  filter(event==1) %>%
  left_join(ccode_names, by="ccode") %>%
  arrange(year) %>%
  select(ccode, country, year, first_entry_year, n_geo_entries, n_geo_launches)

cat("\nEvent list (country names):\n")
print(event_list, n=200)

# ============================================================
# 8) Panel completeness per country: continuous years? gaps?
# ============================================================
cat("\n====================\n8) PANEL CONTINUITY CHECK\n====================\n")

gap_check <- panel %>%
  arrange(ccode, year) %>%
  group_by(ccode) %>%
  summarise(
    n_years = n(),
    year_min = min(year, na.rm=TRUE),
    year_max = max(year, na.rm=TRUE),
    expected = year_max - year_min + 1,
    has_gaps = (n_years != expected),
    .groups="drop"
  )

if (any(gap_check$has_gaps)) {
  cat("\n[!] Countries with year gaps (should be continuous in event-history panel):\n")
  print(gap_check %>% filter(has_gaps) %>% arrange(desc(expected - n_years)), n=200)
} else {
  cat("OK: no year gaps within each ccode.\n")
}

# ============================================================
# 9) Export problem rows for debugging
# ============================================================
cat("\n====================\n9) EXPORT DEBUG FILES\n====================\n")

problems_dir <- file.path(base_dir, "sanity_outputs")
dir.create(problems_dir, showWarnings = FALSE)

write_csv(event_list, file.path(problems_dir, "event_list_with_country.csv"))
write_csv(unmapped <- ccode_names %>% filter(is.na(country)),
          file.path(problems_dir, "ccode_untranslated.csv"))

# risk mismatches
risk_bad_out <- risk_bad %>%
  left_join(ccode_names, by="ccode") %>%
  select(ccode, country, year, event, at_risk, should_at_risk)

write_csv(risk_bad_out, file.path(problems_dir, "risk_mismatches.csv"))

# multiple events
write_csv(multi_event, file.path(problems_dir, "multiple_events.csv"))

# geo_entry_any mismatch
write_csv(bad_entry_any, file.path(problems_dir, "geo_entry_any_mismatch.csv"))

# count weirdness
write_csv(count_weird, file.path(problems_dir, "launch_lt_entry.csv"))

# first_entry_year inconsistencies
write_csv(bad_fe, file.path(problems_dir, "first_entry_year_inconsistency.csv"))

cat("\nSaved sanity outputs to:", problems_dir, "\n")

cat("\n====================\nDONE.\n====================\n")
