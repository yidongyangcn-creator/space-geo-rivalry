## Paths are defined in code/00_setup.R (run from the repository root).
if (!exists("PROJ")) source("code/00_setup.R")

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(slider)

# =========================
# Paths
# =========================
base_dir  <- DATA_INTERIM
main_path <- file.path(base_dir, "1.0_geo_first_entry_panel.csv")
riv_path  <- file.path(DATA_RAW, "strategic_rivalry_data_list_of_rivalries_by_type.csv")
out_path  <- file.path(base_dir, "1.1_geo_first_entry_panel_plus_rivals5yr.csv")

# =========================
# Helpers
# =========================
as_int <- function(x){
  x <- str_trim(as.character(x))
  x[x == ""] <- NA_character_
  suppressWarnings(as.integer(x))
}

# Build active window (st, ed) inside panel years
make_active_window <- function(start, end, pre1816, pre1494, ongoing2020, panel_min, panel_max){
  st <- start
  ed <- end
  
  # If rivalry predates modern period, treat as active at panel start
  st <- ifelse(is.na(st) & (pre1816 == 1 | pre1494 == 1), panel_min, st)
  st <- ifelse(is.na(st), panel_min, st)
  
  # If end missing and marked ongoing -> extend to panel end
  ed <- ifelse(is.na(ed) & ongoing2020 == 1, panel_max, ed)
  ed <- ifelse(is.na(ed), panel_max, ed)
  
  st <- pmax(st, panel_min)
  ed <- pmin(ed, panel_max)
  
  tibble(st = st, ed = ed) %>%
    mutate(ok = !is.na(st) & !is.na(ed) & st <= ed)
}

# =========================
# 1) Read MAIN panel
# =========================
panel0 <- read_csv(main_path, show_col_types = FALSE)

need_main <- c("ccode","year","n_payloads_launched")
miss_main <- setdiff(need_main, names(panel0))
if (length(miss_main) > 0) stop("Main file missing: ", paste(miss_main, collapse=", "))

panel <- panel0 %>%
  mutate(
    ccode = as_int(ccode),
    year  = as_int(year),
    n_payloads_launched = as_int(n_payloads_launched)
  ) %>%
  filter(!is.na(ccode), !is.na(year)) %>%
  arrange(ccode, year)

# hard checks
dup_key <- panel %>% count(ccode, year) %>% filter(n > 1)
if (nrow(dup_key) > 0) stop("Main panel has duplicate (ccode,year) keys.")

panel_min <- min(panel$year, na.rm = TRUE)
panel_max <- max(panel$year, na.rm = TRUE)

cat("Panel years:", panel_min, "-", panel_max, "\n")
cat("Countries:", n_distinct(panel$ccode), "\n")
cat("Rows:", nrow(panel), "\n\n")

# =========================
# 2) Rolling 5-year payload volume per country (t-4..t)
# =========================
cy_vol <- panel %>%
  transmute(ccode, year, payloads = replace_na(n_payloads_launched, 0L)) %>%
  group_by(ccode) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    payloads_5yr = slide_index_int(
      .x = payloads,
      .i = year,
      .f = ~ sum(.x, na.rm = TRUE),
      .before = 4,
      .complete = FALSE
    )
  ) %>%
  ungroup()

# =========================
# 3) Read RIVALRY list
# =========================
riv0 <- read_csv(riv_path, show_col_types = FALSE)

need_riv <- c("ccode1","ccode2","start","end","pre1816","pre1494","ongoing2020")
miss_riv <- setdiff(need_riv, names(riv0))
if (length(miss_riv) > 0) stop("Rivalry file missing: ", paste(miss_riv, collapse=", "))

riv_tbl <- riv0 %>%
  mutate(
    ccode1 = as_int(ccode1),
    ccode2 = as_int(ccode2),
    start  = as_int(start),
    end    = as_int(end),
    pre1816 = as_int(pre1816),
    pre1494 = as_int(pre1494),
    ongoing2020 = as_int(ongoing2020)
  ) %>%
  filter(!is.na(ccode1), !is.na(ccode2), ccode1 != ccode2)

stopifnot(is.data.frame(riv_tbl))

win <- make_active_window(
  start = riv_tbl$start, end = riv_tbl$end,
  pre1816 = riv_tbl$pre1816, pre1494 = riv_tbl$pre1494,
  ongoing2020 = riv_tbl$ongoing2020,
  panel_min = panel_min, panel_max = panel_max
)

riv_tbl <- bind_cols(riv_tbl, win) %>%
  filter(ok) %>%
  transmute(ccode1, ccode2, st, ed) %>%
  distinct()

cat("Rivalry dyads kept:", nrow(riv_tbl), "\n\n")

# =========================
# 4) Expand to dyad-year, then make directed i -> j
# =========================
years_tbl <- tibble(year = panel_min:panel_max)

# SAFER: crossing(riv_tbl, years_tbl) instead of riv_tbl %>% crossing(...)
riv_dyad_year <- tidyr::crossing(riv_tbl, years_tbl) %>%
  filter(year >= st, year <= ed) %>%
  select(ccode1, ccode2, year)

stopifnot(is.data.frame(riv_dyad_year))

# undirected -> directed (both directions)
riv_dir <- bind_rows(
  riv_dyad_year %>% transmute(ccode = ccode1, rival_ccode = ccode2, year),
  riv_dyad_year %>% transmute(ccode = ccode2, rival_ccode = ccode1, year)
) %>%
  distinct()

cat("Directed dyad-years:", nrow(riv_dir), "\n\n")

# =========================
# 5) Attach rival rolling 5-year payloads and aggregate to (ccode,year)
# =========================
riv_feat <- riv_dir %>%
  left_join(
    cy_vol %>% select(rival_ccode = ccode, year, rival_payloads_5yr = payloads_5yr),
    by = c("rival_ccode","year")
  ) %>%
  mutate(rival_payloads_5yr = replace_na(rival_payloads_5yr, 0L)) %>%
  group_by(ccode, year) %>%
  summarise(
    rival_count = n_distinct(rival_ccode),
    rivals_payloads_5yr_sum  = sum(rival_payloads_5yr, na.rm = TRUE),
    rivals_payloads_5yr_mean = ifelse(rival_count > 0, mean(rival_payloads_5yr, na.rm = TRUE), 0),
    rivals_payloads_5yr_max  = ifelse(rival_count > 0, max(rival_payloads_5yr, na.rm = TRUE), 0),
    .groups = "drop"
  )

# =========================
# 6) Merge back into panel
# =========================
panel_out <- panel %>%
  left_join(cy_vol %>% select(ccode, year, payloads_5yr), by = c("ccode","year")) %>%
  left_join(riv_feat, by = c("ccode","year")) %>%
  mutate(
    payloads_5yr = replace_na(payloads_5yr, 0L),
    rival_count = replace_na(rival_count, 0L),
    rivals_payloads_5yr_sum  = replace_na(rivals_payloads_5yr_sum, 0),
    rivals_payloads_5yr_mean = replace_na(rivals_payloads_5yr_mean, 0),
    rivals_payloads_5yr_max  = replace_na(rivals_payloads_5yr_max, 0)
  )

# =========================
# 7) Sanity checks (the correct at_risk logic)
# =========================
cat("=== SANITY ===\n")

# Key preservation
stopifnot(nrow(panel_out) == nrow(panel))
dup_key2 <- panel_out %>% count(ccode, year) %>% filter(n > 1)
stopifnot(nrow(dup_key2) == 0)

# Your panel's at_risk is: 1 UNTIL AND INCLUDING event year, then 0 after.
# That means: at_risk == 1 if year <= first_entry_year; else 0.
if (all(c("at_risk","first_entry_year") %in% names(panel_out))) {
  risk_bad <- panel_out %>%
    mutate(
      should_at_risk = ifelse(!is.na(first_entry_year),
                              as.integer(year <= first_entry_year),
                              1L)  # if never enters, keep at risk (you can change)
    ) %>%
    filter(at_risk != should_at_risk)
  
  cat("Risk mismatches (expect 0):", nrow(risk_bad), "\n")
  if (nrow(risk_bad) > 0) {
    print(risk_bad %>% select(ccode, year, first_entry_year, event, at_risk, should_at_risk) %>% head(50))
    cat("\nIf you WANT at_risk to drop to 0 in the event year, change <= to <.\n")
  }
} else {
  cat("Skipped at_risk check (missing at_risk or first_entry_year).\n")
}

# Quick peek: event years
if ("event" %in% names(panel_out)) {
  cat("\nEvent-year peek:\n")
  print(
    panel_out %>%
      filter(event == 1) %>%
      arrange(year) %>%
      select(ccode, year, n_payloads_launched, payloads_5yr,
             rival_count, rivals_payloads_5yr_sum, rivals_payloads_5yr_max) %>%
      head(40),
    n = 40
  )
}

cat("\nRival feature summary:\n")
print(panel_out %>%
        summarise(
          rival_count_mean = mean(rival_count),
          rivals_sum_p50 = median(rivals_payloads_5yr_sum),
          rivals_sum_p90 = quantile(rivals_payloads_5yr_sum, 0.90),
          rivals_sum_p99 = quantile(rivals_payloads_5yr_sum, 0.99)
        ))

# =========================
# 8) Save
# =========================
write_csv(panel_out, out_path)
cat("\nSaved:", out_path, "\n")
