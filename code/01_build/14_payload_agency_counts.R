## Paths are defined in code/00_setup.R (run from the repository root).
if (!exists("PROJ")) source("code/00_setup.R")

library(readr)
library(dplyr)
library(stringr)
library(countrycode)
library(tidyr)

# ------------------ paths (YOUR machine) ------------------
main_path   <- file.path(DATA_INTERIM, "2.1.csv")
launch_path <- file.path(DATA_RAW, "launchlog.tsv")
orgs_path   <- file.path(DATA_RAW, "orgs.tsv")

# ------------------ helper: read a tsv whose header line starts with '#' and has a 2nd comment line ------------------
read_hash_header_tsv <- function(path) {
  hdr <- read_lines(path, n_max = 1)
  cols <- str_split(str_remove(hdr, "^#"), "\t", simplify = TRUE) |> as.character()
  
  read_tsv(
    path,
    skip = 2,                # skip header line + "Updated" line
    col_names = cols,
    show_col_types = FALSE,
    col_types = cols(.default = col_character())
  )
}

# ------------------ 1) load main and drop wrong old vars ------------------
drop_vars <- c(
  "n_payloads_launched","n_pieces_launched","n_launch_events","payloads_5yr","rival_count",
  "rivals_payloads_5yr_sum","rivals_payloads_5yr_mean","rivals_payloads_5yr_max",
  "world_payloads","world_payloads_5yr","rival_share_5yr",
  "n_launch_total"  # 防止你之前试跑留下这列导致冲突
)

main <- read_csv(main_path, show_col_types = FALSE) %>%
  mutate(
    ccode = as.integer(ccode),
    year  = as.integer(year)
  ) %>%
  select(-any_of(drop_vars))

# ------------------ 2) load launchlog.tsv correctly ------------------
launch_raw <- read_hash_header_tsv(launch_path)

# only keep what we need
launch <- launch_raw %>%
  transmute(
    Launch_Tag  = str_trim(Launch_Tag),
    Launch_Date = Launch_Date,
    year = as.integer(str_extract(Launch_Date, "^\\d{4}")),
    LVState  = na_if(str_trim(LVState), "-"),
    Agency   = na_if(str_trim(Agency), "-"),
    SatOwner = na_if(str_trim(SatOwner), "-")
  ) %>%
  filter(!is.na(year), year >= 1963, year <= 2025)

# ------------------ 3) load orgs.tsv correctly and build Code -> StateCode map ------------------
orgs_raw <- read_hash_header_tsv(orgs_path)

# orgs columns (from your file): Code, StateCode exist
org_map <- orgs_raw %>%
  transmute(
    org_code = str_trim(Code),
    state2   = na_if(str_trim(StateCode), "-")
  ) %>%
  filter(!is.na(org_code), org_code != "") %>%
  distinct(org_code, .keep_all = TRUE)

# ------------------ 4) infer launcher state2 and map to ccode ------------------
launch2 <- launch %>%
  left_join(org_map %>% rename(state2_from_agency = state2),
            by = c("Agency" = "org_code")) %>%
  left_join(org_map %>% rename(state2_from_owner = state2),
            by = c("SatOwner" = "org_code")) %>%
  mutate(
    state2 = coalesce(LVState, state2_from_agency, state2_from_owner),
    state2 = na_if(str_trim(state2), "-")
  ) %>%
  mutate(
    # ISO2 -> COW ccode
    ccode = suppressWarnings(countrycode(state2, origin = "iso2c", destination = "cown")),
    # historical fix: Soviet Union
    ccode = if_else(is.na(ccode) & state2 == "SU", 365L, as.integer(ccode))
  )

# ------------------ 5) diagnostics (DO NOT IGNORE) ------------------
unmapped_n <- sum(is.na(launch2$ccode))
message("Launch rows total (1963-2025): ", nrow(launch2))
message("Unmapped launch rows (no ccode): ", unmapped_n)

# show top unmapped state2 values if any
if (unmapped_n > 0) {
  message("Top unmapped state2 values:")
  print(
    launch2 %>% filter(is.na(ccode)) %>% count(state2, sort = TRUE) %>% head(30),
    n = 30
  )
}

# ------------------ 6) aggregate to country-year: number of launch events ------------------
launch_counts <- launch2 %>%
  filter(!is.na(ccode)) %>%
  group_by(ccode, year) %>%
  summarise(n_launch_total = n_distinct(Launch_Tag), .groups = "drop") %>%
  mutate(ccode = as.integer(ccode), year = as.integer(year))

# sanity: ensure keys unique
dup_keys <- launch_counts %>% count(ccode, year) %>% filter(n > 1)
if (nrow(dup_keys) > 0) {
  print(dup_keys)
  stop("launch_counts has duplicate (ccode,year) keys. Something is wrong.")
}

# ------------------ 7) merge into main skeleton (safe) ------------------
out <- main %>%
  mutate(ccode = as.integer(ccode), year = as.integer(year)) %>%
  left_join(launch_counts, by = c("ccode", "year")) %>%
  mutate(n_launch_total = replace_na(n_launch_total, 0L))

# ensure no row explosion
stopifnot(nrow(out) == nrow(main))

# quick spot check: US ccode==2 for early years (should look plausible)
print(out %>% filter(ccode == 2, year %in% 1963:1968) %>% select(ccode, year, n_launch_total))

# ------------------ 8) write back (overwrite 2.1.csv) ------------------
write_csv(out, main_path)
message("DONE. Wrote updated main table to: ", main_path)
