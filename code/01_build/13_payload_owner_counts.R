## Paths are defined in code/00_setup.R (run from the repository root).
if (!exists("PROJ")) source("code/00_setup.R")

library(readr)
library(dplyr)
library(stringr)
library(countrycode)
library(tidyr)

main_path   <- file.path(DATA_INTERIM, "2.1.csv")
launch_path <- file.path(DATA_RAW, "launchlog.tsv")

# ---- helper: read launchlog with '#'-header + 2nd comment line ----
read_launchlog <- function(path) {
  hdr <- read_lines(path, n_max = 1)
  cols <- str_split(str_remove(hdr, "^#"), "\t", simplify = TRUE) |> as.character()
  
  read_tsv(
    path,
    skip = 2,
    col_names = cols,
    show_col_types = FALSE,
    col_types = cols(.default = col_character())
  )
}

# ---- 1) load main (keep whatever you already have; just ensure keys are int) ----
main <- read_csv(main_path, show_col_types = FALSE) %>%
  mutate(ccode = as.integer(ccode), year = as.integer(year))

# (optional) if you want to delete the old wrong launch variable first:
main <- main %>% select(-any_of(c("n_launch_total", "n_payloads_owner", "n_launch_events_owner")))

# ---- 2) load launchlog ----
ll <- read_launchlog(launch_path)

# ---- 3) build payload-owner country-year counts from SatState ----
owner_counts <- ll %>%
  transmute(
    Launch_Tag  = str_trim(Launch_Tag),
    Launch_Date = Launch_Date,
    year = as.integer(str_extract(Launch_Date, "^\\d{4}")),
    satstate = na_if(str_trim(SatState), "-"),
    piece = str_trim(Piece)
  ) %>%
  filter(!is.na(year), year >= 1963, year <= 2024, !is.na(satstate)) %>%
  mutate(
    # tiny recodes for common historical/alias codes
    satstate = recode(satstate,
                      "UK" = "GB"
    ),
    ccode = suppressWarnings(countrycode(satstate, origin = "iso2c", destination = "cown")),
    ccode = if_else(is.na(ccode) & satstate == "SU", 365L, as.integer(ccode))
  ) %>%
  filter(!is.na(ccode)) %>%
  group_by(ccode, year) %>%
  summarise(
    # payload count = number of pieces/rows (owner-specific payloads)
    n_payloads_owner = n(),
    # launch event count = number of distinct launch tags that included this owner's payloads
    n_launch_events_owner = n_distinct(Launch_Tag),
    .groups = "drop"
  ) %>%
  mutate(ccode = as.integer(ccode), year = as.integer(year))

# ---- 4) diagnostics: how wide is coverage now? ----
message("Owner country-years with payloads: ", nrow(owner_counts))
message("Owner countries with payloads: ", n_distinct(owner_counts$ccode))

# ---- 5) merge into main skeleton, fill 0 ----
out <- main %>%
  left_join(owner_counts, by = c("ccode","year")) %>%
  mutate(
    n_payloads_owner       = replace_na(as.integer(n_payloads_owner), 0L),
    n_launch_events_owner  = replace_na(as.integer(n_launch_events_owner), 0L)
  )

# sanity: row count should not change
stopifnot(nrow(out) == nrow(main))

# quick spot check
print(out %>% summarise(
  pos_payload = sum(n_payloads_owner > 0),
  pos_events  = sum(n_launch_events_owner > 0),
  max_payload = max(n_payloads_owner),
  max_events  = max(n_launch_events_owner)
))
print(out %>% filter(ccode %in% c(2,365,710), year %in% 1963:1970) %>%
        select(country_name, ccode, year, n_payloads_owner, n_launch_events_owner) %>%
        arrange(ccode, year))

# ---- 6) write back ----
write_csv(out, main_path)
message("Wrote updated 2.1.csv with payload-owner launch measures: ", main_path)
