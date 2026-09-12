## Paths are defined in code/00_setup.R (run from the repository root).
if (!exists("PROJ")) source("code/00_setup.R")

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(countrycode)

pop_path  <- file.path(DATA_RAW, "wdi", "API_SP.POP.TOTL_DS2_en_csv_v2_40826.csv")
main_path <- file.path(DATA_INTERIM, "2.3.csv")

# ---------------------------
# 1) Read main + harmonize units
# ---------------------------
main <- read_csv(main_path, show_col_types = FALSE) %>%
  mutate(
    cowcode = as.integer(cowcode),
    year    = as.integer(year),
    country_name = as.character(country_name)
  ) %>%
  mutate(
    cowcode = case_when(
      # Germany: unify all Germany variants to 255
      str_detect(str_to_lower(country_name), "germany") ~ 255L,
      str_detect(str_to_lower(country_name), "german democratic republic") ~ 255L,
      str_detect(str_to_lower(country_name), "west germany") ~ 255L,
      str_detect(str_to_lower(country_name), "federal republic of germany") ~ 255L,
      
      # Vietnam: unify Republic of Vietnam to Vietnam (默认 816；如果你主数据 Vietnam 不是 816，自行改这里)
      str_detect(str_to_lower(country_name), "republic of vietnam") ~ 816L,
      
      # Zanzibar: treat as Tanzania (511). Also fix the obvious junk code 0.
      str_detect(str_to_lower(country_name), "zanzibar") ~ 511L,
      cowcode == 0L & str_detect(str_to_lower(country_name), "zanzibar") ~ 511L,
      
      # Serbia: keep Serbia cowcode 345 (if you have variants, add them here)
      str_detect(str_to_lower(country_name), "^serbia$") ~ 345L,
      
      # Kosovo: keep as 347
      str_detect(str_to_lower(country_name), "kosovo") ~ 347L,
      
      TRUE ~ cowcode
    )
  )

# ---------------------------
# 2) Read WDI population (wide -> long) + map ISO3 -> COW + special cases
# ---------------------------
pop_raw <- read_csv(pop_path, skip = 4, show_col_types = FALSE)
year_cols <- names(pop_raw)[str_detect(names(pop_raw), "^\\d{4}$")]

pop_long <- pop_raw %>%
  select(`Country Name`, `Country Code`, all_of(year_cols)) %>%
  pivot_longer(all_of(year_cols), names_to = "year", values_to = "pop_total") %>%
  mutate(
    year = as.integer(year),
    pop_total = suppressWarnings(as.numeric(pop_total)),
    iso3c = `Country Code`,
    cowcode = suppressWarnings(countrycode(iso3c, origin = "iso3c", destination = "cown")),
    cowcode = as.integer(cowcode),
    
    # Special handling
    cowcode = case_when(
      iso3c == "XKX" ~ 347L,                # Kosovo
      iso3c %in% c("SRB", "SCG", "YUG") ~ 345L,  # Serbia / Serbia&Montenegro / Yugoslavia -> Serbia bucket
      TRUE ~ cowcode
    )
  ) %>%
  filter(!is.na(cowcode)) %>%
  select(cowcode, year, pop_total)

# If multiple ISO3 series map to same cowcode-year (e.g., YUG & SCG overlap), keep non-NA, otherwise max
pop_long <- pop_long %>%
  group_by(cowcode, year) %>%
  summarise(pop_total = suppressWarnings(max(pop_total, na.rm = TRUE)),
            .groups = "drop") %>%
  mutate(pop_total = ifelse(is.infinite(pop_total), NA_real_, pop_total))

# ---------------------------
# 3) Join population into main
# ---------------------------
out <- main %>%
  left_join(pop_long, by = c("cowcode", "year"))

# ---------------------------
# 4) Drop Taiwan entirely
# ---------------------------
out <- out %>%
  filter(!(cowcode == 713L | str_detect(str_to_lower(country_name), "taiwan")))

# ---------------------------
# 5) Diagnostics: check the specific problem units
# ---------------------------
cat("Rows (after drop Taiwan):", nrow(out), "\n")
cat("Pop missing rate:", mean(is.na(out$pop_total)), "\n\n")

check_units <- out %>%
  filter(
    cowcode %in% c(255L, 345L, 347L, 511L, 816L) |
      str_detect(str_to_lower(country_name), "german democratic republic|republic of vietnam|zanzibar|kosovo|serbia|germany|vietnam")
  ) %>%
  summarise(
    missing_in_these_units = sum(is.na(pop_total)),
    n_in_these_units = n()
  )

print(check_units)

# show remaining unmatched entities (should be close to 0 now, except weird leftovers)
unmatched <- out %>%
  filter(is.na(pop_total)) %>%
  count(country_name, cowcode, sort = TRUE)
print(head(unmatched, 50))

# ---------------------------
# 6) Write out
# ---------------------------
out_path <- str_replace(main_path, "\\.csv$", "_with_pop_fix_drop_taiwan.csv")
write_csv(out, out_path)
cat("\nWrote:", out_path, "\n")