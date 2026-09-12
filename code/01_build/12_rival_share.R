## Paths are defined in code/00_setup.R (run from the repository root).
if (!exists("PROJ")) source("code/00_setup.R")

library(readr)
library(dplyr)
library(stringr)
library(slider)

base_dir <- DATA_INTERIM
in_path  <- file.path(base_dir, "1.1_geo_first_entry_panel_plus_rivals5yr.csv")
out_path <- file.path(base_dir, "1.2_geo_first_entry_panel_plus_rivals5yr_plus_rivalshare.csv")

panel <- read_csv(in_path, show_col_types = FALSE) %>%
  mutate(
    ccode = as.integer(ccode),
    year  = as.integer(year),
    n_payloads_launched = as.integer(n_payloads_launched),
    # 你之前算出来的对手5年总量（列名按你实际文件）
    rivals_payloads_5yr_sum = as.numeric(rivals_payloads_5yr_sum)
  ) %>%
  arrange(ccode, year)

# -------------------------
# 1) World total payloads per year
# -------------------------
world_year <- panel %>%
  group_by(year) %>%
  summarise(world_payloads = sum(replace_na(n_payloads_launched, 0L)), .groups = "drop") %>%
  arrange(year)

# -------------------------
# 2) World rolling 5-year total (t-4..t)
# -------------------------
world_year <- world_year %>%
  mutate(
    world_payloads_5yr = slide_index_dbl(
      .x = world_payloads,
      .i = year,
      .f = ~ sum(.x, na.rm = TRUE),
      .before = 4,
      .complete = FALSE
    )
  )

# -------------------------
# 3) Merge back + compute share
# -------------------------
panel2 <- panel %>%
  left_join(world_year %>% select(year, world_payloads, world_payloads_5yr), by = "year") %>%
  mutate(
    # 防止除以 0
    rival_share_5yr = ifelse(is.na(world_payloads_5yr) | world_payloads_5yr <= 0,
                             0,
                             pmin(pmax(rivals_payloads_5yr_sum / world_payloads_5yr, 0), 1))
  )

# -------------------------
# 4) Sanity checks (硬核一点)
# -------------------------
stopifnot(nrow(panel2) == nrow(panel))
stopifnot(nrow(panel2 %>% count(ccode, year) %>% filter(n > 1)) == 0)

cat("rival_share_5yr summary:\n")
print(panel2 %>% summarise(
  p0  = quantile(rival_share_5yr, 0),
  p50 = quantile(rival_share_5yr, 0.5),
  p90 = quantile(rival_share_5yr, 0.9),
  p99 = quantile(rival_share_5yr, 0.99),
  p100= quantile(rival_share_5yr, 1),
  mean= mean(rival_share_5yr, na.rm = TRUE)
))

bad_share <- panel2 %>% filter(is.na(rival_share_5yr) | rival_share_5yr < 0 | rival_share_5yr > 1)
cat("Bad share rows (expect 0):", nrow(bad_share), "\n")

# -------------------------
# 5) Save
# -------------------------
write_csv(panel2, out_path)
cat("Saved:", out_path, "\n")
