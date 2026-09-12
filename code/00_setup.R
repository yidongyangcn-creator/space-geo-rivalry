## ---------------------------------------------------------------------------
## 00_setup.R — paths, packages and a shared plot theme.
## Every other script starts with:  if (!exists("PROJ")) source("code/00_setup.R")
## so run R from the repository root (or open space-geo-rivalry.Rproj).
## ---------------------------------------------------------------------------

PROJ <- normalizePath(getwd())
if (!file.exists(file.path(PROJ, "code", "00_setup.R"))) {
  stop("Set the working directory to the repository root before sourcing 00_setup.R.")
}

DATA_RAW     <- file.path(PROJ, "data", "raw")       # third-party source files (git-ignored)
DATA_INTERIM <- file.path(PROJ, "data", "interim")   # build by-products (git-ignored)
DATA_DERIVED <- file.path(PROJ, "data", "derived")   # the analysis panel (tracked)
OUT_FIG      <- file.path(PROJ, "output", "figures")
OUT_TAB      <- file.path(PROJ, "output", "tables")

for (p in c(DATA_RAW, DATA_INTERIM, DATA_DERIVED, OUT_FIG, OUT_TAB)) {
  if (!dir.exists(p)) dir.create(p, recursive = TRUE)
}

## The estimation panel: 176 countries x 1963-2024, one row per country-year.
PANEL <- file.path(DATA_DERIVED, "panel_country_year.csv")

## ---- Design constants (keep these in one place; several scripts depend on them)
START_YEAR <- 1970   # DID estimation window, first year
END_YEAR   <- 2020   # DID estimation window, last year
JUMP_THR   <- 0.2    # treatment threshold: jump in rival pressure, percentage points
MIN_PRE    <- 5      # required pre-treatment years for a treated unit
MIN_POST   <- 5      # required post-treatment years for a treated unit

## ---- Packages -------------------------------------------------------------
## Not installed automatically on purpose: see README for the exact versions.
.required <- c("readr", "dplyr", "tidyr", "stringr", "slider", "ggplot2")
.missing  <- .required[!vapply(.required, requireNamespace, logical(1), quietly = TRUE)]
if (length(.missing)) {
  stop("Missing packages: ", paste(.missing, collapse = ", "),
       "\nInstall them with install.packages(c(\"",
       paste(.missing, collapse = "\", \""), "\"))")
}

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(stringr); library(ggplot2)
})

theme_paper <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(plot.title    = element_text(face = "bold"),
          plot.subtitle = element_text(colour = "grey30"),
          panel.grid.minor = element_blank(),
          legend.position  = "bottom")
}
theme_set(theme_paper())

save_fig <- function(plot, name, width = 8, height = 5, dpi = 300) {
  ggsave(file.path(OUT_FIG, paste0(name, ".png")), plot,
         width = width, height = height, dpi = dpi)
  ggsave(file.path(OUT_FIG, paste0(name, ".pdf")), plot, width = width, height = height)
  invisible(file.path(OUT_FIG, name))
}

## ---------------------------------------------------------------------------
## build_did_sample() — the single source of truth for treatment assignment.
## Reproduces Equations (2)-(5) of the paper:
##   R^pct_it = 100 * rivals_share_5yr_owner
##   J_it     = R^pct_it - mean(R^pct_{i,t-1..t-3})
##   T_i      = first year with J_it >= JUMP_THR
## Treated units need MIN_PRE years before and MIN_POST years after T_i inside
## the [START_YEAR, END_YEAR] window; never-treated units are all kept.
## ---------------------------------------------------------------------------
build_did_sample <- function(panel_path = PANEL,
                             thr   = JUMP_THR,
                             start = START_YEAR,
                             end   = END_YEAR,
                             min_pre = MIN_PRE, min_post = MIN_POST) {
  stopifnot(requireNamespace("slider", quietly = TRUE))

  raw <- readr::read_csv(panel_path, show_col_types = FALSE) |>
    dplyr::mutate(
      year          = as.integer(year),
      ccode         = as.integer(ccode),
      rivals_pct    = 100 * as.numeric(rivals_share_5yr_owner),
      n_geo_entries = as.numeric(n_geo_entries),
      y_log1p       = log1p(pmax(n_geo_entries, 0))
    ) |>
    dplyr::arrange(ccode, year)

  jumped <- raw |>
    dplyr::group_by(ccode) |> dplyr::arrange(year, .by_group = TRUE) |>
    dplyr::mutate(
      base_mean = slider::slide_dbl(dplyr::lag(rivals_pct, 1),
                                    ~mean(.x, na.rm = TRUE),
                                    .before = 2, .complete = TRUE),
      jump      = rivals_pct - base_mean
    ) |>
    dplyr::ungroup()

  cohorts <- jumped |>
    dplyr::group_by(ccode) |>
    dplyr::summarise(cohort = suppressWarnings(
                       min(year[!is.na(jump) & jump >= thr], na.rm = TRUE)),
                     .groups = "drop") |>
    dplyr::mutate(cohort = ifelse(is.infinite(cohort), 10000L, as.integer(cohort)))

  d <- jumped |>
    dplyr::left_join(cohorts, by = "ccode") |>
    dplyr::filter(year >= start, year <= end)

  units <- d |> dplyr::distinct(ccode, cohort) |>
    dplyr::mutate(treated = cohort < 10000,
                  ok      = treated & (cohort - start >= min_pre) &
                                      (end - cohort >= min_post))

  d |>
    dplyr::filter(ccode %in% units$ccode[units$ok | !units$treated]) |>
    dplyr::left_join(units[, c("ccode", "ok", "treated")], by = "ccode") |>
    dplyr::mutate(
      treated_flag = as.integer(ok),
      post_flag    = as.integer(ok & year >= cohort),
      D            = treated_flag * post_flag,
      rel_time     = ifelse(ok, year - cohort, NA_integer_),
      lgdp         = log(gdp_kd),
      lpop         = log(pop_total),
      mil_bn       = mil_exp_cd / 1e9
    )
}

message("Setup loaded. PROJ = ", PROJ)
