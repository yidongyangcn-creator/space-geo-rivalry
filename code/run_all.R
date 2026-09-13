## ---------------------------------------------------------------------------
## run_all.R — reproduce every result in the paper from the derived panel.
##
##   Rscript code/run_all.R          (from the repository root)
##
## The build stage (code/01_build) is NOT run here: it needs the third-party
## raw files listed in data/README.md, which are not redistributed with this
## repository. The derived panel it produces is tracked, so everything below
## runs on a fresh clone.
##
## Table 2 (baseline OLS) and the Cox table are also available as Stata
## do-files; the R scripts here cover the same ground.
## ---------------------------------------------------------------------------

source("code/00_setup.R")

t0 <- Sys.time()

scripts <- c(
  "code/02_analysis/20_summary_stats.R",        # Table 1
  "code/02_analysis/22_did_twfe_sunabraham.R",  # Table 3, Figures 1 and 5
  "code/02_analysis/23_robustness.R",           # Figures 2, 3, 4, 6, 7
  "code/02_analysis/26_cox_survival.R",         # Appendix E
  "code/02_analysis/27_cox_appendix_figs.R"     # Appendix E, extra figures
)

for (s in scripts) {
  cat("\n\n", strrep("=", 78), "\n  RUNNING: ", s, "\n", strrep("=", 78), "\n\n", sep = "")
  ok <- tryCatch({ source(s, echo = FALSE); TRUE },
                 error = function(e) { message("FAILED: ", s, " — ", conditionMessage(e)); FALSE })
  if (!ok) message("Continuing with the remaining scripts.")
}

cat(sprintf("\n\nAll done in %.1f minutes.\n",
            as.numeric(difftime(Sys.time(), t0, units = "mins"))))
cat("Figures: output/figures   Tables: output/tables\n")
