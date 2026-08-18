# make_out.R — regenerate out/ from the real data, in a fixed order.
#
# Three scripts write into out/, and two of them write the canonical run's file
# names under different options: compare_OLD_vs_NEW.R replays the 2021 core, and
# compare_legacy_formulas.R ends on a USE_ROUGHFIX = TRUE sensitivity pass. So
# whichever ran last decided what efa_fit_log.csv, efa_trimmed_loadings.csv,
# cor_mas_factors_external.csv and corrplot_mas_clean.pdf held. This script
# fixes the order and ends on the canonical run, so those four files always
# describe USE_ROUGHFIX = FALSE with listwise N = 241.
#
# Needs MAS_DATA pointing at the participant file, which is not distributed.
# Run from the repository root:
#   MAS_DATA=/path/to/export.tsv Rscript 2026-reanalysis/make_out.R

source(file.path(getwd(), "2026-reanalysis", "scr", "paths.R"))

if (MAS_ON_SYNTHETIC) {
  stop(
    "make_out.R writes the real-data record in out/. On synthetic input it ",
    "would write out/synthetic/ instead, which is not the record.\n",
    'Set MAS_DATA, or use RUN_ME.R for a synthetic run.'
  )
}

keep <- c(
  "MAS_REPO", "THESIS_2021", "REANALYSIS", "MAS_DATA", "MAS_SYNTHETIC",
  "MAS_ON_SYNTHETIC", "OUT_DIR", "keep", "stage"
)

stage <- function(label, path) {
  message("\n########## ", label, " ##########")
  source(path)
  rm(list = setdiff(ls(envir = globalenv()), keep), envir = globalenv())
}

# 1. The head-to-head against the 2021 core. Writes OLD_vs_NEW_*.
stage("OLD vs NEW", file.path(REANALYSIS, "scr", "compare_OLD_vs_NEW.R"))

# 2. The canonical run, needed for v, and the legacy formula comparison that
#    consumes it. This pass leaves the main files on the sensitivity options.
message("\n########## canonical run, then legacy formulas ##########")
source(file.path(MAS_REPO, "2026-reanalysis", "RUN_ME.R"))
source(file.path(REANALYSIS, "scr", "compare_legacy_formulas.R"))
rm(list = setdiff(ls(envir = globalenv()), keep), envir = globalenv())

# 3. The canonical run again, so it has the last word on the main files.
stage("canonical run, final", file.path(MAS_REPO, "2026-reanalysis", "RUN_ME.R"))

writeLines(capture.output(utils::sessionInfo()), file.path(OUT_DIR, "sessionInfo.txt"))
message("\n=== out/ regenerated: ", OUT_DIR, " ===")
