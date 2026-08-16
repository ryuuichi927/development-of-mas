# ============================================================
#  The one file to source. Runs the 2026 re-analysis.
# ============================================================
# From the repository root:
#   source("2026-reanalysis/RUN_ME.R")
#
# Reads the data named by MAS_DATA, or the synthetic file if that is unset,
# runs the 2021 munge chain over it, then the cleaned EFA.
# Writes to 2026-reanalysis/out/. Nothing under 2021-thesis/ is modified.
# ============================================================

source(file.path(getwd(), "2026-reanalysis", "scr", "paths.R"))
source(file.path(REANALYSIS, "scr", "run_efa_after_mas_munge.R"))
