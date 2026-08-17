# run_efa_after_mas_munge.R
#
# The 2026 re-analysis in one pass: read the file named by MAS_DATA, put it
# through the 2021 munge chain unchanged, then run the cleaned factor analysis.
#
# Called by RUN_ME.R, which is the intended entry point.
#
# Nothing under 2021-thesis/ is written to. That folder is the record of what
# was actually run for the thesis and is kept as it was, faults included; the
# faults are listed with line numbers in 2021-thesis/NOTES.md.

if (!exists("MAS_REPO")) {
  source(file.path(getwd(), "2026-reanalysis", "scr", "paths.R"))
}

# improve_dir is this folder, 2026-reanalysis. MAS_ROOT is 2021-thesis. The
# names are the ones the scripts were written with, when the 2026 work lived in
# a separate kit that read from the 2021 project.
improve_dir <- REANALYSIS

# Libraries needed by MAS munge / read
suppressPackageStartupMessages({
  libs <- c("dplyr", "psych", "tidyr", "ggplot2")
  for (p in libs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop("Missing package: ", p, " — install.packages(\"", p, "\")")
    }
    library(p, character.only = TRUE)
  }
})

source(file.path(improve_dir, "scr/item_sets.R"))

if (!dir.exists(MAS_ROOT)) {
  stop("MAS_ROOT not found: ", MAS_ROOT)
}

message("Reading: ", MAS_DATA)
source(file.path(improve_dir, "scr/read_data.R"))

# The munge chain is the 2021 code, unchanged.
source(file.path(MAS_ROOT, "munge/rename_variables.R"))
source(file.path(MAS_ROOT, "munge/recode_instruments.R"))
message("After munge: N=", nrow(v), " ncol=", ncol(v))

source(file.path(improve_dir, "scr/factor_analysis_clean.R"))
