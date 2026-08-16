# run_efa_after_mas_munge.R
#
# 【これは何？】
#   Improve「改善キット」の本命ランナー。
#   現役 MAS を読んで munge → factor_analysis_clean を回す。
#   本番フォルダの contents.R / factor_analysis.R は書き換えない。
#
# One-shot: live MAS munge → Improve clean EFA
# Does not modify files under the MAS project.

if (!exists("MAS_REPO")) {
  source(file.path(getwd(), "2026-reanalysis", "scr", "paths.R"))
}
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
