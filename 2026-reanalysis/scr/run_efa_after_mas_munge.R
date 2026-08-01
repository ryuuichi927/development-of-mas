# run_efa_after_mas_munge.R
#
# 【これは何？】
#   Improve「改善キット」の本命ランナー。
#   現役 MAS を読んで munge → factor_analysis_clean を回す。
#   本番フォルダの contents.R / factor_analysis.R は書き換えない。
#
# One-shot: live MAS munge → Improve clean EFA
# Does not modify files under the MAS project.

improve_dir <- path.expand(
  "~/Documents/work-folder/Mas R Contents Improve"
)

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

op <- getwd()
on.exit(setwd(op), add = TRUE)
setwd(MAS_ROOT)

message("Working in MAS_ROOT: ", MAS_ROOT)
source("scr/read_data_survey.R")
source("munge/rename_variables.R")
source("munge/recode_instruments.R")
message("After munge: N=", nrow(v), " ncol=", ncol(v))

source(file.path(improve_dir, "scr/factor_analysis_clean.R"))
