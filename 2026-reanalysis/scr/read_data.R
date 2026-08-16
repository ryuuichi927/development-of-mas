# read_data.R — 2026 replacement for 2021-thesis/scr/read_data_survey.R.
#
# Identical behaviour, except that the file to read is resolved from MAS_DATA
# instead of being written into the source. The 2021 script is kept unchanged
# under 2021-thesis/ as a record of what was actually run for the thesis.

if (!exists("MAS_DATA")) {
  stop("source scr/paths.R first")
}
if (!file.exists(MAS_DATA)) {
  stop(
    "Data file not found: ", MAS_DATA, "\n",
    "Either generate the synthetic file:\n",
    '  source("tools/make_synthetic_data.R")\n',
    "or point MAS_DATA at a real Qualtrics export."
  )
}

v <- read.csv(MAS_DATA, header = TRUE, sep = "\t")

if (isTRUE(MAS_ON_SYNTHETIC)) {
  message("NOTE: running on synthetic data. Results are structurally valid and ",
          "substantively meaningless. Output goes to out/synthetic/.")
}

cat("Original data: N x Variables:")
cat(dim(v))
cat("\n===============Reading done!========================\n")
