# paths.R — resolve every location from the repository root.
#
# The 2021 project hardcoded one machine's absolute paths. Nothing here does.
# Set the working directory to the repository root, or export MAS_REPO.

MAS_REPO <- Sys.getenv("MAS_REPO", unset = getwd())
MAS_REPO <- normalizePath(MAS_REPO, mustWork = FALSE)

if (!dir.exists(file.path(MAS_REPO, "2026-reanalysis"))) {
  stop(
    "MAS_REPO does not look like this repository: ", MAS_REPO, "\n",
    "Open the .Rproj at the repository root, or run:\n",
    '  Sys.setenv(MAS_REPO = "/path/to/development-of-mas")'
  )
}

THESIS_2021 <- file.path(MAS_REPO, "2021-thesis")
REANALYSIS  <- file.path(MAS_REPO, "2026-reanalysis")

# Participant data are not distributed with this repository. Without MAS_DATA
# the pipeline runs on the synthetic file, which reproduces the column layout
# and the response options but none of the real correlation structure.
MAS_SYNTHETIC <- file.path(MAS_REPO, "data", "synthetic", "mas_synthetic.tsv")
MAS_DATA <- Sys.getenv("MAS_DATA", unset = MAS_SYNTHETIC)

MAS_ON_SYNTHETIC <- identical(
  normalizePath(MAS_DATA, mustWork = FALSE),
  normalizePath(MAS_SYNTHETIC, mustWork = FALSE)
)

# out/ holds the record of the August 2026 run on the real data. A synthetic
# run must not be able to overwrite it, or the record silently becomes a
# demonstration of the generator.
OUT_DIR <- if (MAS_ON_SYNTHETIC) {
  file.path(REANALYSIS, "out", "synthetic")
} else {
  file.path(REANALYSIS, "out")
}
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
