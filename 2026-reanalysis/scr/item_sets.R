# item_sets.R — constants only (source this first)
# Mas R Contents Improve — revised 2026-08-01

# --- Full facet item names ---------------------------------------------------
MAS_ITEMS_ALL <- sprintf("mas%02d", 1:28)

FACET_SALIENCE   <- sprintf("mas%02d", 1:4)
FACET_MOOD       <- sprintf("mas%02d", 5:8)
FACET_TOLERANCE  <- sprintf("mas%02d", 9:12)
FACET_CONFLICT   <- sprintf("mas%02d", 13:16)
FACET_RELAPSE    <- sprintf("mas%02d", 17:20)
FACET_WITHDRAWAL <- sprintf("mas%02d", 21:24)
FACET_PROBLEMS   <- sprintf("mas%02d", 25:28)

# 2021 factor_analysis.R cross-loaded drop list (legacy default)
CROSSLOADED_2021 <- c(
  "mas12", "mas15", "mas27", "mas11", "mas03", "mas02", "mas22", "mas10"
)

# 2021 trimmed facet item sets (as written in original script)
TRIM_SALIENCE   <- c("mas01", "mas04")
TRIM_MOOD       <- c("mas05", "mas06", "mas07", "mas08")
TRIM_TOLERANCE  <- c("mas09")
TRIM_CONFLICT   <- c("mas13", "mas14", "mas16")
TRIM_RELAPSE    <- c("mas17", "mas18", "mas20")  # mas19 omitted in 2021 trim mean
TRIM_WITHDRAWAL <- c("mas21", "mas23", "mas24")
TRIM_PROBLEMS   <- c("mas25", "mas26", "mas28")

# Analysis options (override before source if needed)
if (!exists("N_FACTORS"))       N_FACTORS <- 2L
if (!exists("ROTATION"))        ROTATION <- "varimax"
if (!exists("USE_ROUGHFIX"))    USE_ROUGHFIX <- FALSE
if (!exists("MIN_LOADING_CUT")) MIN_LOADING_CUT <- 0.30
if (!exists("PARALLEL_N_ITER")) PARALLEL_N_ITER <- 100L
if (!exists("STANDARDIZE"))     STANDARDIZE <- FALSE
# "legacy_2021" = Salience_trim + Mood_full / 2  (matches dissertation-era script)
# "consistent"  = (Salience_trim + Mood_trim + Tolerance_trim) / 3
if (!exists("ENGAGEMENT_TRIM_MODE")) ENGAGEMENT_TRIM_MODE <- "legacy_2021"
# Composites: FALSE matches 2021 (a+b+c+d)/4 → NA if any item NA
if (!exists("COMPOSITE_NA_RM")) COMPOSITE_NA_RM <- FALSE

if (!exists("MAS_REPO")) {
  source(file.path(getwd(), "2026-reanalysis", "scr", "paths.R"))
}
IMPROVE_ROOT <- REANALYSIS
MAS_ROOT <- THESIS_2021
