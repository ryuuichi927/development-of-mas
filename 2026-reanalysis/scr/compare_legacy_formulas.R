# compare_legacy_formulas.R
# Side-by-side: 2021 formula replay vs clean pipeline outputs
# Requires: run munge first, or call via run script below.

improve_dir <- path.expand(
  "~/Documents/work-folder/Mas R Contents Improve"
)
source(file.path(improve_dir, "scr/item_sets.R"))
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages(library(psych))

stopifnot(exists("v"))
v <- as.data.frame(v)

# --- Replay 2021 composites exactly (from original script text) -------------
legacy_mean4 <- function(...) {
  args <- list(...)
  Reduce(`+`, args) / length(args)
}

# ensure numeric mas
for (nm in sprintf("mas%02d", 1:28)) {
  if (nm %in% names(v)) v[[nm]] <- as.numeric(v[[nm]])
}

v$L2021_MAS_Salience <- (v$mas01 + v$mas02 + v$mas03 + v$mas04) / 4
v$L2021_MAS_Mood <- (v$mas05 + v$mas06 + v$mas07 + v$mas08) / 4
v$L2021_MAS_Tolerance <- (v$mas09 + v$mas10 + v$mas11 + v$mas12) / 4
v$L2021_MAS_Conflict <- (v$mas13 + v$mas14 + v$mas15 + v$mas16) / 4
v$L2021_MAS_Relapse <- (v$mas17 + v$mas18 + v$mas19 + v$mas20) / 4
v$L2021_MAS_Withdrawal <- (v$mas21 + v$mas22 + v$mas23 + v$mas24) / 4
v$L2021_MAS_Problems <- (v$mas25 + v$mas26 + v$mas27 + v$mas28) / 4

v$L2021_Addictivecore <- (
  v$L2021_MAS_Conflict + v$L2021_MAS_Relapse +
    v$L2021_MAS_Withdrawal + v$L2021_MAS_Problems
) / 4
v$L2021_Engagementcore <- (
  v$L2021_MAS_Salience + v$L2021_MAS_Mood + v$L2021_MAS_Tolerance
) / 3

v$L2021_trim_Salience <- (v$mas01 + v$mas04) / 2
v$L2021_trim_Conflict <- (v$mas13 + v$mas14 + v$mas16) / 3
v$L2021_trim_Relapse <- (v$mas17 + v$mas18 + v$mas20) / 3
v$L2021_trim_Withdrawal <- (v$mas21 + v$mas23 + v$mas24) / 3
v$L2021_trim_Problems <- (v$mas25 + v$mas26 + v$mas28) / 3
# 2021 bug-feature: reuses full Mood in engagement trim
v$L2021_trim_Addictive <- (
  v$L2021_trim_Conflict + v$L2021_trim_Relapse +
    v$L2021_trim_Withdrawal + v$L2021_trim_Problems
) / 4
v$L2021_trim_Engagement <- (
  v$L2021_trim_Salience + v$L2021_MAS_Mood
) / 2

# Run clean with legacy engagement mode
ENGAGEMENT_TRIM_MODE <- "legacy_2021"
COMPOSITE_NA_RM <- FALSE
USE_ROUGHFIX <- FALSE
source(file.path(improve_dir, "scr/factor_analysis_clean.R"))

cmp <- function(a, b, name) {
  ok <- is.finite(a) & is.finite(b)
  data.frame(
    measure = name,
    n = sum(ok),
    cor = if (sum(ok) > 2) stats::cor(a[ok], b[ok]) else NA_real_,
    mean_diff = mean(a[ok] - b[ok], na.rm = TRUE),
    max_abs_diff = max(abs(a[ok] - b[ok]), na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

rows <- rbind(
  cmp(v$L2021_Addictivecore, v$MAS_Addictivecore, "full_Addictivecore"),
  cmp(v$L2021_Engagementcore, v$MAS_Engagementcore, "full_Engagementcore"),
  cmp(v$L2021_trim_Addictive, v$trimmedMAS_Addictivecore, "trim_Addictivecore"),
  cmp(v$L2021_trim_Engagement, v$trimmedMAS_Engagementcore, "trim_Engagement_legacyMode"),
  cmp(
    v$L2021_trim_Engagement,
    v$trimmedMAS_Engagementcore_consistent,
    "trim_Engagement_legacy_vs_consistent_def"
  )
)

print(rows)
utils::write.csv(rows, file.path(OUT_DIR, "compare_legacy_vs_clean.csv"), row.names = FALSE)

# EFA with roughfix like 2021 (sensitivity)
message("=== Sensitivity: USE_ROUGHFIX=TRUE (2021-like imputation) ===")
USE_ROUGHFIX <- TRUE
ENGAGEMENT_TRIM_MODE <- "legacy_2021"
# stash labeled scores before re-run
v$F_add_listwise <- v$MAS_F_addictive
v$F_eng_listwise <- v$MAS_F_engagement
source(file.path(improve_dir, "scr/factor_analysis_clean.R"))
v$F_add_roughfix <- v$MAS_F_addictive
v$F_eng_roughfix <- v$MAS_F_engagement

sens <- rbind(
  cmp(v$F_add_listwise, v$F_add_roughfix, "F_addictive_listwise_vs_roughfix"),
  cmp(v$F_eng_listwise, v$F_eng_roughfix, "F_engagement_listwise_vs_roughfix")
)
print(sens)
utils::write.csv(
  sens,
  file.path(OUT_DIR, "compare_listwise_vs_roughfix.csv"),
  row.names = FALSE
)

message("Compare done → out/compare_*.csv")
