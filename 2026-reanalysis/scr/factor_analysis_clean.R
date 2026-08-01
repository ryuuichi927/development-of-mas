# factor_analysis_clean.R
# Mas R Contents Improve — cleaned EFA + composites
# Does NOT overwrite 2021-mas-music-addiction/scr/factor_analysis.R
#
# Prerequisites: `v` after MAS munge (mas01..28, ideally hums*, MSI.*)
# Or use run_efa_after_mas_munge.R
#
# Revised 2026-08-01 (Ben): NA policy aligned with 2021; dual engagement trim;
#                           score alignment; fit log; no dead lm/papaja.

suppressPackageStartupMessages({
  library(psych)
})
if (identical(get0("ROTATION", ifnotfound = "varimax"), "oblimin") ||
    identical(get0("ROTATION", ifnotfound = "varimax"), "oblique")) {
  if (!requireNamespace("GPArotation", quietly = TRUE)) {
    stop("ROTATION=", ROTATION, " needs package GPArotation")
  }
}

improve_dir <- path.expand(
  "~/Documents/work-folder/Mas R Contents Improve"
)
source(file.path(improve_dir, "scr/item_sets.R"))

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Mean of columns: default NA if any missing (2021 thesis style)
row_mean <- function(data, cols, na.rm = COMPOSITE_NA_RM) {
  cols <- intersect(cols, names(data))
  if (!length(cols)) {
    return(rep(NA_real_, nrow(data)))
  }
  as.numeric(rowMeans(data[, cols, drop = FALSE], na.rm = na.rm))
}

stopifnot(exists("v"), is.data.frame(v) || is.list(v))
v <- as.data.frame(v)
missing_mas <- setdiff(MAS_ITEMS_ALL, names(v))
if (length(missing_mas)) {
  stop("v is missing MAS items: ", paste(missing_mas, collapse = ", "))
}

message("=== MAS EFA clean ===")
message(
  "USE_ROUGHFIX=", USE_ROUGHFIX,
  " N_FACTORS=", N_FACTORS,
  " ROTATION=", ROTATION,
  " ENGAGEMENT_TRIM_MODE=", ENGAGEMENT_TRIM_MODE,
  " COMPOSITE_NA_RM=", COMPOSITE_NA_RM
)

# ----- item matrix (never call it `df`) --------------------------------------
mas_items <- as.data.frame(lapply(v[, MAS_ITEMS_ALL, drop = FALSE], as.numeric))

if (isTRUE(USE_ROUGHFIX)) {
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop("USE_ROUGHFIX=TRUE requires package randomForest")
  }
  message("Imputing with randomForest::na.roughfix (LEGACY — not thesis-default)")
  mas_items_efa <- randomForest::na.roughfix(mas_items)
  efa_row_index <- seq_len(nrow(v))
} else {
  ok <- stats::complete.cases(mas_items)
  message("Listwise complete MAS-28: N=", sum(ok), " / ", nrow(mas_items))
  mas_items_efa <- mas_items[ok, , drop = FALSE]
  efa_row_index <- which(ok)
}

if (isTRUE(STANDARDIZE)) {
  mas_items_efa <- as.data.frame(lapply(mas_items_efa, function(x) {
    as.numeric(scale(x))
  }))
}

kmo_full <- KMO(mas_items_efa)
message(sprintf(
  "KMO full28: overall=%.3f  item min=%.3f max=%.3f",
  kmo_full$MSA, min(kmo_full$MSAi), max(kmo_full$MSAi)
))

set.seed(20210805)
fa_parallel_full <- fa.parallel(
  mas_items_efa,
  fm = "minres",
  n.iter = as.integer(PARALLEL_N_ITER),
  quant = 0.95,
  cor = "cor",
  plot = FALSE
)
message(
  "fa.parallel full28 → factors=", fa_parallel_full$nfact,
  " components=", fa_parallel_full$ncomp,
  " | using N_FACTORS=", N_FACTORS
)

fit_full <- fa(
  mas_items_efa,
  nfactors = as.integer(N_FACTORS),
  rotate = ROTATION,
  fm = "minres"
)
message(
  "Full28 FA: TLI=", round(fit_full$TLI, 3),
  " RMSEA=", round(fit_full$RMSEA[1], 4)
)
print(fit_full, sort = TRUE, cut = MIN_LOADING_CUT, digits = 3)

# ----- trimmed EFA -----------------------------------------------------------
keep_items <- setdiff(MAS_ITEMS_ALL, CROSSLOADED_2021)
mas_trim <- as.data.frame(lapply(v[, keep_items, drop = FALSE], as.numeric))

if (isTRUE(USE_ROUGHFIX)) {
  mas_trim_efa <- randomForest::na.roughfix(mas_trim)
  trim_efa_index <- seq_len(nrow(v))
} else {
  ok2 <- stats::complete.cases(mas_trim)
  message("Listwise complete trimmed: N=", sum(ok2), " / ", nrow(mas_trim))
  mas_trim_efa <- mas_trim[ok2, , drop = FALSE]
  trim_efa_index <- which(ok2)
}

kmo_tr <- KMO(mas_trim_efa)
message(sprintf(
  "KMO trimmed: overall=%.3f  item min=%.3f max=%.3f",
  kmo_tr$MSA, min(kmo_tr$MSAi), max(kmo_tr$MSAi)
))

fa_parallel_tr <- fa.parallel(
  mas_trim_efa,
  fm = "minres",
  n.iter = as.integer(PARALLEL_N_ITER),
  quant = 0.95,
  cor = "cor",
  plot = FALSE
)
message(
  "fa.parallel trimmed → factors=", fa_parallel_tr$nfact,
  " components=", fa_parallel_tr$ncomp
)

fit_trim <- fa(
  mas_trim_efa,
  nfactors = as.integer(N_FACTORS),
  rotate = ROTATION,
  fm = "minres"
)
message(
  "Trimmed FA: TLI=", round(fit_trim$TLI, 3),
  " RMSEA=", round(fit_trim$RMSEA[1], 4)
)
print(fit_trim, sort = TRUE, cut = MIN_LOADING_CUT, digits = 3)

# Scores for all rows (NA where items incomplete)
scores_obj <- psych::factor.scores(mas_trim, fit_trim, method = "Thurstone")
scores_trim <- as.matrix(scores_obj$scores)

# Label factors by |loading| on theory bundles (among retained items)
L <- as.matrix(unclass(fit_trim$loadings))
add_pool <- intersect(
  unique(c(FACET_CONFLICT, FACET_RELAPSE, FACET_WITHDRAWAL, FACET_PROBLEMS)),
  rownames(L)
)
eng_pool <- intersect(
  unique(c(FACET_SALIENCE, FACET_MOOD, FACET_TOLERANCE)),
  rownames(L)
)
mean_abs <- function(items, j) mean(abs(L[items, j]), na.rm = TRUE)
diff_add_eng <- c(
  mean_abs(add_pool, 1) - mean_abs(eng_pool, 1),
  mean_abs(add_pool, 2) - mean_abs(eng_pool, 2)
)
idx_add <- which.max(diff_add_eng)
idx_eng <- if (idx_add == 1L) 2L else 1L
message(sprintf(
  "Factor labels: col%d → MAS_F_addictive, col%d → MAS_F_engagement",
  idx_add, idx_eng
))

# Raw columns (rotation order) + labeled
v$MAS_F_MR1 <- scores_trim[, 1]
v$MAS_F_MR2 <- scores_trim[, 2]
v$MAS_F_addictive  <- scores_trim[, idx_add]
v$MAS_F_engagement <- scores_trim[, idx_eng]
# Legacy names: DO NOT assume F1=addictive in old paper — store labeled only in F*
# Keep F1/F2 as labeled for downstream compare_means that expect F1/F2,
# but write mapping to log.
v$F1 <- v$MAS_F_addictive
v$F2 <- v$MAS_F_engagement
attr(v, "MAS_F_map") <- list(
  F1 = "MAS_F_addictive",
  F2 = "MAS_F_engagement",
  addictive_column = idx_add,
  engagement_column = idx_eng
)

# ----- HUMS ------------------------------------------------------------------
hums_u <- sprintf("hums%02d", 1:8)
hums_h <- sprintf("hums%02d", 9:13)
if (all(hums_u %in% names(v))) {
  v$HUMS_unhealthy <- row_mean(v, hums_u)
}
if (all(hums_h %in% names(v))) {
  v$HUMS_healthy <- row_mean(v, hums_h)
}

# ----- Full theory composites (never clobbered by trim) ----------------------
v$MAS_Salience         <- row_mean(v, FACET_SALIENCE)
v$MAS_Moodmodification <- row_mean(v, FACET_MOOD)
v$MAS_Tolerance        <- row_mean(v, FACET_TOLERANCE)
v$MAS_Conflict         <- row_mean(v, FACET_CONFLICT)
v$MAS_Relapse          <- row_mean(v, FACET_RELAPSE)
v$MAS_Withdrawal       <- row_mean(v, FACET_WITHDRAWAL)
v$MAS_Problems         <- row_mean(v, FACET_PROBLEMS)

v$MAS_Addictivecore <- (
  v$MAS_Conflict + v$MAS_Relapse + v$MAS_Withdrawal + v$MAS_Problems
) / 4
v$MAS_Engagementcore <- (
  v$MAS_Salience + v$MAS_Moodmodification + v$MAS_Tolerance
) / 3

# ----- Trimmed composites ----------------------------------------------------
v$trimmedMAS_Salience         <- row_mean(v, TRIM_SALIENCE)
v$trimmedMAS_Moodmodification <- row_mean(v, TRIM_MOOD)
v$trimmedMAS_Tolerance        <- row_mean(v, TRIM_TOLERANCE)
v$trimmedMAS_Conflict         <- row_mean(v, TRIM_CONFLICT)
v$trimmedMAS_Relapse          <- row_mean(v, TRIM_RELAPSE)
v$trimmedMAS_Withdrawal       <- row_mean(v, TRIM_WITHDRAWAL)
v$trimmedMAS_Problems         <- row_mean(v, TRIM_PROBLEMS)

v$trimmedMAS_Addictivecore <- (
  v$trimmedMAS_Conflict + v$trimmedMAS_Relapse +
    v$trimmedMAS_Withdrawal + v$trimmedMAS_Problems
) / 4

if (identical(ENGAGEMENT_TRIM_MODE, "legacy_2021")) {
  # 2021 script: (trimmed Salience + FULL Moodmodification) / 2 — no Tolerance
  v$trimmedMAS_Engagementcore <- (
    v$trimmedMAS_Salience + v$MAS_Moodmodification
  ) / 2
  message("Engagement trim mode: legacy_2021 (Salience_trim + Mood_FULL)/2")
} else if (identical(ENGAGEMENT_TRIM_MODE, "consistent")) {
  v$trimmedMAS_Engagementcore <- (
    v$trimmedMAS_Salience + v$trimmedMAS_Moodmodification +
      v$trimmedMAS_Tolerance
  ) / 3
  message("Engagement trim mode: consistent (3 trimmed facets)/3")
} else {
  stop("Unknown ENGAGEMENT_TRIM_MODE: ", ENGAGEMENT_TRIM_MODE)
}

# Also compute alternate for sensitivity (always available)
v$trimmedMAS_Engagementcore_consistent <- (
  v$trimmedMAS_Salience + v$trimmedMAS_Moodmodification +
    v$trimmedMAS_Tolerance
) / 3
v$trimmedMAS_Engagementcore_legacy2021 <- (
  v$trimmedMAS_Salience + v$MAS_Moodmodification
) / 2

# ----- Correlations ----------------------------------------------------------
cor_vars <- c(
  "MAS_F_addictive", "MAS_F_engagement",
  "trimmedMAS_Addictivecore", "trimmedMAS_Engagementcore",
  "HUMS_healthy", "HUMS_unhealthy", "MSI.AE", "MSI.MT"
)
cor_vars <- cor_vars[cor_vars %in% names(v)]
cm <- stats::cor(v[, cor_vars, drop = FALSE], use = "pairwise.complete.obs")
print(round(cm, 2))

utils::write.csv(
  round(cm, 3),
  file.path(OUT_DIR, "cor_mas_factors_external.csv")
)

load_df <- as.data.frame(unclass(fit_trim$loadings))
load_df$item <- rownames(load_df)
utils::write.csv(
  load_df,
  file.path(OUT_DIR, "efa_trimmed_loadings.csv"),
  row.names = FALSE
)

# Fit / options log
fit_log <- data.frame(
  key = c(
    "N_full_listwise", "N_trim_listwise",
    "KMO_full", "KMO_trim",
    "TLI_full", "TLI_trim",
    "RMSEA_full", "RMSEA_trim",
    "parallel_nfact_full", "parallel_nfact_trim",
    "N_FACTORS", "ROTATION", "USE_ROUGHFIX",
    "ENGAGEMENT_TRIM_MODE", "COMPOSITE_NA_RM",
    "addictive_loading_col", "engagement_loading_col"
  ),
  value = c(
    nrow(mas_items_efa), nrow(mas_trim_efa),
    round(kmo_full$MSA, 4), round(kmo_tr$MSA, 4),
    round(fit_full$TLI, 4), round(fit_trim$TLI, 4),
    round(fit_full$RMSEA[1], 4), round(fit_trim$RMSEA[1], 4),
    fa_parallel_full$nfact, fa_parallel_tr$nfact,
    N_FACTORS, ROTATION, USE_ROUGHFIX,
    ENGAGEMENT_TRIM_MODE, COMPOSITE_NA_RM,
    idx_add, idx_eng
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(fit_log, file.path(OUT_DIR, "efa_fit_log.csv"), row.names = FALSE)

# Engagement mode sensitivity
if (all(c(
  "trimmedMAS_Engagementcore_legacy2021",
  "trimmedMAS_Engagementcore_consistent"
) %in% names(v))) {
  eng_cmp <- stats::cor(
    v$trimmedMAS_Engagementcore_legacy2021,
    v$trimmedMAS_Engagementcore_consistent,
    use = "pairwise.complete.obs"
  )
  message(sprintf(
    "Engagement core legacy vs consistent: r=%.3f  mean|diff|=%.4f",
    eng_cmp,
    mean(abs(
      v$trimmedMAS_Engagementcore_legacy2021 -
        v$trimmedMAS_Engagementcore_consistent
    ), na.rm = TRUE)
  ))
}

grDevices::pdf(file.path(OUT_DIR, "corrplot_mas_clean.pdf"), height = 9, width = 9)
gr <- grDevices::colorRampPalette(c("#B52127", "white", "#2171B5"))
psych::cor.plot(
  cm, stars = TRUE, upper = FALSE, diag = FALSE,
  show.legend = FALSE, gr = gr
)
grDevices::dev.off()

# session
writeLines(
  c(
    capture.output(sessionInfo()),
    "",
    paste("MAS_F_map: F1=addictive col", idx_add, "; F2=engagement col", idx_eng)
  ),
  file.path(OUT_DIR, "sessionInfo.txt")
)

message("=== done. Outputs in: ", OUT_DIR, " ===")
message("2021 historical factor_analysis.R was not modified.")
