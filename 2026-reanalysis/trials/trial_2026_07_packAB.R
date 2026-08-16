# 2026-07 MAS improvement trial — Pack A + B
#
# RECORD, not a runnable entry point. This script reads the R workspace that
# the 2021 pipeline left behind, which holds participant data and is therefore
# not distributed. Point MAS_WORKSPACE at a local .RData to re-run it.
# The runnable path is 2026-reanalysis/RUN_ME.R.

suppressPackageStartupMessages({
  library(psych)
  library(GPArotation)
})

if (!exists("MAS_REPO")) {
  source(file.path(getwd(), "2026-reanalysis", "scr", "paths.R"))
}
outdir <- file.path(OUT_DIR, "trials")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

workspace <- Sys.getenv("MAS_WORKSPACE", unset = "")
if (!nzchar(workspace) || !file.exists(workspace)) {
  stop(
    "This trial needs the 2021 workspace, which is not in the repository ",
    "because it contains participant data.\n",
    'Set Sys.setenv(MAS_WORKSPACE = "/path/to/.RData") to re-run it.'
  )
}
load(workspace)

# ---- Gate: item sets (Table 5 EFA membership; audit-locked) ----
items20 <- c(
  "mas01","mas04","mas05","mas06","mas07","mas08","mas09",
  "mas13","mas14","mas16","mas17","mas18","mas19","mas20",
  "mas21","mas23","mas24","mas25","mas26","mas28"
)
add10 <- c("mas13","mas14","mas16","mas17","mas18","mas19","mas20","mas25","mas26","mas28")
eng10 <- c("mas01","mas04","mas05","mas06","mas07","mas08","mas09","mas21","mas23","mas24")
focus <- c("mas21","mas23","mas24")
removed8 <- c("mas02","mas03","mas10","mas11","mas12","mas15","mas22","mas27")

stopifnot(length(items20) == 20, length(add10) == 10, length(eng10) == 10)
stopifnot(setequal(items20, c(add10, eng10)))
stopifnot(all(items20 %in% names(v)))

# Compare to objects saved in original session if present
gate <- list()
gate$v_dim <- dim(v)
gate$items20_in_v <- all(items20 %in% names(v))
if (exists("df_normed_t")) {
  gate$df_normed_t_dim <- dim(df_normed_t)
  gate$df_normed_t_names <- colnames(df_normed_t)
  gate$df_normed_t_matches_items20 <- setequal(colnames(df_normed_t), items20)
}
if (exists("crossloaded")) {
  gate$crossloaded_saved <- as.character(crossloaded)
  gate$removed8_match <- setequal(as.character(crossloaded), removed8)
}

# ---- Raw data matrix for alpha / optional FA ----
raw20 <- v[, items20]
n_raw_full <- nrow(raw20)
ok <- stats::complete.cases(raw20)
raw20c <- raw20[ok, , drop = FALSE]
n_complete <- nrow(raw20c)

# ---- Pack A preprocessing variants ----
# A1: original pipeline objects if available (na.roughfix + z) — primary for Φ comparability
# A2: complete cases + z on raw (sensitivity)

scale_this <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

if (exists("df_normed_t") && setequal(colnames(df_normed_t), items20)) {
  X_orig <- as.data.frame(df_normed_t)[, items20, drop = FALSE]
  prep_primary <- "df_normed_t from .RData (original na.roughfix + z-score; N=nrow)"
} else {
  # rebuild roughfix+z like factor_analysis.R
  df_t <- v[, items20]
  if (anyNA(df_t)) {
    if (!requireNamespace("randomForest", quietly = TRUE)) {
      stop("randomForest needed for na.roughfix rebuild")
    }
    df_t <- randomForest::na.roughfix(df_t)
  }
  X_orig <- as.data.frame(lapply(df_t, scale_this))
  prep_primary <- "rebuilt na.roughfix + z on items20"
}

X_cc <- as.data.frame(lapply(raw20c, scale_this))
prep_cc <- "complete.cases then z-score"

run_fa_pair <- function(X, label) {
  fa_ortho <- psych::fa(X, nfactors = 2, rotate = "varimax", fm = "minres")
  fa_obl   <- psych::fa(X, nfactors = 2, rotate = "oblimin", fm = "minres")
  L_o <- as.matrix(unclass(fa_ortho$loadings))
  L_b <- as.matrix(unclass(fa_obl$loadings))
  # cross-loadings: |loading|>=0.30 on BOTH factors
  cross_count <- function(L, cut = 0.30) {
    sum(abs(L[, 1]) >= cut & abs(L[, 2]) >= cut)
  }
  # factor score correlation (orthogonal)
  sc <- tryCatch(stats::cor(fa_ortho$scores, use = "pairwise"), error = function(e) matrix(NA, 2, 2))
  list(
    label = label,
    fa_ortho = fa_ortho,
    fa_obl = fa_obl,
    Phi = fa_obl$Phi,
    score_cor_varimax = sc,
    cross_ortho = cross_count(L_o),
    cross_obl = cross_count(L_b),
    L_ortho = L_o,
    L_obl = L_b,
    Vaccounted = fa_obl$Vaccounted
  )
}

res_primary <- run_fa_pair(X_orig, prep_primary)
res_cc <- run_fa_pair(X_cc, prep_cc)

# Align factor signs/order: MR1/MR2 naming may swap; report Phi as abs off-diag too
phi_primary <- res_primary$Phi
phi_off <- if (!is.null(phi_primary) && length(phi_primary) >= 4) phi_primary[1, 2] else NA_real_

# Focus item loadings
focus_load <- function(L, items) {
  rn <- rownames(L)
  m <- match(items, rn)
  out <- L[m, , drop = FALSE]
  rownames(out) <- items
  round(out, 3)
}

# Sorted loading tables as text
capture_load <- function(fa_obj, cut = 0.30) {
  paste(capture.output(print(fa_obj$loadings, cutoff = cut, sort = TRUE)), collapse = "\n")
}

# ---- Pack B ----
a_whole <- psych::alpha(raw20c, warnings = FALSE, check.keys = FALSE)
a_add   <- psych::alpha(raw20c[, add10, drop = FALSE], warnings = FALSE, check.keys = FALSE)
a_eng   <- psych::alpha(raw20c[, eng10, drop = FALSE], warnings = FALSE, check.keys = FALSE)

# omega can be chatty / plotty
ow <- NULL
ow_err <- NULL
tryCatch({
  ow <- psych::omega(raw20c, nfactors = 2, plot = FALSE)
}, error = function(e) ow_err <<- conditionMessage(e))

add_sum <- rowMeans(raw20c[, add10, drop = FALSE])
eng_sum <- rowMeans(raw20c[, eng10, drop = FALSE])
core_r <- stats::cor(add_sum, eng_sum)

# Also report original trimmed composites if present
trim_r <- NA_real_
if (all(c("trimmedMAS_Addictivecore", "trimmedMAS_Engagementcore") %in% names(v))) {
  trim_r <- stats::cor(v$trimmedMAS_Addictivecore, v$trimmedMAS_Engagementcore,
                       use = "pairwise.complete.obs")
}

# ---- Write RDS ----
saveRDS(list(
  date = Sys.time(),
  gate = gate,
  items20 = items20,
  add10 = add10,
  eng10 = eng10,
  n_raw_full = n_raw_full,
  n_complete = n_complete,
  prep_primary = prep_primary,
  res_primary = res_primary,
  res_cc = res_cc,
  alpha_whole = a_whole$total,
  alpha_add = a_add$total,
  alpha_eng = a_eng$total,
  omega = if (!is.null(ow)) list(
    omega_tot = ow$omega.tot,
    omega_h = ow$omega_h,
    alpha = ow$alpha,
    schmid = if (!is.null(ow$schmid)) ow$schmid else NULL
  ) else NULL,
  omega_error = ow_err,
  core_r_efa10 = core_r,
  core_r_trimmed_formula = trim_r
), file.path(outdir, "packAB_raw.rds"))

# ---- Human-readable report ----
fmt_phi <- function(Phi) {
  if (is.null(Phi)) return("NULL")
  paste(capture.output(print(round(Phi, 4))), collapse = "\n")
}
fmt_mat <- function(M) paste(capture.output(print(round(M, 3))), collapse = "\n")

report <- c(
  "# 2026-07 MAS improvement trial — RESULTS",
  "",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
  paste0("Project: ", proj),
  "",
  "## Gate",
  paste0("- v dim: ", paste(gate$v_dim, collapse = " x ")),
  paste0("- items20 all in v: ", gate$items20_in_v),
  if (!is.null(gate$df_normed_t_dim)) paste0("- df_normed_t dim: ", paste(gate$df_normed_t_dim, collapse = " x ")),
  if (!is.null(gate$df_normed_t_matches_items20)) paste0("- df_normed_t names == items20: ", gate$df_normed_t_matches_items20),
  if (!is.null(gate$removed8_match)) paste0("- removed8 matches .RData crossloaded: ", gate$removed8_match),
  paste0("- N complete cases (raw 20): ", n_complete, " / ", n_raw_full),
  paste0("- add10: ", paste(add10, collapse = ", ")),
  paste0("- eng10: ", paste(eng10, collapse = ", ")),
  "",
  "## Pack A — primary prep: ", prep_primary,
  "",
  "### Φ (oblimin) — PRIMARY",
  "```",
  fmt_phi(res_primary$Phi),
  "```",
  paste0("- Φ off-diagonal (MR1,MR2): **", round(phi_off, 4), "**"),
  paste0("- varimax factor-score cor (off): **", round(res_primary$score_cor_varimax[1, 2], 4), "**"),
  paste0("- cross-loadings |λ|≥.30 both factors: varimax=", res_primary$cross_ortho, ", oblimin=", res_primary$cross_obl),
  "",
  "### Focus items (Withdrawal mas21/23/24)",
  "oblimin:",
  "```",
  fmt_mat(focus_load(res_primary$L_obl, focus)),
  "```",
  "varimax:",
  "```",
  fmt_mat(focus_load(res_primary$L_ortho, focus)),
  "```",
  "",
  "### Loadings oblimin (cutoff .30, sorted)",
  "```",
  capture_load(res_primary$fa_obl),
  "```",
  "",
  "### Loadings varimax (cutoff .30, sorted)",
  "```",
  capture_load(res_primary$fa_ortho),
  "```",
  "",
  "## Pack A — sensitivity: ", prep_cc,
  "```",
  fmt_phi(res_cc$Phi),
  "```",
  paste0("- Φ off: ", round(res_cc$Phi[1, 2], 4)),
  paste0("- varimax score cor off: ", round(res_cc$score_cor_varimax[1, 2], 4)),
  paste0("- cross ortho/obl: ", res_cc$cross_ortho, " / ", res_cc$cross_obl),
  "",
  "## Pack B — reliability (complete cases N=", n_complete, ")",
  paste0("- whole20 raw_alpha: **", round(a_whole$total$raw_alpha, 4), "**  (std.alpha=", round(a_whole$total$std.alpha, 4), ")"),
  paste0("- add10 raw_alpha: **", round(a_add$total$raw_alpha, 4), "**  (std.alpha=", round(a_add$total$std.alpha, 4), ")"),
  paste0("- eng10 raw_alpha: **", round(a_eng$total$raw_alpha, 4), "**  (std.alpha=", round(a_eng$total$std.alpha, 4), ")"),
  if (!is.null(ow)) paste0("- omega.tot (nfactors=2): **", round(ow$omega.tot, 4), "**  omega_h=", round(ow$omega_h, 4)),
  if (!is.null(ow_err)) paste0("- omega ERROR: ", ow_err),
  paste0("- core composite r (EFA add10 vs eng10 means): **", round(core_r, 4), "**"),
  paste0("- trimmedMAS_* formula r (if in v): ", if (is.na(trim_r)) "NA" else round(trim_r, 4)),
  "",
  "## Interpretation (auto draft; human polish later)",
  paste0("- Thesis reported F1-F2 score r≈0.06 under varimax; this run varimax score r=", round(res_primary$score_cor_varimax[1, 2], 4), "."),
  paste0("- Oblimin Φ=", round(phi_off, 4), " vs composite core r (EFA10)=", round(core_r, 4), " vs thesis trmd=.58 / full=.74."),
  paste0("- Alphas: whole=", round(a_whole$total$raw_alpha, 3),
         " add=", round(a_add$total$raw_alpha, 3),
         " eng=", round(a_eng$total$raw_alpha, 3), "."),
  "- Cap: high subscale α + high core r => substantial overlap / weak separation — NOT automatic 'same construct' without CFA/external validity.",
  ""
)

report_path <- file.path(outdir, "RESULTS.md")
writeLines(report, report_path)

# CSV summaries
phi_df <- data.frame(
  prep = c("primary_orig", "sensitivity_cc"),
  phi_12 = c(res_primary$Phi[1, 2], res_cc$Phi[1, 2]),
  varimax_score_r = c(res_primary$score_cor_varimax[1, 2], res_cc$score_cor_varimax[1, 2]),
  cross_varimax = c(res_primary$cross_ortho, res_cc$cross_ortho),
  cross_oblimin = c(res_primary$cross_obl, res_cc$cross_obl),
  N = c(nrow(X_orig), nrow(X_cc))
)
write.csv(phi_df, file.path(outdir, "packA_phi_summary.csv"), row.names = FALSE)

alpha_df <- data.frame(
  scale = c("whole20", "add10", "eng10"),
  n_items = c(20, 10, 10),
  N = n_complete,
  raw_alpha = c(a_whole$total$raw_alpha, a_add$total$raw_alpha, a_eng$total$raw_alpha),
  std_alpha = c(a_whole$total$std.alpha, a_add$total$std.alpha, a_eng$total$std.alpha),
  average_r = c(a_whole$total$average_r, a_add$total$average_r, a_eng$total$average_r)
)
write.csv(alpha_df, file.path(outdir, "packB_alpha_summary.csv"), row.names = FALSE)

# loadings csv
write.csv(round(res_primary$L_obl, 4), file.path(outdir, "packA_loadings_oblimin_primary.csv"))
write.csv(round(res_primary$L_ortho, 4), file.path(outdir, "packA_loadings_varimax_primary.csv"))

cat("=== DONE ===\n")
cat("Phi primary:", round(phi_off, 4), "\n")
cat("varimax score r:", round(res_primary$score_cor_varimax[1, 2], 4), "\n")
cat("alpha whole/add/eng:", round(c(a_whole$total$raw_alpha, a_add$total$raw_alpha, a_eng$total$raw_alpha), 4), "\n")
cat("core r EFA10:", round(core_r, 4), "\n")
if (!is.null(ow)) cat("omega.tot:", round(ow$omega.tot, 4), "\n")
cat("wrote:", report_path, "\n")
