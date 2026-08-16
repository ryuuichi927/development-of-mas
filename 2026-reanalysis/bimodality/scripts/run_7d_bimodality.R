# MAS 7D bimodality axis trial
# 2026-07-28
#
# RECORD, not a runnable entry point. Needs the 2021 workspace, which holds
# participant data and is not distributed. Set MAS_WORKSPACE to re-run.

suppressPackageStartupMessages({
  library(psych)
  library(mclust)
  library(diptest)
  library(jsonlite)
})

if (!exists("MAS_REPO")) {
  source(file.path(getwd(), "2026-reanalysis", "scr", "paths.R"))
}
root <- file.path(REANALYSIS, "bimodality")
outdir <- file.path(root, "results")
figdir <- file.path(root, "figures")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
dir.create(figdir, recursive = TRUE, showWarnings = FALSE)

set.seed(20260728)
workspace <- Sys.getenv("MAS_WORKSPACE", unset = "")
if (!nzchar(workspace) || !file.exists(workspace)) {
  stop('Set Sys.setenv(MAS_WORKSPACE = "/path/to/.RData") to re-run this trial.')
}
load(workspace)
stopifnot(exists("v"))

dims <- c(
  Salience = "MAS_Salience",
  MoodMod = "MAS_Moodmodification",
  Tolerance = "MAS_Tolerance",
  Conflict = "MAS_Conflict",
  Relapse = "MAS_Relapse",
  Withdrawal = "MAS_Withdrawal",
  Problems = "MAS_Problems"
)

# Build dimension scores if missing (match factor_analysis.R mapping)
ensure_dims <- function(v) {
  need <- unname(dims)
  if (all(need %in% names(v))) return(v)
  mk <- function(cols) rowMeans(v[, cols, drop = FALSE], na.rm = TRUE)
  if (!("MAS_Salience" %in% names(v))) v$MAS_Salience <- mk(paste0("mas", sprintf("%02d", 1:4)))
  if (!("MAS_Moodmodification" %in% names(v))) v$MAS_Moodmodification <- mk(paste0("mas", sprintf("%02d", 5:8)))
  if (!("MAS_Tolerance" %in% names(v))) v$MAS_Tolerance <- mk(paste0("mas", sprintf("%02d", 9:12)))
  if (!("MAS_Conflict" %in% names(v))) v$MAS_Conflict <- mk(paste0("mas", sprintf("%02d", 13:16)))
  if (!("MAS_Relapse" %in% names(v))) v$MAS_Relapse <- mk(paste0("mas", sprintf("%02d", 17:20)))
  if (!("MAS_Withdrawal" %in% names(v))) v$MAS_Withdrawal <- mk(paste0("mas", sprintf("%02d", 21:24)))
  if (!("MAS_Problems" %in% names(v))) v$MAS_Problems <- mk(paste0("mas", sprintf("%02d", 25:28)))
  v
}

v <- ensure_dims(v)

Xraw <- as.data.frame(v[, unname(dims), drop = FALSE])
colnames(Xraw) <- names(dims)
ok <- stats::complete.cases(Xraw)
Xraw <- Xraw[ok, , drop = FALSE]
n <- nrow(Xraw)

# z-score space
Xz <- scale(Xraw)
Xz <- matrix(as.numeric(Xz), nrow = n, dimnames = list(NULL, names(dims)))

# baselines
total <- as.numeric(scale(rowMeans(Xz)))
eng <- as.numeric(scale(rowMeans(Xz[, c("Salience", "MoodMod", "Tolerance"), drop = FALSE])))
add <- as.numeric(scale(rowMeans(Xz[, c("Conflict", "Relapse", "Withdrawal", "Problems"), drop = FALSE])))
old_diff <- as.numeric(scale(add - eng))

pca <- prcomp(Xz, center = FALSE, scale. = FALSE)
pc1 <- as.numeric(scale(pca$x[, 1]))
w_pc1 <- as.numeric(pca$rotation[, 1])
names(w_pc1) <- names(dims)

# equal-weight severity direction in z-space
w_total <- rep(1 / sqrt(7), 7)
names(w_total) <- names(dims)

# old-diff direction: +add dims, -eng dims (unit)
w_old <- c(Salience = -1, MoodMod = -1, Tolerance = -1,
           Conflict = 1, Relapse = 1, Withdrawal = 1, Problems = 1)
w_old <- w_old / sqrt(sum(w_old^2))

align_sign <- function(w, ref) {
  if (sum(w * ref) < 0) w <- -w
  w
}

unit <- function(w) {
  nrm <- sqrt(sum(w^2))
  if (!is.finite(nrm) || nrm < 1e-12) return(w)
  w / nrm
}

project <- function(X, w) as.numeric(X %*% w)

# residualize matrix on total (column-wise)
resid_on_total <- function(X, tot = total) {
  apply(X, 2, function(col) {
    fit <- stats::lm(col ~ tot)
    as.numeric(residuals(fit))
  })
}

X_perp <- resid_on_total(Xz, total)
# re-center/scale residual space lightly
X_perp <- scale(X_perp)
X_perp <- matrix(as.numeric(X_perp), nrow = n, dimnames = list(NULL, names(dims)))

# ---- Bimodality helpers ----
bimod_coef <- function(x) {
  x <- x[is.finite(x)]
  n0 <- length(x)
  if (n0 < 10) return(NA_real_)
  m3 <- mean((x - mean(x))^3) / (sd(x)^3)
  # excess kurtosis (psych style: m4/s^4 - 3)
  m4 <- mean((x - mean(x))^4) / (sd(x)^4) - 3
  (m3^2 + 1) / (m4 + 3 * ((n0 - 1)^2) / ((n0 - 2) * (n0 - 3)))
}

dip_stat <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 10) return(c(dip = NA_real_, p = NA_real_))
  d <- diptest::dip.test(x, simulate.p.value = TRUE, B = 500)
  c(dip = unname(d$statistic), p = d$p.value)
}

ashman_D <- function(x, g) {
  # g: 1/2 labels
  x1 <- x[g == 1]; x2 <- x[g == 2]
  if (length(x1) < 5 || length(x2) < 5) return(NA_real_)
  abs(mean(x1) - mean(x2)) / sqrt((var(x1) + var(x2)) / 2)
}

summarize_proj <- function(s, label, g = NULL) {
  ds <- dip_stat(s)
  bc <- bimod_coef(s)
  out <- data.frame(
    label = label,
    n = length(s),
    mean = mean(s),
    sd = sd(s),
    skew = as.numeric(psych::skew(s)),
    kurtosis = as.numeric(psych::kurtosi(s)),
    bimod_coef = bc,
    dip = ds["dip"],
    dip_p = ds["p"],
    stringsAsFactors = FALSE
  )
  if (!is.null(g)) {
    out$ashman_D <- ashman_D(s, g)
    out$n1 <- sum(g == 1)
    out$n2 <- sum(g == 2)
  } else {
    out$ashman_D <- NA_real_
    out$n1 <- NA_integer_
    out$n2 <- NA_integer_
  }
  out
}

# ---- GMM axis ----
gmm_axis <- function(X, modelNames = c("EEE", "EEI", "VVI", "VVV")) {
  fit <- mclust::Mclust(X, G = 2, modelNames = modelNames, verbose = FALSE)
  if (is.null(fit) || is.null(fit$parameters$mean)) stop("GMM failed")
  mu <- fit$parameters$mean
  if (is.null(dim(mu))) {
    # 1D case
    w <- c(1)
  } else {
    w <- as.numeric(mu[, 2] - mu[, 1])
    names(w) <- colnames(X)
  }
  w <- unit(w)
  cl <- fit$classification
  list(fit = fit, w = w, class = cl, mu = mu, model = fit$modelName, bic = fit$bic)
}

gmm_raw <- gmm_axis(Xz)
gmm_perp <- gmm_axis(X_perp)

# align signs to old_diff for readability (positive ~ more addictive-profile)
gmm_raw$w <- align_sign(gmm_raw$w, w_old)
gmm_perp$w <- align_sign(gmm_perp$w, w_old)
w_pc1 <- align_sign(unit(w_pc1), w_old)

s_gmm_raw <- project(Xz, gmm_raw$w)
s_gmm_perp <- project(X_perp, gmm_perp$w)
s_pc1 <- project(Xz, w_pc1)
s_old <- project(Xz, w_old)
s_total <- project(Xz, w_total)

# ---- Projection pursuit (random directions) on perp space ----
rand_pp <- function(X, B = 400) {
  p <- ncol(X)
  best <- list(bc = -Inf, dip = Inf, w = NULL, s = NULL)
  records <- vector("list", B + p + 3)
  k <- 0
  # random
  for (b in seq_len(B)) {
    w <- rnorm(p)
    w <- unit(w)
    s <- project(X, w)
    bc <- bimod_coef(s)
    dp <- dip_stat(s)["dip"]
    k <- k + 1
    records[[k]] <- c(w, bc = bc, dip = dp)
    if (is.finite(bc) && bc > best$bc) best <- list(bc = bc, dip = dp, w = w, s = s, how = "max_bc")
  }
  # also track min dip separately
  best_dip <- list(dip = Inf, bc = NA, w = NULL, s = NULL)
  for (b in seq_len(B)) {
    w <- rnorm(p); w <- unit(w); s <- project(X, w)
    dp <- as.numeric(dip_stat(s)["dip"]); bc <- bimod_coef(s)
    if (is.finite(dp) && dp < best_dip$dip) best_dip <- list(dip = dp, bc = bc, w = w, s = s, how = "min_dip")
  }
  # axes: coordinates
  for (j in seq_len(p)) {
    w <- rep(0, p); w[j] <- 1; names(w) <- colnames(X)
    s <- project(X, w)
    bc <- bimod_coef(s); dp <- as.numeric(dip_stat(s)["dip"])
    if (is.finite(bc) && bc > best$bc) best <- list(bc = bc, dip = dp, w = w, s = s, how = paste0("coord_", colnames(X)[j]))
    if (is.finite(dp) && dp < best_dip$dip) best_dip <- list(dip = dp, bc = bc, w = w, s = s, how = paste0("coord_", colnames(X)[j]))
  }
  # include GMM perp and old residualized direction
  list(best_bc = best, best_dip = best_dip)
}

pp <- rand_pp(X_perp, B = 400)
if (!is.null(pp$best_bc$w)) pp$best_bc$w <- align_sign(unit(setNames(as.numeric(pp$best_bc$w), names(dims))), w_old)
if (!is.null(pp$best_dip$w)) pp$best_dip$w <- align_sign(unit(setNames(as.numeric(pp$best_dip$w), names(dims))), w_old)

s_pp_bc <- if (!is.null(pp$best_bc$w)) project(X_perp, pp$best_bc$w) else rep(NA, n)
s_pp_dip <- if (!is.null(pp$best_dip$w)) project(X_perp, pp$best_dip$w) else rep(NA, n)

# ---- Summaries ----
sum_tab <- rbind(
  summarize_proj(s_total, "total", NULL),
  summarize_proj(s_pc1, "pc1", NULL),
  summarize_proj(s_old, "old_diff_add_minus_eng", NULL),
  summarize_proj(s_gmm_raw, "gmm_raw", gmm_raw$class),
  summarize_proj(s_gmm_perp, "gmm_perp_total", gmm_perp$class),
  summarize_proj(s_pp_bc, "pp_max_bc_perp", NULL),
  summarize_proj(s_pp_dip, "pp_min_dip_perp", NULL)
)

# angles / cosines between directions (in original z naming; perp w lives in residual coords ~ same labels)
cos_w <- function(a, b) sum(unit(a) * unit(b))
w_table <- rbind(
  total = w_total,
  pc1 = w_pc1,
  old_diff = w_old,
  gmm_raw = gmm_raw$w,
  gmm_perp = gmm_perp$w,
  pp_bc = if (!is.null(pp$best_bc$w)) pp$best_bc$w else rep(NA, 7),
  pp_dip = if (!is.null(pp$best_dip$w)) pp$best_dip$w else rep(NA, 7)
)
colnames(w_table) <- names(dims)

cos_mat <- matrix(NA_real_, nrow(w_table), nrow(w_table),
                  dimnames = list(rownames(w_table), rownames(w_table)))
for (i in seq_len(nrow(w_table))) {
  for (j in seq_len(nrow(w_table))) {
    wi <- w_table[i, ]; wj <- w_table[j, ]
    if (anyNA(wi) || anyNA(wj)) next
    cos_mat[i, j] <- cos_w(wi, wj)
  }
}

# correlations among person scores
score_df <- data.frame(
  total = s_total, pc1 = s_pc1, old_diff = s_old,
  gmm_raw = s_gmm_raw, gmm_perp = s_gmm_perp,
  pp_bc = s_pp_bc, pp_dip = s_pp_dip
)
score_cor <- cor(score_df, use = "pairwise")

# external
ext_names <- c("HUMS_healthy", "HUMS_unhealthy", "MSI.AE", "MSI.MT",
               "trimmedMAS_Addictivecore", "trimmedMAS_Engagementcore",
               "MAS_Addictivecore", "MAS_Engagementcore", "F1", "F2")
ext_present <- ext_names[ext_names %in% names(v)]
ext_cor <- NULL
if (length(ext_present)) {
  E <- as.data.frame(v[ok, ext_present, drop = FALSE])
  ext_cor <- cor(cbind(score_df, E), use = "pairwise")[colnames(score_df), ext_present, drop = FALSE]
}

# ---- Bootstrap stability of gmm_perp w ----
Bboot <- 200
w_boot <- matrix(NA_real_, Bboot, 7)
colnames(w_boot) <- names(dims)
ok_boot <- 0L
for (b in seq_len(Bboot)) {
  idx <- sample.int(n, replace = TRUE)
  Xb <- Xz[idx, , drop = FALSE]
  tb <- as.numeric(scale(rowMeans(Xb)))
  Xp <- resid_on_total(Xb, tb)
  Xp <- scale(Xp)
  Xp <- matrix(as.numeric(Xp), nrow = nrow(Xp), dimnames = list(NULL, names(dims)))
  fitb <- tryCatch(gmm_axis(Xp), error = function(e) NULL)
  if (is.null(fitb)) next
  wb <- align_sign(fitb$w, gmm_perp$w)
  w_boot[b, ] <- wb
  ok_boot <- ok_boot + 1L
}
w_boot_ok <- w_boot[stats::complete.cases(w_boot), , drop = FALSE]
boot_mean <- colMeans(w_boot_ok)
boot_sd <- apply(w_boot_ok, 2, sd)
sign_agree <- colMeans(sign(w_boot_ok) == sign(gmm_perp$w[colnames(w_boot_ok)]))
# correlation of each boot w with original
boot_cos <- apply(w_boot_ok, 1, function(w) cos_w(w, gmm_perp$w))

# classification sizes
cls_raw <- table(gmm_raw$class)
cls_perp <- table(gmm_perp$class)

# ---- Write artifacts ----
write.csv(sum_tab, file.path(outdir, "projection_summaries.csv"), row.names = FALSE)
write.csv(w_table, file.path(outdir, "axes_weights.csv"), row.names = TRUE)
write.csv(cos_mat, file.path(outdir, "axes_cosine.csv"), row.names = TRUE)
write.csv(score_cor, file.path(outdir, "score_correlations.csv"), row.names = TRUE)
if (!is.null(ext_cor)) write.csv(ext_cor, file.path(outdir, "external_correlations.csv"), row.names = TRUE)

person_out <- data.frame(
  row_id = which(ok),
  Xraw,
  total = s_total,
  pc1 = s_pc1,
  old_diff = s_old,
  gmm_raw = s_gmm_raw,
  gmm_raw_class = gmm_raw$class,
  gmm_perp = s_gmm_perp,
  gmm_perp_class = gmm_perp$class,
  pp_bc = s_pp_bc,
  pp_dip = s_pp_dip
)
write.csv(person_out, file.path(outdir, "person_scores.csv"), row.names = FALSE)

boot_summary <- data.frame(
  dim = names(dims),
  w_gmm_perp = as.numeric(gmm_perp$w),
  boot_mean = as.numeric(boot_mean),
  boot_sd = as.numeric(boot_sd),
  sign_agree = as.numeric(sign_agree)
)
write.csv(boot_summary, file.path(outdir, "bootstrap_w_gmm_perp.csv"), row.names = FALSE)

meta <- list(
  date = "2026-07-28",
  n_complete = n,
  n_v = nrow(v),
  gmm_raw_model = gmm_raw$model,
  gmm_raw_bic = gmm_raw$bic,
  gmm_perp_model = gmm_perp$model,
  gmm_perp_bic = gmm_perp$bic,
  cls_raw = as.list(cls_raw),
  cls_perp = as.list(cls_perp),
  pp_bc_how = pp$best_bc$how,
  pp_bc_value = pp$best_bc$bc,
  pp_dip_how = pp$best_dip$how,
  pp_dip_value = pp$best_dip$dip,
  boot_ok = ok_boot,
  boot_B = Bboot,
  boot_cos_mean = mean(boot_cos),
  boot_cos_sd = sd(boot_cos),
  boot_cos_q10 = unname(quantile(boot_cos, 0.10)),
  cos_gmm_perp_vs_old = cos_mat["gmm_perp", "old_diff"],
  cos_gmm_perp_vs_total = cos_mat["gmm_perp", "total"],
  cos_gmm_perp_vs_pc1 = cos_mat["gmm_perp", "pc1"],
  cos_gmm_raw_vs_old = cos_mat["gmm_raw", "old_diff"],
  cos_gmm_raw_vs_total = cos_mat["gmm_raw", "total"],
  cor_gmm_perp_old = score_cor["gmm_perp", "old_diff"],
  cor_gmm_perp_total = score_cor["gmm_perp", "total"],
  cor_gmm_raw_total = score_cor["gmm_raw", "total"]
)
writeLines(jsonlite::toJSON(meta, auto_unbox = TRUE, pretty = TRUE, digits = 6),
           file.path(outdir, "meta.json"))

# ---- Figures ----
png(file.path(figdir, "densities.png"), width = 1200, height = 900, res = 120)
op <- par(mfrow = c(3, 3), mar = c(4, 4, 3, 1))
plot_den <- function(s, main) {
  plot(stats::density(s, na.rm = TRUE), main = main, xlab = "score", ylab = "density")
  rug(s)
}
plot_den(s_total, "total")
plot_den(s_pc1, "PC1")
plot_den(s_old, "old_diff (add-eng)")
plot_den(s_gmm_raw, "GMM axis (raw z)")
plot_den(s_gmm_perp, "GMM axis (⊥ total) PRIMARY")
plot_den(s_pp_bc, "PP max bimod coef (⊥total)")
plot_den(s_pp_dip, "PP min dip (⊥total)")
# weight bars primary
barplot(gmm_perp$w, las = 2, main = "w GMM ⊥total", ylab = "weight", col = ifelse(gmm_perp$w >= 0, "#B52127", "#2171B5"))
abline(h = 0)
# cosine bar vs baselines
barplot(c(
  old = cos_mat["gmm_perp", "old_diff"],
  total = cos_mat["gmm_perp", "total"],
  pc1 = cos_mat["gmm_perp", "pc1"]
), ylim = c(-1, 1), main = "cos(gmm_perp, baselines)", ylab = "cosine")
abline(h = 0)
par(op)
dev.off()

png(file.path(figdir, "scatter_gmm_perp_vs_old.png"), width = 900, height = 700, res = 120)
plot(s_old, s_gmm_perp, pch = 16, col = adjustcolor(gmm_perp$class + 1, 0.5),
     xlab = "old_diff (add - eng)", ylab = "gmm_perp score",
     main = sprintf("r = %.2f", cor(s_old, s_gmm_perp)))
abline(lm(s_gmm_perp ~ s_old), col = "black", lwd = 2)
dev.off()

png(file.path(figdir, "weights_compare.png"), width = 1100, height = 700, res = 120)
op <- par(mfrow = c(1, 1), mar = c(8, 4, 3, 1))
M <- rbind(gmm_raw = gmm_raw$w, gmm_perp = gmm_perp$w, old = w_old, pc1 = w_pc1)
barplot(M, beside = TRUE, las = 2, legend.text = TRUE,
        args.legend = list(x = "topright", bty = "n", cex = 0.8),
        main = "Axis weights (sign-aligned toward old_diff)",
        ylab = "weight", col = c("#333333", "#B52127", "#888888", "#2171B5"))
abline(h = 0)
par(op)
dev.off()

# ---- Console verdict helpers ----
verdict <- function() {
  c1 <- abs(meta$cos_gmm_perp_vs_total)
  c2 <- abs(meta$cos_gmm_perp_vs_old)
  # After ⊥total, cos with total weights should be ~0 by construction of space,
  # but w is estimated in residual space — compare score cor and old cos
  if (meta$cor_gmm_raw_total > 0.85 && c2 < 0.5) return("A_severity_like")
  if (c2 >= 0.75) return("B_old_core_shadow")
  if (c2 < 0.5 && abs(meta$cor_gmm_perp_old) < 0.5 && meta$boot_cos_mean > 0.7) return("C_oblique_stable")
  if (c2 >= 0.5) return("Bish_mixed_old_core")
  return("mixed_or_weak")
}

sink(file.path(outdir, "console_summary.txt"))
cat("=== MAS 7D bimodality axis ===\n")
cat("N complete:", n, " / v rows:", nrow(v), "\n")
cat("GMM raw model:", gmm_raw$model, "BIC", round(gmm_raw$bic, 1), " class", paste(cls_raw, collapse = "/"), "\n")
cat("GMM perp model:", gmm_perp$model, "BIC", round(gmm_perp$bic, 1), " class", paste(cls_perp, collapse = "/"), "\n")
cat("\nWeights gmm_perp:\n"); print(round(gmm_perp$w, 3))
cat("\nWeights gmm_raw:\n"); print(round(gmm_raw$w, 3))
cat("\nCosine matrix (selected):\n")
print(round(cos_mat[c("gmm_perp", "gmm_raw", "old_diff", "total", "pc1"),
                    c("gmm_perp", "gmm_raw", "old_diff", "total", "pc1")], 3))
cat("\nScore cors:\n")
print(round(score_cor[c("gmm_perp", "gmm_raw", "old_diff", "total", "pc1"),
                      c("gmm_perp", "gmm_raw", "old_diff", "total", "pc1")], 3))
cat("\nProjection summaries:\n")
print(sum_tab)
cat("\nBootstrap cos(w, w0): mean", round(mean(boot_cos), 3),
    "sd", round(sd(boot_cos), 3), "q10", round(quantile(boot_cos, .1), 3), "\n")
cat("Sign agree:\n"); print(round(sign_agree, 3))
if (!is.null(ext_cor)) {
  cat("\nExternal cors (scores x ext):\n")
  print(round(ext_cor, 3))
}
cat("\nAuto-verdict code:", verdict(), "\n")
cat("PP max BC how/value:", pp$best_bc$how, round(pp$best_bc$bc, 3), "\n")
cat("PP min dip how/value:", pp$best_dip$how, round(pp$best_dip$dip, 4), "\n")
sink()

cat("DONE n=", n, " verdict=", verdict(), "\n", sep = "")
cat("results: ", outdir, "\n", sep = "")
