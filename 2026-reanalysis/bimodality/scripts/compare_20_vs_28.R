# Compare MAS 28-item 7D vs trimmed 20-item (7D dims + item-level 20D)
#
# RECORD, not a runnable entry point. Needs the 2021 workspace; set
# MAS_WORKSPACE to re-run. Figure labels use a CJK font when one is available.
suppressPackageStartupMessages({
  library(mclust)
  library(diptest)
  library(showtext)
  library(sysfonts)
})
cjk_font <- Sys.getenv("MAS_CJK_FONT", unset = "/System/Library/Fonts/Hiragino Sans GB.ttc")
if (file.exists(cjk_font)) {
  sysfonts::font_add("cjk", cjk_font)
  showtext::showtext_auto()
  showtext::showtext_opts(dpi = 140)
}

if (!exists("MAS_REPO")) {
  source(file.path(getwd(), "2026-reanalysis", "scr", "paths.R"))
}
root <- file.path(REANALYSIS, "bimodality")
figdir <- file.path(root, "figures")
outdir <- file.path(root, "results")
set.seed(20260728)
workspace <- Sys.getenv("MAS_WORKSPACE", unset = "")
if (!nzchar(workspace) || !file.exists(workspace)) {
  stop('Set Sys.setenv(MAS_WORKSPACE = "/path/to/.RData") to re-run this trial.')
}
load(workspace)

items20 <- c(
  "mas01", "mas04", "mas05", "mas06", "mas07", "mas08", "mas09",
  "mas13", "mas14", "mas16", "mas17", "mas18", "mas19", "mas20",
  "mas21", "mas23", "mas24", "mas25", "mas26", "mas28"
)
add10 <- c("mas13", "mas14", "mas16", "mas17", "mas18", "mas19", "mas20", "mas25", "mas26", "mas28")
eng10 <- c("mas01", "mas04", "mas05", "mas06", "mas07", "mas08", "mas09", "mas21", "mas23", "mas24")
stopifnot(setequal(items20, c(add10, eng10)), all(items20 %in% names(v)))

dim_items20 <- list(
  Salience = c("mas01", "mas04"),
  MoodMod = c("mas05", "mas06", "mas07", "mas08"),
  Tolerance = c("mas09"),
  Conflict = c("mas13", "mas14", "mas16"),
  Relapse = c("mas17", "mas18", "mas19", "mas20"),
  Withdrawal = c("mas21", "mas23", "mas24"),
  Problems = c("mas25", "mas26", "mas28")
)
dim_items28 <- list(
  Salience = paste0("mas", sprintf("%02d", 1:4)),
  MoodMod = paste0("mas", sprintf("%02d", 5:8)),
  Tolerance = paste0("mas", sprintf("%02d", 9:12)),
  Conflict = paste0("mas", sprintf("%02d", 13:16)),
  Relapse = paste0("mas", sprintf("%02d", 17:20)),
  Withdrawal = paste0("mas", sprintf("%02d", 21:24)),
  Problems = paste0("mas", sprintf("%02d", 25:28))
)

unit <- function(w) {
  w <- as.numeric(w)
  nrm <- sqrt(sum(w^2))
  if (!is.finite(nrm) || nrm < 1e-12) w else w / nrm
}
align_sign <- function(w, ref) if (sum(w * ref) < 0) -w else w

build_X <- function(dim_items) {
  M <- sapply(dim_items, function(cols) rowMeans(v[, cols, drop = FALSE], na.rm = TRUE))
  M <- as.data.frame(M)
  ok <- stats::complete.cases(M)
  list(raw = M[ok, , drop = FALSE], ok = ok, n = sum(ok))
}

prep_perp <- function(raw) {
  Xz <- scale(as.matrix(raw))
  colnames(Xz) <- colnames(raw)
  tot <- as.numeric(scale(rowMeans(Xz)))
  Xp <- scale(apply(Xz, 2, function(col) residuals(stats::lm(col ~ tot))))
  colnames(Xp) <- colnames(raw)
  list(Xz = Xz, X = Xp, total = tot)
}

ashman_D <- function(s, g) {
  s1 <- s[g == 1]
  s2 <- s[g == 2]
  if (length(s1) < 5 || length(s2) < 5) return(NA_real_)
  abs(mean(s1) - mean(s2)) / sqrt((var(s1) + var(s2)) / 2)
}
calinski_1d <- function(s, g) {
  k <- 2
  m <- mean(s)
  B <- sum(sapply(split(s, g), function(z) length(z) * (mean(z) - m)^2))
  W <- sum(sapply(split(s, g), function(z) sum((z - mean(z))^2)))
  if (W < 1e-12) return(Inf)
  (B / (k - 1)) / (W / (length(s) - k))
}
labels_2means <- function(s) {
  km <- stats::kmeans(matrix(s, ncol = 1), centers = 2, nstart = 25)
  g <- km$cluster
  if (mean(s[g == 2]) < mean(s[g == 1])) g <- 3L - g
  g
}
sep_dir <- function(X, w) {
  w <- unit(w)
  s <- as.numeric(X %*% w)
  g <- labels_2means(s)
  list(w = w, s = s, g = g, ash = ashman_D(s, g), ch = calinski_1d(s, g), n1 = sum(g == 1), n2 = sum(g == 2))
}
dip_p <- function(s) diptest::dip.test(s, simulate.p.value = TRUE, B = 400)$p.value
bimod_coef <- function(x) {
  x <- x[is.finite(x)]
  n0 <- length(x)
  m3 <- mean((x - mean(x))^3) / (sd(x)^3)
  m4 <- mean((x - mean(x))^4) / (sd(x)^4) - 3
  (m3^2 + 1) / (m4 + 3 * ((n0 - 1)^2) / ((n0 - 2) * (n0 - 3)))
}

refine_ash <- function(X, w0, n_iter = 55, step0 = 0.35) {
  w <- unit(w0)
  cur <- sep_dir(X, w)
  step <- step0
  p <- ncol(X)
  for (it in seq_len(n_iter)) {
    improved <- FALSE
    for (t in 1:12) {
      prop <- unit(w + stats::rnorm(p, sd = step))
      sp <- sep_dir(X, prop)
      if (is.finite(sp$ash) && sp$ash > cur$ash) {
        w <- prop
        cur <- sp
        improved <- TRUE
      }
    }
    if (!improved) step <- step * 0.55
    if (step < 1e-3) break
  }
  cur
}

max_ash_search <- function(X, seeds) {
  best <- list(ash = -Inf)
  p <- ncol(X)
  for (i in 1:450) {
    sp <- sep_dir(X, unit(stats::rnorm(p)))
    if (is.finite(sp$ash) && sp$ash > best$ash) best <- sp
  }
  seed_list <- c(list(best$w), seeds)
  for (j in seq_len(p)) {
    e <- rep(0, p)
    e[j] <- 1
    seed_list[[length(seed_list) + 1]] <- e
  }
  for (w0 in seed_list) {
    sp <- refine_ash(X, w0)
    if (is.finite(sp$ash) && sp$ash > best$ash) best <- sp
  }
  best
}

get_sigmas <- function(fit) {
  out <- vector("list", 2)
  if (!is.null(fit$parameters$variance$sigma)) {
    for (k in 1:2) out[[k]] <- fit$parameters$variance$sigma[, , k]
  } else {
    for (k in 1:2) {
      sh <- fit$parameters$variance$shape
      shk <- if (is.matrix(sh)) sh[, k] else sh
      sc <- fit$parameters$variance$scale
      sck <- if (length(sc) > 1) sc[k] else sc
      out[[k]] <- diag(as.numeric(sck * shk), fit$d)
    }
  }
  out
}

ellipse_pts <- function(mean2, sigma2, level = 0.95, n = 200) {
  r <- sqrt(stats::qchisq(level, 2))
  eig <- eigen(sigma2)
  vals <- pmax(eig$values, 1e-8)
  tt <- seq(0, 2 * pi, length.out = n)
  circ <- rbind(cos(tt), sin(tt))
  A <- eig$vectors %*% diag(sqrt(vals)) * r
  sweep(t(A %*% circ), 2, as.numeric(mean2), "+")
}

run_suite <- function(tag, dim_items) {
  b <- build_X(dim_items)
  pr <- prep_perp(b$raw)
  X <- pr$X
  p <- ncol(X)
  dn <- colnames(X)

  eng_d <- intersect(c("Salience", "MoodMod", "Tolerance"), dn)
  add_d <- intersect(c("Conflict", "Relapse", "Withdrawal", "Problems"), dn)
  w_old <- rep(0, p)
  names(w_old) <- dn
  w_old[eng_d] <- -1
  w_old[add_d] <- 1
  w_old <- unit(w_old)

  fit <- mclust::Mclust(X, G = 2, modelNames = c("EEE", "EEI", "VVI", "VVV"), verbose = FALSE)
  m1 <- fit$parameters$mean[, 1]
  m2 <- fit$parameters$mean[, 2]
  w_gmm <- unit(m2 - m1)
  names(w_gmm) <- dn
  w_gmm <- align_sign(w_gmm, w_old)
  if (sum((m2 - m1) * w_gmm) < 0) {
    tmp <- m1
    m1 <- m2
    m2 <- tmp
    cl <- 3L - fit$classification
    Sig <- get_sigmas(fit)[c(2, 1)]
  } else {
    cl <- fit$classification
    Sig <- get_sigmas(fit)
  }
  mu_use <- cbind(m1, m2)
  w_gmm <- unit(m2 - m1)
  names(w_gmm) <- dn

  sp_gmm <- sep_dir(X, w_gmm)
  sp_old <- sep_dir(X, w_old)
  s_gmm <- as.numeric(X %*% w_gmm)
  s_old <- as.numeric(X %*% w_old)

  best <- max_ash_search(X, list(w_gmm, w_old, unit(prcomp(X, center = FALSE, scale. = FALSE)$rotation[, 1])))
  w_max <- align_sign(best$w, w_old)
  names(w_max) <- dn
  sp_max <- sep_dir(X, w_max)

  # orthogonal y for gmm plane
  X_res <- X - (X %*% w_gmm) %*% t(w_gmm)
  w_y <- unit(prcomp(X_res, center = FALSE, scale. = FALSE)$rotation[, 1])
  wo_r <- w_old - sum(w_old * w_gmm) * w_gmm
  if (sqrt(sum(wo_r^2)) > 1e-8) {
    wo_r <- unit(wo_r)
    if (sum(w_y * wo_r) < 0) w_y <- -w_y
  } else {
    wo_r <- w_y
  }
  Q_old <- cbind(w_gmm, wo_r)

  X_res2 <- X - (X %*% w_max) %*% t(w_max)
  w_y2 <- unit(prcomp(X_res2, center = FALSE, scale. = FALSE)$rotation[, 1])
  Q_max <- cbind(w_max, w_y2)

  plot_plane <- function(Q, gcol, fname, main, xlab, ylab) {
    Y <- X %*% Q
    mu2 <- t(Q) %*% mu_use
    sig2 <- lapply(1:2, function(k) t(Q) %*% Sig[[k]] %*% Q)
    e1 <- ellipse_pts(mu2[, 1], sig2[[1]])
    e2 <- ellipse_pts(mu2[, 2], sig2[[2]])
    e1b <- ellipse_pts(mu2[, 1], sig2[[1]], 0.5)
    e2b <- ellipse_pts(mu2[, 2], sig2[[2]], 0.5)
    cols <- c("#2171B5", "#B52127")
    s1 <- Y[, 1]
    dp <- dip_p(s1)
    ash <- ashman_D(s1, gcol)
    ch <- calinski_1d(s1, gcol)
    xr <- range(c(Y[, 1], e1[, 1], e2[, 1]))
    yr <- range(c(Y[, 2], e1[, 2], e2[, 2]))
    xr <- xr + diff(xr) * c(-0.08, 0.08)
    yr <- yr + diff(yr) * c(-0.08, 0.08)

    png(file.path(figdir, fname), width = 1400, height = 1200, res = 140)
    par(mar = c(5.6, 5.1, 4.3, 1.4), family = "cjk")
    plot(NA, xlim = xr, ylim = yr, asp = 1, xlab = xlab, ylab = ylab, main = main, cex.lab = 1.1, cex.main = 1.0)
    grid(col = "grey90")
    abline(h = 0, v = 0, col = "grey75")
    points(Y[, 1], Y[, 2], pch = 16, col = adjustcolor(cols[gcol], 0.48), cex = 0.9)
    lines(e1[, 1], e1[, 2], col = cols[1], lwd = 2.6)
    lines(e2[, 1], e2[, 2], col = cols[2], lwd = 2.6)
    lines(e1b[, 1], e1b[, 2], col = cols[1], lwd = 1.5, lty = 2)
    lines(e2b[, 1], e2b[, 2], col = cols[2], lwd = 1.5, lty = 2)
    points(mu2[1, ], mu2[2, ], pch = 21, bg = cols, col = "black", cex = 2, lwd = 1.4)
    arrows(mu2[1, 1], mu2[2, 1], mu2[1, 2], mu2[2, 2], code = 3, length = 0.11, lwd = 3, col = "#222222")
    vg <- as.numeric(t(Q) %*% w_gmm)
    vo <- as.numeric(t(Q) %*% w_old)
    arrows(0, 0, vg[1] * 2, vg[2] * 2, length = 0.1, lwd = 2, col = "#111111")
    arrows(0, 0, vo[1] * 2, vo[2] * 2, length = 0.1, lwd = 2, col = "#666666")
    text(vg[1] * 2, vg[2] * 2, " gmm", pos = 4, cex = 0.9)
    text(vo[1] * 2, vo[2] * 2, " 旧核", pos = 4, cex = 0.9, col = "#555555")
    legend("topleft", bty = "n", cex = 0.88, legend = c(
      sprintf("n类 %d / %d", sum(gcol == 1), sum(gcol == 2)),
      sprintf("横轴 Ashman=%.2f  CH=%.1f", ash, ch),
      sprintf("横轴 dip p≈%.2f", dp),
      sprintf("|cos|横轴vs gmm=%.2f vs旧=%.2f", abs(sum(Q[, 1] * w_gmm)), abs(sum(Q[, 1] * w_old)))
    ))
    mtext(sprintf("%s | N=%d | 模型 %s | r(gmm,old)=%.2f", tag, b$n, fit$modelName, stats::cor(s_gmm, s_old)),
          side = 1, line = 4.3, cex = 0.82, col = "grey30")
    dev.off()
    invisible(list(ash = ash, ch = ch, dip = dp))
  }

  plot_plane(Q_old, cl, sprintf("%s_gmm_vs_old_plane.png", tag),
             sprintf("%s：gmm 轴 × 旧核差（纵轴正交化）", tag),
             "gmm_perp", "旧核差垂直分量")
  plot_plane(Q_max, sp_max$g, sprintf("%s_maxsep_plane.png", tag),
             sprintf("%s：专挑最大 Ashman 平面", tag),
             "最大分离方向", "正交横切")

  png(file.path(figdir, sprintf("%s_density_compare.png", tag)), width = 1300, height = 720, res = 140)
  par(mfrow = c(1, 3), mar = c(4.2, 4, 3.5, 1), family = "cjk")
  plot(stats::density(s_gmm), main = sprintf("gmm\nAsh=%.2f dip p=%.2f", sp_gmm$ash, dip_p(s_gmm)),
       xlab = "分", col = "#222222", lwd = 2)
  graphics::rug(s_gmm, col = adjustcolor(1, 0.25))
  plot(stats::density(s_old), main = sprintf("旧核差\nAsh=%.2f", sp_old$ash),
       xlab = "分", col = "#666666", lwd = 2)
  graphics::rug(s_old, col = adjustcolor(1, 0.25))
  plot(stats::density(sp_max$s), main = sprintf("max Ashman\nAsh=%.2f dip p=%.2f", sp_max$ash, dip_p(sp_max$s)),
       xlab = "分", col = "#B52127", lwd = 2)
  graphics::rug(sp_max$s, col = adjustcolor("#B52127", 0.25))
  dev.off()

  list(
    tag = tag, n = b$n, model = fit$modelName,
    gmm = list(ash = sp_gmm$ash, ch = sp_gmm$ch, dip = dip_p(s_gmm), bc = bimod_coef(s_gmm), n1 = sp_gmm$n1, n2 = sp_gmm$n2, s = s_gmm),
    old = list(ash = sp_old$ash, ch = sp_old$ch, dip = dip_p(s_old), bc = bimod_coef(s_old), s = s_old),
    max = list(ash = sp_max$ash, ch = sp_max$ch, dip = dip_p(sp_max$s), bc = bimod_coef(sp_max$s), n1 = sp_max$n1, n2 = sp_max$n2, s = sp_max$s),
    cos_gmm_old = sum(w_gmm * w_old),
    cos_max_gmm = sum(w_max * w_gmm),
    cos_max_old = sum(w_max * w_old),
    cor_scores = stats::cor(s_gmm, s_old),
    w_gmm = w_gmm, w_old = w_old, w_max = w_max
  )
}

run_item20 <- function() {
  I <- as.matrix(v[, items20])
  ok <- stats::complete.cases(I)
  I <- I[ok, , drop = FALSE]
  Iz <- scale(I)
  tot <- as.numeric(scale(rowMeans(Iz)))
  X <- scale(apply(Iz, 2, function(col) residuals(stats::lm(col ~ tot))))
  colnames(X) <- items20
  w_old <- rep(0, ncol(X))
  names(w_old) <- items20
  w_old[add10] <- 1
  w_old[eng10] <- -1
  w_old <- unit(w_old)

  fit <- mclust::Mclust(X, G = 2, modelNames = c("EEE", "EEI", "VVI", "VVV"), verbose = FALSE)
  w_gmm <- unit(fit$parameters$mean[, 2] - fit$parameters$mean[, 1])
  names(w_gmm) <- items20
  w_gmm <- align_sign(w_gmm, w_old)
  sp_gmm <- sep_dir(X, w_gmm)
  sp_old <- sep_dir(X, w_old)
  best <- max_ash_search(X, list(w_gmm, w_old))
  w_max <- align_sign(best$w, w_old)
  sp_max <- sep_dir(X, w_max)

  list(
    n = nrow(X), model = fit$modelName,
    gmm = list(ash = sp_gmm$ash, ch = sp_gmm$ch, dip = dip_p(sp_gmm$s), bc = bimod_coef(sp_gmm$s), n1 = sp_gmm$n1, n2 = sp_gmm$n2, s = sp_gmm$s),
    old = list(ash = sp_old$ash, ch = sp_old$ch, dip = dip_p(sp_old$s), bc = bimod_coef(sp_old$s), s = sp_old$s),
    max = list(ash = sp_max$ash, ch = sp_max$ch, dip = dip_p(sp_max$s), bc = bimod_coef(sp_max$s), n1 = sp_max$n1, n2 = sp_max$n2, s = sp_max$s),
    cos_gmm_old = sum(w_gmm * w_old),
    cos_max_gmm = sum(w_max * w_gmm),
    cos_max_old = sum(w_max * w_old),
    w_gmm = w_gmm, w_old = w_old, w_max = w_max
  )
}

res28 <- run_suite("mas28_7d", dim_items28)
res20 <- run_suite("mas20_7d", dim_items20)
resI <- run_item20()

cmp <- rbind(
  data.frame(version = "28item_7d", axis = "gmm", ash = res28$gmm$ash, ch = res28$gmm$ch, dip_p = res28$gmm$dip, bc = res28$gmm$bc, n1 = res28$gmm$n1, n2 = res28$gmm$n2, cos_or_note = res28$cos_gmm_old, cor_gmm_old = res28$cor_scores),
  data.frame(version = "28item_7d", axis = "old", ash = res28$old$ash, ch = res28$old$ch, dip_p = res28$old$dip, bc = res28$old$bc, n1 = NA, n2 = NA, cos_or_note = res28$cos_gmm_old, cor_gmm_old = res28$cor_scores),
  data.frame(version = "28item_7d", axis = "maxAsh", ash = res28$max$ash, ch = res28$max$ch, dip_p = res28$max$dip, bc = res28$max$bc, n1 = res28$max$n1, n2 = res28$max$n2, cos_or_note = res28$cos_max_gmm, cor_gmm_old = res28$cos_max_old),
  data.frame(version = "20item_7d", axis = "gmm", ash = res20$gmm$ash, ch = res20$gmm$ch, dip_p = res20$gmm$dip, bc = res20$gmm$bc, n1 = res20$gmm$n1, n2 = res20$gmm$n2, cos_or_note = res20$cos_gmm_old, cor_gmm_old = res20$cor_scores),
  data.frame(version = "20item_7d", axis = "old", ash = res20$old$ash, ch = res20$old$ch, dip_p = res20$old$dip, bc = res20$old$bc, n1 = NA, n2 = NA, cos_or_note = res20$cos_gmm_old, cor_gmm_old = res20$cor_scores),
  data.frame(version = "20item_7d", axis = "maxAsh", ash = res20$max$ash, ch = res20$max$ch, dip_p = res20$max$dip, bc = res20$max$bc, n1 = res20$max$n1, n2 = res20$max$n2, cos_or_note = res20$cos_max_gmm, cor_gmm_old = res20$cos_max_old),
  data.frame(version = "20item_itemlvl", axis = "gmm", ash = resI$gmm$ash, ch = resI$gmm$ch, dip_p = resI$gmm$dip, bc = resI$gmm$bc, n1 = resI$gmm$n1, n2 = resI$gmm$n2, cos_or_note = resI$cos_gmm_old, cor_gmm_old = NA),
  data.frame(version = "20item_itemlvl", axis = "old", ash = resI$old$ash, ch = resI$old$ch, dip_p = resI$old$dip, bc = resI$old$bc, n1 = NA, n2 = NA, cos_or_note = resI$cos_gmm_old, cor_gmm_old = NA),
  data.frame(version = "20item_itemlvl", axis = "maxAsh", ash = resI$max$ash, ch = resI$max$ch, dip_p = resI$max$dip, bc = resI$max$bc, n1 = resI$max$n1, n2 = resI$max$n2, cos_or_note = resI$cos_max_gmm, cor_gmm_old = resI$cos_max_old)
)
utils::write.csv(cmp, file.path(outdir, "compare_20_vs_28.csv"), row.names = FALSE)

wt <- rbind(w28 = res28$w_gmm, w20 = res20$w_gmm[names(res28$w_gmm)])
utils::write.csv(wt, file.path(outdir, "compare_20_vs_28_gmm_weights.csv"), row.names = TRUE)

png(file.path(figdir, "compare_20_vs_28_gmm_density.png"), width = 1400, height = 700, res = 140)
par(mfrow = c(1, 3), mar = c(4.3, 4.1, 3.8, 1), family = "cjk")
plot(stats::density(res28$gmm$s), main = sprintf("28题→7维 gmm\nAsh=%.2f dip p=%.2f", res28$gmm$ash, res28$gmm$dip),
     xlab = "投影分", lwd = 2, col = "#222222")
graphics::rug(res28$gmm$s, col = adjustcolor(1, 0.25))
plot(stats::density(res20$gmm$s), main = sprintf("20题→7维 gmm\nAsh=%.2f dip p=%.2f", res20$gmm$ash, res20$gmm$dip),
     xlab = "投影分", lwd = 2, col = "#B52127")
graphics::rug(res20$gmm$s, col = adjustcolor("#B52127", 0.25))
plot(stats::density(resI$gmm$s), main = sprintf("20题项目级 gmm\nAsh=%.2f dip p=%.2f", resI$gmm$ash, resI$gmm$dip),
     xlab = "投影分", lwd = 2, col = "#2171B5")
graphics::rug(resI$gmm$s, col = adjustcolor("#2171B5", 0.25))
dev.off()

png(file.path(figdir, "mas20_itemlevel_density.png"), width = 1300, height = 650, res = 140)
par(mfrow = c(1, 2), mar = c(4.3, 4.1, 3.6, 1), family = "cjk")
plot(stats::density(resI$gmm$s), main = sprintf("20题项目级 gmm\nAsh=%.2f dip p=%.2f", resI$gmm$ash, resI$gmm$dip),
     xlab = "分", lwd = 2)
graphics::rug(resI$gmm$s, col = adjustcolor(1, 0.25))
plot(stats::density(resI$max$s), main = sprintf("20题项目级 maxAsh\nAsh=%.2f dip p=%.2f n=%d/%d", resI$max$ash, resI$max$dip, resI$max$n1, resI$max$n2),
     xlab = "分", lwd = 2, col = "#B52127")
graphics::rug(resI$max$s, col = adjustcolor("#B52127", 0.25))
dev.off()

sink(file.path(outdir, "compare_20_vs_28_console.txt"))
cat("=== 20 vs 28 comparison ===\n")
print(cmp)
cat("\nGMM weights 28:\n")
print(round(res28$w_gmm, 3))
cat("\nGMM weights 20:\n")
print(round(res20$w_gmm, 3))
cat("\ncos(w_gmm20, w_gmm28) =", sum(res20$w_gmm * res28$w_gmm[names(res20$w_gmm)]), "\n")
cat("item20 cos gmm-old =", resI$cos_gmm_old, "\n")
sink()

writeLines(c(
  "# 20题 vs 28题：是否更好两分？",
  "",
  "## 做法",
  "- 28题→7维（每维4题）",
  "- 20题→7维（trim 后题组；Tolerance 仅 1 题）",
  "- 20题项目级 20 维 + ⊥总分",
  "",
  "## 结果表",
  "`compare_20_vs_28.csv`",
  "",
  "## 图",
  "- compare_20_vs_28_gmm_density.png",
  "- mas20_7d_gmm_vs_old_plane.png / mas20_7d_maxsep_plane.png / mas20_7d_density_compare.png",
  "- mas20_itemlevel_density.png",
  "- mas28_7d_* 对照重跑"
), file.path(outdir, "COMPARE_20_VS_28.md"))

cat("DONE\n")
print(cmp)
