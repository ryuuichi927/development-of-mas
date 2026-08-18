# OLD (2021 factor_analysis core) vs NEW (Improve clean) head-to-head
suppressPackageStartupMessages({
  library(dplyr)
  library(psych)
  library(tidyr)
  library(ggplot2)
})

if (!exists("MAS_REPO")) {
  source(file.path(getwd(), "2026-reanalysis", "scr", "paths.R"))
}
mas <- THESIS_2021
improve <- REANALYSIS
out <- OUT_DIR
dir.create(out, showWarnings = FALSE, recursive = TRUE)

source(file.path(improve, "scr/read_data.R"))
source(file.path(mas, "munge/rename_variables.R"))

# Same capture and correction as run_efa_after_mas_munge.R; see scr/fix_msi_mt.R.
gold_mt_raw <- v[, grep("^GOLD[1-7]\\.MT$", names(v)), drop = FALSE]
gold_mt_raw$PID <- paste0("S", seq_len(nrow(v)))

source(file.path(mas, "munge/recode_instruments.R"))
source(file.path(improve, "scr/fix_msi_mt.R"))
v0 <- v
message("MUNGE N=", nrow(v0), " ncol=", ncol(v0))

run_old <- function(v) {
  library(psych)
  library(randomForest)
  df <- v[, which(names(v) == "mas01"):which(names(v) == "mas28")]
  df <- na.roughfix(df)
  scale_this <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  df_normed <- data.frame(lapply(df, scale_this))
  kmo_full <- KMO(df_normed)$MSA
  set.seed(20210805)
  fit_full <- fa(df_normed, 2, rotate = "varimax", fm = "minres")

  crossloaded <- c("mas12", "mas15", "mas27", "mas11", "mas03", "mas02", "mas22", "mas10")
  ind <- which(names(v) == "mas01"):which(names(v) == "mas28")
  cl <- which(names(v) %in% crossloaded)
  trimmed_idx <- setdiff(ind, cl)
  item_names <- names(v)[trimmed_idx]

  # Original: scale columns from v without re-impute (may contain NA)
  df_trimmed_raw <- as.data.frame(lapply(v[, item_names, drop = FALSE], as.numeric))
  # fa needs complete or impute — original often worked if munge left few NA.
  # Match practical 2021 run: roughfix then z (stable)
  df_tr <- na.roughfix(df_trimmed_raw)
  df_normed_t <- data.frame(lapply(df_tr, scale_this))
  fit1 <- fa(df_normed_t, 2, rotate = "varimax", fm = "minres")
  sc <- as.matrix(fit1$scores)
  F1 <- sc[, 1]
  F2 <- sc[, 2]

  v$HUMS_unhealthy <- (v$hums01 + v$hums02 + v$hums03 + v$hums04 +
    v$hums05 + v$hums06 + v$hums07 + v$hums08) / 8
  v$HUMS_healthy <- (v$hums09 + v$hums10 + v$hums11 + v$hums12 + v$hums13) / 5
  v$MAS_Salience <- (v$mas01 + v$mas02 + v$mas03 + v$mas04) / 4
  v$MAS_Moodmodification <- (v$mas05 + v$mas06 + v$mas07 + v$mas08) / 4
  v$MAS_Tolerance <- (v$mas09 + v$mas10 + v$mas11 + v$mas12) / 4
  v$MAS_Conflict <- (v$mas13 + v$mas14 + v$mas15 + v$mas16) / 4
  v$MAS_Relapse <- (v$mas17 + v$mas18 + v$mas19 + v$mas20) / 4
  v$MAS_Withdrawal <- (v$mas21 + v$mas22 + v$mas23 + v$mas24) / 4
  v$MAS_Problems <- (v$mas25 + v$mas26 + v$mas27 + v$mas28) / 4
  v$MAS_Addictivecore <- (v$MAS_Conflict + v$MAS_Relapse +
    v$MAS_Withdrawal + v$MAS_Problems) / 4
  v$MAS_Engagementcore <- (v$MAS_Salience + v$MAS_Moodmodification +
    v$MAS_Tolerance) / 3
  v$trimmedMAS_Salience <- (v$mas01 + v$mas04) / 2
  v$trimmedMAS_Conflict <- (v$mas13 + v$mas14 + v$mas16) / 3
  v$trimmedMAS_Relapse <- (v$mas17 + v$mas18 + v$mas20) / 3
  v$trimmedMAS_Withdrawal <- (v$mas21 + v$mas23 + v$mas24) / 3
  v$trimmedMAS_Problems <- (v$mas25 + v$mas26 + v$mas28) / 3
  v$trimmedMAS_Addictivecore <- (v$trimmedMAS_Conflict + v$trimmedMAS_Relapse +
    v$trimmedMAS_Withdrawal + v$trimmedMAS_Problems) / 4
  v$trimmedMAS_Engagementcore <- (v$trimmedMAS_Salience +
    v$MAS_Moodmodification) / 2

  list(
    v = v, F1 = F1, F2 = F2, fit = fit1,
    kmo_full = kmo_full,
    TLI = unname(fit1$TLI), RMSEA = unname(fit1$RMSEA[1]),
    loadings = as.matrix(unclass(fit1$loadings)),
    item_names = item_names
  )
}

old <- run_old(v0)
message("OLD TLI=", round(old$TLI, 4), " RMSEA=", round(old$RMSEA, 4),
        " N_F=", sum(is.finite(old$F1)))

# NEW listwise
v <- v0
PARALLEL_N_ITER <- 30L
USE_ROUGHFIX <- FALSE
ENGAGEMENT_TRIM_MODE <- "legacy_2021"
COMPOSITE_NA_RM <- FALSE
source(file.path(improve, "scr/factor_analysis_clean.R"))
new_ls <- list(
  v = v,
  F_add = v$MAS_F_addictive,
  F_eng = v$MAS_F_engagement,
  TLI = as.numeric(utils::read.csv(file.path(out, "efa_fit_log.csv"))$value[
    utils::read.csv(file.path(out, "efa_fit_log.csv"))$key == "TLI_trim"
  ]),
  RMSEA = as.numeric(utils::read.csv(file.path(out, "efa_fit_log.csv"))$value[
    utils::read.csv(file.path(out, "efa_fit_log.csv"))$key == "RMSEA_trim"
  ]),
  loadings = utils::read.csv(file.path(out, "efa_trimmed_loadings.csv"))
)
message("NEW listwise done")

# NEW roughfix
v <- v0
USE_ROUGHFIX <- TRUE
ENGAGEMENT_TRIM_MODE <- "legacy_2021"
COMPOSITE_NA_RM <- FALSE
source(file.path(improve, "scr/factor_analysis_clean.R"))
new_rf <- list(
  v = v,
  F_add = v$MAS_F_addictive,
  F_eng = v$MAS_F_engagement,
  TLI = as.numeric(utils::read.csv(file.path(out, "efa_fit_log.csv"))$value[
    utils::read.csv(file.path(out, "efa_fit_log.csv"))$key == "TLI_trim"
  ]),
  RMSEA = as.numeric(utils::read.csv(file.path(out, "efa_fit_log.csv"))$value[
    utils::read.csv(file.path(out, "efa_fit_log.csv"))$key == "RMSEA_trim"
  ])
)
message("NEW roughfix done")

cmp_vec <- function(a, b, name) {
  a <- as.numeric(a)
  b <- as.numeric(b)
  n <- min(length(a), length(b))
  a <- a[seq_len(n)]
  b <- b[seq_len(n)]
  ok <- intersect(which(is.finite(a)), which(is.finite(b)))
  data.frame(
    measure = name,
    n = length(ok),
    cor = if (length(ok) > 3) stats::cor(a[ok], b[ok]) else NA_real_,
    mean_old = if (length(ok)) mean(a[ok]) else NA_real_,
    mean_new = if (length(ok)) mean(b[ok]) else NA_real_,
    mean_diff_new_minus_old = if (length(ok)) mean(b[ok] - a[ok]) else NA_real_,
    max_abs_diff = if (length(ok)) max(abs(b[ok] - a[ok])) else NA_real_,
    stringsAsFactors = FALSE
  )
}

rows <- rbind(
  cmp_vec(old$v$MAS_Addictivecore, new_ls$v$MAS_Addictivecore, "FULL_Addictive_core"),
  cmp_vec(old$v$MAS_Engagementcore, new_ls$v$MAS_Engagementcore, "FULL_Engagement_core"),
  cmp_vec(old$v$trimmedMAS_Addictivecore, new_ls$v$trimmedMAS_Addictivecore, "TRIM_Addictive_core"),
  cmp_vec(old$v$trimmedMAS_Engagementcore, new_ls$v$trimmedMAS_Engagementcore, "TRIM_Engagement_core_legacy"),
  cmp_vec(old$v$MAS_Salience, new_ls$v$MAS_Salience, "FACET_Salience"),
  cmp_vec(old$v$MAS_Moodmodification, new_ls$v$MAS_Moodmodification, "FACET_Mood"),
  cmp_vec(old$v$MAS_Conflict, new_ls$v$MAS_Conflict, "FACET_Conflict"),
  cmp_vec(old$v$HUMS_healthy, new_ls$v$HUMS_healthy, "HUMS_healthy"),
  cmp_vec(old$F1, new_ls$F_add, "Fscore_OLD_F1_vs_NEW_listwise_addictive"),
  cmp_vec(old$F1, new_ls$F_eng, "Fscore_OLD_F1_vs_NEW_listwise_engagement"),
  cmp_vec(old$F2, new_ls$F_add, "Fscore_OLD_F2_vs_NEW_listwise_addictive"),
  cmp_vec(old$F2, new_ls$F_eng, "Fscore_OLD_F2_vs_NEW_listwise_engagement"),
  cmp_vec(old$F1, new_rf$F_add, "Fscore_OLD_F1_vs_NEW_roughfix_addictive"),
  cmp_vec(old$F1, new_rf$F_eng, "Fscore_OLD_F1_vs_NEW_roughfix_engagement"),
  cmp_vec(old$F2, new_rf$F_add, "Fscore_OLD_F2_vs_NEW_roughfix_addictive"),
  cmp_vec(old$F2, new_rf$F_eng, "Fscore_OLD_F2_vs_NEW_roughfix_engagement")
)

utils::write.csv(rows, file.path(out, "OLD_vs_NEW_comparison.csv"), row.names = FALSE)
print(rows)

fit_tab <- data.frame(
  version = c("OLD_2021_style", "NEW_listwise", "NEW_roughfix"),
  TLI = c(old$TLI, new_ls$TLI, new_rf$TLI),
  RMSEA = c(old$RMSEA, new_ls$RMSEA, new_rf$RMSEA),
  stringsAsFactors = FALSE
)
utils::write.csv(fit_tab, file.path(out, "OLD_vs_NEW_fit.csv"), row.names = FALSE)
print(fit_tab)

# loading correlation per factor (absolute congruence rough)
# map old loadings colnames to new
Lold <- old$loadings
Lnew <- as.matrix(new_ls$loadings[, c("MR1", "MR2")])
rownames(Lnew) <- new_ls$loadings$item
common <- intersect(rownames(Lold), rownames(Lnew))
lc <- data.frame(
  pair = c("oldMR1_newMR1", "oldMR1_newMR2", "oldMR2_newMR1", "oldMR2_newMR2"),
  cor = c(
    cor(Lold[common, 1], Lnew[common, 1]),
    cor(Lold[common, 1], Lnew[common, 2]),
    cor(Lold[common, 2], Lnew[common, 1]),
    cor(Lold[common, 2], Lnew[common, 2])
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(lc, file.path(out, "OLD_vs_NEW_loading_cor.csv"), row.names = FALSE)
print(lc)

sink(file.path(out, "OLD_efa_trim_print.txt"))
print(old$fit, sort = TRUE, cut = 0.30, digits = 3)
sink()

message("DONE -> out/OLD_vs_NEW_*.csv")
