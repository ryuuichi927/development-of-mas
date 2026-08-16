# Follow-up: cross-loadings pattern vs structure; composite inflation
#
# RECORD, not a runnable entry point. Needs the 2021 workspace; see
# trial_2026_07_packAB.R for why it is not in the repository.
library(psych)
library(GPArotation)

if (!exists("MAS_REPO")) {
  source(file.path(getwd(), "2026-reanalysis", "scr", "paths.R"))
}
outdir <- file.path(OUT_DIR, "trials")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

workspace <- Sys.getenv("MAS_WORKSPACE", unset = "")
if (!nzchar(workspace) || !file.exists(workspace)) {
  stop('Set Sys.setenv(MAS_WORKSPACE = "/path/to/.RData") to re-run this trial.')
}
load(workspace)

items20 <- c(
  "mas01","mas04","mas05","mas06","mas07","mas08","mas09",
  "mas13","mas14","mas16","mas17","mas18","mas19","mas20",
  "mas21","mas23","mas24","mas25","mas26","mas28"
)
add10 <- c("mas13","mas14","mas16","mas17","mas18","mas19","mas20","mas25","mas26","mas28")
eng10 <- c("mas01","mas04","mas05","mas06","mas07","mas08","mas09","mas21","mas23","mas24")

X <- as.data.frame(df_normed_t)[, items20, drop = FALSE]
fa_obl <- fa(X, nfactors = 2, rotate = "oblimin", fm = "minres")
P <- as.matrix(unclass(fa_obl$loadings))
Phi_raw <- fa_obl$Phi
S <- P %*% Phi_raw

# Label factors: addictive = higher mean abs load on add10
m1 <- mean(abs(P[add10, 1]))
m2 <- mean(abs(P[add10, 2]))
if (m2 > m1) {
  P <- P[, c(2, 1), drop = FALSE]
  S <- S[, c(2, 1), drop = FALSE]
  Phi <- Phi_raw[c(2, 1), c(2, 1), drop = FALSE]
} else {
  Phi <- Phi_raw
}
colnames(P) <- colnames(S) <- c("Add", "Eng")
dimnames(Phi) <- list(c("Add", "Eng"), c("Add", "Eng"))

cross_at <- function(M, cut) {
  both <- abs(M[, 1]) >= cut & abs(M[, 2]) >= cut
  if (!any(both)) {
    return(data.frame(item = character(), Add = numeric(), Eng = numeric(),
                      sec = numeric()))
  }
  data.frame(
    item = rownames(M)[both],
    Add = round(M[both, 1], 3),
    Eng = round(M[both, 2], 3),
    sec = round(pmin(abs(M[both, 1]), abs(M[both, 2])), 3)
  )
}

sec_p <- apply(abs(P), 1, min)
pri_p <- apply(abs(P), 1, max)
tab <- data.frame(
  item = rownames(P),
  Add = round(P[, "Add"], 3),
  Eng = round(P[, "Eng"], 3),
  pri = round(pri_p, 3),
  sec = round(sec_p, 3),
  assigned = ifelse(rownames(P) %in% add10, "add10", "eng10"),
  prim_factor = ifelse(abs(P[, "Add"]) >= abs(P[, "Eng"]), "Add", "Eng"),
  stringsAsFactors = FALSE
)
tab <- tab[order(-tab$sec), ]

raw <- v[, items20]
ok <- stats::complete.cases(raw)
raw <- raw[ok, , drop = FALSE]

clean <- rownames(P)[sec_p < 0.20]
add_c <- intersect(add10, clean)
eng_c <- intersect(eng10, clean)
r_full <- stats::cor(rowMeans(raw[, add10]), rowMeans(raw[, eng10]))
r_clean <- if (length(add_c) >= 3 && length(eng_c) >= 3) {
  stats::cor(rowMeans(raw[, add_c, drop = FALSE]),
             rowMeans(raw[, eng_c, drop = FALSE]))
} else {
  NA_real_
}
eng_no_w <- setdiff(eng10, c("mas21", "mas23", "mas24"))
r_no_w <- stats::cor(rowMeans(raw[, add10]), rowMeans(raw[, eng_no_w]))

sec_s <- apply(abs(S), 1, min)
struct_ambig <- names(sec_s)[sec_s >= 0.30]
add_s <- setdiff(add10, struct_ambig)
eng_s <- setdiff(eng10, struct_ambig)
r_structclean <- if (length(add_s) >= 3 && length(eng_s) >= 3) {
  stats::cor(rowMeans(raw[, add_s]), rowMeans(raw[, eng_s]))
} else {
  NA_real_
}

# pure: only items with secondary pattern < .15 and assigned matches prim_factor
pure <- tab$item[tab$sec < 0.15 & tab$assigned == ifelse(tab$prim_factor == "Add", "add10", "eng10")]
add_p <- intersect(add10, pure)
eng_p <- intersect(eng10, pure)
r_pure <- if (length(add_p) >= 3 && length(eng_p) >= 3) {
  stats::cor(rowMeans(raw[, add_p, drop = FALSE]),
             rowMeans(raw[, eng_p, drop = FALSE]))
} else {
  NA_real_
}

lines <- c(
  "=== Pack A follow-up: pattern vs structure cross-loadings ===",
  paste0("Phi Add-Eng: ", round(Phi["Add", "Eng"], 4)),
  paste0("Phi^2 shared var: ", round(Phi["Add", "Eng"]^2, 4)),
  "",
  "--- PATTERN both |load| >= cut ---"
)
for (cut in c(0.15, 0.20, 0.25, 0.30, 0.35)) {
  cc <- cross_at(P, cut)
  lines <- c(lines, sprintf("cut=%.2f n=%d %s", cut, nrow(cc),
                            if (nrow(cc)) paste(cc$item, collapse = ",") else ""))
  if (nrow(cc)) {
    lines <- c(lines, capture.output(print(cc, row.names = FALSE)))
  }
}
lines <- c(lines, "", "--- STRUCTURE both |r| >= cut ---")
for (cut in c(0.20, 0.25, 0.30, 0.35, 0.40, 0.45)) {
  cc <- cross_at(S, cut)
  lines <- c(lines, sprintf("cut=%.2f n=%d %s", cut, nrow(cc),
                            if (nrow(cc)) paste(cc$item, collapse = ",") else ""))
  if (nrow(cc)) {
    lines <- c(lines, capture.output(print(cc, row.names = FALSE)))
  }
}
lines <- c(
  lines, "",
  "--- All items by secondary |pattern| ---",
  capture.output(print(tab, row.names = FALSE)),
  "",
  "--- Composite r decomposition ---",
  paste0("r EFA10 full:              ", round(r_full, 4)),
  paste0("r pattern sec < .20 clean: ", round(r_clean, 4),
         "  (add n=", length(add_c), " eng n=", length(eng_c), ")"),
  paste0("r eng w/o withdrawal:      ", round(r_no_w, 4)),
  paste0("r drop structure sec>=.30: ", round(r_structclean, 4),
         "  ambig=", paste(struct_ambig, collapse = ",")),
  paste0("r pure sec<.15 match:      ", round(r_pure, 4),
         "  (add n=", length(add_p), " eng n=", length(eng_p), ")"),
  paste0("Phi:                       ", round(Phi["Add", "Eng"], 4))
)

writeLines(lines, file.path(outdir, "packA_crossloading_followup.txt"))
write.csv(round(P, 4), file.path(outdir, "packA_pattern_labeled.csv"))
write.csv(round(S, 4), file.path(outdir, "packA_structure_labeled.csv"))
write.csv(tab, file.path(outdir, "packA_secondary_loadings.csv"), row.names = FALSE)

cat(paste(lines, collapse = "\n"), "\n")
cat("DONE\n")
