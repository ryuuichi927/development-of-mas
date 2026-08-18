# fix_msi_mt.R
#
# munge/recode_instruments.R line 119 builds gold6.MT from GOLD5.MT while using
# the response options of GOLD6.MT. The two questions do not share options, so
# most answers fall outside the levels and become NA. MSI.MT is a row mean over
# the seven musical training items rescaled by seven, so for those respondents
# it is a six-item mean stretched to a seven-item scale.
#
# The 2021 folder is the record of what was run and is not edited. MSI.MT is
# recomputed here, in the re-analysis layer, from the column the formula meant.
#
# Needs gold_mt_raw, captured in run_efa_after_mas_munge.R before the munge
# chain drops the raw columns.

if (!exists("gold_mt_raw")) {
  stop("gold_mt_raw is missing. It has to be captured before the munge chain runs.")
}

# Same coding rule as 2021: position in the response options, with the
# "prefer not to say" option at position 8 treated as missing.
code_msi <- function(x, levels) {
  y <- as.numeric(factor(x, levels = levels))
  y[y == 8] <- NA
  y
}

MSI_AGREE <- c(
  "Completely disagree", "Strongly disagree", "Disagree",
  "Neither agree nor disagree", "Agree", "Strongly agree",
  "Completely agree", "prefer not to say"
)

mt <- data.frame(
  gold1.MT = code_msi(gold_mt_raw$GOLD1.MT, c("0", "1", "2", "3", "4-5", "6-9", "10 or more")),
  gold2.MT = code_msi(gold_mt_raw$GOLD2.MT, c("0", "0.5", "1", "1.5", "2", "3-4", "5 or more")),
  gold3.MT = code_msi(gold_mt_raw$GOLD3.MT, MSI_AGREE),
  gold4.MT = code_msi(gold_mt_raw$GOLD4.MT, c("0", "0.5", "1", "2", "3", "4-6", "7 or more")),
  gold5.MT = code_msi(gold_mt_raw$GOLD5.MT, c("0", "0.5", "1", "2", "3-5", "6-9", "10 or more")),
  gold6.MT = code_msi(gold_mt_raw$GOLD6.MT, c("0", "1", "2", "3", "4", "5", "6 or more")),
  gold7.MT = code_msi(gold_mt_raw$GOLD7.MT, MSI_AGREE)
)

# Items 3 and 7 are the reverse-worded ones, as in 2021.
mt$gold3.MT <- 8 - mt$gold3.MT
mt$gold7.MT <- 8 - mt$gold7.MT

msi_mt_fixed <- round(rowMeans(mt, na.rm = TRUE) * 7)

idx <- match(as.character(v$PID), gold_mt_raw$PID)
if (anyNA(idx)) {
  stop("Could not align the raw musical training items to the munged rows.")
}

msi_mt_2021 <- v$MSI.MT
v$MSI.MT <- msi_mt_fixed[idx]

message(sprintf(
  "MSI.MT recomputed from GOLD6.MT: %d of %d rows change, mean %.2f -> %.2f, NA %d -> %d",
  sum(msi_mt_2021 != v$MSI.MT, na.rm = TRUE), nrow(v),
  mean(msi_mt_2021, na.rm = TRUE), mean(v$MSI.MT, na.rm = TRUE),
  sum(is.na(msi_mt_2021)), sum(is.na(v$MSI.MT))
))

rm(mt, msi_mt_fixed, idx, code_msi, MSI_AGREE)
