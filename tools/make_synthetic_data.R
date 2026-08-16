# make_synthetic_data.R
#
# Writes data/synthetic/mas_synthetic.tsv: a stand-in for the 2021 Qualtrics
# export, with the same column names and the same response options, so that the
# whole pipeline can be run by someone who does not have the real data.
#
# What it reproduces: the column layout, the response wording, the coding of
# missing and "prefer not to say", and a two-factor structure strong enough for
# the EFA to converge.
#
# What it does not reproduce: anything substantive. The item means, the factor
# correlation and the relationship with HUMS and Gold-MSI are invented. No
# result obtained from this file says anything about music addiction.
#
# Usage, from the repository root:
#   source("tools/make_synthetic_data.R")

set.seed(20260816)

N <- 300

MAS_LEVELS <- c(
  "Completely disagree", "Strongly disagree", "Disagree",
  "Neither agree nor disagree", "Agree", "Strongly agree", "Completely agree"
)
HUMS_LEVELS <- c("Never", "Almost never", "Sometimes", "Often", "Always")
MSI_LEVELS <- c(MAS_LEVELS, "prefer not to say")

# Latent traits. The 28 MAS items load on two correlated factors, matching the
# structure the thesis reported, so that fa.parallel returns two.
phi <- 0.45
z_add <- rnorm(N)
z_eng <- phi * z_add + sqrt(1 - phi^2) * rnorm(N)

# Items 1 to 12 are the engagement side, 13 to 28 the addictive side, which is
# how the facets are ordered in the instrument.
item_factor <- c(rep("eng", 12), rep("add", 16))
loading <- runif(28, 0.45, 0.80)

to_likert <- function(x, levels_vec) {
  k <- length(levels_vec)
  cuts <- qnorm(seq_len(k - 1) / k)
  levels_vec[findInterval(x, cuts) + 1L]
}

mas <- vapply(seq_len(28), function(i) {
  z <- if (item_factor[i] == "add") z_add else z_eng
  latent <- loading[i] * z + sqrt(1 - loading[i]^2) * rnorm(N)
  to_likert(latent, MAS_LEVELS)
}, character(N))
colnames(mas) <- paste0("MAS", seq_len(28))

# HUMS: unhealthy items track the addictive factor, healthy items do not.
hums <- vapply(seq_len(13), function(i) {
  z <- if (i <= 8) z_add else z_eng
  latent <- 0.5 * z + sqrt(1 - 0.25) * rnorm(N)
  to_likert(latent, HUMS_LEVELS)
}, character(N))
colnames(hums) <- paste0("HUMS", seq_len(13))

pick <- function(levels_vec, prob = NULL) {
  sample(levels_vec, N, replace = TRUE, prob = prob)
}

gold <- data.frame(
  `GOLD1-AE` = pick(MSI_LEVELS), `GOLD2-AE` = pick(MSI_LEVELS),
  `GOLD3-AE` = pick(MSI_LEVELS),
  `GOLD4-AE` = pick(c("0", "1", "2", "3", "4-6", "7-10", "11 or more")),
  `GOLD5-AE` = pick(MSI_LEVELS), `GOLD6-AE` = pick(MSI_LEVELS),
  `GOLD7-AE` = pick(c("0-15 min", "15-30 min", "30-60 min", "60-90 min",
                      "2 hrs", "2-3 hrs", "4 hrs or more", "prefer not to say")),
  `GOLD8-AE` = pick(MSI_LEVELS), `GOLD9-AE` = pick(MSI_LEVELS),
  `GOLD1-MT` = pick(c("0", "1", "2", "3", "4-5", "6-9", "10 or more")),
  `GOLD2-MT` = pick(c("0", "0.5", "1", "1.5", "2", "3-4", "5 or more")),
  `GOLD3-MT` = pick(MSI_LEVELS),
  `GOLD4-MT` = pick(c("0", "0.5", "1", "2", "3", "4-6", "7 or more")),
  `GOLD5-MT` = pick(c("0", "0.5", "1", "2", "3-5", "6-9", "10 or more")),
  `GOLD6-MT` = pick(c("0", "1", "2", "3", "4", "5", "6 or more")),
  `GOLD7-MT` = pick(MSI_LEVELS),
  check.names = FALSE, stringsAsFactors = FALSE
)

# Administrative columns. The identifying ones are kept as empty columns so the
# 2021 munge, which drops them by name, still runs. Nothing is invented into
# them: a synthetic IP address would be a worse idea than no IP address.
blank <- rep("", N)

out <- data.frame(
  StartDate = blank,
  EndDate = blank,
  Status = "IP Address",
  IPAddress = blank,
  Progress = sample(91:100, N, replace = TRUE),
  `Duration (in seconds)` = sample(200:1800, N, replace = TRUE),
  Finished = "True",
  RecordedDate = blank,
  ResponseId = sprintf("SYN_%04d", seq_len(N)),
  RecipientLastName = blank,
  RecipientFirstName = blank,
  RecipientEmail = blank,
  ExternalReference = blank,
  LocationLatitude = blank,
  LocationLongitude = blank,
  DistributionChannel = "anonymous",
  UserLanguage = "EN",
  Q1 = "I consent to participate in this study",
  Q2 = sample(18:65, N, replace = TRUE),
  Q3 = pick(c("Male", "Female", "Prefer not to say")),
  Q4 = pick(c("High school", "Bachelor's degree", "Master's degree", "Doctorate")),
  Q5 = pick(c("English", "Other")),
  Q5_9_TEXT = blank,
  Q6 = pick(c("Elementary", "Intermediate", "Advanced", "Native")),
  Q7 = pick(c("Country A", "Country B", "Country C")),
  check.names = FALSE, stringsAsFactors = FALSE
)

out <- cbind(out, hums, mas, gold)

# A realistic amount of item-level missingness, so that the complete-case and
# imputation branches of the 2026 script both get exercised.
item_cols <- c(colnames(hums), colnames(mas))
for (j in item_cols) {
  drop <- runif(N) < 0.01
  out[drop, j] <- NA
}

dest_dir <- file.path(getwd(), "data", "synthetic")
dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
dest <- file.path(dest_dir, "mas_synthetic.tsv")

write.table(
  out, dest,
  sep = "\t", quote = FALSE, row.names = FALSE, na = "", fileEncoding = "UTF-8"
)

message("Wrote ", dest, ": ", nrow(out), " rows x ", ncol(out), " columns.")
message("Synthetic. Structurally valid, substantively meaningless.")
