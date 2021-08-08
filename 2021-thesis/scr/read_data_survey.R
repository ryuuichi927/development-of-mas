# read_data_survey.R
# Part of R_template by Tuomas Eerola, https://github.com/tuomaseerola/R_template/
#
# make sure the raw ascii tab separated data file is encoded in UTF-8!
#
v <- read.csv("data/Music+Addiction+Study+-+Ryuichi_15+July+2021_11.35.tsv", header=TRUE, sep = "\t")

#### How many factors or components are there in the data? -----------------------
cat('Original data: N x Variables:')
cat(dim(v))
cat("\n===============Reading done!========================")

