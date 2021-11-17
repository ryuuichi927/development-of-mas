# contents.R
# Part of R_template by Tuomas Eerola, https://github.com/tuomaseerola/R_template/
#
# Adapted to MAS data in 4/5/2021

#####  CHANGES RYUICHI MADE 
###   ----Factor Analysis script----
##    Different cross-loading items from the EFA 
#       Back into the MAS(previously high cross-loading) [mas20(I have managed to limit the time on listening to music for periods, and then experienced relapse)]
#       Removed from the MAS(new high cross-loading)[mas12(I spend more and more time listening to music.)]
##    Added 9 variables to v [7 MAS dimensions, 2 core criteria]
###   ----Interrater reliability script----
##    Added a short Cronbach alpha script [alpha() function of the psych package]
###   ----Added a Descriptive statistic script----
##    Cannot access to the script from the contents.R [source()]
##    still in progress



#### INITIALISE: SET PATH, CLEAR MEMORY AND LOAD LIBRARIES -----------------------------------
rm(list=ls(all=TRUE))                     # Cleans the R memory, just in case
library(ggplot2)
library(psych)

#### READ data (from Qualtrics TSV)  ---------------------------------------------------------
source('scr/read_data_survey.R')          # Produces data matrix v with a lot of variables

#### MUNGE data (preprocess, recode, etc.)  --------------------------------------------------
source('munge/rename_variables.R')        # Renames some of the columns in the v
source('munge/recode_instruments.R')      # Produces df from v that contains all data in long form

#### DIAGNOSE data ---------------------------------------------------------------------------
source('scr/demographics_info.R')         # Reports the N and Age and musical expertise
source('scr/interrater_reliability.R')    # Pull together instruments
source('scr/descriptive_statistic.R')     # Frequency, central tendency, variability, and descriptive stats of different groups

#### ANALYSE data ---------------------------------------------------------------------------
source('scr/factor_analysis.R')           # Factor analysis
source('scr/compare_means.R')
