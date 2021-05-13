# rename_variables.R
# Part of R_template by Tuomas Eerola, https://github.com/tuomaseerola/R_template/

# Note. This is unusually extensive renaming of the variables as the variables have not 
# been coded in the Qualtrics. 

# drop unnecessary stuff
v <- dplyr::select(v,-StartDate,-EndDate,-IPAddress,-LocationLatitude,-LocationLongitude,-DistributionChannel,-UserLanguage,-RecipientLastName,-RecipientFirstName,-RecipientEmail,-ExternalReference)

#### 1. Rename variable headers ---------------------------------------------------
colnames(v)[colnames(v)=="Duration..in.seconds."]<-"Time"
colnames(v)[colnames(v)=="Q1"]<-"Consent"
colnames(v)[colnames(v)=="Q2"]<-"Age"
colnames(v)[colnames(v)=="Q3"]<-"Gender"
colnames(v)[colnames(v)=="Q4"]<-"Education"
colnames(v)[colnames(v)=="Q5"]<-"EnglishLanguage"
colnames(v)[colnames(v)=="Q5_9_TEXT"]<-"NativeLanguage"
colnames(v)[colnames(v)=="Q6"]<-"LanguageCompetence"
colnames(v)[colnames(v)=="Q7"]<-"Country"

cat("\n===============Renaming done!========================")
