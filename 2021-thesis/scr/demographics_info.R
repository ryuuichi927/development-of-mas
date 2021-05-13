## Demographic info of participants
# Part of R_template by Tuomas Eerola, https://github.com/tuomaseerola/R_template/

print(paste("N =",nrow(v)))

# Age
print(paste("Mean age",round(mean(v$Age,na.rm=TRUE),2)))
print(paste("SD age",round(sd(v$Age,na.rm=TRUE),2)))
print(paste("Youngest",min(v$Age,na.rm=TRUE),"years"))
print(paste("Oldest",max(v$Age,na.rm=TRUE),"years"))

# Gender
print(table(v$Gender))

# Education
print(table(v$Education))

# Language
print(table(v$EnglishLanguage))
print(table(v$LanguageCompetence))
print(table(v$NativeLanguage))
# Country
print(table(v$Country))
