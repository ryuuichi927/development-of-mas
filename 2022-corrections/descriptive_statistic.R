#Descriptive statistic for MAS
#Calculate descriptive statistic

v[v == 'NaN'] <- NA

psych::describe(v,na.rm = TRUE)

#Filter responds that scored high in certain items
library(dplyr)

# responded positive to mas28 (Music is like an addiction to me) score
v$mas28C <- cut(v$mas28,c(0,4,10),labels = c("4 or lower","above 4"))

v$EngagedC <-cut(v$MAS_Engagementcore, c(0,4,10), labels = c("4 or lower","above 4"))

v$JustengagedC <-cut(v$trimmedMAS_Engagementcore, c(0,5,10), labels = c("5 or lower","above 5"))

v$AddictedC <-cut(v$trimmedMAS_Addictivecore, c(0,4,10), labels = c("4 or lower","above 4"))

table(v$JustengagedC) # this is optional but shows you what you got.
# You can do this for all the questions. and then test the whatever the instrument you want in compare_means with aov.

##########################################################
# New addition on 5th August 2022 by TE:
# Put these onto same variable

v$grouping <- 'Ordinary' # define this to capture those who are not in any of the below groups
v$grouping[v$mas28 > 4] <- 'Severely Addicted'
v$grouping[v$trimmedMAS_Engagementcore > 4] <- 'Engaged'
v$grouping[v$trimmedMAS_Engagementcore > 5] <- 'Just Engaged'
v$grouping[v$trimmedMAS_Addictivecore > 4] <- 'Addicted'
table(v$grouping)

m1 <- aov(HUMS_unhealthy ~ grouping, data=v) # ANOVA analysis
s1<-summary(m1,correlation=FALSE)           # show result
print(s1)

# THIS WOULD BE A BETTER SOLUTION, MUCH CLEARER CONCEPTUALLY:
# Take MAS, divide it into 3 quantiles (below 33%, above 33% and below 66%, and above 66%), and then run ANOVA

v$grouping_MAS_Addictive <-cut(v$trimmedMAS_Addictivecore, c(0,quantile(v$trimmedMAS_Addictivecore,0.3333,na.rm = T),quantile(v$trimmedMAS_Addictivecore,0.6666,na.rm = T),10),labels=c('Low Addictive','Medium Addictive','High Addictive'))
table(v$grouping_MAS_Addictive) # This divides the sample into three equal groups

m1 <- aov(HUMS_unhealthy ~ grouping_MAS_Addictive, data=v) # ANOVA analysis
s1<-summary(m1,correlation=FALSE)           # show result
print(s1)

# This carries out posthoc comparison between the groups
library(emmeans)
fit2_emm <- emmeans(m1, "grouping_MAS_Addictive", data=v)
pairs(fit2_emm, adjust="tukey")
plot(fit2_emm, comparisons = TRUE) # plot (as extra, not really needed)

# and you can do this for the other variables, keep the grouping variable the same but change the HUMS_unhealthy, like this
# ... and you report should first have the anova (as above, with F and df and p values) and then for
# posthoc analyses you report that "posthoc between the groups suggested a statistically significant difference
# between low and medium addictive group (t(238)=-4.19, p<.001) and.... 

m1 <- aov(HUMS_healthy ~ grouping_MAS_Addictive, data=v) # ANOVA analysis
s1<-summary(m1,correlation=FALSE)           # show result
print(s1)
fit2_emm <- emmeans(m1, "grouping_MAS_Addictive", data=v)
pairs(fit2_emm, adjust="tukey")
plot(fit2_emm, comparisons = TRUE) # plot (as extra, not really needed)
# this time there is a different between Low addictive groups and medium addictive, but not anymore a difference between high addictive and medium addictive.

#anova for MSI.AE
m1 <- aov(MSI.AE ~ grouping_MAS_Addictive, data=v) # ANOVA analysis
s1<-summary(m1,correlation=FALSE)           # show result
print(s1)

# This carries out posthoc comparison between the groups
library(emmeans)
fit2_emm <- emmeans(m1, "grouping_MAS_Addictive", data=v)
pairs(fit2_emm, adjust="tukey")
plot(fit2_emm, comparisons = TRUE) # plot (as extra, not really needed)

#anova for MSI.MT
m1 <- aov(MSI.MT ~ grouping_MAS_Addictive, data=v) # ANOVA analysis
s1<-summary(m1,correlation=FALSE)           # show result
print(s1)

# This carries out posthoc comparison between the groups
library(emmeans)
fit2_emm <- emmeans(m1, "grouping_MAS_Addictive", data=v)
pairs(fit2_emm, adjust="tukey")
plot(fit2_emm, comparisons = TRUE) # plot (as extra, not really needed)

##########################################################


#responded positive to Salience dimension 
highSalience <- filter(v, v$MAS_Salience > 4)
psych::describe(highSalience)

#responded positive to Mood modification dimension 
highMood <- filter(v, v$MAS_Moodmodification > 4)
psych::describe(highMood)

#responded positive to Tolerance dimension 
highTolerance <- filter(v, v$MAS_Tolerance > 4)
psych::describe(highTolerance)

#responded positive to Conflict dimension 
highConflict <- filter(v, v$MAS_Conflict > 4)
psych::describe(highConflict)

#responded positive to Relapse dimension 
highRelapse <- filter(v, v$MAS_Relapse > 4)
psych::describe(highRelapse)

#responded positive to Withdrawal dimension 
highWithdrawal <- filter(v, v$MAS_Withdrawal > 4)
psych::describe(highWithdrawal)

#responded positive to Problems dimension 
highProblems <- filter(v, v$MAS_Problems > 4)
psych::describe(highProblems)

#Those who scoEred high in engagement core
Engaged <- filter(v, v$MAS_Engagementcore > 4)
psych::describe(Engaged)

#Needs to run the factor_analysis script first to run below!
#Those who scored high in engagement core but low in addictive core
Justengaged <- filter(v, v$trimmedMAS_Engagementcore > 5)
psych::describe(Justengaged)

#Those who score high in trimmed addictive core criteria within the engaged group
Addictive <- filter(v, v$trimmedMAS_Addictivecore > 4)
psych::describe(Addictive)

Nonengaged <- filter(v, v$MAS_Engagementcore < 3)
psych::describe(Nonengaged)
