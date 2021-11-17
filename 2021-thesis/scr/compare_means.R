# compare_means.R
# Part of R_template by Tuomas Eerola, https://github.com/tuomaseerola/R_template/

#### PLOT MAS item means -----------
library(tidyr)
MAS_LABELS<-c("Salience", "Salience", "Salience", "Salience", "Mood modification", "Mood modification", "Mood modification", "Mood modification", "Tolerance", "Tolerance", "Tolerance", "Tolerance", "Conflict", "Conflict", "Conflict", "Conflict", "Relapse", "Relapse", "Relapse", "Relapse", "Withdrawal ", "Withdrawal ", "Withdrawal ", "Withdrawal ", "Problems", "Problems", "Problems", "Problems")

D<-pivot_longer(data = v,cols = c(16:43),)
S<-dplyr::summarise(dplyr::group_by(D,name),M=mean(value,na.rm = T),SD=sd(value,na.rm = T))
S$labels<-MAS_LABELS

g1<-ggplot(S,aes(x=name,y=M,fill=labels))+
  geom_col()+
  geom_errorbar(S, mapping=aes(x=name, ymin=M-SD, ymax=M+SD), width=.5, size=.4,show.legend = FALSE) + 
  coord_flip(ylim = c(1,7))+
  xlab('')+
  ylab('Mean rating ± SD')+
  scale_y_continuous(breaks = 1:7)+
#  scale_x_discrete(breaks = 1:28,labels=MAS_LABELS)+
  theme_bw()
g1

plotflag<-0
if(plotflag==1){
  ggsave(filename = 'figures/MAS_means.pdf',g1,height = 7,width = 5)
}

#### Are the factors different across expertise gender? ----

v$MSI.MT_C <- cut(v$MSI.MT,breaks = c(0,median(v$MSI.MT,na.rm = T),100)) # Here we divide the group those who score higher or lower in the median score
table(v$MSI.MT_C) # just a check
m1 <- aov(F1 ~ Age * Gender * MSI.MT_C, data=v) # ANOVA analysis, testing whether Factor 1 differs across gender and age
s1<-summary(m1,correlation=FALSE)
print(s1)

m2 <- aov(F2 ~ Age * Gender * MSI.MT_C, data=v)
s2<-summary(m2,correlation=FALSE)
print(s2)

#### What about people who score high/low on addiction, are they different in terms of HUMS?
v$F1_C <- cut(v$F1,breaks = c(-10,median(v$F1,na.rm = T),10),labels = c("Low","High"))
table(v$F1_C)

m1 <- aov(HUMS_healthy ~ F1_C, data=v)
s1<-summary(m1,correlation=FALSE)
print(s1)
summarise(group_by(v,F1_C),M=mean(HUMS_healthy,na.rm=TRUE),SD=sd(HUMS_healthy,na.rm=TRUE),n=n())

v$F2_C <- cut(v$F2,breaks = c(-10,median(v$F2,na.rm = T),10),labels = c("Low","High"))
m1 <- aov(HUMS_healthy ~ F2_C, data=v)
s1<-summary(m1,correlation=FALSE)
print(s1)
summarise(group_by(v,F2_C),M=mean(HUMS_healthy,na.rm=TRUE),SD=sd(HUMS_healthy,na.rm=TRUE),n=n())

m1 <- aov(HUMS_unhealthy ~ F1_C, data=v)
s1<-summary(m1,correlation=FALSE)
print(s1)
summarise(group_by(v,F1_C),M=mean(HUMS_unhealthy,na.rm=TRUE),SD=sd(HUMS_unhealthy,na.rm=TRUE),n=n())

m1 <- aov(HUMS_unhealthy ~ F2_C, data=v)
s1<-summary(m1,correlation=FALSE)
print(s1)
summarise(group_by(v,F2_C),M=mean(HUMS_unhealthy,na.rm=TRUE),SD=sd(HUMS_unhealthy,na.rm=TRUE),n=n())

# If you want to define different versions of the cut off points, you can just your own
# here's the example that you were thinking of. Are the participants who rated the mas28 question 4 or higher different in HUMS_unhealthy?
v$mas28_C <- cut(v$mas28,breaks = c(0,4,8)) # people who rate 4 or above in mas28
m1 <- aov(HUMS_unhealthy ~ mas28_C, data=v) # ANOVA analysis
s1<-summary(m1,correlation=FALSE)           # show result
print(s1)
summarise(group_by(v,Engaged),M=mean(HUMS_unhealthy,na.rm=TRUE),SD=sd(HUMS_unhealthy,na.rm=TRUE),n=n())

#ANOVA on HUMS unhealthy between subject gorups
m1 <- aov(HUMS_unhealthy~ EngagedC, data=v) # ANOVA analysis
s1<-summary(m1,correlation=FALSE)           # show result
print(s1)

m1 <- aov(HUMS_unhealthy ~ JustengagedC, data=v) # ANOVA analysis
s1<-summary(m1,correlation=FALSE)           # show result
print(s1)

m1 <- aov(HUMS_unhealthy ~ AddictedC, data=v) # ANOVA analysis
s1<-summary(m1,correlation=FALSE)           # show result
print(s1)

#ANOVA on HUMS healthy between subject gorups

m1 <- aov(HUMS_healthy ~ EngagedC, data=v) # ANOVA analysis
s1<-summary(m1,correlation=FALSE)           # show result
print(s1)

m1 <- aov(HUMS_healthy ~ JustengagedC, data=v) # ANOVA analysis
s1<-summary(m1,correlation=FALSE)           # show result
print(s1)

m1 <- aov(HUMS_healthy ~ AddictedC, data=v) # ANOVA analysis
s1<-summary(m1,correlation=FALSE)           # show result
print(s1)

#ANOVA on Gold-MSI AE between subject groups

m1 <- aov(MSI.AE ~ EngagedC, data=v) # ANOVA analysis
s1<-summary(m1,correlation=FALSE)           # show result
print(s1)

m1 <- aov(MSI.AE ~ JustengagedC, data=v) # ANOVA analysis
s1<-summary(m1,correlation=FALSE)           # show result
print(s1)

m1 <- aov(MSI.AE ~ AddictedC, data=v) # ANOVA analysis
s1<-summary(m1,correlation=FALSE)           # show result
print(s1)

#ANOVA on Gold-MSI MT between subject groups

m1 <- aov(MSI.MT ~ EngagedC, data=v) # ANOVA analysis
s1<-summary(m1,correlation=FALSE)           # show result
print(s1)

m1 <- aov(MSI.MT ~ JustengagedC, data=v) # ANOVA analysis
s1<-summary(m1,correlation=FALSE)           # show result
print(s1)

m1 <- aov(MSI.MT ~ AddictedC, data=v) # ANOVA analysis
s1<-summary(m1,correlation=FALSE)           # show result
print(s1)


# You would report this as: Anova analysis of HUMS_unhealthy scores across mas28 items split between those who agreed with 
# the xxxx statement (17) and others (230) suggested that their scores are significantly different, the means are 1.96 (SD=0.596) for the
# others and 2.57 (SD=0.676) for those who score above four (F[1,243]=15.86, p<.001).

