# factor_analysis.R
# This is incomplete and premature!
# T. Eerola, 4/5/2021

library(psych)

#### FIRST EFA: HOW MANY FACTORS WITHIN MAS ITEMS?

#df <- v[,which(names(v)=='mas01'):which(names(v)=='hums13')]
df <- v[,which(names(v)=='mas01'):which(names(v)=='mas28')]

# deal with the missing numbers
library(randomForest)
df <- na.roughfix(df)

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

#### Normalise the scale ------------------------------
df_normed <- lapply(df, scale_this)
df_normed <- data.frame(df_normed)

kmo <- KMO(df_normed)
cat(paste("KMO all:",round(kmo$MSA,3), "item. min.:",round(min(kmo$MSAi),3),"max:",round(max(kmo$MSAi),3),"\n"))


#### How many factors or components are there in the data? -----------------------
fa.paral <- fa.parallel(df_normed, fm="minres",n.iter=1000,quant = .95,cor = "cor")
# 2 factors or 2 components

### FA - factor analysis run with different scale3 (above). 
##Other scale variations blocked out below
fit1 <- fa(df_normed,2,rotate="varimax")
print(fit1,sort=TRUE,cut = .30,digits=3) # Fit based upon off diagonal values = 0.999, TLI = 0.9592
# RMSEA index =  0.0685  and the 90 % confidence intervals are 0.0643 0.0726
knitr::kable(data.frame(psych::fa.sort(fit1$loadings)[,]),digits = 2)


## Eliminate items that are cross-loaded
crossloaded<-c("mas12", "mas15","mas27","mas11","mas03","mas02", "mas22","mas10")

ind <-which(names(v)=='mas01'):which(names(v)=='mas28')
cl <- which(names(v) %in% crossloaded)
trimmed<-setdiff(ind,cl)
df_trimmed <- v[,trimmed]
df_normed_t <- lapply(df_trimmed, scale_this)
df_normed_t <- data.frame(df_normed_t)

kmo <- KMO(df_normed_t)
cat(paste("KMO all:",round(kmo$MSA,3), "item. min.:",round(min(kmo$MSAi),3),"max:",round(max(kmo$MSAi),3),"\n"))


#### How many factors or components are there in the data? -----------------------
fa.paral <- fa.parallel(df_normed_t, fm="minres",n.iter=1000,quant = .95,cor = "cor")
# 2 factors or 2 components

fit1 <- fa(df_normed_t,2,rotate="varimax")
print(fit1,sort=TRUE,cut = .30,digits=3) # Fit based upon off diagonal values = 0.999, TLI = 0.9592
knitr::kable(data.frame(psych::fa.sort(fit1$loadings)[,]),digits = 2)

#####################################

v$F1 <- fit1$scores[,1]
v$F2 <- fit1$scores[,2]

## HUMS facets
v$HUMS_unhealthy <- (v$hums01 + v$hums02 + v$hums03 + v$hums04 + v$hums05 + v$hums06 + v$hums07 + v$hums08)/8
v$HUMS_healthy <- (v$hums09 + v$hums10 + v$hums11 + v$hums12 + v$hums13)/5

##MAS dimensions and core criteria
v$MAS_Salience <- (v$mas01 + v$mas02 + v$mas03 + v$mas04)/4
v$MAS_Moodmodification <- (v$mas05 + v$mas06 + v$mas07 + v$mas08)/4
v$MAS_Tolerance <- (v$mas09 + v$mas10 + v$mas11 + v$mas12)/4
v$MAS_Conflict <- (v$mas13 + v$mas14 + v$mas15 + v$mas16)/4
v$MAS_Relapse <- (v$mas17 + v$mas18 + v$mas19 + v$mas20)/4
v$MAS_Withdrawal <- (v$mas21 + v$mas22 + v$mas23 + v$mas24)/4
v$MAS_Problems <- (v$mas25 + v$mas26 + v$mas27 + v$mas28)/4

v$MAS_Addictivecore <- (v$MAS_Conflict + v$MAS_Relapse + v$MAS_Withdrawal + v$MAS_Problems)/4
v$MAS_Engagementcore <- (v$MAS_Salience + v$MAS_Moodmodification + v$MAS_Tolerance)/3

##Trimmed MAS dimensions and core criteria, with high cross loading removed
v$trimmedMAS_Salience <- (v$mas01 + v$mas04)/2
v$MAS_Moodmodification <- (v$mas05 + v$mas06 + v$mas07 + v$mas08)/4
#v$MAS_Tolerance <- (v$mas09 + v$mas10 + v$mas11 + v$mas12)/4
v$trimmedMAS_Conflict <- (v$mas13 + v$mas14 + v$mas16)/3
v$trimmedMAS_Relapse <- (v$mas17 + v$mas18 + v$mas20)/3
v$trimmedMAS_Withdrawal <- (v$mas21  + v$mas23 + v$mas24)/3
v$trimmedMAS_Problems <- (v$mas25 + v$mas26 + v$mas28)/3

v$trimmedMAS_Addictivecore <- (v$trimmedMAS_Conflict + v$trimmedMAS_Relapse + v$trimmedMAS_Withdrawal + v$trimmedMAS_Problems)/4
v$trimmedMAS_Engagementcore <- (v$trimmedMAS_Salience + v$MAS_Moodmodification)/2

####  Correlations between MAS factors and other constructs -----------------------------

tmp<-data.frame(v$F1,v$F2,v$HUMS_healthy,v$HUMS_unhealthy,v$MSI.AE,v$MSI.MT)
head(tmp)
cm <- cor(tmp,use = "pairwise.complete.obs")
rownames(cm)<-c('MAS-F1','MAS-F2','HUMS-He.','HUMS-Unhe.','MSI-AE','MSI-MT')
colnames(cm)<-c('MAS-F1','MAS-F2','HUMS-He.','HUMS-Unhe.','MSI-AE','MSI-MT')

knitr::kable(cm,digits = 2)

gr <- colorRampPalette(c("#B52127", "white", "#2171B5"))

pdf('corrplot.pdf',height = 9, width = 9)
psych::cor.plot(cm,stars = TRUE,upper = FALSE,diag=FALSE,show.legend=FALSE,gr=gr)
dev.off()

colnames(tmp)<-c('MAS.F1','MAS.F2','HUMS.He','HUMS.Unhe','MSI.AE','MSI.MT')

head(tmp)
head(v)

m1 <- lm(F1 ~ F2 + HUMS_unhealthy + HUMS_healthy + MSI.AE + MSI.MT, data = v)
summary(m1)
knitr::kable(papaja::apa_print(summary(m1)))

m2 <- lm(F2 ~ HUMS_unhealthy + HUMS_healthy + MSI.AE + MSI.MT, data = v)
summary(m2)
knitr::kable(papaja::apa_print(summary(m2)))

### Added trimmed two MAS core criteria into the correlation analysis


tmp<-data.frame(v$F1,v$F2,v$trimmedMAS_Addictivecore,v$trimmedMAS_Engagementcore,v$HUMS_healthy,v$HUMS_unhealthy,v$MSI.AE,v$MSI.MT)
head(tmp)
cm <- cor(tmp,use = "pairwise.complete.obs")
rownames(cm)<-c('MAS-F1','MAS-F2','MAS-Addictive','MAS-Engagement','HUMS-He.','HUMS-Unhe.','MSI-AE','MSI-MT')
colnames(cm)<-c('MAS-F1','MAS-F2','MAS-Addictive','MAS-Engagement','HUMS-He.','HUMS-Unhe.','MSI-AE','MSI-MT')

knitr::kable(cm,digits = 2)


col1 = c(v$F1,v$F2,v$trimmedMAS_Addictivecore,v$trimmedMAS_Engagementcore,v$HUMS_healthy,v$HUMS_unhealthy,)
col2 = c(v$F1,v$F2,v$trimmedMAS_Addictivecore,v$trimmedMAS_Engagementcore,v$HUMS_healthy,v$HUMS_unhealthy,)
cor.test(v$MAS_Tolerance,v$MAS_Problems)


m1 <- lm(F1 ~ F2 + trimmedMAS_Addictivecore + trimmedMAS_Engagementcore + HUMS_healthy + HUMS_unhealthy + MSI.AE + MSI.MT, data =)
summary(m1)
knitr::kable(papaja::apa_print(summary(m1)))

m2 <- lm(F2 ~ trimmedMAS_Addictivecore + trimmedMAS_Engagementcore + HUMS_unhealthy + HUMS_healthy + MSI.AE + MSI.MT, data = v)
summary(m2)
knitr::kable(papaja::apa_print(summary(m2)))

###  Correlation between each MAS dimensions and core criteria

tmp<-data.frame(v$MAS_Addictivecore,v$MAS_Engagementcore,v$MAS_Salience,v$MAS_Moodmodification,v$MAS_Tolerance,v$MAS_Conflict,v$MAS_Relapse,v$MAS_Withdrawal,v$MAS_Problems)
head(tmp)
cm <- cor(tmp,use = "pairwise.complete.obs")
rownames(cm)<-c('MAS-Addictive','MAS-Engagement','MAS_Salience','MAS_Moodmodification','MAS_Tolerance','MAS_Conflict','MAS_Relapse','MAS_Withdrawal','MAS_Problems')
colnames(cm)<-c('MAS-Addictive','MAS-Engagement','MAS_Salience','MAS_Moodmodification','MAS_Tolerance','MAS_Conflict','MAS_Relapse','MAS_Withdrawal','MAS_Problems')

knitr::kable(cm,digits = 2)




