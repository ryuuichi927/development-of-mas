# interrater_reliability.R
# Part of R_template by Tuomas Eerola, https://github.com/tuomaseerola/R_template/

#### 1. Response times --------------------------------------------------------------------
# let's see how fast some people were (this could be related to quality)

print(paste("Fastest response",round(min(v$Time/60,na.rm = T),2),'mins')) # in minutes
print(paste("Slowest response",round(max(v$Time/60,na.rm = T),2),'mins')) # in minutes
print(paste("Median response",round(median(v$Time/60,na.rm = T),2),'mins')) # in minutes

fastest <- which(min(v$Time,na.rm = T)==v$Time)
as.character(v$PID[fastest[1]]) # PID

slowest <- which(max(v$Time, na.rm = T)==v$Time)
as.character(v$PID[slowest[1]]) # PID

# OK, let's keep them in for now.

#### 2. Distributions ---------------------------------------------------------------------

# Do we have participants who use the scale in weird ways?
library(tidyr)
n<-names(v)


pivoted <- pivot_longer(v,n[27:54],names_to = 'MAS',values_to = 'Rating')

max(pivoted$Rating,na.rm=T)
min(pivoted$Rating,na.rm=T)

g1<-ggplot(pivoted,aes(Rating))+
  geom_histogram(bins = 7,colour='black',fill='lightblue')+
  scale_x_continuous(breaks = 1:7)+
  facet_wrap(.~MAS)+
  theme_bw()

print(g1)

n
n[44:56]
pivoted2 <- pivot_longer(v,n[44:56],names_to = 'HUMS',values_to = 'Rating')
max(pivoted2$Rating,na.rm = T)
g2<-ggplot(pivoted2,aes(Rating))+
  geom_histogram(bins = 5,colour='black',fill='orange')+
  scale_x_continuous(breaks = 1:5)+
  facet_wrap(.~HUMS)+
  theme_bw()
print(g2)

names(v)

# OK, none eliminated because of the scale use. 
# Let's see next whether we have some "random" responders

#### 3. Inter-rater reliability for all scales using Cronbach Alphas -------------------------

# Don't know how scripts below are supposed to work, thus using psych alpha() function
#Reliability 28 MAS items
alpha(df)

#Reliability 20 MAS items with cross-loading items removed
alpha(df_normed_t)


# return
# 
# U<-unique(v$MAS)
# S<-unique(v$PID)
# alpha<-matrix(0,1,length(U))
# for (k in 1:length(U)) {
# #  print(as.character(U[k]))
#   B <- dplyr::filter(v,MAS==U[k])
#   TMP<-NULL
#   for (i in 1:length(S)) {
#     tmp<-dplyr::filter(B,PID==S[i])
#     TMP<-cbind(TMP,as.numeric(tmp$Rating))
#   }
#   colnames(TMP)<-S
#   a<-suppressMessages(suppressWarnings(psych::alpha(TMP,check.keys = FALSE,warnings = FALSE))) # t transpose
#   alpha[k]<-a$total$raw_alpha
# }
# 
# colnames(alpha)<-U
# print(knitr::kable(alpha,digits=3,caption='Inter-reliability ratings (Cronbach alphas)'))
# 
# # We can explore the individuals who do not "conform" in more detail after this operation.
# # Once the inconsistent participants have been identified, they can be discarded provided that you
# # document and argue the elimination on a sound basis.
# 
# #### 4. Clean --------------------------------------------------------------------
# rm(U,alpha,TMP,a,B,S,fastest,slowest,i,k,tmp,g1)
