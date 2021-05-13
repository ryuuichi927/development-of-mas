# recode_instruments.R
# Part of R_template by Tuomas Eerola, https://github.com/tuomaseerola/R_template/

#### 1. Trim variables ------------------------------------------------------------
# eliminating first unnecessary columns

#### 2. Add participant ID's -------------------------------------------
v$ID <- c(1:length(v$Age)) # Status = dataframe length
v$PID <- paste("S",v$ID,sep="")
v$PID<-factor(v$PID)
ind<-colnames(v)!='ID'
v <- v[, ind]  ## Delete ID and just retain PID

#### 3. Eliminate incomplete responses ---------------------------------------
# Here we first created a row to identify the NAs (missing values) in the dataset, 
# afterwards we created a threshold of 95% completion rate. If participants completed 
# more than 95% of the survey, we keep them.

threshold<-90
v <- dplyr::filter(v,Progress>threshold)
v <- dplyr::filter(v,Time > 3 * 60) # discard those that spent less than 4 mins
v <- dplyr::filter(v,Status !='Survey Preview')
v <- dplyr::filter(v,Status !='Spam')


cat(paste('Trimmed data: N=',nrow(v)))
#hist(v$Time/60,breaks = 50)
v$Time/60
###

MAS_LEVELS <- c("Completely disagree", "Strongly disagree","Disagree","Neither agree nor disagree","Agree","Strongly agree","Completely agree")
v$mas01 <-factor(v$MAS1 ,levels = MAS_LEVELS); v$mas01 <-as.numeric(v$mas01 )
v$mas02 <-factor(v$MAS2 ,levels = MAS_LEVELS); v$mas02 <-as.numeric(v$mas02 )
v$mas03 <-factor(v$MAS3 ,levels = MAS_LEVELS); v$mas03 <-as.numeric(v$mas03 )
v$mas04 <-factor(v$MAS4 ,levels = MAS_LEVELS); v$mas04 <-as.numeric(v$mas04 )
v$mas05 <-factor(v$MAS5 ,levels = MAS_LEVELS); v$mas05 <-as.numeric(v$mas05 )
v$mas06 <-factor(v$MAS6 ,levels = MAS_LEVELS); v$mas06 <-as.numeric(v$mas06 )
v$mas07 <-factor(v$MAS7 ,levels = MAS_LEVELS); v$mas07 <-as.numeric(v$mas07 )
v$mas08 <-factor(v$MAS8 ,levels = MAS_LEVELS); v$mas08 <-as.numeric(v$mas08 )
v$mas09 <-factor(v$MAS9 ,levels = MAS_LEVELS); v$mas09 <-as.numeric(v$mas09 )
v$mas10<-factor(v$MAS10,levels = MAS_LEVELS); v$mas10<-as.numeric(v$mas10)
v$mas11<-factor(v$MAS11,levels = MAS_LEVELS); v$mas11<-as.numeric(v$mas11)
v$mas12<-factor(v$MAS12,levels = MAS_LEVELS); v$mas12<-as.numeric(v$mas12)
v$mas13<-factor(v$MAS13,levels = MAS_LEVELS); v$mas13<-as.numeric(v$mas13)
v$mas14<-factor(v$MAS14,levels = MAS_LEVELS); v$mas14<-as.numeric(v$mas14)
v$mas15<-factor(v$MAS15,levels = MAS_LEVELS); v$mas15<-as.numeric(v$mas15)
v$mas16<-factor(v$MAS16,levels = MAS_LEVELS); v$mas16<-as.numeric(v$mas16)
v$mas17<-factor(v$MAS17,levels = MAS_LEVELS); v$mas17<-as.numeric(v$mas17)
v$mas18<-factor(v$MAS18,levels = MAS_LEVELS); v$mas18<-as.numeric(v$mas18)
v$mas19<-factor(v$MAS19,levels = MAS_LEVELS); v$mas19<-as.numeric(v$mas19)
v$mas20<-factor(v$MAS20,levels = MAS_LEVELS); v$mas20<-as.numeric(v$mas20)
v$mas21<-factor(v$MAS21,levels = MAS_LEVELS); v$mas21<-as.numeric(v$mas21)
v$mas22<-factor(v$MAS22,levels = MAS_LEVELS); v$mas22<-as.numeric(v$mas22)
v$mas23<-factor(v$MAS23,levels = MAS_LEVELS); v$mas23<-as.numeric(v$mas23)
v$mas24<-factor(v$MAS24,levels = MAS_LEVELS); v$mas24<-as.numeric(v$mas24)
v$mas25<-factor(v$MAS25,levels = MAS_LEVELS); v$mas25<-as.numeric(v$mas25)
v$mas26<-factor(v$MAS26,levels = MAS_LEVELS); v$mas26<-as.numeric(v$mas26)
v$mas27<-factor(v$MAS27,levels = MAS_LEVELS); v$mas27<-as.numeric(v$mas27)
v$mas28<-factor(v$MAS28,levels = MAS_LEVELS); v$mas28<-as.numeric(v$mas28)
v<-dplyr::select(v,-starts_with("MAS",ignore.case = FALSE))

#### 3. HUMS ---------------------------------
HUMS_LEVELS <- c("Never", "Almost never","Sometimes","Often","Always")
v$hums01 <-factor(v$HUMS1,levels = HUMS_LEVELS); v$hums01<-as.numeric(v$hums01)
v$hums02 <-factor(v$HUMS2 ,levels = HUMS_LEVELS); v$hums02 <-as.numeric(v$hums02 )
v$hums03 <-factor(v$HUMS3 ,levels = HUMS_LEVELS); v$hums03 <-as.numeric(v$hums03 )
v$hums04 <-factor(v$HUMS4 ,levels = HUMS_LEVELS); v$hums04 <-as.numeric(v$hums04 )
v$hums05 <-factor(v$HUMS5 ,levels = HUMS_LEVELS); v$hums05 <-as.numeric(v$hums05 )
v$hums06 <-factor(v$HUMS6 ,levels = HUMS_LEVELS); v$hums06 <-as.numeric(v$hums06 )
v$hums07 <-factor(v$HUMS7 ,levels = HUMS_LEVELS); v$hums07 <-as.numeric(v$hums07 )
v$hums08 <-factor(v$HUMS8 ,levels = HUMS_LEVELS); v$hums08 <-as.numeric(v$hums08 )
v$hums09 <-factor(v$HUMS9 ,levels = HUMS_LEVELS); v$hums09 <-as.numeric(v$hums09 )
v$hums10<-factor(v$HUMS10,levels = HUMS_LEVELS); v$hums10<-as.numeric(v$hums10)
v$hums11<-factor(v$HUMS11,levels = HUMS_LEVELS); v$hums11<-as.numeric(v$hums11)
v$hums12<-factor(v$HUMS12,levels = HUMS_LEVELS); v$hums12<-as.numeric(v$hums12)
v$hums13<-factor(v$HUMS13,levels = HUMS_LEVELS); v$hums13<-as.numeric(v$hums13)
v<-dplyr::select(v,-starts_with("HUMS",ignore.case = FALSE))


#### 4. Goldsmith MSI scores ---------------------------------

# This is the order of items in the survey, the original order as number
# no. 1 I spend a lot of my free time doing music-related activities.
# no. 2 I enjoy writing about music, for example on blogs and forums.
# no. 3 I'm intrigued by musical styles I'm not familiar with and want to find out more.
# no. 4 I have attended _ live music events as an audience member in the past twelve months (including remote events).
# no. 5 I often read or search the internet for things related to music.
# no. 6 I don't spend much of my disposable income on music.     THIS NEEDS TO BE REVERSED
# no. 8 I listen attentively to music for _ per day.
# no. 9 I keep track of new music that I come across (e.g. new artists or recordings).
# no. 7 Music is kind of an addiction for me - I couldn't live without it.

MSI_LEVELS <- c("Completely disagree", "Strongly disagree","Disagree","Neither agree nor disagree","Agree","Strongly agree","Completely agree","prefer not to say")

v$gold1.AE <- factor(v$GOLD1.AE,levels = MSI_LEVELS); v$gold1.AE<-as.numeric(v$gold1.AE); v$gold1.AE[v$gold1.AE==8]<-NA
v$gold2.AE <- factor(v$GOLD2.AE,levels = MSI_LEVELS); v$gold2.AE<-as.numeric(v$gold2.AE); v$gold2.AE[v$gold2.AE==8]<-NA
v$gold3.AE <- factor(v$GOLD3.AE,levels = MSI_LEVELS); v$gold3.AE<-as.numeric(v$gold3.AE); v$gold3.AE[v$gold3.AE==8]<-NA
v$gold4.AE <- factor(v$GOLD4.AE,levels = c("0","1","2","3","4-6","7-10","11 or more")); v$gold4.AE<-as.numeric(v$gold4.AE); v$gold4.AE[v$gold4.AE==8]<-NA
v$gold5.AE <- factor(v$GOLD5.AE,levels = MSI_LEVELS); v$gold5.AE<-as.numeric(v$gold5.AE); v$gold5.AE[v$gold5.AE==8]<-NA
v$gold6.AE <- factor(v$GOLD6.AE,levels = MSI_LEVELS); v$gold6.AE<-as.numeric(v$gold6.AE); v$gold6.AE[v$gold6.AE==8]<-NA
v$gold7.AE <- factor(v$GOLD7.AE,levels = c("0-15 min","15-30 min","30-60 min","60-90 min","2 hrs","2-3 hrs","4 hrs or more","prefer not to say")); v$gold7.AE<-as.numeric(v$gold7.AE); v$gold7.AE[v$gold7.AE==8]<-NA
v$gold8.AE <- factor(v$GOLD8.AE,levels = MSI_LEVELS); v$gold8.AE<-as.numeric(v$gold8.AE); v$gold8.AE[v$gold8.AE==8]<-NA
v$gold9.AE <- factor(v$GOLD9.AE,levels = MSI_LEVELS); v$gold9.AE<-as.numeric(v$gold9.AE); v$gold9.AE[v$gold9.AE==8]<-NA

# This is the order of items in the survey, the original as number
# no. 3  I engaged in regular, daily practice of a musical instrument (including voice) for_ years.
# no. 4  At the peak of my interest, I practiced my primary instrument for _ hours per day.
# no. 1  I have never been complimented for my talents as a musical performer.
# no. 5  I have had formal training in music theory for _ years.
# no. 6  I have had _ years of formal training on a musical instrument (including voice) during my lifetime.
# no. 7  I can play _ musical instruments.
# no. 2  I would not consider myself a musician.

v$gold1.MT <- factor(v$GOLD1.MT,levels = c("0","1", "2", "3", "4-5","6-9","10 or more")); v$gold1.MT<-as.numeric(v$gold1.MT); v$gold1.MT[v$gold1.MT==8]<-NA
v$gold2.MT <- factor(v$GOLD2.MT,levels = c("0","0.5", "1", "1.5", "2","3-4","5 or more")); v$gold2.MT<-as.numeric(v$gold2.MT); v$gold2.MT[v$gold2.MT==8]<-NA
v$gold3.MT <- factor(v$GOLD3.MT,levels = MSI_LEVELS); v$gold3.MT<-as.numeric(v$gold3.MT); v$gold3.MT[v$gold3.MT==8]<-NA
v$gold4.MT <- factor(v$GOLD4.MT,levels = c("0","0.5", "1", "2", "3","4-6","7 or more")); v$gold4.MT<-as.numeric(v$gold4.MT); v$gold4.MT[v$gold4.MT==8]<-NA
v$gold5.MT <- factor(v$GOLD5.MT,levels = c("0","0.5", "1", "2", "3-5","6-9","10 or more")); v$gold5.MT<-as.numeric(v$gold5.MT); v$gold5.MT[v$gold5.MT==8]<-NA
v$gold6.MT <- factor(v$GOLD5.MT,levels = c("0","1", "2", "3", "4","5","6 or more")); v$gold6.MT<-as.numeric(v$gold6.MT); v$gold6.MT[v$gold6.MT==8]<-NA
v$gold7.MT <- factor(v$GOLD7.MT,levels = MSI_LEVELS); v$gold7.MT<-as.numeric(v$gold7.MT); v$gold7.MT[v$gold7.MT==8]<-NA

#### Reverse some items in MSI --------------
# Active engagement, it is the 6th item (I don't spend much of my disposable income on music.)
v$gold6.AE <- 8 - v$gold6.AE
# Musical training: Reverse items are questions 1 and 2 in the original, which are 3 and 7 here
v$gold3.MT <- 8 - v$gold3.MT
v$gold7.MT <- 8 - v$gold7.MT

#### Summarise the scores ---------------
v$MSI.AE <- round(rowMeans(v[,which(names(v)=='gold1.AE'):which(names(v)=='gold9.AE')],na.rm = TRUE)*9)
v$MSI.MT <- round(rowMeans(v[,which(names(v)=='gold1.MT'):which(names(v)=='gold7.MT')],na.rm = TRUE)*7)

#### And finally delete the unnecessary stuff
v<-dplyr::select(v,-starts_with("gold"))

cat("\n===============Recoding done!========================")
