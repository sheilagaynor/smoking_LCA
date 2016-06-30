##############################
########Load data/package#####
##############################
library(foreign)
library(poLCA)
setwd("~/Desktop")
smoke <- as.data.frame(read.spss("MERGED UK + US- RECODED FOR DESCRIPTIVES 26_06_15.sav"))
#Get only those who are unmotivated to quit
smoke <- smoke[as.numeric(smoke$s6a)==2,]




##############################
##########Clean varables######
##############################
#Make demographic by merging US/UK variables
smoke$c1 <- rep(NA, 500)
smoke$c2 <- rep(NA, 500)
smoke$c3 <- rep(NA, 500)
smoke$c4 <- rep(NA, 500)
smoke$c5 <- rep(NA, 500)
smoke$c6 <- rep(NA, 500)

for (p in 1:500){
  if (smoke$country[p] == "US"){
    smoke$c1[p] <- ifelse( (as.numeric(smoke$us1)[p]==1 | as.numeric(smoke$us1)[p]==2 | as.numeric(smoke$us1)[p]==4), 2, 1)
    smoke$c2[p] <- ifelse( smoke$us2[p] == "Caucasian/White", 2, 1)
    smoke$c3[p] <- ifelse( (as.numeric(smoke$us3)[p]==1 | as.numeric(smoke$us3)[p]==6), 2, 1)
    smoke$c4[p] <- ifelse( (as.numeric(smoke$us4)[p]==2 | as.numeric(smoke$us4)[p]==6 | as.numeric(smoke$us4)[p]==7), 1, 0)
    smoke$c5[p] <- ifelse( (as.numeric(smoke$us5)[p]==1 | as.numeric(smoke$us5)[p]==2), 2, 1)
    smoke$c6[p] <- ifelse( (as.numeric(smoke$us6)[p]==1), 2, 1)
  } else {
    smoke$c1[p] <- ifelse( (as.numeric(smoke$uk1)[p]==1 | as.numeric(smoke$uk1)[p]==2), 2, 1)
    smoke$c2[p] <- ifelse( (as.numeric(smoke$uk2)[p]==1 | as.numeric(smoke$uk2)[p]==2 | as.numeric(smoke$uk2)[p]==3), 2, 1)
    smoke$c3[p] <- ifelse( (as.numeric(smoke$uk3)[p]==1 | as.numeric(smoke$uk3)[p]==6), 2, 1)
    smoke$c4[p] <- ifelse( (as.numeric(smoke$uk4)[p]==2 | as.numeric(smoke$uk4)[p]==6 | as.numeric(smoke$uk4)[p]==7), 1, 0)
    smoke$c5[p] <- ifelse( (as.numeric(smoke$uk5)[p]==1 | as.numeric(smoke$uk5)[p]==2 | as.numeric(smoke$uk5)[p]==3), 2, 1)
    smoke$c6[p] <- ifelse( (as.numeric(smoke$uk7)[p]==1), 2, 1)
  }}

smoke$q2gRecode <- ifelse( as.numeric(smoke$q2g)==1, 2, 1)
smoke$q2eRecode <- ifelse( as.numeric(smoke$q2e)==1 | as.numeric(smoke$q2e)==2 , 1, 2)
smoke$OBRecode <- ifelse( as.numeric(smoke$q1h_1)==1 | as.numeric(smoke$q1h_1)==2 |
                            as.numeric(smoke$q1h_2)==1 | as.numeric(smoke$q1h_2)==2 |
                            as.numeric(smoke$q1h_3)==1 | as.numeric(smoke$q1h_3)==2 
                          , 2, 1)
smoke$q3eRecode <- as.numeric(smoke$q3e)
smoke$q4e_1Recode <- ifelse(is.na(smoke$q4e_1)==TRUE, 1, smoke$q4e_1)




##############################
#######Make binary varables###
##############################
##Note that variables must all be positive integers for poLCA
##Thus we run with this code, of 1/2 and then immediately switch to 1/0 
##for easier proportions
smoke$packsWeek <- ifelse( smoke$s4_1 > 9, 1, 2)
smoke$wakeupSmoke <- ifelse( as.numeric(smoke$q2e)== 1 | as.numeric(smoke$q2e)==2 , 1, 2)
smoke$pastsmokeIllness <- ifelse( as.numeric(smoke$q8e)== 1 , 1, 2)
smoke$currentsmokeIllness <- ifelse( as.numeric(smoke$q9e)== 1 , 1, 2)
smoke$pVulnerability <- ifelse( as.numeric(smoke$PVSelf) > 14 , 1, 2)
smoke$pExpectation <- ifelse( as.numeric(smoke$PESelf) > 8 , 1, 2)
smoke$optimBias <- ifelse( as.numeric(smoke$OBRecode) == 2 , 1, 2)
smoke$develIllness <- ifelse( as.numeric(smoke$q2gRecode) == 1 , 1, 2)
smoke$cesDepression <- ifelse( as.numeric(smoke$Depressed) == 2 , 1, 2)
smoke$wantQuit <- ifelse( as.numeric(smoke$q1aa_1) > 5 , 1, 2)
smoke$confQuit <- ifelse( as.numeric(smoke$q2aa_1) > 5 , 1, 2)
smoke$partnerSmoke <- ifelse( as.numeric(smoke$q4e_1Recode) == 2, 1, 2)
smoke$triedQuit <- ifelse( as.numeric(smoke$q6e) == 2, 1, 2)
smoke$employ <- ifelse( as.numeric(smoke$c3) == 2, 1, 2)
smoke$children <- ifelse( as.numeric(smoke$c6) == 2, 1, 2)
smoke$ethnicity <- ifelse( as.numeric(smoke$c2) == 1, 1, 2)
smoke$gender <- ifelse( as.numeric(smoke$s1) == 2, 1, 2)
smoke$country <- ifelse( as.numeric(smoke$country) == 1, 1, 2)
smoke$age <- ifelse( as.numeric(smoke$s2_1) < 40, 2, 1)
smoke$smokeHousehold <- ifelse( as.numeric(smoke$q3e) > 1, 1, 2)
smoke$timesQuit <- 2
smoke$timesQuit <- ifelse( (is.na(smoke$q7e_1)==FALSE) & (as.numeric(smoke$q7e_1) > 1), 1, smoke$timesQuit)
