
###########################
#####Recode for easier proportion calculations
###########################
smoke$packsWeek <- ifelse( smoke$s4_1 > 9, 1, 0)
smoke$wakeupSmoke <- ifelse( as.numeric(smoke$q2e)== 1 | as.numeric(smoke$q2e)==2 , 1, 0)
smoke$pastsmokeIllness <- ifelse( as.numeric(smoke$q8e)== 1 , 1, 0)
smoke$currentsmokeIllness <- ifelse( as.numeric(smoke$q9e)== 1 , 1, 0)
smoke$pVulnerability <- ifelse( as.numeric(smoke$PVSelf) > 14 , 1, 0)
smoke$pExpectation <- ifelse( as.numeric(smoke$PESelf) > 8 , 1, 0)
smoke$optimBias <- ifelse( as.numeric(smoke$OBRecode) == 2 , 1, 0)
smoke$develIllness <- ifelse( as.numeric(smoke$q2gRecode) == 1 , 1, 0)
smoke$cesDepression <- ifelse( as.numeric(smoke$Depressed) == 2 , 1, 0)
smoke$wantQuit <- ifelse( as.numeric(smoke$q1aa_1) > 5 , 1, 0)
smoke$confQuit <- ifelse( as.numeric(smoke$q2aa_1) > 5 , 1, 0)
smoke$partnerSmoke <- ifelse( as.numeric(smoke$q4e_1Recode) == 2, 1, 0)
smoke$triedQuit <- ifelse( as.numeric(smoke$q6e) == 2, 1, 0)
smoke$employ <- ifelse( as.numeric(smoke$c3) == 2, 1, 0)
smoke$children <- ifelse( as.numeric(smoke$c6) == 2, 1, 0)
smoke$ethnicity <- ifelse( as.numeric(smoke$c2) == 1, 1, 0)
smoke$gender <- ifelse( as.numeric(smoke$s1) == 2, 1, 0)
smoke$country <- ifelse( as.numeric(smoke$country) == 1, 1, 0)
smoke$age <- ifelse( as.numeric(smoke$s2_1) < 40, 0, 1)
smoke$smokeHousehold <- ifelse( as.numeric(smoke$q3e) > 1, 1, 0)
smoke$timesQuit <- 0
smoke$timesQuit <- ifelse( (is.na(smoke$q7e_1)==FALSE) & (as.numeric(smoke$q7e_1) > 1), 1, smoke$timesQuit)




##############################
##Class Proportions
##############################
#Switch the binary coding to 0/1 for easy proportion calculations
classID <- LCA_best$predclass
dataSm <- smoke[,c(295:309, 313:314)]

####Table of means and comparisons
table(classID)
total.case <- cbind(rep(166, 17), rep(192,17), rep(142,17))
case <- cbind(unname(unlist(lapply(dataSm[classID==1,], sum))),
              unname(unlist(lapply(dataSm[classID==2,], sum))),
              unname(unlist(lapply(dataSm[classID==3,], sum))))
propTables <- matrix(NA, nrow=17, ncol=11)
for (m in 1:17){
  propTables[m,1] <- names(dataSm)[m]
  propTables[m,2] <- round(unname(prop.test(case[m,], total.case[m,])$est[1]), 3)
  propTables[m,3] <- round(unname(prop.test(case[m,], total.case[m,])$est[2]), 3)
  propTables[m,4] <- round(unname(prop.test(case[m,], total.case[m,])$est[3]), 3)
  propTables[m,5] <- (unname(prop.test(case[m,], total.case[m,])$p.value))
  propTables[m,6] <- (unname(prop.test(case[m,1:2], total.case[m,1:2])$p.value))
  propTables[m,7] <- (unname(prop.test(case[m,c(1,3)], total.case[m,c(1,3)])$p.value))
  propTables[m,8] <- (unname(prop.test(case[m,2:3], total.case[m,2:3])$p.value))
  if (unname(prop.test(case[m,], total.case[m,])$est[1]) > unname(prop.test(case[m,], total.case[m,])$est[2])){
    propTables[m,9] <- (unname(prop.test(case[m,1:2], total.case[m,1:2], alternative="greater")$p.value))
  } else {
    propTables[m,9] <- (unname(prop.test(case[m,1:2], total.case[m,1:2], alternative="less")$p.value))
  }
  if (unname(prop.test(case[m,], total.case[m,])$est[1]) > unname(prop.test(case[m,], total.case[m,])$est[3])){
    propTables[m,10] <- (unname(prop.test(case[m,c(1,3)], total.case[m,c(1,3)], alternative="greater")$p.value))
  } else {
    propTables[m,10] <- (unname(prop.test(case[m,c(1,3)], total.case[m,c(1,3)], alternative="less")$p.value))
  }
  if (unname(prop.test(case[m,], total.case[m,])$est[2]) > unname(prop.test(case[m,], total.case[m,])$est[3])){
    propTables[m,11] <- (unname(prop.test(case[m,2:3], total.case[m,2:3], alternative="greater")$p.value))
  } else {
    propTables[m,11] <- (unname(prop.test(case[m,2:3], total.case[m,2:3], alternative="less")$p.value))
  }
}
write.csv(propTables, "propTables.csv")




library(ggplot2)
plotTables <- data.frame(cbind(rep(1:17,3), c(propTables[c(14,15,1,2,13,17,12,16,3,4,10,11,9,5,6,7,8),2], 
                                              propTables[c(14,15,1,2,13,17,12,16,3,4,10,11,9,5,6,7,8),3], propTables[c(14,15,1,2,13,17,12,16,3,4,10,11,9,5,6,7,8),4]),
                               c(rep("HCS",17), rep("SPB",17), rep("UCS",17))))
names(plotTables) <- c("ind", "prop", "grp")
plotTables$grp <- as.factor(plotTables$grp)
plotTables$ind <- as.numeric(as.character(plotTables$ind))
plotTables$prop <- as.numeric(as.character(sub("," , ".", plotTables$prop)))

ggplot(data=plotTables, aes(x=ind, y=prop, group=grp, colour=grp)) +
  geom_line(aes(linetype=grp), size=.8) +
  geom_point(aes(shape=grp, size=1)) +
  scale_color_manual(values=c("dodgerblue4",
                              "firebrick3", "goldenrod1")) +
  scale_x_discrete(breaks= c(1:17),
                   labels=c("Employed", "No children", " >=10 Cigarettes/Day", "Smoke within 30min waking",
                            "Never tried quitting", ">1 Quit attempts last year", "Partner smokes", ">1 Person smokes in home",
                            "Past Illness", "Current Illness", "Want to Quit", "Confidence to Quit",
                            "Depression", "Perceived Vulnerability", "Outcome Expectation", "Optimistic Bias",
                            "Serious Illness") ) +
  scale_y_continuous(breaks= seq(0,1,.1), labels=seq(0,1,.1)) +            
  labs(x="", y="Proportion") + 
  theme(legend.title=element_blank(), legend.text =  element_text(size = rel(1.8)),
        axis.text.x = element_text(angle=45, hjust=1, size=26 ),
        axis.text.y = element_text(size=24 ),
        axis.title=element_text(size=24)) 





##############################
##Demographic Proportions
##############################
classID <- LCA_best$predclass
smoke[,6] <- ifelse( as.numeric(smoke[,6]) == 2, 0, 1)
smoke[,284] <- ifelse( as.numeric(smoke[,284]) == 2, 0, 1)
smoke[,285] <- ifelse( as.numeric(smoke[,285]) == 2, 0, 1)
smoke[,286] <- ifelse( as.numeric(smoke[,286]) == 2, 0, 1)
smoke[,289] <- ifelse( as.numeric(smoke[,289]) == 2, 0, 1)
smoke[,291] <- ifelse( as.numeric(smoke[,291]) == 2, 0, 1)
dataDemo <- smoke[,c(6, 2, 284:287, 289, 291 )]


####Table of means and comparisons
table(classID)
total.case <- cbind(rep(166, 17), rep(192,17), rep(142,17))
case <- cbind(unname(unlist(lapply(dataDemo[classID==1,], sum))),
              unname(unlist(lapply(dataDemo[classID==2,], sum))),
              unname(unlist(lapply(dataDemo[classID==3,], sum))))
propTables <- matrix(NA, nrow=8, ncol=12)
for (m in 1:8){
  propTables[m,1] <- names(dataDemo)[m]
  propTables[m,2] <- paste( round(rowSums(case)[m]/500, 3), " (", rowSums(case)[m], ")", sep="") 
  propTables[m,3] <- paste( round(unname(prop.test(case[m,], total.case[m,])$est[1]), 3), " (", case[m,1], ")", sep="")
  propTables[m,4] <- paste( round(unname(prop.test(case[m,], total.case[m,])$est[2]), 3), " (", case[m,2], ")", sep="")
  propTables[m,5] <- paste( round(unname(prop.test(case[m,], total.case[m,])$est[3]), 3), " (", case[m,3], ")", sep="")
  propTables[m,6] <- (unname(prop.test(case[m,], total.case[m,])$p.value))
  propTables[m,7] <- (unname(prop.test(case[m,1:2], total.case[m,1:2])$p.value))
  propTables[m,8] <- (unname(prop.test(case[m,c(1,3)], total.case[m,c(1,3)])$p.value))
  propTables[m,9] <- (unname(prop.test(case[m,2:3], total.case[m,2:3])$p.value))
  if (unname(prop.test(case[m,], total.case[m,])$est[1]) > unname(prop.test(case[m,], total.case[m,])$est[2])){
    propTables[m,10] <- (unname(prop.test(case[m,1:2], total.case[m,1:2], alternative="greater")$p.value))
  } else {
    propTables[m,10] <- (unname(prop.test(case[m,1:2], total.case[m,1:2], alternative="less")$p.value))
  }
  if (unname(prop.test(case[m,], total.case[m,])$est[1]) > unname(prop.test(case[m,], total.case[m,])$est[3])){
    propTables[m,11] <- (unname(prop.test(case[m,c(1,3)], total.case[m,c(1,3)], alternative="greater")$p.value))
  } else {
    propTables[m,11] <- (unname(prop.test(case[m,c(1,3)], total.case[m,c(1,3)], alternative="less")$p.value))
  }
  if (unname(prop.test(case[m,], total.case[m,])$est[2]) > unname(prop.test(case[m,], total.case[m,])$est[3])){
    propTables[m,12] <- (unname(prop.test(case[m,2:3], total.case[m,2:3], alternative="greater")$p.value))
  } else {
    propTables[m,12] <- (unname(prop.test(case[m,2:3], total.case[m,2:3], alternative="less")$p.value))
  }
}

write.csv(propTables, "DemoTables.csv")

# Demographic table - continuous:
gr1 <- smoke[LCA_best$predclass==1,]
gr2 <- smoke[LCA_best$predclass==2,]
gr3 <- smoke[LCA_best$predclass==3,]

##Age s2_1
c(mean(smoke$s2_1), sd(smoke$s2_1))
c(mean(gr1$s2_1), sd(gr1$s2_1))
c(mean(gr2$s2_1), sd(gr2$s2_1))
c(mean(gr3$s2_1), sd(gr3$s2_1))
t.test(gr1$s2_1, gr2$s2_1)$p.value
t.test(gr1$s2_1, gr3$s2_1)$p.value
t.test(gr2$s2_1, gr3$s2_1)$p.value

t.test(gr1$s2_1, gr2$s2_1, alternative="greater")$p.value
t.test(gr1$s2_1, gr3$s2_1, alternative="less")$p.value
t.test(gr2$s2_1, gr3$s2_1, alternative="less")$p.value

dat = data.frame(factor=c(rep("A",166), rep("B",192), rep("C",142)), response=c(gr1$s2_1,gr2$s2_1,gr3$s2_1))
summary(aov(response~factor, data=dat))

##Cigs per day s4_1
c(mean(smoke$s4_1), sd(smoke$s4_1))
c(mean(gr1$s4_1), sd(gr1$s4_1))
c(mean(gr2$s4_1), sd(gr2$s4_1))
c(mean(gr3$s4_1), sd(gr3$s4_1))
t.test(gr1$s4_1, gr2$s4_1)$p.value
t.test(gr1$s4_1, gr3$s4_1)$p.value
t.test(gr2$s4_1, gr3$s4_1)$p.value

t.test(gr1$s4_1, gr2$s4_1, alternative="less")$p.value
t.test(gr1$s4_1, gr3$s4_1, alternative="greater")$p.value
t.test(gr2$s4_1, gr3$s4_1, alternative="greater")$p.value

dat = data.frame(factor=c(rep("A",166), rep("B",192), rep("C",142)), response=c(gr1$s4_1,gr2$s4_1,gr3$s4_1))
summary(aov(response~factor, data=dat))
