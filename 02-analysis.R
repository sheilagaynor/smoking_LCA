##############################
##Response Probabilities
##############################
####Table of means and p-values
table(LCA_best$predclass)

propTables <- matrix(NA, nrow=16, ncol=8)
for (m in 1:16){
  propTables[m,1] <- names(LCA_best$probs)[m]
  propTables[m,2] <- paste(round( LCA_best$probs[[m]][1,1] , 3), " (",
                           round( LCA_best$probs.se[[m]][1,1] , 3), ")",sep="")
  propTables[m,3] <- paste(round( LCA_best$probs[[m]][2,1] , 3), " (",
                           round( LCA_best$probs.se[[m]][2,1] , 3), ")",sep="")
  propTables[m,4] <- paste(round( LCA_best$probs[[m]][3,1] , 3), " (",
                           round( LCA_best$probs.se[[m]][3,1] , 3), ")",sep="")
  propTables[m,5] <- paste(prop.test(as.numeric(LCA_best$probs[[m]][,1]*table(LCA_best$predclass)), 
                                       as.numeric(table(LCA_best$predclass)))$p.value, " (",
                           round(prop.test(as.numeric(LCA_best$probs[[m]][,1]*table(LCA_best$predclass)),
                                     as.numeric(table(LCA_best$predclass)))$statistic,3), ")", sep="")
  
  if (LCA_best$probs[[m]][1,1] > LCA_best$probs[[m]][2,1]  ){
    propTables[m,6] <- prop.test( (as.numeric(LCA_best$probs[[m]][,1]*table(LCA_best$predclass)))[1:2],
                               as.numeric(table(LCA_best$predclass))[1:2], alternative="greater")$p.value
  } else {
    propTables[m,6] <- prop.test( (as.numeric(LCA_best$probs[[m]][,1]*table(LCA_best$predclass)))[1:2],
                                  as.numeric(table(LCA_best$predclass))[1:2], alternative="less")$p.value }
  
  if (LCA_best$probs[[m]][1,1] > LCA_best$probs[[m]][3,1]  ){
    propTables[m,7] <- prop.test( (as.numeric(LCA_best$probs[[m]][,1]*table(LCA_best$predclass)))[c(1,3)],
                                  as.numeric(table(LCA_best$predclass))[c(1,3)], alternative="greater")$p.value
    } else {
    propTables[m,7] <- prop.test( (as.numeric(LCA_best$probs[[m]][,1]*table(LCA_best$predclass)))[c(1,3)],
                                  as.numeric(table(LCA_best$predclass))[c(1,3)], alternative="less")$p.value}
  
  if (LCA_best$probs[[m]][2,1] > LCA_best$probs[[m]][3,1]  ){
    propTables[m,8] <- prop.test( (as.numeric(LCA_best$probs[[m]][,1]*table(LCA_best$predclass)))[2:3],
                                  as.numeric(table(LCA_best$predclass))[2:3], alternative="greater")$p.value
  } else {
    propTables[m,8] <- prop.test( (as.numeric(LCA_best$probs[[m]][,1]*table(LCA_best$predclass)))[2:3],
                                  as.numeric(table(LCA_best$predclass))[2:3], alternative="less")$p.value}
 }
write.csv(propTables[c(14,15,1,2,13,12,16,3,4,10,11,9,5,6,7,8),], "pvalTables.csv")


####Table of means and test statistics
propTables <- matrix(NA, nrow=16, ncol=8)
for (m in 1:16){
  propTables[m,1] <- names(LCA_best$probs)[m]
  propTables[m,2] <- paste(round( LCA_best$probs[[m]][1,1] , 3), " (",
                           round( LCA_best$probs.se[[m]][1,1] , 3), ")",sep="")
  propTables[m,3] <- paste(round( LCA_best$probs[[m]][2,1] , 3), " (",
                           round( LCA_best$probs.se[[m]][2,1] , 3), ")",sep="")
  propTables[m,4] <- paste(round( LCA_best$probs[[m]][3,1] , 3), " (",
                           round( LCA_best$probs.se[[m]][3,1] , 3), ")",sep="")
  propTables[m,5] <- paste(prop.test(as.numeric(LCA_best$probs[[m]][,1]*table(LCA_best$predclass)), 
                                     as.numeric(table(LCA_best$predclass)))$p.value, " (",
                           round(prop.test(as.numeric(LCA_best$probs[[m]][,1]*table(LCA_best$predclass)),
                                           as.numeric(table(LCA_best$predclass)))$statistic,3), ")", sep="")
  
  if (LCA_best$probs[[m]][1,1] > LCA_best$probs[[m]][2,1]  ){
    propTables[m,6] <- prop.test( (as.numeric(LCA_best$probs[[m]][,1]*table(LCA_best$predclass)))[1:2],
                                  as.numeric(table(LCA_best$predclass))[1:2], alternative="greater")$statistic
  } else {
    propTables[m,6] <- prop.test( (as.numeric(LCA_best$probs[[m]][,1]*table(LCA_best$predclass)))[1:2],
                                  as.numeric(table(LCA_best$predclass))[1:2], alternative="less")$statistic }
  
  if (LCA_best$probs[[m]][1,1] > LCA_best$probs[[m]][3,1]  ){
    propTables[m,7] <- prop.test( (as.numeric(LCA_best$probs[[m]][,1]*table(LCA_best$predclass)))[c(1,3)],
                                  as.numeric(table(LCA_best$predclass))[c(1,3)], alternative="greater")$statistic
  } else {
    propTables[m,7] <- prop.test( (as.numeric(LCA_best$probs[[m]][,1]*table(LCA_best$predclass)))[c(1,3)],
                                  as.numeric(table(LCA_best$predclass))[c(1,3)], alternative="less")$statistic}
  
  if (LCA_best$probs[[m]][2,1] > LCA_best$probs[[m]][3,1]  ){
    propTables[m,8] <- prop.test( (as.numeric(LCA_best$probs[[m]][,1]*table(LCA_best$predclass)))[2:3],
                                  as.numeric(table(LCA_best$predclass))[2:3], alternative="greater")$statistic
  } else {
    propTables[m,8] <- prop.test( (as.numeric(LCA_best$probs[[m]][,1]*table(LCA_best$predclass)))[2:3],
                                  as.numeric(table(LCA_best$predclass))[2:3], alternative="less")$statistic}
}
write.csv(propTables[c(14,15,1,2,13,12,16,3,4,10,11,9,5,6,7,8),], "statTables.csv")



####Response probability plot
library(ggplot2)
plotTables <- data.frame(cbind(rep(1:16,3), c(propTables[c(14,15,1,2,13,12,16,3,4,10,11,9,5,6,7,8),2], 
                                              propTables[c(14,15,1,2,13,12,16,3,4,10,11,9,5,6,7,8),3], 
                                              propTables[c(14,15,1,2,13,12,16,3,4,10,11,9,5,6,7,8),4]),
                               c(rep("UCS",16), rep("SPB",16), rep("HCS",16))))
names(plotTables) <- c("ind", "prop", "grp")
plotTables$grp <- as.factor(plotTables$grp)
plotTables$ind <- as.numeric(as.character(plotTables$ind))
plotTables$prop <- as.character(plotTables$prop)
for (k in 1:48){
  plotTables[k,2] <- strsplit(as.character(plotTables[k,2])," ")[[1]][1]
}
plotTables$prop <- as.numeric(plotTables$prop)

ggplot(data=plotTables, aes(x=ind, y=prop, group=grp, colour=grp)) +
  geom_line(aes(linetype=grp), size=.8) +
  geom_point(aes(shape=grp, size=1)) +
  scale_color_manual(values=c("dodgerblue4",
                              "firebrick3", "goldenrod1")) +
  scale_x_continuous(breaks= c(1:16),
                   labels=c("Employed", "No children", " >=10 Cigarettes/Day", "Smoke within 30min waking",
                            "Never tried quitting", "Partner smokes", ">=1 Person smokes in home",
                            "Past Illness", "Current Illness", "Want to Quit", "Confidence to Quit",
                            "Depression", "Perceived Vulnerability", "Outcome Expectation", "Optimistic Bias",
                            "Serious Illness") ) +
  scale_y_continuous(breaks= seq(0,1,.1), labels=seq(0,1,.1)) +            
  labs(x="", y="Proportion") + 
  theme(legend.title=element_blank(), legend.text =  element_text(size = rel(1.6)),
        axis.text.x = element_text(angle=45, hjust=1, size=14 ),
        axis.text.y = element_text(size=14 ),
        axis.title = element_text(size=16)) 








##############################
##Covar/Demographic Proportions
##############################
classID <- LCA_best$predclass
smoke$genderDem <- ifelse( as.numeric(smoke$s1) == 1, 1, 0)
smoke$ageDem <-  ifelse( as.numeric(smoke$s2_1) < 40, 0, 1)
smoke$countryDem <- ifelse( as.numeric(smoke$country) == 2, 0, 1)
smoke$ethnicityDem <- ifelse( as.numeric(smoke$c2) == 1, 0, 1)
smoke$eduDem <- ifelse( as.numeric(smoke$c1) == 1, 1, 0)
smoke$employDem <- ifelse( as.numeric(smoke$c3) == 1, 0, 1)
smoke$relateDem <- ifelse( as.numeric(smoke$c4) == 1, 1, 0)
dataDemo <- smoke[,314:320]

####Table of means and p-values
table(classID)
total.case <- cbind(rep(142, 17), rep(192,17), rep(166,17))
case <- cbind(unname(unlist(lapply(dataDemo[classID==1,], sum))),
              unname(unlist(lapply(dataDemo[classID==2,], sum))),
              unname(unlist(lapply(dataDemo[classID==3,], sum))))

propTables <- matrix(NA, nrow=7, ncol=9)
for (m in 1:7){
  propTables[m,1] <- names(dataDemo)[m]
  propTables[m,2] <- paste( round(rowSums(case)[m]/500, 3), " (", rowSums(case)[m], ")", sep="") 
  propTables[m,3] <- paste( round(unname(prop.test(case[m,], total.case[m,])$est[1]), 3), " (", case[m,1], ")", sep="")
  propTables[m,4] <- paste( round(unname(prop.test(case[m,], total.case[m,])$est[2]), 3), " (", case[m,2], ")", sep="")
  propTables[m,5] <- paste( round(unname(prop.test(case[m,], total.case[m,])$est[3]), 3), " (", case[m,3], ")", sep="")
  propTables[m,6] <- paste(unname(prop.test(case[m,], total.case[m,])$p.value), "(",
                           unname(prop.test(case[m,], total.case[m,])$statistic), ")", sep="")
  if (unname(prop.test(case[m,], total.case[m,])$est[1]) > unname(prop.test(case[m,], total.case[m,])$est[2])){
    propTables[m,7] <- (unname(prop.test(case[m,1:2], total.case[m,1:2], alternative="greater")$p.value))
  } else {
    propTables[m,7] <- (unname(prop.test(case[m,1:2], total.case[m,1:2], alternative="less")$p.value))
  }
  if (unname(prop.test(case[m,], total.case[m,])$est[1]) > unname(prop.test(case[m,], total.case[m,])$est[3])){
    propTables[m,8] <- (unname(prop.test(case[m,c(1,3)], total.case[m,c(1,3)], alternative="greater")$p.value))
  } else {
    propTables[m,8] <- (unname(prop.test(case[m,c(1,3)], total.case[m,c(1,3)], alternative="less")$p.value))
  }
  if (unname(prop.test(case[m,], total.case[m,])$est[2]) > unname(prop.test(case[m,], total.case[m,])$est[3])){
    propTables[m,9] <- (unname(prop.test(case[m,2:3], total.case[m,2:3], alternative="greater")$p.value))
  } else {
    propTables[m,9] <- (unname(prop.test(case[m,2:3], total.case[m,2:3], alternative="less")$p.value))
  }
}

write.csv(propTables, "DemoTablesPval.csv")

#Table with test statistics
propTables <- matrix(NA, nrow=7, ncol=9)
for (m in 1:6){
  propTables[m,1] <- names(dataDemo)[m]
  propTables[m,2] <- paste( round(rowSums(case)[m]/500, 3), " (", rowSums(case)[m], ")", sep="") 
  propTables[m,3] <- paste( round(unname(prop.test(case[m,], total.case[m,])$est[1]), 3), " (", case[m,1], ")", sep="")
  propTables[m,4] <- paste( round(unname(prop.test(case[m,], total.case[m,])$est[2]), 3), " (", case[m,2], ")", sep="")
  propTables[m,5] <- paste( round(unname(prop.test(case[m,], total.case[m,])$est[3]), 3), " (", case[m,3], ")", sep="")
  propTables[m,6] <- paste(unname(prop.test(case[m,], total.case[m,])$p.value), "(",
                           unname(prop.test(case[m,], total.case[m,])$statistic), ")", sep="")
  if (unname(prop.test(case[m,], total.case[m,])$est[1]) > unname(prop.test(case[m,], total.case[m,])$est[2])){
    propTables[m,7] <- (unname(prop.test(case[m,1:2], total.case[m,1:2], alternative="greater")$statistic))
  } else {
    propTables[m,7] <- (unname(prop.test(case[m,1:2], total.case[m,1:2], alternative="less")$statistic))
  }
  if (unname(prop.test(case[m,], total.case[m,])$est[1]) > unname(prop.test(case[m,], total.case[m,])$est[3])){
    propTables[m,8] <- (unname(prop.test(case[m,c(1,3)], total.case[m,c(1,3)], alternative="greater")$statistic))
  } else {
    propTables[m,8] <- (unname(prop.test(case[m,c(1,3)], total.case[m,c(1,3)], alternative="less")$statistic))
  }
  if (unname(prop.test(case[m,], total.case[m,])$est[2]) > unname(prop.test(case[m,], total.case[m,])$est[3])){
    propTables[m,9] <- (unname(prop.test(case[m,2:3], total.case[m,2:3], alternative="greater")$statistic))
  } else {
    propTables[m,9] <- (unname(prop.test(case[m,2:3], total.case[m,2:3], alternative="less")$statistic))
  }
}

write.csv(propTables, "DemoTablesStat.csv")

