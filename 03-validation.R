library(VGAM)

##First Comparing classes for never or sometime quitting
#smoke$s5 <-factor(smoke$s5, ordered = TRUE)
smoke$Y0 <- as.numeric(smoke$s5 =="1 - 6 months")
smoke$Y1 <- as.numeric(smoke$s5 =="7- 11 months")
smoke$Y2 <- as.numeric(smoke$s5 =="1 - 5 years")
smoke$Y3 <- as.numeric(smoke$s5 =="6 or more years")
smoke$Y4 <- as.numeric(smoke$s5 =="I do not see myself ever quitting smoking")
smoke$UCS <- as.numeric(LCA_best$predclass==1)
smoke$SPB <- as.numeric(LCA_best$predclass==2)
smoke$HCS <- as.numeric(LCA_best$predclass==3)

#Fit proportional model
FIT <- vglm(cbind(Y4,Y3,Y2,Y1,Y0) ~ SPB + HCS, family=cumulative(parallel=TRUE, reverse=FALSE), data=smoke)
#Fit nonproportional model
FIT2 <- vglm(cbind(Y4,Y3,Y2,Y1,Y0) ~ SPB + HCS, family=cumulative(parallel=FALSE, reverse=FALSE), data=smoke)

FIT3 <- vglm(s5 ~ SPB + HCS, family=cumulative(parallel=FALSE, reverse=FALSE), data=smoke)


#Assess proportionality assumption
(ctable <- coef(summary(m)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
#Get output
summary(FIT2)


#Second among those that want to quit, under or over 1 year
smoke$s5Binary <- ifelse( as.numeric(smoke$s5)==5, 0, 1)
smokeQuit <- smoke[as.numeric(smoke$s5Binary)==1,]
classWhen <- LCA_best$predclass[as.numeric(smoke$s5Binary)==1]
polr(smoke$s5 ~ as.factor(classWhen), Hess=TRUE)
