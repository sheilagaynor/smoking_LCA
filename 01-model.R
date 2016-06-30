##############################
#####Run bayes lca on k=2-7###
##############################
bicMatrix <- matrix(NA, nrow=1000,ncol=6)
aicMatrix <- matrix(NA, nrow=1000,ncol=6)
chiMatrix <- matrix(NA, nrow=1000,ncol=6)
nparMatrix <- matrix(NA, nrow=1000,ncol=6)
llikMatrix <- matrix(NA, nrow=1000,ncol=6)
entMatrix <- matrix(NA, nrow=1000,ncol=6)

for (m in 2:7){
  set.seed(128) #make reproducible
  min_bic <- 100000
  for (n in 1:1000){
    covFit <- poLCA(cbind(packsWeek, wakeupSmoke, pastsmokeIllness,
                          currentsmokeIllness,  pVulnerability,  pExpectation,
                          optimBias,  develIllness,  cesDepression,  wantQuit,
                          confQuit,  partnerSmoke,  triedQuit,  employ,
                          children,  smokeHousehold)~
                      ethnicity +  gender +  country +
                      age,  smoke, nclass=m, maxiter=500)
    bicMatrix[n,m-1] <- covFit$bic 
    chiMatrix[n,m-1] <- covFit$Chisq
    nparMatrix[n,m-1] <- covFit$npar
    llikMatrix[n,m-1] <- covFit$llik
    aicMatrix[n,m-1] <- covFit$aic   
    entMatrix[n,m-1] <- poLCA.entropy(covFit)
    if (covFit$bic < min_bic){
      min_bic <- covFit$bic
      LCA_best <- covFit
    }
  }
}

write.csv(bicMatrix, "bicMatrix.csv")
write.csv(aicMatrix, "aicMatrix.csv")
write.csv(chiMatrix, "chiMatrix.csv")
write.csv(nparMatrix, "nparMatrix.csv")
write.csv(llikMatrix, "llikMatrix.csv")
write.csv(entMatrix, "entMatrix.csv")

#Get summary of fit statistics for each number of groups
kClassSummary <- matrix(NA, nrow=7, ncol=7)
kClassSummary[,1] <- c("", "BIC", "AIC", "Chi Sq", "N Param", "Log-lik", "Entropy")
kClassSummary[1,] <- c("", "2", "3", "4", "5", "6", "7")
kClassSummary[2,2:7] <- round(colMeans(bicMatrix),3)
kClassSummary[3,2:7] <- round(colMeans(aicMatrix),3)
kClassSummary[4,2:7] <- round(colMeans(chiMatrix),3)
kClassSummary[5,2:7] <- round(colMeans(nparMatrix),3)
kClassSummary[6,2:7] <- round(colMeans(llikMatrix),3)
kClassSummary[7,2:7] <- round(colMeans(entMatrix),3)
write.csv(kClassSummary, "kClass.csv")

###########################
#####Get the model with min bic -- LCA_best
###########################
m <- 3
set.seed(128) #make reproducible
min_bic <- 1000000
for (n in 1:1000){
  covFit <- poLCA(cbind(packsWeek, wakeupSmoke, pastsmokeIllness,
                        currentsmokeIllness,  pVulnerability,  pExpectation,
                        optimBias,  develIllness,  cesDepression,  wantQuit,
                        confQuit,  partnerSmoke,  triedQuit,  employ,
                        children,  smokeHousehold)~
                    ethnicity +  gender +  country +
                    age,  smoke, nclass=m, maxiter=500) 
  if (covFit$bic < min_bic){
    min_bic <- covFit$bic
    LCA_best <- covFit #Now LCA_best holds the final model
  }
}