##First Comparing classes for never or sometime quitting
smoke$s5 <-factor(smoke$s5)
smoke$s5Binary <- ifelse( as.numeric(smoke$s5)==5, 0, 1)

wantQuit <- table(LCA_best$predclass, smoke$s5Binary)
chisq.test(wantQuit)
prop.test(wantQuit[c(1,2),1:2], alternative='less')
prop.test(wantQuit[c(1,3),1:2], alternative='less')
prop.test(wantQuit[c(2,3),1:2], alternative='less')
s5BinaryRegression <- glm(smoke$s5Binary ~ as.factor(LCA_best$predclass), family=binomial)

#Second among those that want to quit, under or over 1 year
smokeQuit <- smoke[as.numeric(smoke$s5Binary)==1,]
classWhen <- LCA_best$predclass[as.numeric(smoke$s5Binary)==1]
smokeQuit$s5BinWhen <- ifelse( as.numeric(smokeQuit$s5)==1 | as.numeric(smokeQuit$s5)==2, 0, 1)

whenQuit <- table(classWhen, smokeQuit$s5BinWhen)
chisq.test(whenQuit)
prop.test(whenQuit[c(1,2),1:2], alternative='greater')
prop.test(whenQuit[c(1,3),1:2], alternative='greater')
prop.test(whenQuit[c(2,3),1:2], alternative='greater')
