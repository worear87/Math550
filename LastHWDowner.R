library(leaps)
library(data.table)
library(car)
library(alr3)
setwd("C:\\code/Math550/")

## response: Outcome 1-survived, 0-no

cows <- read.csv("downer.csv")

attach(cows)
pairs(cows[,c("Outcome","Daysrec", "CK")], col=ifelse(Outcome==1,"green","red"), pch=19)
pairs(cows[,c("Outcome","AST", "Urea", "PCV")], col=ifelse(Outcome==1,"green","red"), pch=19)

par(mfrow=c(1,1))
boxplot(Daysrec~Outcome, xlab="Outcome: 1=Survived 0=Did not", ylab="Daysrec") 
boxplot(CK~Outcome, xlab="Outcome: 1=Survived 0=Did not", ylab="CK") #right skewed
boxplot(AST~Outcome, xlab="Outcome: 1=Survived 0=Did not", ylab="AST") #right skewed
boxplot(Urea~Outcome, xlab="Outcome: 1=Survived 0=Did not", ylab="Urea") #right skewed
boxplot(PCV~Outcome, xlab="Outcome: 1=Survived 0=Did not", ylab="PCV")

## see if taking log of CK,AST, and Urea make the variance better
par(mfrow=c(1,3))
boxplot(log(CK)~Outcome, xlab="Outcome: 1=Survived 0=Did not", ylab="log(CK)") 
boxplot(log(AST)~Outcome, xlab="Outcome: 1=Survived 0=Did not", ylab="log(AST)") 
boxplot(log(Urea)~Outcome, xlab="Outcome: 1=Survived 0=Did not", ylab="log(Urea)") 

cows.full.lm <- glm(Outcome ~ Daysrec + log(CK) + log(AST) + log(Urea) + PCV, data=cows, family = binomial)
summary(cows.full.lm)

n <- length(cows.full.lm$residuals)
cowsBackAIC <- step(cows.full.lm,direction="backward", data=cows)
#returns Outcome ~ Daysrec + log(AST) + log(Urea) + PCV
cowsBackBIC <- step(cows.full.lm,direction="backward", data=cows, k=log(n))
#Outcome ~ Daysrec + log(AST) + log(Urea)

mint <- glm(Outcome~1,data=cows,family=binomial)
cowsForwardAIC <- step(mint,scope=list(lower=~1, 
                                   upper=~Daysrec + log(CK) + log(AST) + log(Urea) + PCV),
                   direction="forward", data=cows)
#Outcome ~ log(AST) + log(Urea) + Daysrec + PCV
cowsForwardBIC <- step(mint,scope=list(lower=~1, 
                                   upper=~Daysrec + log(CK) + log(AST) + log(Urea) + PCV),
                   direction="forward", data=cows,k=log(n))
#Outcome ~ log(AST) + log(Urea) + Daysrec

## looks like log(CK) is left out of all the models, so I'll leave it out
## AIC forward and back choose the larger model, BIC chooses the smaller

cows.size4.lm <- glm(Outcome ~ log(AST) + log(Urea)  + Daysrec + PCV, data=cows, family=binomial)
cows.size3.lm <- glm(Outcome ~ log(AST) + log(Urea) + Daysrec, data=cows, family=binomial)
cows.size3.plus.lm <- glm(Outcome ~ AST + Urea + log(AST) + log(Urea) + Daysrec, data=cows, family=binomial)

mmps(cows.size3.lm)
mmps(cows.size3.plus.lm)


summary(cows.size3.lm)
summary(cows.size3.plus.lm)

anova(cows.size3.lm, cows.size3.plus.lm,test="Chisq")

avPlots(cows.size3.plus.lm)

