### homework 5

library(leaps)
library(data.table)
library(car)
setwd("C:\\code/Math550/")

golf <- data.table(read.csv("Data/pgatour2006.csv"))

attach(golf)

golf.full.lm <- lm(log(PrizeMoney) ~ DrivingAccuracy + GIR + PuttingAverage + BirdieConversion + SandSaves + Scrambling + PuttsPerRound, data=golf)


regsubsets.out <-
  regsubsets(log(PrizeMoney) ~ DrivingAccuracy + GIR + PuttingAverage + BirdieConversion + SandSaves + Scrambling + PuttsPerRound,
             data = golf,
             nbest = 1,       # 1 best model for each number of predictors
             nvmax = NULL,    # NULL for no limit on number of variables
             force.in = NULL, force.out = NULL,
             method = "exhaustive")

rs <- summary(regsubsets.out)
par(mfrow=c(1,2))
plot(1:7,rs$adjr2,xlab="Subset Size",ylab="Adjusted R-squared")

subsets(regsubsets.out,statistic=c("adjr2"))
rs$adjr2

m1 <- lm(log(PrizeMoney) ~ GIR, data=golf)
m2 <- lm(log(PrizeMoney) ~ GIR + PuttsPerRound, data=golf)
m3 <- lm(log(PrizeMoney) ~ GIR + BirdieConversion + Scrambling, data=golf)
m4 <- lm(log(PrizeMoney) ~ GIR + BirdieConversion + SandSaves + Scrambling, data=golf)
m5 <- lm(log(PrizeMoney) ~ GIR + BirdieConversion + SandSaves + Scrambling + PuttsPerRound, data=golf)
m6 <- lm(log(PrizeMoney) ~ DrivingAccuracy + GIR + BirdieConversion + SandSaves + Scrambling + PuttsPerRound, data=golf)
m7 <- golf.full.lm

#Subset size=1
n <- length(m1$residuals)
npar <- length(m1$coefficients) +1
#Calculate AIC
extractAIC(m1,k=2)
#Calculate AICc
extractAIC(m1,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(m1,k=log(n))

#Subset size=2
n <- length(m2$residuals)
npar <- length(m2$coefficients) +1
#Calculate AIC
extractAIC(m2,k=2)
#Calculate AICc
extractAIC(m2,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(m2,k=log(n))

#Subset size=3
n <- length(m3$residuals)
npar <- length(m3$coefficients) +1
#Calculate AIC
extractAIC(m3,k=2)
#Calculate AICc
extractAIC(m3,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(m3,k=log(n))

#Subset size=4
n <- length(m4$residuals)
npar <- length(m4$coefficients) +1
#Calculate AIC
extractAIC(m4,k=2)
#Calculate AICc
extractAIC(m4,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(m4,k=log(n))

#Subset size=5
n <- length(m5$residuals)
npar <- length(m5$coefficients) +1
#Calculate AIC
extractAIC(m5,k=2)
#Calculate AICc
extractAIC(m5,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(m5,k=log(n))

#Subset size=6
n <- length(m6$residuals)
npar <- length(m6$coefficients) +1
#Calculate AIC
extractAIC(m6,k=2)
#Calculate AICc
extractAIC(m6,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(m6,k=log(n))

#Subset size=7
n <- length(m7$residuals)
npar <- length(m7$coefficients) +1
#Calculate AIC
extractAIC(m7,k=2)
#Calculate AICc
extractAIC(m7,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(m7,k=log(n))

## backwards step
backAIC <- step(golf.full.lm,direction="backward", data=golf)
backBIC <- step(golf.full.lm,direction="backward", data=golf, k=log(n))

## forwards step
mint <- lm(log(PrizeMoney)~1,data=golf)
forwardAIC <- step(mint,scope=list(lower=~1, 
                                   upper=~DrivingAccuracy + GIR + PuttingAverage + BirdieConversion + SandSaves + Scrambling + PuttsPerRound),
                   direction="forward", data=golf)
forwardBIC <- step(mint,scope=list(lower=~1, 
                                   upper=~DrivingAccuracy + GIR + PuttingAverage + BirdieConversion + SandSaves + Scrambling + PuttsPerRound),
                   direction="forward", data=golf,k=log(n))
