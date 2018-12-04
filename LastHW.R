### homework 5

library(leaps)
library(data.table)
library(car)
setwd("C:\\code/Math550/")

golf <- data.table(read.csv("Data/pgatour2006.csv"))

attach(golf)


##### trying package

subsets <- regsubsets(x = golf[,.(DrivingAccuracy , GIR , PuttingAverage , BirdieConversion , SandSaves , Scrambling , PuttsPerRound)]
      , y = log(golf$PrizeMoney))
summary(subsets)
plot(subsets)


#### end

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


