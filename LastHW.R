### homework 5

library(leaps)
library(data.table)
library(car)
setwd("F:\\code/Math550/")

golf <- data.table(read.csv("Data/pgatour2006.csv"))

attach(golf)


##### trying package

subsets <- regsubsets(x = golf[,.(DrivingAccuracy , GIR , PuttingAverage , BirdieConversion , SandSaves , Scrambling , PuttsPerRound)]
      , y = golf$PrizeMoney)
summary(subsets)
plot(subsets)


#### end

golf.full.lm <- lm(PrizeMoney ~ DrivingAccuracy + GIR + PuttingAverage + BirdieConversion + SandSaves + Scrambling + PuttsPerRound, data=golf)


regsubsets.out <-
  regsubsets(PrizeMoney ~ DrivingAccuracy + GIR + PuttingAverage + BirdieConversion + SandSaves + Scrambling + PuttsPerRound,
             data = golf,
             nbest = 1,       # 1 best model for each number of predictors
             nvmax = NULL,    # NULL for no limit on number of variables
             force.in = NULL, force.out = NULL,
             method = "exhaustive")
summary.out <- summary(regsubsets.out)


