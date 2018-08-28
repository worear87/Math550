#### homework 1 
#### problems: ch2: 1,2,4,4,6

library(readr)

setwd("C:/code/Math550")


## 2.1
playbill <- read.csv("Data/playbill.csv")
playbill.lm <- lm(CurrentWeek ~ LastWeek, data = playbill)
summary(playbill.lm)


## 2.2
indicators <- read.csv("Data/indicators.txt", sep="")
indicators.lm <- lm(PriceChange ~ LoanPaymentsOverdue, data=indicators)
summary(indicators.lm)


## 2.3
invoices <- read.csv("Data/invoices.txt", sep="")
invoices.lm <- lm(Time ~ Invoices, data= invoices)
summary(invoices.lm)
