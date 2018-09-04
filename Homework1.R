#### homework 1 
#### problems: ch2: 1,2,3,4,6

library(readr)

setwd("F:/code/Math550")


## 2.1
playbill <- read.csv("Data/playbill.csv")
playbill.lm <- lm(CurrentWeek ~ LastWeek, data = playbill)
plot(x = playbill$CurrentWeek,y = playbill$LastWeek, pch=19)
summary(playbill.lm)
abline(lsfit(x=playbill$CurrentWeek,y=playbill$LastWeek))

round(confint(playbill.lm),2)

## 2.2
indicators <- read.csv("Data/indicators.txt", sep="")
indicators.lm <- lm(PriceChange ~ LoanPaymentsOverdue, data=indicators)
plot(indicators$PriceChange,indicators$LoanPaymentsOverdue,pch=19)
summary(indicators.lm)
abline(lsfit(indicators$PriceChange,indicators$LoanPaymentsOverdue))

round(confint(indicators.lm),2)

## 2.3
invoices <- read.csv("Data/invoices.txt", sep="")
invoices.lm <- lm(Time ~ Invoices, data= invoices)
summary(invoices.lm)
