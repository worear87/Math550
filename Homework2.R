### Wallace O'Rear Homework 2
### Chapter 3: 1, 3, 4, 5, 8
library(readr)
library(ggplot2)
setwd("C:/code/Math550")

##1
airfares <- read.csv("Data/airfares.txt", sep = "")
airfarePlot <- ggplot(airfares,aes(Distance,Fare, label=City)) + geom_point() + geom_text(aes(label=City),hjust=0, vjust=0)
airfarePlot

airfares.lm <- lm(Fare ~ Distance, data=airfares)
summary(airfares.lm)

plot(airfares.lm)
tail(airfares)


airfares.lm.sq <- lm(Fare ~ sqrt(Distance), data=airfares)
plot(airfares.lm.sq)

