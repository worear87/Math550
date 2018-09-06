#### homework 1 Wallace O'Rear
#### problems: ch2: 1,2,3,4,6

library(readr)

setwd("C:/code/Math550")


## 2.1
playbill <- read.csv("Data/playbill.csv")
playbill.lm <- lm(CurrentWeek ~ LastWeek, data = playbill)
plot(x = playbill$LastWeek,y = playbill$CurrentWeek, pch=19, ylab="Current Week", xlab="Last Week", main="Playbill")
abline(lsfit(x=playbill$LastWeek,y=playbill$CurrentWeek))

summary(playbill.lm)

# Call:
#   lm(formula = CurrentWeek ~ LastWeek, data = playbill)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -36926  -7525  -2581   7782  35443 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 6.805e+03  9.929e+03   0.685    0.503    
# LastWeek    9.821e-01  1.443e-02  68.071   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 18010 on 16 degrees of freedom
# Multiple R-squared:  0.9966,	Adjusted R-squared:  0.9963 
# F-statistic:  4634 on 1 and 16 DF,  p-value: < 2.2e-16


round(confint(playbill.lm),2)

#a)                 2.5 %   97.5 %
#   (Intercept) -14244.33 27854.10
#   LastWeek         0.95     1.01
#
# Looks like 1 is a plausible value for the slope of the regression model

# b)
#  the estimated B0 is 6805, with a standard error of 9929, we can probably accept the null hypothesis of B0=10000

# c)
predict(playbill.lm, data.frame(LastWeek = 400000), interval = "prediction")

# fit      lwr      upr
# 399637.5 359832.8 439442.2

# We can probably reject that $450K will be a feasible value for a show that grossed $400K last week.

#d) looking at the plot and the prediction for 400K, saying that next week's results will be equal to this week's results is appropriate.


## 2.2
indicators <- read.csv("Data/indicators.txt", sep="")
indicators.lm <- lm(PriceChange ~ LoanPaymentsOverdue, data=indicators)
plot(indicators$PriceChange,indicators$LoanPaymentsOverdue,pch=19, xlab="% Price Change", ylab="Loan Payments Overdue", main="Indicators")
abline(lsfit(indicators$PriceChange,indicators$LoanPaymentsOverdue))

summary(indicators.lm)

# Call:
#   lm(formula = PriceChange ~ LoanPaymentsOverdue, data = indicators)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -4.6541 -3.3419 -0.6944  2.5288  6.9163 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)           4.5145     3.3240   1.358   0.1933  
# LoanPaymentsOverdue  -2.2485     0.9033  -2.489   0.0242 *
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3.954 on 16 degrees of freedom
# Multiple R-squared:  0.2792,	Adjusted R-squared:  0.2341 
# F-statistic: 6.196 on 1 and 16 DF,  p-value: 0.02419


round(confint(indicators.lm),2)

#                       2.5 % 97.5 %
#   (Intercept)         -2.53  11.56
# LoanPaymentsOverdue   -4.16  -0.33

# a) The 95% confidence interval for the slope is all < 0, so we can say that there is a good chance that there is a negative
#    linear association.

predict(indicators.lm, data.frame(LoanPaymentsOverdue = 4), interval = "confidence")

#       fit       lwr       upr
#  -4.479585 -6.648849 -2.310322

# b) 0% is not a feasible value for E(Y | X=4), since the lower and upper value for the confidence interval is all under 0.

## 2.3
invoices <- read.csv("Data/invoices.txt", sep="")
invoices.lm <- lm(Time ~ Invoices, data= invoices)
plot(invoices$Invoices,invoices$Time,pch=19, xlab = "# of Invoices", ylab="Amount of Time", main = "Invoices")
abline(lsfit(invoices$Invoices,invoices$Time))

summary(invoices.lm)
# 
# Call:
#   lm(formula = Time ~ Invoices, data = invoices)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.59516 -0.27851  0.03485  0.19346  0.53083 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.6417099  0.1222707   5.248 1.41e-05 ***
#   Invoices    0.0112916  0.0008184  13.797 5.17e-14 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.3298 on 28 degrees of freedom
# Multiple R-squared:  0.8718,	Adjusted R-squared:  0.8672 
# F-statistic: 190.4 on 1 and 28 DF,  p-value: 5.175e-14

round(confint(invoices.lm),4)

#             2.5 % 97.5 %
# (Intercept) 0.3912 0.8922
# Invoices    0.0096 0.0130

#a) 95% confidence level for B0 is 0.3912 to 0.8922.


#b) Since 0.01 falls within the 95% confidence range, we can't reject the null hypothesis of B1 = 0.01.

#c)

predict(invoices.lm, data.frame(Invoices = 130), interval = "prediction") #point estimate

#     fit      lwr    upr
#  2.109624 1.422947 2.7963

predict(invoices.lm, data.frame(Invoices = 130), interval = "confidence") #confidence

#     fit      lwr      upr
# 2.109624 1.986293 2.232954

