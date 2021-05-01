library(survival)
#Survival Curves

#Exponential code
banksurv <- read.csv("C:/Users/valay/OneDrive/Desktop/Ivey/Winter Term 2021/Big Data Analytics/Class 9/Assignment/Bankruptcy Data - Haxo Corporation.csv")

out<-survfit(Surv(Years_to_Bankrupt, Bankruptcy) ~ 1, data=banksurv)
y <- out$surv
y

y<- -log(y)
t<-out$time
plot(t,y)

#This gives expected number of failures over the time t
#this plot should be a straight line or else it indicates that the relationship is not exponential

#after exponential, use Weibull model
#in Weibull you plot the minimum or maximum of independent random variables
#Difference between Weibull and Exponential is that gamma = 1 for exponential model

#This is the weibull model code
cox.out<-coxph(Surv(Years_to_Bankrupt, Bankruptcy) ~ Net_Debt_EBITDA +Profit_Margin +Current_Ratio +Debt_to_Cash_Flow_From_Ops +EBITDA, data=banksurv)
summary(cox.out)
#the effect of financial aid is exp(coef) = 0.69 in terms of hazard rate (committing crime)
#the rate for people with financial rate is 70% of the hazard rate for people without financial aid
#meaning it is 30% less if people have financial aid vs. they do not