library(Ecdat)
#library(forecast)
library(fGarch)
library(timeDate)
library(timeSeries)
library(fBasics)
data(Capm)
dr=diff(Capm$rf)
#ARMA
arima(dr,order = c(1,0,1))
arima(dr,order = c(1,0,2))
arima(dr,order = c(2,0,1))
#library(evir) 
#data(bmw) 
#ARMA(1,1)/GARCH(1,1)
fit=garchFit(~arma(1,1) + garch(1,1), data = dr, cond.dist="norm") 
summary(fit)
aic1 = 2*6-2*764.8582
res = residuals(fit,standardize=T)
qqnorm(res)
qqline(res)

fit2=garchFit(~ arma(1,1) + garch(1,1), data = dr, cond.dist="ged") 
summary(fit2)
aic2=14-2*772.3694
fit3=garchFit(~ arma(1,1) + garch(1,1), data = dr, cond.dist="std") 
summary(fit3)
aic3=14-2*771.8679


dat <- read.table("E:/study/522hw7q2.txt", header = TRUE)
x <- dat[,3]
y = dat[,2]
lm1 = lm(y ~ x)
summary(lm1)
confint(lm1,"x")
#p-value < 0.05 reject H0: beta2 = 0

newdata=data.frame(x=4)
predict(lm1,newdata,interval="confidence")






