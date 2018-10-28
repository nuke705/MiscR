dat <- read.table("E:/study/AAPL.csv", header = TRUE,sep=",")
adjclose <- dat[,6] #adjusted return
date <- as.Date(dat[,1])
logreturn <- dat[,8]
mean <- mean(logreturn,na.rm =T)
sd <- sd(logreturn,na.rm =T)
library(moments)
sk <- skewness(logreturn,na.rm =T)
k <- kurtosis(logreturn,na.rm =T)

hist(logreturn, breaks=60,prob=TRUE)
curve(dnorm(x,mean,sd),add =T, col = "red", lwd=3)
plot(date,adjclose,type = "l", main = "time series of price")
plot(date,logreturn,type = "l", main = "time series of return")

qqnorm(logreturn)
qqline(logreturn)
