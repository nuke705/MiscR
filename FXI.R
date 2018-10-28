library(Ecdat)
#library(forecast)
library(fGarch)
library(timeDate)
library(timeSeries)
library(fBasics)
library(TTR)
setwd("E:/study")
dat <- read.table("E:/study/FXI.csv", header = TRUE,sep=",")
fxi = dat[,2]
n = length(fxi);
#fxi = fxi[length(fxi):1]
dates <- as.Date(dat[,1])
#dates = dates[length(fxi):1]
plot(dates,fxi,type = "l")
logret =log(fxi[2:n]/fxi[1:n-1])
qqnorm(logret)
plot(dates[2:n],logret,type = "l")
#rweek = fxi[6:n]/fxi[1:n-1]-1
rweek = fxi[seq(6,n,5)]/fxi[seq(1,n-5,5)]-1

#fit=garchFit(~arma(1,1)+garch(1,1), data = logret, cond.dist="ged") 
#summary(fit)
                       
#rsi = RSI(fxi)
#plot(dates[3000:n],rsi[3000:n],type = "l",col = "red",ylim = c(20,90))
#lines(dates[3000:n],fxi[3000:n], type = "l")

#https://www.investopedia.com/articles/active-trading/042114/overbought-or-oversold-use-relative-strength-index-find-out.asp


library(TTR)
rsi = RSI(fxi,n = 5)
macd = MACD(fxi)[,2]
#rvi = RVI(fxi,n=30)
start = 2000
plot(dates[start:n],rsi[start:n],type = "l",col = "red")
abline(h=50)
plot(dates[start:n],macd[start:n],type = "l",col = "blue")
abline(h=0)
plot(dates[start:n],rsi[start:n],type = "l",col = "red",ylim = c(-2,100))
lines(dates[start:n],fxi[start:n])
lines(dates[start:n],macd[start:n],col = "blue")

indicator = rep(0,n-start+1)
trackindex = c()
lagday = 5
j=1
for(i in seq(start,n,lagday)){
  indicatorindex = i+1-start
  trackindex[j]=indicatorindex
  if((rsi[i] > 50) & (macd[i]-macd[i-5]>0.01)){
    indicator[indicatorindex]= 1
  }else if((rsi[i] < 50) & (macd[i]-macd[i-5] < -0.01)){
    indicator[indicatorindex]= -1
  }else {
    indicator[indicatorindex]= 0
  }
  #indicator[indicatorindex]= 6
  j=j+1
}
indicatorcut=c()
jj=1
for(i in seq(1,length(indicator),5)){
  indicatorcut[jj]=indicator[i]
  jj=jj+1
}
#dput(indicatorcut,file = "indicator")
hitcount = c()
returnindex = length(rweek)-length(indicatorcut)
upcount = 0
downcount = 0
for(i in seq(1,length(indicatorcut),1)){
  #returnindex = length(rweek)-length(indicatorcut)
  
  if(indicatorcut[i]*rweek[returnindex+1]> 0){
    hitcount[i]=1
  }else if(indicatorcut[i]==0){
    
   hitcount[i]=-1
  
  }else{
  hitcount[i]=0
  }
  returnindex=returnindex+1
  
}
hit = 0
totaltry = 0
for(i in 1:length(hitcount)){
  if(hitcount[i]!= -1){
    if(hitcount[i]==1){
      hit=hit+1
    }
    totaltry = totaltry+1
  }
}
hitratio = hit/totaltry
write.csv(indicatorcut, "RSI-MACD.csv")
write.csv(rweek[(length(rweek)-length(indicatorcut)):671], "rweek.csv")

