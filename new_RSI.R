setwd("E:/study")
data_etf <- read.table("tracker_HS_2.csv", header = TRUE,sep=",")
#data_etf <- read.table("FXI.csv", header = TRUE,sep=",")
library(Ecdat)
#library(forecast)
library(fGarch)
library(timeDate)
library(timeSeries)
library(fBasics)
library(TTR)
#library(kernlab)
#library(rpart)

n=length(data_etf[,2])
predict_days=5
rsidays=5
hs = data_etf[,2]
hs= hs[n:1]
rsi = RSI(hs,n = 5)
rsiweek = rsi[seq(6,2286,5)]


rweek = hs[seq(6,n,5)]/hs[seq(1,n-5,5)]-1
rsi = RSI(hs,n = 5)
macd = MACD(hs)[,1]
macdsig = MACD(hs)[,2]
signal=c()
signalindex = 1
nosig = 0
#NA_number=length(which(is.na(data_etf$macd)))
for(i in seq(40,n,predict_days) ){
  
  #if ((is.na(rsi[i] != TRUE )) & (is.na(macd[i - 5]) != TRUE)) {
  #if((rsi[i]>60) & (macd[i] > macdsig[i] )){
  #  signal[signalindex]=1
  #}else if((rsi[i]<40)& (macd[i] < macdsig[i]) ){
  #  signal[signalindex]=-1
  #}else{
  #  signal[signalindex]=0
  #  nosig = nosig +1
  #}
  if((rsi[i] > 60) & (macd[i] > macdsig[i]) & (macd[i-5] < macdsig[i-5])){
    signal[signalindex]=1
  }else if((rsi[i] < 40)&(macd[i] < macdsig[i]) & (macd[i-5] > macdsig[i-5]) ){
    signal[signalindex]=-1
  }else{
    signal[signalindex]=0
    nosig = nosig +1
  }
  
  
  signalindex =  signalindex + 1
}
hsoutput = NA
hsoutput$signal = signal
rweekones = c()
for (i in 8:length(rweek)){
  if (rweek[i]>0) {
    rweekones[i]= 1
  } else if(rweek[i]<0){
    rweekones[i]= -1
  } else {
    rweekones[i]= 1
  }
}

rweekones = rweekones[8:length(rweek)]
hsoutput$weeklyr = rweekones
guess = 0
hit = 0
for( i in 2:(440)){
  if( signal[(i-1)] != 0){
    if (signal[(i-1)]*rweekones[i]>0){
      hit = hit + 1;
    }
    guess = guess+1
  }
  
  
}
rate = hit/guess


