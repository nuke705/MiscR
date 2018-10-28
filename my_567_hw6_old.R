setwd("E:/study")
library(readxl)
##1
goog<-read_excel("F567C.s2018.HW6.GOOG data.xlsx",sheet="GOOG")
ret=log(goog$close[2:length(goog$close)]/goog$close[1:(length(goog$close)-1)])

google<-data.frame(goog[2:length(goog$close),],ret)

ddvar<-c()
r2sum=0
n=1
date=google$date[1]
for (i in 1:length(google$date)){
  if (date==google$date[i]) {
    r2sum=r2sum+google$ret[i]^2
  }
  else {
    ## write var
    ddvar[n]=r2sum
    r2sum=0
    n=n+1
    ## change date
    date=google$date[i]
  }
  if (i==length(google$date)) {ddvar[n]=r2sum}
}
mean(ddvar)
##2

s<-c(2,5,10,15)
dvar=matrix(data=0,nrow=10,ncol=4)
col=0

for (i in s){
  col = col+1
  row = 1
  #for each day
  for (j in seq(1,3910,391)) {
    tempsum = 0
    #for each time interval in that day
    for (k in 0:(s-1)){
      for (l in seq(j+k,j+391-1,i)){
        if ((l+i) <= (j+391-1)){
        rettemp = log(goog$close[l+i]/goog$close[l])
        #  rettemp = google$ret[k+i]+google$ret[i]
        tempsum = tempsum + rettemp^2
        }
      }
    }
    dvar[row,col] = tempsum/i
    row = row +1
  }
}


##2a
dvol=matrix(data=0,nrow=11,ncol=5)
dvol[1:10,1]=ddvar
dvol[11,1]=mean(ddvar)
dvol[1:10,2:5]=dvar
dvol[11,2]=mean(dvar[,1]);dvol[11,3]=mean(dvar[,2]);dvol[11,4]=mean(dvar[,3]);dvol[11,5]=mean(dvar[,4]);

##b
##maybe average the 25 15-minute variances and add the mean to get the day variance
s<-c(2,5,10,15)
dvar=matrix(data=0,nrow=10,ncol=4)
col=0
for (j in s){
  col=col+1
  r2sum=0
  n=1
  
  for (k in 1:j){
    n=1
    date=goog$date[k]
    r2sum=0
    i=k
    while (i<=(length(goog$date)-j)){
      i=i+j
      if (goog$date[i]!=date){
        ## write var
        if (k!=1) 
        {dvar[n,col]=dvar[n,col]+(390/j+1)*r2sum/j}
        else
        {dvar[n,col]=dvar[n,col]+r2sum/j}
        r2sum=0
        n=n+1
        ## change date
        date=goog$date[i]
        i=i-j+1+(k>1)*j
        
      }
      else{
        r=log(goog$close[i]/goog$close[i-j])
        r2sum=r2sum+r^2
      }
      if (i>(length(goog$date)-j)) {dvar[n,col]=dvar[n,col]+r2sum/j; }
    }    
  }
}
##c
##decrease
ratio15_1= dvol[11,5]/dvol[11,1]
ratio15_10=dvol[11,5]/dvol[11,4]



##3

ge<-read_excel("F567C.s2018.HW6.GE data.xlsx",sheet="GE")

##4

