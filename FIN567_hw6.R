setwd("E:/study")
library(readxl)
##1
goog<-read_excel("F567C.s2018.HW6.GOOG data.xlsx",sheet="GOOG")
ret=log(goog$close[2:length(goog$close)]/goog$close[1:(length(goog$close)-1)])
google<-data.frame(goog[2:length(goog$close),],ret)

eachdaygoog=split(goog,goog$date)


var1<-c()
r2sum=0
n=1
date=google$date[1]
for (i in 1:length(google$date)){
  if (date==google$date[i]) {
    r2sum=r2sum+google$ret[i]^2
  } else {
    var1[n]=r2sum
    r2sum=0
    n=n+1
    date=google$date[i]
  }
  if (i==length(google$date)) {
    var1[n]=r2sum
  }
}
mean(var1)
##2---1
#rv2=c()
#rv5= c()
#rv10= c()
#rv15 = c()
#for (i in 1:10){
#data2=data.frame(eachdaygoog[i])[seq(1,391,2),]
#  data5=data.frame(eachdaygoog[i])[seq(1,391,5),]
#  data10=data.frame(eachdaygoog[i])[seq(1,391,10),]
#  data15=data.frame(eachdaygoog[i])[seq(1,391,15),]
#  #8th col is close price
#  rv2[i]=sum(diff(log(data2[,8]), lag = 1)^2)
#  rv5[i]=sum(diff(log(data5[,8]), lag = 1)^2)
#  rv10[i]=sum(diff(log(data10[,8]), lag = 1)^2)
#  rv15[i]=sum(diff(log(data15[,8]), lag = 1)^2)
#}
#answer is this table
#goog_res<-data.frame(var1,rv2,rv5,rv10,rv15)
#goog_res[nrow(goog_res)+1,]<-c(mean(var1),mean(rv2),mean(rv5),mean(rv10),mean(rv15))

##2----2
rv2_1= c();rv5_1= c();rv10_1= c();rv15_1= c();
for (i in 1:10){
  rv2_temp=c();rv5_temp=c();rv10_temp=c();rv15_temp=c();
  for (j in 1:2){
    data2=data.frame(eachdaygoog[i])[seq(j,391,2),]
    rv2_temp[j]=sum(diff(log(data2[,8]), lag = 1)^2)
  }
  rv2_1[i]=mean(rv2_temp)
  for (j in 1:5){
    data5=data.frame(eachdaygoog[i])[seq(j,391,5),]
    rv5_temp[j]=sum(diff(log(data5[,8]), lag = 1)^2)
  }
  rv5_1[i]=mean(rv5_temp)
  for (j in 1:10){
    data10=data.frame(eachdaygoog[i])[seq(j,391,10),]
    rv10_temp[j]=sum(diff(log(data10[,8]), lag = 1)^2)
  }
  rv10_1[i]=mean(rv10_temp)
  for (j in 1:15){
    data15=data.frame(eachdaygoog[i])[seq(j,391,15),]
    rv15_temp[j]=sum(diff(log(data15[,8]), lag = 1)^2)
  }
  rv15_1[i]=mean(rv15_temp)
}

googA<-data.frame(var1,rv2_1,rv5_1,rv10_1,rv15_1)
#answer is this table
googA[nrow(googA)+1,]<-c(mean(var1),mean(rv2_1),mean(rv5_1),mean(rv10_1),mean(rv15_1))


#Q2B 
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
        if (k!=1) 
        {dvar[n,col]=dvar[n,col]+(390/j+1)/(390/j)*r2sum/j}
        else
        {dvar[n,col]=dvar[n,col]+r2sum/j}
        r2sum=0
        n=n+1
        date=goog$date[i]
        i=i-j+1+(k>1)*j
      }else{
        r=log(goog$close[i]/goog$close[i-j])
        r2sum=r2sum+r^2
      }
      if (i>(length(goog$date)-j)) 
        {dvar[n,col]=dvar[n,col]+r2sum/j; }
    }    
  }
}
mean(dvar[1:10,1])

Q2B=matrix(data=0,nrow=11,ncol=5)
Q2B[1:10,1]=var1
Q2B[11,1]=mean(var1)
Q2B[1:10,2:5]=dvar
Q2B[11,2]=mean(dvar[,1]);Q2B[11,3]=mean(dvar[,2]);
Q2B[11,4]=mean(dvar[,3]);Q2B[11,5]=mean(dvar[,4]);



#Q2---3:
(googA$rv15_1[11])/(googA$var1[11])
(googA$rv15_1[11])/(googA$rv10_1[11])

#Q3
ge=read_excel("F567C.s2018.HW6.GE data.xlsx",sheet = 'GE')


retge=log(ge$close[2:length(ge$close)]/ge$close[1:(length(ge$close)-1)])
geret<-data.frame(ge[2:length(ge$close),],retge)

#eachdayge=split(ge,ge$date)


var1ge<-c()
r2sum=0
n=1
date=geret$date[1]
for (i in 1:length(geret$date)){
  if (date==geret$date[i]) {
    r2sum=r2sum+geret$ret[i]^2
  } else {
    var1ge[n]=r2sum
    r2sum=0
    n=n+1
    date=geret$date[i]
  }
  if (i==length(geret$date)) {
    var1ge[n]=r2sum
  }
}
mean(var1ge)


#Q3a
s<-c(2,5,10,15)
gevar=matrix(data=0,nrow=10,ncol=4)
col=0
for (j in s){
  col=col+1
  r2sum=0
  n=1
  
  for (k in 1:j){
    n=1
    date=ge$date[k]
    r2sum=0
    i=k
    while (i<=(length(ge$date)-j)){
      i=i+j
      if (ge$date[i]!=date){
        if (k!=1) 
        {gevar[n,col]=gevar[n,col]+(390/j+1)/(390/j)*r2sum/j}
        else
        {gevar[n,col]=gevar[n,col]+r2sum/j}
        r2sum=0
        n=n+1
        date=ge$date[i]
        i=i-j+1+(k>1)*j
      }else{
        r=log(ge$close[i]/ge$close[i-j])
        r2sum=r2sum+r^2
      }
      if (i>(length(ge$date)-j)) {
        gevar[n,col]=gevar[n,col]+r2sum/j; }
    }    
  }
}
mean(gevar[1:10,1])


Q3=matrix(data=0,nrow=11,ncol=5)
Q3[1:10,1]=var1ge
Q3[11,1]=mean(var1ge)
Q3[1:10,2:5]=gevar
Q3[11,2]=mean(gevar[,1]);Q3[11,3]=mean(gevar[,2]);
Q3[11,4]=mean(gevar[,3]);Q3[11,5]=mean(gevar[,4]);



#Q3:
(Q3[11,5])/(Q3[11,1])
(Q3[11,5])/(Q3[11,4])

#3(3):
#ge is more dependent on return interval


#section 4(1):
# price on feb 5 =765.74
temp=data.frame(eachdaygoog[1])
r1=(log(temp$X2013.02.06.close[length(temp$X2013.02.06.close)])-log(765.74))^2

for (i in 2:10){
  r1=r1+(log(data.frame(eachdaygoog[i])[391,8])-
           log(data.frame(eachdaygoog[i-1])[391,8]))^2
}
rv24_goog=rep(0,10)
for (i in 1:10){
  rv24_goog[i]=googA$rv15_1[i]*(r1/sum(googA$rv15_1[1:10]))
}

tempr=(log(data.frame(eachdaygoog[1])[1,5])-log(765.74))^2
rv24_goog_2= c(tempr+googA$rv15_1[1])

for (i in 2:10){
  rv24_goog_2[i]=(log(data.frame(eachdaygoog[i])[1,5])-
                    log(data.frame(eachdaygoog[i-1])[391,8]))^2+googA$rv15_1[i]
}







