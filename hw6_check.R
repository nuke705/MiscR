#Group Member: Jiahao Huang, Shang Zeng, Qi Tang
#section 1:
library(readxl)

goog=read_excel("D:\\UIUC\\FE567\\F567C.s2018.HW6.GOOG data.xlsx")
ge=read_excel("D:\\UIUC\\FE567\\F567C.s2018.HW6.GE data.xlsx")

group_goog=split(goog,goog$date)
goog_rv=rep(0,10)

for (i in 1:10){
  data_=data.frame(group_goog[i])
  colnames(data_)=c('symbol','date','time','time-step','open','high','low','close','x_1','x_2','x_3')
  goog_rv[i]=sum((diff(log(data_$close)))^2)
}

mean(goog_rv)

#section 2(1):
rv2=rep(0,10)
rv5=rep(0,10)
rv10=rep(0,10)
rv15=rep(0,10)
for (i in 1:10){
  data2=data.frame(group_goog[i])[seq(1,391,by=2),]
  data5=data.frame(group_goog[i])[seq(1,391,by=5),]
  data10=data.frame(group_goog[i])[seq(1,391,by=10),]
  data15=data.frame(group_goog[i])[seq(1,391,by=15),]
  colnames(data2)=c('symbol','date','time','time-step','open','high','low','close','x_1','x_2','x_3')
  colnames(data5)=c('symbol','date','time','time-step','open','high','low','close','x_1','x_2','x_3')
  colnames(data10)=c('symbol','date','time','time-step','open','high','low','close','x_1','x_2','x_3')
  colnames(data15)=c('symbol','date','time','time-step','open','high','low','close','x_1','x_2','x_3')
  rv2[i]=sum(diff(log(data2$close), lag = 1)^2)
  rv5[i]=sum(diff(log(data5$close), lag = 1)^2)
  rv10[i]=sum(diff(log(data10$close), lag = 1)^2)
  rv15[i]=sum(diff(log(data15$close), lag = 1)^2)
}

total_goog<-data.frame(goog_rv,rv2,rv5,rv10,rv15)
total_goog[nrow(total_goog)+1,]<-c(mean(goog_rv),mean(rv2),mean(rv5),mean(rv10),mean(rv15))

#2(2):
#To adjust missing data, we can calculate the 15 realized variance of 15-minute returns and calculate their mean
rv2_1=rep(0,10)
rv5_1=rep(0,10)
rv10_1=rep(0,10)
rv15_1=rep(0,10)
for (i in 1:10){
  adjust_rv2=rep(0,2)
  adjust_rv5=rep(0,5)
  adjust_rv10=rep(0,10)
  adjust_rv15=rep(0,15)
  for (j in 1:2){
    data2=data.frame(group_goog[i])[seq(j,391,by=2),]
    colnames(data2)=c('symbol','date','time','time-step','open','high','low','close','x_1','x_2','x_3')
    adjust_rv2[j]=sum(diff(log(data2$close), lag = 1)^2)
  }
  rv2_1[i]=mean(adjust_rv2)
  for (j in 1:5){
    data5=data.frame(group_goog[i])[seq(j,391,by=5),]
    colnames(data5)=c('symbol','date','time','time-step','open','high','low','close','x_1','x_2','x_3')
    adjust_rv5[j]=sum(diff(log(data5$close), lag = 1)^2)
  }
  rv5_1[i]=mean(adjust_rv5)
  for (j in 1:10){
    data10=data.frame(group_goog[i])[seq(j,391,by=10),]
    colnames(data10)=c('symbol','date','time','time-step','open','high','low','close','x_1','x_2','x_3')
    adjust_rv10[j]=sum(diff(log(data10$close), lag = 1)^2)
  }
  rv10_1[i]=mean(adjust_rv10)
  for (j in 1:15){
    data15=data.frame(group_goog[i])[seq(j,391,by=15),]
    colnames(data15)=c('symbol','date','time','time-step','open','high','low','close','x_1','x_2','x_3')
    adjust_rv15[j]=sum(diff(log(data15$close), lag = 1)^2)
  }
  rv15_1[i]=mean(adjust_rv15)
}

adjust_goog<-data.frame(goog_rv,rv2_1,rv5_1,rv10_1,rv15_1)
adjust_goog[nrow(adjust_goog)+1,]<-c(mean(goog_rv),mean(rv2_1),mean(rv5_1),mean(rv10_1),mean(rv15_1))

#2(3):
(adjust_goog$rv15_1[11])/(adjust_goog$goog_rv[11])
(adjust_goog$rv15_1[11])/(adjust_goog$rv10_1[11])


#section 3(1):
group_ge=split(ge,ge$date)
ge_rv=rep(0,10)

for (i in 1:10){
  data_=data.frame(group_ge[i])
  colnames(data_)=c('symbol','date','time','time-step','open','high','low','close','volume')
  ge_rv[i]=sum((diff(log(data_$close)))^2)
}

ge_rv2_1=rep(0,10)
ge_rv5_1=rep(0,10)
ge_rv10_1=rep(0,10)
ge_rv15_1=rep(0,10)
for (i in 1:10){
  ge_adjust_rv2=rep(0,2)
  ge_adjust_rv5=rep(0,5)
  ge_adjust_rv10=rep(0,10)
  ge_adjust_rv15=rep(0,15)
  for (j in 1:2){
    data2=data.frame(group_ge[i])[seq(j,391,by=2),]
    colnames(data2)=c('symbol','date','time','time-step','open','high','low','close','volume')
    ge_adjust_rv2[j]=sum(diff(log(data2$close), lag = 1)^2)
  }
  ge_rv2_1[i]=mean(ge_adjust_rv2)
  for (j in 1:5){
    data5=data.frame(group_ge[i])[seq(j,391,by=5),]
    colnames(data5)=c('symbol','date','time','time-step','open','high','low','close','volume')
    ge_adjust_rv5[j]=sum(diff(log(data5$close), lag = 1)^2)
  }
  ge_rv5_1[i]=mean(ge_adjust_rv5)
  for (j in 1:10){
    data10=data.frame(group_ge[i])[seq(j,391,by=10),]
    colnames(data10)=c('symbol','date','time','time-step','open','high','low','close','volume')
    ge_adjust_rv10[j]=sum(diff(log(data10$close), lag = 1)^2)
  }
  ge_rv10_1[i]=mean(ge_adjust_rv10)
  for (j in 1:15){
    data15=data.frame(group_ge[i])[seq(j,391,by=15),]
    colnames(data15)=c('symbol','date','time','time-step','open','high','low','close','volume')
    ge_adjust_rv15[j]=sum(diff(log(data15$close), lag = 1)^2)
  }
  ge_rv15_1[i]=mean(ge_adjust_rv15)
}

adjust_ge<-data.frame(ge_rv,ge_rv2_1,ge_rv5_1,ge_rv10_1,ge_rv15_1)
adjust_ge[nrow(adjust_ge)+1,]<-c(mean(ge_rv),mean(ge_rv2_1),mean(ge_rv5_1),mean(ge_rv10_1),mean(ge_rv15_1))

#3(2):
adjust_ge$ge_rv15_1[11]/adjust_ge$ge_rv[11]
adjust_ge$ge_rv15_1[11]/adjust_ge$ge_rv10_1[11]

#3(3):
#ge is more dependent on return interval


#section 4(1):
feb5=765.74
temp=data.frame(group_goog[1])
r1=log(temp$X2013.02.06.close[length(temp$X2013.02.06.close)])-log(feb5)
r1=r1^2
for (i in 2:10){
  r1=r1+(log(data.frame(group_goog[i])[391,8])-log(data.frame(group_goog[i-1])[391,8]))^2
}
rv24_goog=rep(0,10)
for (i in 1:10){
  rv24_goog[i]=adjust_goog$rv15_1[i]*(r1/sum(adjust_goog$rv15_1[1:10]))
}

r1_=(log(data.frame(group_goog[1])[1,5])-log(feb5))^2
rv24_goog_2=rep(0,10)
rv24_goog_2[1]=r1_+adjust_goog$rv15_1[1]
for (i in 2:10){
  rv24_goog_2[i]=(log(data.frame(group_goog[i])[1,5])-log(data.frame(group_goog[i-1])[391,8]))^2+adjust_goog$rv15_1[i]
}
