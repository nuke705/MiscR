library(readxl)
#这个有巴菲特
setwd("E:/study")
Fund<-read_excel("Principal_data_1.xlsx",sheet="Fund_Flow")
ERI<-read_excel("Principal_data_1.xlsx",sheet="ERI")
ESI<-read_excel("Principal_data_1.xlsx",sheet="ESI")
Buffet<-read_excel("Principal_data_1.xlsx",sheet="Buffet_Model")
Hibor<-read_excel("Principal_data_1.xlsx",sheet="HIBOR")
Buffet$Date=as.POSIXct(Buffet$Date)
ETF<-read_excel("Principal_data_1.xlsx",sheet="ETF")
HS<-read_excel("Principal_data_1.xlsx",sheet="HS")
data1<-merge(Fund,ERI,by="Date",all=TRUE)
#data2<-merge(data1,ESI,by="Date",all=)
data2<-merge(data1,Buffet,by="Date",all=TRUE)
#write.csv(data3,"E:/学习/UIUC/2018 Spring/Practicum/data/data3.csv")
#data4<-read.csv("E:/学习/UIUC/2018 Spring/Practicum/data/data3.csv")
data3<-merge(data2,ETF,by="Date",all=FALSE)
data<-merge(data3,Hibor,by="Date",all=FALSE)
data[is.na(data)]<-0
signal_fund<-c()
signal_fund[1]=0
signal_fund[2]=0
for(i in 3:nrow(data))
{
  if(data$`15W_percentage`[i]>0.02)
  {
    if((data$percentage[i]>data$percentage[i-1])&(data$percentage[i-1]>data$percentage[i-2]))
    {
      signal_fund[i]=1
    }
    else
    {
      signal_fund[i]=-1
    }
      
  }
  else if(data$`15W_percentage`[i]<(-0.02))
  {
    if((data$percentage[i]<data$percentage[i-1])&(data$percentage[i-1]<data$percentage[i-2]))
    {
      signal_fund[i]=-1
    }
    else
    {
      signal_fund[i]=1
    }
  }
  else
    {
      signal_fund[i]=0
    }
}
signal_Hibor<-c()
for(i in 1:nrow(data))
{
  signal_Hibor[i]=data$signal_hibor[i]
}
#signal_ESI<-c()
#for(i in 1:164)
#{
 # signal_ESI[i]=data$ESI_signal[i]
#}

signal_Buffet<-c()
for(i in 1:nrow(data))
{
  signal_Buffet[i]=data$newsignal[i]
}
signal_Momentum<-c()
for(i in 1:nrow(data))
{
  signal_Momentum[i]=data$MA_signal[i]
}
signal_overall<-c()
for(i in 1:nrow(data))
{
  #signal_overall[i]=data$signal[i]
  signal_overall[i]=signal_fund[i]+signal_Hibor[i]+signal_Buffet[i]+signal_Momentum[i]

}
value<-c()
num4=0
num3=0
num_3=0
num_4=0
value[1] = 100
for(i in 2:nrow(data))
{
    if(signal_overall[i-1]==5)
    {
      value[i]=value[i-1]*(0.17*1.0004+0.83*(1+3*(data$Price[i]/data$Price[i-1]-1)))
    }
    if(signal_overall[i-1]==4)
    {
      value[i]=value[i-1]*(0.33*1.0004+0.17*(1+3*(data$Price[i]/data$Price[i-1]-1))+0.5*(1+(data$Price[i]/data$Price[i-1]-1)))
      num4=num4+1
    }
    if(signal_overall[i-1]==3)
    {
      value[i]=value[i-1]*(0.33*1.0004+0.17*(1+3*(data$Price[i]/data$Price[i-1]-1))+0.5*(1+(data$Price[i]/data$Price[i-1]-1)))
      num3=num3+1
    }
    if(signal_overall[i-1]==2)
    {
      value[i]=value[i-1]*(0.5*1.0004+0.5*(1+(data$Price[i]/data$Price[i-1]-1)))
    }
    if(signal_overall[i-1]==1)
    {
      value[i]=value[i-1]*(0.5*1.0004+0.5*(1+(data$Price[i]/data$Price[i-1]-1)))
    }
    if(signal_overall[i-1]==0)
    {
      value[i]=value[i-1]*(0.33*1.0004+0.17*(1-3*(data$Price[i]/data$Price[i-1]-1))+0.5*(1+(data$Price[i]/data$Price[i-1]-1)))
    }
    if(signal_overall[i-1]==-1)
    {
      value[i]=value[i-1]*(0.17*1.0004+0.33*(1-3*(data$Price[i]/data$Price[i-1]-1))+0.5*(1+(data$Price[i]/data$Price[i-1]-1)))
    }
    if(signal_overall[i-1]==-2)
    {
      value[i]=value[i-1]*(0.17*1.0004+0.33*(1-3*(data$Price[i]/data$Price[i-1]-1))+0.5*(1+(data$Price[i]/data$Price[i-1]-1)))
    }
    if(signal_overall[i-1]==-3)
    {
      value[i]=value[i-1]*(0.5*(1-3*(data$Price[i]/data$Price[i-1]-1))+0.5*(1+(data$Price[i]/data$Price[i-1]-1)))
      num_3=num_3+1
    }
    if(signal_overall[i-1]==-4)
    {
      value[i]=value[i-1]*(0.5*(1-3*(data$Price[i]/data$Price[i-1]-1))+0.5*(1+(data$Price[i]/data$Price[i-1]-1)))
      num_4=num_4+1
    }
    if(signal_overall[i-1]==-5)
    {
      value[i]=value[i-1]*(0.17*1.0004+0.83*(1-3*(data$Price[i]/data$Price[i-1]-1)))
    }
}
return<-rep(0,nrow(data))
return1<-rep(0,nrow(data))
for(i in 1:(nrow(data)-1))
{
  return[i]=log(value[i+1]/value[i])
  return1[i]=log(data$Price[i+1]/data$Price[i])
}


data$signal_fund <- ""
plot(value)
log(value[nrow(data)]/100)/(nrow(data)/50)
sd(return,na.rm=T)*sqrt(50)
log(data$Price[nrow(data)]/data$Price[1])/(nrow(data)/50)
sd(return1,na.rm=T)*sqrt(50)
(log(value[nrow(data)]/100)/(nrow(data)/50))/(sd(return,na.rm=T)*sqrt(50))
(log(data$Price[nrow(data)]/data$Price[1])/(nrow(data)/50))/(sd(return1,na.rm=T)*sqrt(50))
num3
num4
num_3
num_4
min(return,na.rm=TRUE)
cutoff<-rep(0,50)
for(i in 1:50)
{
  cutoff[i]=quantile(return1,i*0.01,na.rm=TRUE)
}


output=NULL
output$signal1=signal_fund
output$signal2=signal_ERI
output$signal3=signal_Buffet
output$signal4=signal_Momentum
output$sigalall=signal_overall
output$rt=c(return1,0)
#write.csv(output,"E:/学习/UIUC/2018 Spring/Practicum/data/output.csv")

realsignal<-rep(0,164)
for(i in 1:164)
{
  if(return1[i]>0.03)
  {
    realsignal[i]=4
  }
    
  if(return1[i]>0.015 && return1[i]<=0.03)
  {
    realsignal[i]=3
  } 
  
  if(return1[i]>0.006 && return1[i]<=0.015)
  {
    realsignal[i]=2
  } 
  
  if(return1[i]>0.001 && return1[i]<=0.006)
  {
    realsignal[i]=1
  } 
  
  if(return1[i]>(-0.001) && return1[i]<=0.001)
  {
    realsignal[i]=0
  } 
  
  if(return1[i]>(-0.006) && return1[i]<=(-0.001))
  {
    realsignal[i]=-1
  } 
  
  if(return1[i]>(-0.015) && return1[i]<=(-0.006))
  {
    realsignal[i]=-2
  } 
  
  if(return1[i]>(-0.03) && return1[i]<=(-0.015))
  {
    realsignal[i]=-3
  } 
  
  if(return1[i]<=(-0.03))
  {
    realsignal[i]=-4
  } 
}
drawdown<-rep(0,35)
for(i in seq(1,139,1))
{
  v1=value[i+1]/value[i]-1
  v2=value[i+2]/value[i]-1
  v3=value[i+3]/value[i]-1
  v4=value[i+2]/value[i+1]-1
  v5=value[i+3]/value[i+1]-1
  v6=value[i+3]/value[i+2]-1
  drawdown[i]=min(c(v1,v2,v3,v4,v5,v6))
}

min(drawdown)

output=NULL
output$signal1=signal_fund[1:164]
output$signal2=signal_ERI[1:164]
output$signal3=signal_Buffet[1:164]
output$signal4=signal_Momentum[1:164]
output$signal=realsignal
write.csv(output,"/Users/yifanwang/2018 Spring/Practicum/data/regression.csv")