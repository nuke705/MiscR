library(readxl)
setwd("E:/ѧϰ/UIUC/2018 Spring/Practicum/data")
Fund<-read_excel("Principal_data.xlsx",sheet="Fund_Flow")
ERI<-read_excel("Principal_data.xlsx",sheet="ERI")
ESI<-read_excel("Principal_data.xlsx",sheet="ESI")
Buffet<-read_excel("Principal_data.xlsx",sheet="Buffet_Model")
ETF<-read_excel("Principal_data.xlsx",sheet="ETF")
signal_fund<-c()
signal_fund[1]=0
signal_fund[2]=0
for(i in 3:dim(Fund$percentage))
{
  if(Fund$`15W_percentage`[i]>0.02)
  {
    if((Fund$percentage[i]>Fund$percentage[i-1])&(Fund$percentage[i-1]>Fund$percentage[i-2]))
    {
      signal_fund[i]=1
    }
    else
    {
      signal_fund[i]=-1
    }
      
  }
  else if(Fund$`15W_percentage`[i]<-0.02)
  {
    if((Fund$percentage[i]<Fund$percentage[i-1])&(Fund$percentage[i-1]<Fund$percentage[i-2]))
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
signal_ERI<-c()
for(i in 1:dim(ERI$`Earning Revision Index`))
{
  if(ERI$`Earning Revision Index`[i]>0.35)
  {
    signal_ERI[i]=-1
    next
  }
  if(ERI$`Earning Revision Index`[i]<-0.35)
  {
    signal_ERI[i]=1
    next
  }
  if(ERI$`1W Chg`[i]>0.15)
  {
    signal_ERI[i]=1
    next
  }
  if(ERI$`1W Chg`[i]<-0.15)
  {
    signal_ERI[i]=-1
    next
  }
  if(ERI$`Diff with 3W MA`[i]>0.07)
  {
    signal_ERI[i]=1
    next
  }
  if(ERI$`Diff with 3W MA`[i]<-0.07)
  {
    signal_ERI[i]=-1
    next
  }
 else
  {
    signal_ERI[i]=0
  }
}
signal_ESI_day<-c()
signal_ESI_day[1]=0
signal_ESI_day[2]=0
signal_ESI_day[3]=0
signal_ESI_day[4]=0
signal_ESI_day[5]=0
for(i in 6:dim(ESI$`Economic Surprise Index`))
{
  if(ESI$`Economic Surprise Index`[i-1]>70)
  {
    signal_ESI_day[i]=-1
    next
  }
  if(ESI$`Economic Surprise Index`[i-1]<-10)
  {
    signal_ESI_day[i]=1
    next
  }
  if((ESI$`Economic Surprise Index`[i-1]-ESI$`Economic Surprise Index`[i-5])>2)
  {
    signal_ESI_day[i]=1
    next
  }
  if((ESI$`Economic Surprise Index`[i-1]-ESI$`Economic Surprise Index`[i-5])<-2)
  {
    signal_ESI_day[i]=-1
    next
  }
  else{
    signal_ESI_day[i]=0
  }
}
signal_ESI_week<-c()
for(i in 1:(0.2*dim(signal_ESI_day)))
{
  signal_ESI_week[i]=signal_ESI_day[5*(i-1)+1]
}
signal_Buffet_day<-c()
for(i in 1:dim(Buffet$`Buffet Model`))
{
  if(Buffet$`Buffet Model`[i]<Buffet$Low)
  {
    signal_Buffet_day[i]=-1
  }
  if(Buffet$`Buffet Model`[i]>Buffet$High)
  {
    signal_Buffet_day[i]=1
  }
  else
  {
    signal_Buffet_day[i]=0
  }
}
signal_Buffet_week<-c()
for(i in 1:(0.2*dim(signal_Buffet_day)))
{
  signal_Buffet_week[i]=signal_Buffet_day[5*(i-1)+1]
}
signal_overall<-c()
for(i in 1:(0.2*dim(ETF$price)))
{
  signal_overall=signal_fund+signal_ERI+signal_ESI_week+signal_Buffet_week

}
value<-100
for(i in seq(1,dim(ETF$price),5))
{
  if(signal_overall[(i-1)/5+1]==4)
  {
    value=value*(0.83+0.17*(1+3*(ETF$price[i+5]/ETF$price[i+1]-1)))
  }
  if(signal_overall[(i-1)/5+1]==3)
  {
    value=value*(0.83+0.17*(1+3*(ETF$price[i+5]/ETF$price[i+1]-1)))
  }
  if(signal_overall[(i-1)/5+1]==2)
  {
    value=value
  }
  if(signal_overall[(i-1)/5+1]==1)
  {
    value=value
  }
  if(signal_overall[(i-1)/5+1]==0)
  {
    value=value*(0.83+0.17*(1+3*(ETF$price[i+1]/ETF$price[i+5]-1)))
  }
  if(signal_overall[(i-1)/5+1]==-1)
  {
    value=value*(0.67+0.33*(1+3*(ETF$price[i+1]/ETF$price[i+5]-1)))
  }
  if(signal_overall[(i-1)/5+1]==-2)
  {
    value=value*(0.67+0.33*(1+3*(ETF$price[i+1]/ETF$price[i+5]-1)))
  }
  if(signal_overall[(i-1)/5+1]==-3)
  {
    value=value*(0.5+0.5*(1+3*(ETF$price[i+1]/ETF$price[i+5]-1)))
  }
  if(signal_overall[(i-1)/5+1]==-4)
  {
    value=value*(0.5+0.5*(1+3*(ETF$price[i+1]/ETF$price[i+5]-1)))
  }
}

