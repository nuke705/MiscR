library(readxl)
setwd("/Users/admin/Desktop/msfe/principal practicum/Week9")
odsg=read_excel("signal.xlsx",sheet="Sheet2")
odsg=odsg[,1:6]
odsg[is.na(odsg)]=0
odsg$signal=NA
len=727
for(i in 1:len){
  if((odsg$`Factor 4`[i]<7.265)&(odsg$`Factor 1`[i]<(-0.002412))&(odsg$`Factor 4`[i]<4.546)){
    odsg$signal[i]=3
  }
  else if((odsg$`Factor 4`[i]>=7.265)){
    odsg$signal[i]=2
  }
  else if((odsg$`Factor 4`[i]<7.265)&(odsg$`Factor 1`[i]<(-0.002412))&(odsg$`Factor 4`[i]>=4.546)){
    odsg$signal[i]=1
  }
  else if((odsg$`Factor 4`[i]<7.265)&(odsg$`Factor 1`[i]>=(-0.002412))&(odsg$`Factor 1`[i]>=0.00357)&(odsg$`Factor 1`[i]<0.03694)){
    odsg$signal[i]=0
  }
  else if((odsg$`Factor 4`[i]<7.265)&(odsg$`Factor 1`[i]>=(-0.002412))&(odsg$`Factor 1`[i]>=0.00357)&(odsg$`Factor 1`[i]>=0.03694)){
    odsg$signal[i]=-1
  }
  else if((odsg$`Factor 4`[i]<7.265)&(odsg$`Factor 1`[i]>=(-0.002412))&(odsg$`Factor 1`[i]<0.00357)){
    odsg$signal[i]=-2
  }
}
length(which(odsg$signal==-2))

ini_v=100
odsg$v=NA
odsg$v[1]=0.5*100+0.5*100*(1+odsg$`Next 1 W Index Return`[1])
for(i in 2:len){
  if(odsg$signal[i]==3){
    odsg$v[i]=0.33*odsg$v[i-1]+0.5*odsg$v[i-1]*(1+odsg$`Next 1 W Index Return`[i])+0.17*odsg$v[i-1]*(1+3*odsg$`Next 1 W Index Return`[i])
  }
  else if(odsg$signal[i]==2){
    odsg$v[i]=0.33*odsg$v[i-1]+0.5*odsg$v[i-1]*(1+odsg$`Next 1 W Index Return`[i])+0.17*odsg$v[i-1]*(1+3*odsg$`Next 1 W Index Return`[i])
  }
  else if(odsg$signal[i]==1){
    odsg$v[i]=0.5*odsg$v[i-1]+0.5*odsg$v[i-1]*(1+odsg$`Next 1 W Index Return`[i])
  }
  else if(odsg$signal[i]==0){
    odsg$v[i]=0.5*odsg$v[i-1]+0.5*odsg$v[i-1]*(1+odsg$`Next 1 W Index Return`[i])
  }
  else if(odsg$signal[i]==-1){
    odsg$v[i]=0.33*odsg$v[i-1]+0.5*odsg$v[i-1]*(1+odsg$`Next 1 W Index Return`[i])+0.17*odsg$v[i-1]*(1-3*odsg$`Next 1 W Index Return`[i])
  }
  else if(odsg$signal[i]==-2){
    odsg$v[i]=0.17*odsg$v[i-1]+0.5*odsg$v[i-1]*(1+odsg$`Next 1 W Index Return`[i])+0.33*odsg$v[i-1]*(1-3*odsg$`Next 1 W Index Return`[i])
  }
}

plot(odsg$v)
Annulized_return=(odsg$v[727]/100)^(1/14)-1
Annulized_return
odsg$return=NA
for(i in 2:727){
  odsg$return[i]=odsg$v[i]/odsg$v[i-1]-1
}

Annulized_std=sd(odsg$return,na.rm=T)*sqrt(52)
Annulized_std
Sharpratio=Annulized_return/Annulized_std
Sharpratio
odsg$er=NA
for(i in 2:727){
  odsg$er[i]=odsg$return[i]-odsg$`Next 1 W Index Return`[i]
}
tracking_error=sd(odsg$er,na.rm=T)*sqrt(52)
tracking_error
return=1
for(i in 1:727){
  return=return*(1+odsg$`Next 1 W Index Return`[i])
}
information_ratio=((odsg$v[727]/100-return)^(1/14)-1)/tracking_error
write.csv(odsg,"zy.csv")
