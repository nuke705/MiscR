library(readxl)
setwd("E:/study")
tree<-read_excel("Principal_data_tree.xlsx",sheet="Sheet1")
#dat = read_excel("Principal_data_tree.xlsx",sheet="sheet1")
esi = tree$ESI
hibor = tree$hibor
ret = tree$`Next 1 W Index Return`


value<-c()
num4=0
num3=0
num_3=0
num_4=0
num0=0
value[1] = 100
for(i in 2:(length(ret)-1))
{
  if ((is.na(hibor[i-1]) != T)&(is.na(esi[i-1]) != T)){
      
    if((hibor[i-1] >= 3.898)){
        #4
        value[i]=value[i-1]*(0.33*1+0.17*(1+3*ret[i-1])+0.5*(1+ret[i-1]))
        num4=num4+1
        
      }else if( (hibor[i-1] < 0.098)& (esi[i-1] < 2.85)){
        #3
        value[i]=value[i-1]*(0.33*1+0.17*(1+3*ret[i-1])+0.5*(1+ret[i-1]))
        num3=num3+1
        
      }else if((hibor[i-1] < 0.1136)& (esi[i-1] >= 36.3)){
        #2
        value[i]=value[i-1]*(0.5*1+0.5*(1+ret[i-1]))
        
      }else if((hibor[i-1] >= 0.1136)&(hibor[i-1] < 1.547)){
        #1
        value[i]=value[i-1]*(0.5*1+0.5*(1+ret[i-1]))
        
      }else if( (esi[i-1]>= 2.85)& (esi[i-1]<36.3)& (hibor[i-1] < 0.1136)) {
        #-1
        value[i]=value[i-1]*(0.17*1+0.33*(1-3*(ret[i-1]))+0.5*(1+(ret[i-1])))
        
      }else if( (hibor[i-1]<0.1136)&(hibor[i-1]>= 0.09815)&(esi[i-1]<2.85)) {
        #-2
        value[i]=value[i-1]*(0.17*1+0.33*(1-3*(ret[i-1]))+0.5*(1+(ret[i-1])))
        
      }else if((hibor[i-1]<3.898 )&(hibor[i-1]>=1.547)){
        #-3
        value[i]=value[i-1]*(0.5*(1-3*(ret[i-1]))+0.5*(1+(ret[i-1])))
        num_3=num_3+1
        
      }else {
      
        value[i]=value[i-1]*(0.33*1+0.17*(1-3*(ret[i-1]))+0.5*(1+(ret[i-1])))
        num0=num0+1
      }
  }else {
    value[i]=value[i-1]*(0.33*1+0.17*(1-3*(ret[i-1]))+0.5*(1+(ret[i-1])))
    num0=num0+1
    
  }
  
}

plot(value)
Annulized_return=(value[727]/100)^(1/14)-1
Annulized_return
return=NA
for(i in 2:727){
  return[i]=value[i]/value[i-1]-1
}

Annulized_std=sd(return,na.rm=T)*sqrt(52)
Annulized_std
Sharpratio=Annulized_return/Annulized_std
Sharpratio
er=NA
for(i in 2:727){
  er[i]=return[i]-ret[i]
}
tracking_error=sd(er,na.rm=T)*sqrt(52)
tracking_error
return=1
for(i in 1:727){
  return=return*(1+ret[i])
}
information_ratio=((value[727]/100-return)^(1/14)-1)/tracking_error
information_ratio



#drawdown<-rep(0,182)
#for(i in seq(1,727,1))
#{
 # v1=value[i+1]/value[i]-1
 # v2=value[i+2]/value[i]-1
 # v3=value[i+3]/value[i]-1
 # v4=value[i+2]/value[i+1]-1
 # v5=value[i+3]/value[i+1]-1
 # v6=value[i+3]/value[i+2]-1
 # drawdown[i]=min(c(v1,v2,v3,v4,v5,v6))
#

#min(drawdown,na.rm = T)
maxd = c(0)
for (i in 2:length(value)){
  maxd[i]=(value[i]-max(value[1:i]))/max(value[1:i])
}
min(maxd)





