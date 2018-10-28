setwd("/Users/macbook/Desktop/hw/567hw/hw3")
price<- read.csv("index price.csv",header=T)
ivol<- read.csv("data1.csv")
spx=price$close[322:488]
djx=price$close[78:244]
ivol_spx=ivol$impl_volatility[168:334]
ivol_djx=ivol$impl_volatility[1:167]

#2(b)
rspx=log(spx[2:length(spx)]/spx[2:length(spx)-1])
rdjx=log(djx[2:length(djx)]/djx[2:length(djx)-1])
rvspx=log(ivol_spx[2:length(ivol_spx)]/ivol_spx[2:length(ivol_spx)-1])
rvdjx=log(ivol_djx[2:length(ivol_djx)]/ivol_djx[2:length(ivol_djx)-1])


cov=matrix(data=NA,nrow=4,ncol=4)
  total11=0; total12=0; total13=0; total14=0; total22=0; total23=0; total24=0; total33=0;
  total34=0; total44=0;
  for(n in 1:165){
    total11=total11+0.06/0.94*0.94^n*rspx[166-n]*rspx[166-n]
    total12=total12+0.06/0.94*0.94^n*rspx[166-n]*rdjx[166-n]
    total13=total13+0.06/0.94*0.94^n*rspx[166-n]*rvspx[166-n]
    total14=total14+0.06/0.94*0.94^n*rspx[166-n]*rvdjx[166-n]
    total22=total22+0.06/0.94*0.94^n*rdjx[166-n]*rdjx[166-n]
    total23=total23+0.06/0.94*0.94^n*rdjx[166-n]*rvspx[166-n]
    total24=total24+0.06/0.94*0.94^n*rdjx[166-n]*rvdjx[166-n]
    total33=total33+0.06/0.94*0.94^n*rvspx[166-n]*rvspx[166-n]
    total34=total34+0.06/0.94*0.94^n*rvspx[166-n]*rvdjx[166-n]
    total44=total44+0.06/0.94*0.94^n*rvdjx[166-n]*rvdjx[166-n]
  }
  cov[1,1]=0.06*rspx[166]*rspx[166]+0.94*total11;
  cov[1,2]=0.06*rspx[166]*rdjx[166]+0.94*total12;
  cov[1,3]=0.06*rspx[166]*rvspx[166]+0.94*total13;
  cov[1,4]=0.06*rspx[166]*rvdjx[166]+0.94*total14;
  cov[2,1]=cov[1,2]
  cov[2,2]=0.06*rdjx[166]*rdjx[166]+0.94*total22;
  cov[2,3]=0.06*rdjx[166]*rvspx[166]+0.94*total23;
  cov[2,4]=0.06*rdjx[166]*rvdjx[166]+0.94*total24;
  cov[3,1]=cov[1,3]
  cov[3,2]=cov[2,3]
  cov[3,3]=0.06*rvspx[166]*rvspx[166]+0.94*total33;
  cov[3,4]=0.06*rvspx[166]*rvdjx[166]+0.94*total34;
  cov[4,1]=cov[1,4]
  cov[4,2]=cov[2,4]
  cov[4,3]=cov[3,4]
  cov[4,4]=0.06*rvdjx[166]*rvdjx[166]+0.94*total44;

a=chol(cov)
x=rnorm(4)  
x

y=a %*% x
y
y[1]
#2(c)
asd_rspx=sqrt(cov[1,1]*252)
asd_rdjx=sqrt(cov[2,2]*252)
asd_rvspx=sqrt(cov[3,3]*252)
asd_rvdjx=sqrt(cov[4,4]*252)

#3
install.packages("fOptions")
library(fOptions)
a=chol(cov)
spx_price=integer(10000)
djx_price=integer(10000)
spx_vol_call=integer(10000)
spx_vol_put=integer(10000)
djx_vol_call=integer(10000)
djx_vol_put=integer(10000)
for(i in 1:10000)
{
  x=rnorm(4)
  y=x %*% a
  spx_price[i]=exp(y[1])*2079.61
  djx_price[i]=expand.grid(y[2])*173.4873
  spx_vol_call[i]=exp(y[3])*0.13889
  spx_vol_put[i]=exp(y[3])*0.127412
  djx_vol_call[i]=exp(y[4])*0.147749
  djx_vol_put[i]=exp(y[4])*0.136259
}


call_spx<-GBSOption(TypeFlag = "c", spx_price, 2080, 1/12, 0.00198833, 0.00198833-0.01844812, spx_vol_call)@price
put_spx=GBSOption(TypeFlag = "p", S = spx_price, X = 2080, Time = 1/12, r = 0.00198833, b = 0.00198833-0.0186723, sigma = spx_vol_put)@price
call_djx=GBSOption(TypeFlag = "c", S = djx_price, X = 173, Time = 1/12, r = 0.00198833, b = 0.00198833-0.02186723, sigma = djx_vol_call)@price
put_djx=GBSOption(TypeFlag = "p", S = djx_price, X = 173, Time = 1/12, r = 0.00198833, b = 0.00198833-0.02186723, sigma = djx_vol_put)@price

value=550*(call_djx+put_djx)-50*(call_spx+put_spx)


var3=quantile(value[1:10000],0.05)

#4
var=value[value<=var3]
var[order(var)]
shortfall=mean(var)





