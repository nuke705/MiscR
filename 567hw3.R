setwd("E:/study")

djx <- read.csv("djxprice.csv",header = T)
spx <- read.csv("spxprice.csv",header = T)
djximv <- read.csv("djximv.csv",header = T)
spximv <- read.csv("spximv.csv",header = T)
djprice<-djx$close
spprice<-spx$close
djvol<-djximv$impl_volatility
spvol<-spximv$impl_volatility
rsp=log(spprice[1:length(spprice)-1]/spprice[2:length(spprice)])
rsp = data.frame(Date = spx[1:length(rsp),2],rsp)
rdj=log(djprice[1:length(djprice)-1]/djprice[2:length(djprice)])
rdj = data.frame(Date = spx[1:length(rdj),2],rdj)
rdjvol=log(djvol[1:length(djvol)-1]/djvol[2:length(djvol)])
rdjvol = data.frame(Date = spx[1:length(rdjvol),2],rdjvol)
rspvol=log(spvol[1:length(spvol)-1]/spvol[2:length(spvol)])
rspvol = data.frame(Date = spx[1:length(rspvol),2],rspvol)

cov=matrix(data=0,nrow=4,ncol=4)
lam=0.94
for(n in 1:130){
  l=lam^(n-1)
  cov[1,1]=cov[1,1]+(1-lam)*l*rsp$rsp[(n+9)]*rsp$rsp[(n+9)]
  cov[1,2]=cov[1,2]+(1-lam)*l*rsp$rsp[(n+9)]*rdj$rdj[(n+9)]
  cov[1,3]=cov[1,3]+(1-lam)*l*rsp$rsp[(n+9)]*rspvol$rspvol[(n+9)]
  cov[1,4]=cov[1,4]+(1-lam)*l*rsp$rsp[(n+9)]*rdjvol$rdjvol[(n+9)]
  cov[2,1]=cov[1,2]
  cov[2,2]=cov[2,2]+(1-lam)*l*rdj$rdj[(n+9)]*rdj$rdj[(n+9)]
  cov[2,3]=cov[2,3]+(1-lam)*l*rdj$rdj[(n+9)]*rspvol$rspvol[(n+9)]
  cov[2,4]=cov[2,4]+(1-lam)*l*rdj$rdj[(n+9)]*rdjvol$rdjvol[(n+9)]
  cov[3,1]=cov[1,3]
  cov[3,2]=cov[2,3]
  cov[3,3]=cov[3,3]+(1-lam)*l*rspvol$rspvol[(n+9)]*rspvol$rspvol[(n+9)]
  cov[3,4]=cov[3,4]+(1-lam)*l*rspvol$rspvol[(n+9)]*rdjvol$rdjvol[(n+9)]
  cov[4,1]=cov[1,4]
  cov[4,2]=cov[2,4]
  cov[4,3]=cov[3,4]
  cov[4,4]=cov[4,4]+(1-lam)*l*rdjvol$rdjvol[(n+9)]*rdjvol$rdjvol[(n+9)]
}
varrsp=cov[1,1]*252
varrdj=cov[2,2]*252
varrspvol=cov[3,3]*252
varrdjvol=cov[4,4]*252

sdrsp = sqrt(varrsp)
sdrdj=  sqrt(varrdj)
sdspvol=sqrt(varrspvol)
sddjvol=sqrt(varrdjvol)
#August 19, 2015 the S&P 500 was at 2,079.61 and the
#Dow Jones index was at 17,348.73

library(MASS)
library(fOptions)

a=chol(cov)
spx_price=    c()
djx_price=    c()
spx_vol_call= c()
spx_vol_put=  c()
djx_vol_call= c()
djx_vol_put=  c()
for(i in 1:10000)
{
  x=rnorm(4)
  y=a %*% x
  spx_price[i]=exp(y[1])*2079.61
  djx_price[i]=exp(y[2])*173.4873
  spx_vol_call[i]=exp(y[3])*0.13889
  spx_vol_put[i]=exp(y[3])*0.127412
  djx_vol_call[i]=exp(y[4])*0.147749
  djx_vol_put[i]=exp(y[4])*0.136259
}
r = 0.00198833
b = 0.00198833-0.0186723
b1 =0.00198833-0.0219
call_spx<-GBSOption(TypeFlag = "c", spx_price, 2080,1/12, r,b, spx_vol_call)@price
put_spx=  GBSOption(TypeFlag = "p", spx_price, 2080, 1/12, r,b,  spx_vol_put)@price
call_djx= GBSOption(TypeFlag = "c", djx_price, 173, 1/12, r,b1, djx_vol_call)@price
put_djx=  GBSOption(TypeFlag = "p", djx_price, 173, 1/12, r,b1, djx_vol_put)@price



initialval=-(3.05+2.06)*100*550+(30.1+30.4)*100*50
value=-100*550*(call_djx+put_djx)+100*50*(call_spx+put_spx)
pl = value - initialval
#pl = value
varpf=quantile(pl,0.05)

#var3=-quantile(value[1:10000],0.05)*100
#thevar = var3*100
#4
#var=value[value<=var3]
#var[order(var)]
#shortfall=-mean(var)
#sigma = (-var3/qnorm(0.05))
#ES= sigma*dnorm(-var3/sigma)/pnorm(-var3/sigma)


eshortfall=pl[pl<=varpf]
eshortfall[order(eshortfall)]
ES=mean(eshortfall)
varpf
ES

