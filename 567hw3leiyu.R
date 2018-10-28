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
sqrt(varrsp)
varrdj=cov[2,2]*252
sqrt(varrdj)
varrspvol=cov[3,3]*252
sqrt(varrspvol)
varrdjvol=cov[4,4]*252
sqrt(varrdjvol)
library(timeDate)
library(timeSeries)
library(fBasics)
library(fOptions)
library(mvtnorm)
spx_price=c()
djx_price=c()
spx_vol_call=c()
spx_vol_put=c()
djx_vol_call=c()
djx_vol_put=c()
for (i in 1:10000){
  a=chol(cov)
  e=rnorm(4)
  x=a %*% e
  spx_price[i]=exp(x[1])*2079.61
  djx_price[i]=exp(x[2])*173.4873
  spx_vol_call[i]=exp(x[3])*0.13889
  spx_vol_put[i]=exp(x[3])*0.127412
  djx_vol_call[i]=exp(x[4])*0.147749
  djx_vol_put[i]=exp(x[4])*0.136259
}
call_spx=GBSOption(TypeFlag = "c", spx_price, 2080, 1/12, 0.00198833, 0.00198833-0.01844812, spx_vol_call)@price
put_spx=GBSOption(TypeFlag = "p",  spx_price,  2080,  1/12,  0.00198833,  0.00198833-0.01844812, spx_vol_put)@price
call_djx=GBSOption(TypeFlag = "c",  djx_price,  173,  1/12,  0.00198833,  0.00198833-0.02186723, djx_vol_call)@price
put_djx=GBSOption(TypeFlag = "p",  djx_price,  173,  1/12,  0.00198833,  0.00198833-0.02186723, djx_vol_put)@price

initialval=-(3.05+2.06)*100*550+(30.1+30.4)*100*50
value=-100*550*(call_djx+put_djx)+100*50*(call_spx+put_spx)
pnl=value-initialval
var=quantile(pnl,0.05)

eshortfall=pnl[pnl<=var]
eshortfall[order(eshortfall)]
shortfall=mean(eshortfall)
var
shortfall

