##1 ngarch
library(fGarch)
library(readxl)
setwd("E:/study")
dat<- read_excel("F567.HW7.data.xlsx",sheet="returns data")


Er<-dat$EAFEA__1[2:length(dat$EAFEA__1)]
Rr<-dat$Russell__1[2:length(dat$Russell__1)]

siga1=sqrt(sum(Er[1:10]*Er[1:10])/10)
siga=sqrt(sum(Er*Er)/length(Er))
alpha=0.2
beta=0.1
theta=0.1
initialvalue=c(siga1,siga,alpha,beta,theta);


garchDist=function(z,hh){dnorm(x=z/hh)/hh}  
f= function (parm){
  siga1=parm[1]; siga=parm[2]; alpha=parm[3]; beta=parm[4]; theta=parm[5];
  
  h<-c()
  h[1]=siga1^2
  
  for (i in 1:length(ret)){
    h[i+1]=(1-alpha*(1+theta^2)-beta)*siga^2+alpha*(ret[i]-theta*sqrt(h[i]))^2+beta*h[i];
  }
  f <-(1/(sqrt(2*pi*sigmasqhat)))*exp(-0.5*ret^2/sigmasqhat)
  NeglogLH = -sum(log(f))
  
  hh=sqrt(abs(h))
  llh=-sum(log(garchDist(Er,hh[1:length(ret)])))
  llh
} 

ret=Er
f(initialvalue)
fit1=optim(initialvalue,fn=function(parm){f(parm)})
fit1

ret=Rr
siga1=0.8
siga=sqrt(sum(Rr*Rr)/length(Rr))
initialvalue=c(siga1,siga,alpha,beta,theta);
f(initialvalue)

fit2

##2 dcc
Esiga<-c()
Esiga[1]=fit1$par[1]
for (i in 1:(length(Er)-1)){
  Esiga[i+1]=(1-fit1$par[3]*(1+fit1$par[5]^2)-fit1$par[4])*fit1$par[2]^2+
    fit1$par[3]*(Er[i]-fit1$par[5]*sqrt(Esiga[i]))^2+fit1$par[4]*Esiga[i];
}
z1=Er/Esiga

Rsiga<-c()
Rsiga[1]=fit2$par[1]
for (i in 1:(length(Rr)-1)){
  Rsiga[i+1]=(1-fit2$par[3]*(1+fit2$par[5]^2)-fit2$par[4])*fit2$par[2]^2+
    fit2$par[3]*(Rr[i]-fit2$par[5]*sqrt(Rsiga[i]))^2+fit2$par[4]*Rsiga[i];
}
z2=Rr/Rsiga


