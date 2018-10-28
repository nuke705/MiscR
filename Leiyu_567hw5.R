library(fGarch)
#setwd("/Users/leiyuzhou/Desktop/fin567")
data <- read.table("E:/study/HW5DATA.csv", header = TRUE,sep=",")

return<-data$return
siga1=sqrt(sum(return[1:10]*return[1:10])/10)
siga=sqrt(sum(return*return)/length(return))
alpha=0.2
beta=0.1
forcast=c()

initialvalue=c(siga1,siga,alpha,beta);

S=1e-6
lowerBounds= c(0, 0, alpha=S, beta= S)
upperBounds= c(1, 1, alpha=1-S, beta= 1-S)
garchDist=function(z,hh){dnorm(x=z/hh)/hh}  
f= function (parm){
  siga1=parm[1]; siga=parm[2]; alpha=parm[3]; beta=parm[4];
  e=(1-alpha-beta)*siga^2+alpha*return[1:(length(return)-1)]^2
  h=filter(e,beta,"r",init = siga1^2)
  hh=sqrt(abs(h))
  llh=-sum(log(garchDist(return[2:length(return)],hh)))-log(garchDist(return[1],siga1))
  llh
} 
print(f(initialvalue))
fit=nlminb(start = initialvalue, objective = f, lower= lowerBounds, upper=upperBounds,control = list(trace=3))
fit1=optim(initialvalue,fn=function(parm){f(parm)})
fit1

e=(1-fit1$par[3]-fit1$par[4])*fit1$par[2]^2+fit1$par[3]*return[1:(length(return))]^2
h=filter(e,fit1$par[4],"r",init = fit1$par[1]^2)
forcast[1]=h[1000]



f2= function (parm){
  siga1=parm[1]; alpha=parm[2]; beta=parm[3];
  siga=stdev(data$return)
  e=(1-alpha-beta)*siga^2+alpha*return[1:(length(return)-1)]^2
  h=filter(e,beta,"r",init = siga1^2)
  hh=sqrt(abs(h))
  llh=-sum(log(garchDist(return[2:length(return)],hh)))-log(garchDist(return[1],siga1))
  llh
} 
initialvalue=c(siga1,alpha,beta);
fit2=optim(initialvalue,fn=function(parm){f2(parm)})
fit2
siga=stdev(data$return)
e=(1-fit2$par[2]-fit2$par[3])*siga^2+fit2$par[2]*return[1:(length(return))]^2
h=filter(e,fit2$par[3],"r",init = fit2$par[1]^2)
forcast[2]=h[1000]



f3= function (parm){
  siga=parm[1]; alpha=parm[2]; beta=parm[3];
  siga1=stdev(data$return)
  
  e=(1-alpha-beta)*siga^2+alpha*return[1:(length(return)-1)]^2
  h=filter(e,beta,"r",init = siga1^2)
  hh=sqrt(abs(h))
  llh=-sum(log(garchDist(return[2:length(return)],hh)))-log(garchDist(return[1],siga1))
  llh
} 
initialvalue=c(siga,alpha,beta);
fit3=optim(initialvalue,fn=function(parm){f3(parm)})
fit3
siga1=stdev(data$return)
e=(1-fit3$par[2]-fit3$par[3])*fit3$par[1]^2+fit3$par[2]*return[1:(length(return))]^2
h=filter(e,fit3$par[3],"r",init = siga1^2)
forcast[3]=h[1000]


f4= function (parm){
  alpha=parm[1]; beta=parm[2];
  siga1=stdev(data$return)
  siga=stdev(data$return)
  e=(1-alpha-beta)*siga^2+alpha*return[1:(length(return)-1)]^2
  h=filter(e,beta,"r",init = siga1^2)
  hh=sqrt(abs(h))
  llh=-sum(log(garchDist(return[2:length(return)],hh)))-log(garchDist(return[1],siga1))
  llh
} 
initialvalue=c(alpha,beta);
fit4=optim(initialvalue,fn=function(parm){f4(parm)})
fit4
siga1=stdev(data$return)
siga=stdev(data$return)
e=(1-fit4$par[1]-fit4$par[2])*siga^2+fit4$par[1]*return[1:(length(return))]^2
h=filter(e,fit4$par[2],"r",init = siga1^2)
forcast[4]=h[1000]



##question 2
v1<-c();v2<-c();v3<-c();v4<-c();
for (i in 1: 21){
  v1[i]=fit1$par[2]^2+(fit1$par[3]+fit1$par[4])^(i-1)*(forcast[1]-fit1$par[2]^2)
  v2[i]=stdev(data$return)^2+(fit1$par[2]+fit1$par[3])^(i-1)*(forcast[2]-stdev(data$return)^2)
  v3[i]=fit1$par[1]^2+(fit1$par[2]+fit1$par[3])^(i-1)*(forcast[3]-fit1$par[1]^2)
  v4[i]=stdev(data$return)^2+(fit1$par[1]+fit1$par[2])^(i-1)*(forcast[4]-stdev(data$return)^2)
}
realizedv<-c()
realizedv[1]=sum(v1); realizedv[2]=sum(v2); realizedv[3]=sum(v3); realizedv[4]=sum(v4);
anualizedv=sqrt(realizedv)*sqrt(252/21)
 
##question 3
library(tseries)
vol=data$return^2
vol.garch=garch(data$return,order=c(1,1))


volfit=garchFit(~ arma(0,0)+garch(1,1), data=data$return, cond.dist="norm")
vol.garch$coef
volfit@fit$par























##
  var1=function(siga1){siga1*siga1}
var2=function(alpha,beta,return1,var1,siga){(1-alpha-beta)*siga*siga+alpha*return1*return1+beta*var1}
vart=function(alpha,beta,siga,return,var2){(1-alpha-beta)*siga*siga+alpha*return*return+beta*var2}
f=function(return,var){log(1/sqrt(2*pi*var)*exp(-0.5*return*return/var))}


optim(initialvalue,fn=function(){f(0,var1(siga1))+f(alpha,beta,return[1],var1(siga1),siga)+
    sum(f(return[3:length(return)],vart(alpha, beta, siga, return[3:length(return)], var2)))}
    
    garchDist=function(z,hh){dnorm(x=z/hh)/hh}
    garchLLH=function (parm){
      mu=parm[1]; omega=parm[2]; alpha=parm[3]; beta=parm[4]
      z=(x-mu); Mean=mean(z^2);
      e=omega+alpha*c(Mean, z[-length(x)]^2)
      h= filter(e,beta,"r", init=Mean)
      hh=sqrt(abs(h))
      llh=-sum(log(garchDist(z,hh)))
      llh
    }   
    print(garchLLH(params))
    */ 
      
      ???*
      estimate_vart[1]=siga1*siga1
    for (i in 2:length(return)){
      estimate_vart[i]=(1-alpha-beta)*siga*siga+alpha*return[i-1]*return[i-1]+beta*estimate_vart[i-1]
    }
    
    logf=log(1/sqrt(2*pi*estimate_vart)*exp(-0.5*return*return/estimate_vart))
##
     