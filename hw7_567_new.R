##1 ngarch
library(fGarch)
setwd("E:/study")
dat<- read_excel("F567.HW7.data.xlsx",sheet="returns data")


Er<-dat$EAFEA__1[2:length(dat$EAFEA__1)]
Rr<-dat$Russell__1[2:length(dat$Russell__1)]
ret=Er
siga1=sqrt(sum(Er[1:10]*Er[1:10])/10)
siga=sqrt(sum(Er*Er)/length(Er))
alpha=0.2
beta=0.1
theta=0.1
omega=0.04
initialvalue=c(siga1,siga,alpha,beta,theta);


garchDist=function(z,hh){dnorm(x=z/hh)/hh} 
f= function (parm){
  siga1=parm[1]; siga=parm[2]; alpha=parm[3]; beta=parm[4]; theta=parm[5];
  
  h<-c()
  h[1]=siga1^2
  
  for (i in 1:(length(ret)-1)){
    h[i+1]=(1-alpha*(1+theta^2)-beta)*siga^2+alpha*(ret[i]-theta*sqrt(h[i]))^2+beta*h[i];
  }
  
  if ((parm[3]*(1+parm[5]^2)+parm[4])>=1 || parm[3]<0 || parm[4]<0 || parm[1]<0.001||  parm[2]<0.001){
    llh = 9999
  } 
  else{
    hh=sqrt(abs(h))
    llh=-sum(log(garchDist(ret,hh[1:length(ret)])))
    llh
  }
} 

ret=Er
f(initialvalue)
fit1=optim(initialvalue,fn=function(parm){f(parm)})
fit1

ret=Rr
siga1=sqrt(sum(Rr[1:10]*Rr[1:10])/10)
siga=sqrt(sum(Rr*Rr)/length(Rr))
initialvalue=c(siga1,siga,alpha,beta,theta);
f(initialvalue)
fit2=optim(initialvalue,fn=function(parm){f(parm)})
fit2

##2 dcc
##siga1=parm[1]; siga=parm[2]; alpha=parm[3]; beta=parm[4]; theta=parm[5];
Evol<-c()
Evol[1]=fit1$par[1]^2
for (i in 1:(length(Er)-1)){
  Evol[i+1]=(1-fit1$par[3]*(1+fit1$par[5]^2)-fit1$par[4])*fit1$par[2]^2+
    fit1$par[3]*(Er[i]-fit1$par[5]*sqrt(Evol[i]))^2+fit1$par[4]*Evol[i];
}
z1=Er/sqrt(Evol)

Rvol<-c()
Rvol[1]=fit2$par[1]^2
for (i in 1:(length(Rr)-1)){
  Rvol[i+1]=(1-fit2$par[3]*(1+fit2$par[5]^2)-fit2$par[4])*fit2$par[2]^2+
    fit2$par[3]*(Rr[i]-fit2$par[5]*sqrt(Rvol[i]))^2+fit2$par[4]*Rvol[i];
}
z2=Rr/sqrt(Rvol)

dcc=function (parm){
  alpha=parm[1]; beta=parm[2]; 
  
  meanrho=mean(z1*z2)
  q12<-c()
  q12[1]=meanrho
  
  q11<-c()
  q11[1]=1
  
  q22<-c()
  q22[1]=1
  
  rho12<-c()
  rho12[1]=q12[1]/sqrt(q11[1]*q22[1])
  
  for (i in 1:(length(z1)-1)){
    q12[i+1]=meanrho+alpha*(z1[i]*z2[i]-meanrho)+beta*(q12[i]-meanrho)

    q11[i+1]=1+alpha*(z1[i]^2-1)+beta*(q11[i]-1)
 
    q22[i+1]=1+alpha*(z2[i]^2-1)+beta*(q22[i]-1)
    
    rho12[i+1]=q12[i+1]/sqrt(q11[i+1]*q22[i+1])
  }
  
  if (parm[1]+parm[2]>1 || parm[1]<0 || parm[2]<0){
    llh = 99999
  } 
  else{
    ##llh=1/2*sum(log(1-rho12^2)+(z1^2+z2^2-2*rho12*z1*z2)/(1-rho12^2))
    llh=-sum(log(1/(2*pi*sqrt(1-rho12^2))*exp(-(z1^2 - 2*rho12*z1*z2+ z2^2)/(2-2* rho12^2))))
  }
  llh
}
initialvalue=c(0.4,0.5)
dcc(initialvalue)
fit3=optim(initialvalue,fn=function(parm){dcc(parm)})
fit3

fit3$par[1]+fit3$par[2]


meanrho=mean(z1*z2)

q12<-c()
q12[1]=meanrho
for (i in 1:(length(z1)-1)){
  q12[i+1]=meanrho+fit3$par[1]*(z1[i]*z2[i]-meanrho)+fit3$par[2]*(q12[i]-meanrho)
}

q11<-c()
q11[1]=1
for (i in 1:(length(z1)-1)){
  q11[i+1]=1+fit3$par[1]*(z1[i]^2-1)+fit3$par[2]*(q11[i]-1)
}

q22<-c()
q22[1]=1
for (i in 1:(length(z2)-1)){
  q22[i+1]=1+fit3$par[1]*(z2[i]^2-1)+fit3$par[2]*(q22[i]-1)
}

rho12=q12/sqrt(q11*q22)

##3
fEvol<-c()
fEvol[1]=(1-fit1$par[3]*(1+fit1$par[5]^2)-fit1$par[4])*fit1$par[2]^2+
  fit1$par[3]*(Er[3253]-fit1$par[5]*sqrt(Evol[3253]))^2+fit1$par[4]*Evol[3253];
fRvol<-c()
fRvol[1]=(1-fit1$par[3]*(1+fit1$par[5]^2)-fit1$par[4])*fit1$par[2]^2+
  fit1$par[3]*(Rr[3253]-fit1$par[5]*sqrt(Rvol[3253]))^2+fit1$par[4]*Rvol[3253];
fq12<-c()
fq12[1]=meanrho+fit3$par[1]*(z1[3253]*z2[3253]-meanrho)+fit3$par[2]*(q12[3253]-meanrho)
fq11<-c()
fq11[1]=1+fit3$par[1]*(z1[3253]^2-1)+fit3$par[2]*(q11[3253]-1)
fq22<-c()
fq22[1]=1+fit3$par[1]*(z2[3253]^2-1)+fit3$par[2]*(q22[3253]-1)
frho12<-c()
frho12[1]=fq12[1]/sqrt(fq11[1]*fq22[1])
fEr<-c()
fRr<-c()
fz1<-c();fz1[1]=0;
fz2<-c();fz2[1]=0;
bond_value<-c()
for(trial in 1:100000){
  for (i in 1:252){
    ##set.seed(0)
    #cov=matrix(data=0,nrow=2,ncol=2)
    #cov[1,1]=1*fEvol[i]; cov[2,2]=1*fRvol[i]; 
    #cov[1,2]=frho12[i]*sqrt(fEvol[i])*sqrt(fRvol[i]); cov[2,1]=frho12[i]*sqrt(fEvol[i])*sqrt(fRvol[i]);
    #a=chol(cov)
    #e=rnorm(2)
    #x=a %*% e
  
    #fEr[i]=x[1]
    #fRr[i]=x[2]
    
    #fz1[i]=fEr[i]/sqrt(fEvol[i])
    #fz2[i]=fRr[i]/sqrt(fRvol[i])
    
    fz1[i]=rnorm(1)
    fz2[i]=frho12[i] * fz1[i] + sqrt(1 - frho12[i]^2)*rnorm(1)
    fEr[i]=fz1[i]*sqrt(fEvol[i])
    fRr[i]=fz2[i]*sqrt(fRvol[i])
  
    fEvol[i+1]=(1-fit1$par[3]*(1+fit1$par[5]^2)-fit1$par[4])*fit1$par[2]^2+
      fit1$par[3]*(fEr[i]-fit1$par[5]*sqrt(fEvol[i]))^2+fit1$par[4]*fEvol[i];
    fRvol[i+1]=(1-fit1$par[3]*(1+fit1$par[5]^2)-fit1$par[4])*fit1$par[2]^2+
    fit1$par[3]*(fRr[i]-fit1$par[5]*sqrt(fRvol[i]))^2+fit1$par[4]*fRvol[i];
  
    fq12[i+1]=meanrho+fit3$par[1]*(fz1[i]*fz2[i]-meanrho)+fit3$par[2]*(fq12[i]-meanrho)
    fq11[i+1]=1+fit3$par[1]*(fz1[i]^2-1)+fit3$par[2]*(fq11[i]-1)
    fq22[i+1]=1+fit3$par[1]*(fz2[i]^2-1)+fit3$par[2]*(fq22[i]-1)
  
    frho12[i+1]=fq12[i+1]/sqrt(fq11[i+1]*fq22[i+1])
  }
  fEvalue<-c(); fEvalue[1]=100
  fRvalue<-c(); fRvalue[1]=100

  for (i in 1:252){
    fEvalue[i+1]=exp(fEr[i])*fEvalue[i]
    fRvalue[i+1]=exp(fRr[i])*fRvalue[i]
  }

  Eret=log(fEvalue[253]/fEvalue[1])
  Rret=log(fRvalue[253]/fRvalue[1])

  T_payoff=1000;
  w_payoff=4.1833
  if ((Eret<(-0.15)) || (Rret<(-0.15)) ){
    min_ret=min(Eret,Rret)
    T_payoff=1000+1000*(min_ret+0.15)*1.1765
  } 

  bond_value[trial]=T_payoff
}
mean(bond_value)+4.1833*12

