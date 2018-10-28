setwd("/Users/leiyuzhou/Desktop/fin567")
updatedata <- read.csv("sampledata.csv",header = T)
ft= updatedata$FT.SE.100 * updatedata$USDGBP
dax=updatedata$DAX*updatedata$USDEUR
sp=updatedata$S.P.500
compoundrsp=log(sp[1:length(sp)-1]/sp[2:length(sp)])
compoundrsp = data.frame(Date = updatedata[1:length(compoundrsp),1],compoundrsp)
compoundrft=log(ft[1:length(ft)-1]/ft[2:length(ft)])
compoundrft = data.frame(Date = updatedata[1:length(compoundrft),1],compoundrft)
compoundrdax=log(dax[1:length(dax)-1]/dax[2:length(dax)])
compoundrdax = data.frame(Date = updatedata[1:length(compoundrdax),1],compoundrdax)

simplersp= (sp[1:length(sp)-1]-sp[2:length(sp)])/sp[2:length(sp)]
simplersp = data.frame(Date = updatedata[1:length(simplersp),1],simplersp)
simplerft=(ft[1:length(ft)-1]-ft[2:length(ft)])/ft[2:length(ft)]
simplerft = data.frame(Date = updatedata[1:length(simplerft),1],simplerft)
simplerdax=(dax[1:length(dax)-1]-dax[2:length(dax)])/dax[2:length(dax)]
simplerdax = data.frame(Date = updatedata[1:length(simplerdax),1],simplerdax)

mean(simplersp$simplersp[14:2469])
mean(simplerft$simplerft[14:2469])
mean(simplerdax$simplerdax[14:2469])
mean(compoundrsp$compoundrsp[14:2469])
mean(compoundrft$compoundrft[14:2469])
mean(compoundrdax$compoundrdax[14:2469])

VaR=c()
for (i in 14:253){
  portreturn<-1/3*(simplersp$simplersp[(i+1):(i+2000)]+simplerft$simplerft[(i+1):(i+2000)]+simplerdax$simplerdax[(i+1):(i+2000)])
  VaR[i-13]=-3000000*quantile(portreturn,0.01,rm=T)
}
VaR = data.frame(Date = updatedata[14:253,1],VaR)

DeltaVaR=c()
cov=matrix(data=NA,nrow=3,ncol=3)
w=c(1/3,1/3,1/3)
for (i in 14:253){
  cov[1,1]=mean(simplersp$simplersp[(i+1):(i+2000)]*simplersp$simplersp[(i+1):(i+2000)])
  cov[1,2]=mean(simplersp$simplersp[(i+1):(i+2000)]*simplerft$simplerft[(i+1):(i+2000)])
  cov[1,3]=mean(simplersp$simplersp[(i+1):(i+2000)]*simplerdax$simplerdax[(i+1):(i+2000)])
  cov[2,1]=cov[1,2]
  cov[2,2]=mean(simplerft$simplerft[(i+1):(i+2000)]*simplerft$simplerft[(i+1):(i+2000)])
  cov[2,3]=mean(simplerft$simplerft[(i+1):(i+2000)]*simplerdax$simplerdax[(i+1):(i+2000)])
  cov[3,1]=cov[1,3]
  cov[3,2]=cov[2,3]
  cov[3,3]=mean(simplerdax$simplerdax[(i+1):(i+2000)]*simplerdax$simplerdax[(i+1):(i+2000)])
  varport=t(w)%*%cov%*%w
  
  DeltaVaR[i-13]=3000000*2.33*sqrt(varport)
}
DeltaVaR = data.frame(Date = updatedata[14:253,1],DeltaVaR)


expoDeltaVaR=c()
cov=matrix(data=NA,nrow=3,ncol=3)
lamuda=0.94
for (i in 14:253){
  total11=0; total12=0; total13=0; total22=0; total23=0; total33=0;
  for(n in 1:100){
    l=lamuda^(n-1)
    total11=total11+(1-lamuda)*l*simplersp$simplersp[(i+n+1)]*simplersp$simplersp[(i+n+1)]
    total12=total12+(1-lamuda)*l*simplersp$simplersp[(i+n+1)]*simplerft$simplerft[(i+n+1)]
    total13=total13+(1-lamuda)*l*simplersp$simplersp[(i+n+1)]*simplerdax$simplerdax[(i+n+1)]
    total22=total22+(1-lamuda)*l*simplerft$simplerft[(i+n+1)]*simplerft$simplerft[(i+n+1)]
    total23=total23+(1-lamuda)*l*simplerft$simplerft[(i+n+1)]*simplerdax$simplerdax[(i+n+1)]
    total33=total33+(1-lamuda)*l*simplerdax$simplerdax[(i+n+1)]*simplerdax$simplerdax[(i+n+1)]
  }
  cov[1,1]=(1-lamuda)*simplersp$simplersp[(i+1)]*simplersp$simplersp[(i+1)]+lamuda*total11;
  cov[1,2]=(1-lamuda)*simplersp$simplersp[(i+1)]*simplerft$simplerft[(i+1)]+lamuda*total12;
  cov[1,3]=(1-lamuda)*simplersp$simplersp[(i+1)]*simplerdax$simplerdax[(i+1)]+lamuda*total13;
  cov[2,1]=cov[1,2]
  cov[2,2]=(1-lamuda)*simplerft$simplerft[(i+1)]*simplerft$simplerft[(i+1)]+lamuda*total22;
  cov[2,3]=(1-lamuda)*simplerft$simplerft[(i+1)]*simplerdax$simplerdax[(i+1)]+lamuda*total23;
  cov[3,1]=cov[1,3]
  cov[3,2]=cov[2,3]
  cov[3,3]=(1-lamuda)*simplerdax$simplerdax[(i+1)]*simplerdax$simplerdax[(i+1)]+lamuda*total33;
  varport=t(w)%*%cov%*%w
  
  expoDeltaVaR[i-13]=3000000*2.33*sqrt(varport)
}
expoDeltaVaR = data.frame(Date = updatedata[14:253,1],expoDeltaVaR)

rescaledrVaR =c()
sdsp=c(); sdft=c(); sddax=c();
lamuda=0.94
for (i in 14:(253+2000)){
  total11=0; total12=0; total13=0; total22=0; total23=0; total33=0;
  for(n in 1:100){
    l=lamuda^(n-1)
    total11=total11+(1-lamuda)*l*simplersp$simplersp[(i+n+1)]*simplersp$simplersp[(i+n+1)]
    total22=total22+(1-lamuda)*l*simplerft$simplerft[(i+n+1)]*simplerft$simplerft[(i+n+1)]
    total33=total33+(1-lamuda)*l*simplerdax$simplerdax[(i+n+1)]*simplerdax$simplerdax[(i+n+1)]
  }
  sdsp[i]=sqrt((1-lamuda)*simplersp$simplersp[(i+1)]*simplersp$simplersp[(i+1)]+lamuda*total11);
  sdft[i]=sqrt((1-lamuda)*simplerft$simplerft[(i+1)]*simplerft$simplerft[(i+1)]+lamuda*total22);
  sddax[i]=sqrt((1-lamuda)*simplerdax$simplerdax[(i+1)]*simplerdax$simplerdax[(i+1)]+lamuda*total33);
}

u1=c(); u2=c(); u3=c(); portreturn=c();
for (i in 14:253){
  sd1=sdsp[i];
  sd2=sdft[i];
  sd3=sddax[i];
  for (n in 1:2000){
    u1[n]=sd1*simplersp$simplersp[i+n]/sdsp[i+n]
    u2[n]=sd2*simplerft$simplerft[i+n]/sdft[i+n]
    u3[n]=sd3*simplerdax$simplerdax[i+n]/sddax[i+n]
  }
  portreturn=1/3*(u1+u2+u3);
  rescaledrVaR[i-13]=-3000000*quantile(portreturn,0.01,rm=T)
}
rescaledrVaR = data.frame(Date = updatedata[14:253,1],rescaledrVaR)

write.csv(VaR,"/Users/leiyuzhou/Desktop/fin567/VaR.csv") 
write.csv(DeltaVaR,"/Users/leiyuzhou/Desktop/fin567/DeltaVaR.csv") 

write.csv(expoDeltaVaR,"/Users/leiyuzhou/Desktop/fin567/expoDeltaVaR.csv") 

write.csv(rescaledrVaR,"/Users/leiyuzhou/Desktop/fin567/rescaledrVaR.csv") 
