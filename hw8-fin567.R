##1(a)
loss<-c()
loss[1:10000]=0
for (trial in 1:10000){ 
  z=rnorm(100)
  u=pnorm(z)
  tau=-log(1-u)/0.02
  for (i in 1:80){
    if (tau[i]<=1) {loss[trial]=loss[trial]+10}
  }
  for (i in 81:100){
    if (tau[i]<=1) {loss[trial]=loss[trial]+20}
  }
}
hist(loss)
mean(loss)
(80*10+20*20)*0.02
#2(a)
quantile(loss,0.999)-mean(loss)

##1(b)
loss<-c()
loss[1:100000]=0
rho=0.3
number_of_de<-c()
pab<-c()
for (trial in 1:100000){
  n=0;
  m=rnorm(1)
  e=rnorm(100)
  z=sqrt(rho)*m+sqrt(1-rho)*e
  u=pnorm(z)
  tau=-log(1-u)/0.02
  for (i in 1:80){
    if (tau[i]<=1) {loss[trial]=loss[trial]+10; n=n+1}
  }
  for (i in 81:100){
    if (tau[i]<=1) {loss[trial]=loss[trial]+20; n=n+1}
  }
  number_of_de[trial]=n;
  pab[trial]=(n*(n-1)/2)/(100*99/2)
}
hist(loss)
mean(loss)
##2(b)
quantile(loss,0.999)-mean(loss)

##1(c)

##pab=(6*5/2)/(100*99/2)
#de_corr=(pab-0.02*0.02)/(0.02*0.98)
de_corr=(mean(pab)-0.02*0.02)/(0.02*0.98)

##1(d)

f=function(parm){
  rho= parm[1]
  error=0.00001
  number_of_de<-c()
  de_corr<-c()
  for (trial in 1:10000){
    n=0;
    m=rnorm(1)
    e=rnorm(100)
    z=sqrt(rho)*m+sqrt(1-rho)*e
    u=pnorm(z)
    tau=-log(1-u)/0.02
    for (i in 1:80){
      if (tau[i]<=1) {n=n+1}
    }
    for (i in 81:100){
      if (tau[i]<=1) {n=n+1}
    }
    number_of_de[trial]=n;
    pab=(n*(n-1)/2)/(100*99/2)
    de_corr[trial]=(pab-0.02*0.02)/(0.02*0.98)
  }
  mean(de_corr)
  #if ((mean(de_corr)<(0.05+error))&(mean(de_corr)>(0.05-error))){
  #  print(mean(de_corr))
  #  llh=-999999
  #}
  #llh = 0
  #llh
}

for ( i in seq(0.25,0.3,0.0001)){
  rhoo = f(i)
  if (abs(rhoo - 0.05) < 0.0001){
    print(c(i,rhoo))
  }
}

##3
##(a)

loss_a<-c()
loss_a[1:10000]=0
loss_b<-c()
loss_b[1:10000]=0
loss<-c()
loss[1:10000]=0
for (trial in 1:10000){ 
  z=rnorm(100)
  u=pnorm(z)
  tau=-log(1-u)/0.02
  for (i in 1:80){
    if (tau[i]<=1) {loss_a[trial]=loss_a[trial]+10}
  }
  for (i in 81:100){
    if (tau[i]<=1) {loss_b[trial]=loss_b[trial]+20}
  }
  loss_b[trial]=loss_b[trial]-40
  if(loss_b[trial]<0) {loss_b[trial]=0}
  loss[trial]=loss_a[trial]+loss_b[trial]
}
hist(loss)
mean(loss)
##b
quantile(loss,0.999)-mean(loss)

##c
loss_a<-c()
loss_a[1:10000]=0
loss_b<-c()
loss_b[1:10000]=0
loss<-c()
loss[1:10000]=0
rho=0.3
number_of_de<-c()
de_corr<-c()
for (trial in 1:10000){
  n=0;
  m=rnorm(1)
  e=rnorm(100)
  z=sqrt(rho)*m+sqrt(1-rho)*e
  u=pnorm(z)
  tau=-log(1-u)/0.02
  for (i in 1:80){
    if (tau[i]<=1) {loss_a[trial]=loss_a[trial]+10}
  }
  for (i in 81:100){
    if (tau[i]<=1) {loss_b[trial]=loss_b[trial]+20}
  }
  loss_b[trial]=loss_b[trial]-40
  if(loss_b[trial]<0) {loss_b[trial]=0}
  loss[trial]=loss_a[trial]+loss_b[trial]
}
hist(loss)
mean(loss)
quantile(loss,0.999)-mean(loss)



##4
##(a)

beta=function(){
  bool=FALSE
  while (bool==FALSE){
    u1=runif(1)
    u2=runif(1)
    b=4*u2*(1-u2)
    if (u1<b) {random=u2; bool=TRUE}
  }
  random
}


loss<-c()
loss[1:10000]=0
for (trial in 1:10000){ 
  z=rnorm(100)
  u=pnorm(z)
  tau=-log(1-u)/0.02
  for (i in 1:80){
    if (tau[i]<=1) {loss[trial]=loss[trial]+10*(1-beta())}
  }
  for (i in 81:100){
    if (tau[i]<=1) {loss[trial]=loss[trial]+20*(1-beta())}
  }
}
hist(loss)
mean(loss)
##(b)

loss<-c()
loss[1:10000]=0
rho=0.3
number_of_de<-c()
de_corr<-c()
for (trial in 1:10000){
  n=0;
  m=rnorm(1)
  e=rnorm(100)
  z=sqrt(rho)*m+sqrt(1-rho)*e
  u=pnorm(z)
  tau=-log(1-u)/0.02
  for (i in 1:80){
    if (tau[i]<=1) {loss[trial]=loss[trial]+10*(1-beta())}
  }
  for (i in 81:100){
    if (tau[i]<=1) {loss[trial]=loss[trial]+20*(1-beta())}
  }

}
hist(loss)
mean(loss)


