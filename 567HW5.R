dat <- read.table("E:/study/HW5DATA.csv", header = TRUE,sep=",")
sp500 = dat[,3]
return = dat[,4]
n <- length(sp500)
#s2 = function(a,b,s,s1){(1-a-b)*s^2+a*return^2+b*E14}
#L = function(a,b,s,s1){1/(sqrt(2*pi*(s1^2)))*exp(-0.5*rt^2/(s1^2))}

s1 = function(return,a,b,s){(1-a-b)*s^2 + a*return^2 + b*s^2}
L = function(a,b,s,s1){-0.5*log((s1^2))-0.5**return^2/(s1^2)}
#maxL = function(a,b,s,s1) {sum(-0.5*log((s1^2))-0.5**return^2/(s1^2))};
th = rep(0,3)
#a,b,s
th[1]=0.08
th[2]=0.8
th[3]=0.01
#maxL = function(return,th) {
#  sum(-0.5*log(((1-th[1]-th[2])*th[3]^2 + th[1]*return^2 + th[2]*th[3]^2)^2)
#  -0.5**return^2/(((1-th[1]-th[2])*th[3]^2 + th[1]*return^2 + th[2]*th[3]^2)^2))
#}

maxr1 = function(s1temp){(1/sqrt(2*pi*s1temp^2))*exp(-return[1]^2/(2*s1temp^2))}
firstresult = optim(0.01,fn=function(s1temp){-maxr1(s1temp)},method = "L-BFGS-B")
firstresult
s1 = firstresult$par

st = list() #rep(0,n)
st[[1]]=s1
#s2_at_t = function(th, s){ (1-th[1]-th[2])*th[3]^2+ th[1]*return1^2 + st12^2 }
likelihood = list() #rep(0,n)
likelihood[[1]]= firstresult$value

#st2  
for(i in 2:n){
  s2_at_t = function(th, ret,stprev){ (1-th[1]-th[2])*th[3]^2+ th[1]*ret^2 +th[2]*stprev*stprev}
  st[[i]]= function(th)s2_at_t(th,return[i-1],st[i-1])
  likelihood[[i]]= 1/(sqrt(2*pi*(st[i]^2)))*exp(-0.5*return[i]^2/(st[i]^2))
}


maxL = function(th){
  sigma12=th[1]; s=th[2]; a=th[3]; b=th[4];
  e=(1-a-b)*s^2+a*return[1:(length(return)-1)]^2
  h=filter(e,beta,"r",init = siga1^2)
  hh=sqrt(abs(h))
  llh=-sum(log(garchDist(return[2:length(return)],hh)))-log(garchDist(return[1],siga1))
  
  
  
}

maxL = function(th) { sum(likelihood)}


initialvalue = c(0.08,0.8,0.01)
result <- optim(initialvalue,fn=function(th){-maxL(th)},method = "L-BFGS-B")
result


longs2 = var(return)

library(fGarch)
library(tseries)

fit1 = garch(return,c(1,1))
fit2 = garchFit(~ garch(1, 1), data = return)
#summary(fit1)
fit1$coef
fit2@fit$par[2:4]


