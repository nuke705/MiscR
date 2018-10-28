## Data Cleaning ##
#mydata <- read.table("hw5data.csv",header = TRUE,sep = ",")
#mydata <- read.table("E:/study/hw5DATA.csv", header = TRUE,sep=",")
mydata <- read.table("E:/study/midpart2.csv",header = TRUE,sep = ",")
#attach(mydata)
ret = mydata[,4]
#Sample standard deviation
spstd = sqrt(sum(ret^2)/length(ret))


## Initial guess for paramter ##
Ini = c(0.1,0.8,0.01,0.01)

##Question 1 
##Q1A
# parameters to estimate are alpha, beta, sigma, sigma_1

#Objective function (negative log likelihood)
fr1 <- function(x) {  
sigmasqhat = rep(0,length(ret))
sigmasqhat[1] = x[4]^2 

#[1] [2]   [3]     [4]
# a,  b,  sigma,  sigma1
##(negative) Log Likelihood 
if (x[1]+x[2]>=1 || x[1]<0 || x[2]<0 || x[3]<0.001||  x[4]<0.001){
  NeglogLH = 9999
} else {
  for (i in 1:999) {
sigmasqhat[i+1] = (1-x[1]-x[2])*x[3]^2+x[1]*ret[i]^2+x[2]*sigmasqhat[i]
  }
f <-(1/(sqrt(2*pi*sigmasqhat)))*exp(-0.5*ret^2/sigmasqhat)
NeglogLH = -sum(log(f))
}

return(NeglogLH)
}

#Optimization
Q1A =optim(Ini,fr1)
Q1A

##Q1B
# parameters to estimate are alpha, beta, sigma_1
# sigma is set to sample std. deviation

#Objective function (negative log likelihood)
fr2 <- function(x) {  
  sigmasqhat = rep(0,length(ret))
  sigmasqhat[1] = x[3]^2 
  
  #(negative) log likelihood
  if (x[1]+x[2]>=1 || x[1]<0 || x[2]<0 ||  x[3]<0.001){
    NeglogLH = 9999
  } else {
    for (i in 1:999) {
    sigmasqhat[i+1] = (1-x[1]-x[2])*spstd^2+x[1]*ret[i]^2+x[2]*sigmasqhat[i]
    }
  f <-(1/(sqrt(2*pi*sigmasqhat)))*exp(-0.5*ret^2/sigmasqhat)
  NeglogLH = -sum(log(f))
  }
  
return(NeglogLH)
}

# Optimization #
Q1B =optim(c(0.1,0.8,0.001),fr2)
Q1B

##Q1C
# parameters to estimate are alpha, beta, sigma
# sigma_1 is set to sample std. deviation

#(negative) log likelihood
fr3 <- function(x) {  
  sigmasqhat = rep(0,length(ret))
  sigmasqhat[1] = spstd^2 
  
  #Constraints
  if (x[1]+x[2]>=1 || x[1]<0 || x[2]<0 ||  x[3]<0.001){
    NeglogLH = 9999
  } else {
  for (i in 1:999) {
    sigmasqhat[i+1] = (1-x[1]-x[2])*x[3]^2+x[1]*ret[i]^2+x[2]*sigmasqhat[i]
    }
  
  f <-(1/(sqrt(2*pi*sigmasqhat)))*exp(-0.5*ret^2/sigmasqhat)
  NeglogLH = -sum(log(f))
}
  
return(NeglogLH)
}

# Optimization #
Q1C =optim(c(0.1,0.8,0.001), fr3)

Q1C
##Q1D
# parameters to estimate are alpha and beta
# sigma and sigma_1 are set to sample std. deviation

#(negative) log likelihood
fr4 <- function(x) {  
  sigmasqhat = rep(0,length(ret))
  sigmasqhat[1] = spstd^2  
  
  #(negative) log likelihood
  if (x[1]+x[2]>=1 || x[1]<0 || x[2]<0 ){
    neglogLH = 9999
  } else {
    for (i in 1:999) {
    sigmasqhat[i+1] = (1-x[1]-x[2])*spstd^2+x[1]*ret[i]^2+x[2]*sigmasqhat[i]
  }
  
  #Likelihood 
  f <-(1/(sqrt(2*pi*sigmasqhat)))*exp(-0.5*ret^2/sigmasqhat)
  NeglogLH = -sum(log(f))
  }
 
    return(NeglogLH)
}

# Optimization #
Q1D =optim(c(0.1,0.8),fr4)
Q1D

##Q1E
#Q1A prediction
sigmasqhatA = rep(0,length(ret)+1)
sigmasqhatA[1] = Q1A$par[4]^2 

for (i in 1:1000) {
  sigmasqhatA[i+1]=(1-Q1A$par[1]-Q1A$par[2])*Q1A$par[3]^2+Q1A$par[1]*ret[i]^2+Q1A$par[2]*sigmasqhatA[i]
}

sqrt(sigmasqhatA[1001])

#Q1B prediction
sigmasqhatB = rep(0,length(ret)+1)
sigmasqhatB[1] = Q1B$par[3]^2 

for (i in 1:1000) {
  sigmasqhatB[i+1]=(1-Q1B$par[1]-Q1B$par[2])*spstd^2+Q1B$par[1]*ret[i]^2+Q1B$par[2]*sigmasqhatB[i]
}

sqrt(sigmasqhatB[1001])

#Q1C prediction
sigmasqhatC = rep(0,length(ret)+1)
sigmasqhatC[1] = spstd^2

for (i in 1:1000) {
  sigmasqhatC[i+1]=(1-Q1C$par[1]-Q1C$par[2])*Q1C$par[3]^2+Q1C$par[1]*ret[i]^2+Q1C$par[2]*sigmasqhatC[i]
}

sqrt(sigmasqhatC[1001])

#Q1D prediction
sigmasqhatD = rep(0,length(ret)+1)
sigmasqhatD[1] = spstd^2

for (i in 1:1000) {
  sigmasqhatD[i+1]=(1-Q1D$par[1]-Q1D$par[2])*spstd^2+Q1D$par[1]*ret[i]^2+Q1D$par[2]*sigmasqhatD[i]
}

sqrt(sigmasqhatD[1001])


##Question 2
#A
sigmasqhatA[1000]
ret[1000]^2

devsigA <- rep(0,21)
devsigA[1]<-Q1A$par[1]*(ret[1000]^2-Q1A$par[3]^2)+ Q1A$par[2]*(sigmasqhatA[1000]-Q1A$par[3]^2)

for (i in 1:20) {
  devsigA[i+1]= (Q1A$par[1]+Q1A$par[2])*devsigA[i]
}

EsigA = devsigA + Q1A$par[3]^2
RV_A <- sum(EsigA)
vol_A <- sqrt(sum(EsigA))*sqrt(252/21)

#B
sigmasqhatB[1000]
ret[1000]^2

devsigB <- rep(0,21)
devsigB[1]<-Q1B$par[1]*(ret[1000]^2-spstd^2)+ Q1B$par[2]*(sigmasqhatB[1000]-spstd^2)

for (i in 1:20) {
  devsigB[i+1]= (Q1B$par[1]+Q1B$par[2])*devsigB[i]
}

EsigB = devsigB + spstd^2
RV_B <- sum(EsigB)
vol_B <- sqrt(sum(EsigB))*sqrt(252/21)

#C
sigmasqhatC[1000]
ret[1000]^2

devsigC <- rep(0,21)
devsigC[1]<-Q1C$par[1]*(ret[1000]^2-Q1C$par[3]^2)+ Q1C$par[2]*(sigmasqhatC[1000]-Q1C$par[3]^2)

for (i in 1:20) {
  devsigC[i+1]= (Q1C$par[1]+Q1C$par[2])*devsigC[i]
}

EsigC = devsigC + Q1C$par[3]^2
RV_C <- sum(EsigC)
vol_C <- sqrt(sum(EsigC))*sqrt(252/21)


#D
sigmasqhatD[1000]
ret[1000]^2

devsigD <- rep(0,21)
devsigD[1]<-Q1D$par[1]*(ret[1000]^2-spstd^2)+ Q1D$par[2]*(sigmasqhatD[1000]-spstd^2)

for (i in 1:20) {
  devsigD[i+1]= (Q1D$par[1]+Q1D$par[2])*devsigD[i]
}

EsigD = devsigD + spstd^2
RV_D <- sum(EsigD)
vol_D <- sqrt(sum(EsigD))*sqrt(252/21)


##Q3

##A

library(tseries)  #must have installed package "tseries"
inter = (1-Q1A$par[1]-Q1A$par[2])*Q1A$par[3]^2
Q3A= garch(ts(ret),order = c(1,1),control = garch.control(start = c(inter,Q1A$par[1],Q1A$par[2])))
# Q3A= garch(ts(ret),order = c(1,1)) #almost same as garchfit solution
omega_Q3A <- coef(Q3A)[1]
alpha_Q3A <- coef(Q3A)[2]
beta_Q3A <- coef(Q3A)[3]
sigmasq_Q3A <- omega_Q3A/(1-alpha_Q3A-beta_Q3A)

library(fGarch)  #must have installed package "fGarch"

Q3B = garchFit(~ garch(1,1), data = ret, include.mean = FALSE)
omega_Q3B <- coef(Q3B)[1]
alpha_Q3B <- coef(Q3B)[2]
beta_Q3B <- coef(Q3B)[3]
sigmasq_Q3B <- omega_Q3B/(1-alpha_Q3B-beta_Q3B)

## try NGARCH ## success!
fr5 <- function(x) {  
  sigmasqhat = rep(0,length(ret))
  sigmasqhat[1] = x[4]^2 
  
#[1] [2]   [3]     [4]      [5]
# a,  b,  sigma,  sigma1,  theta
##(negative) Log Likelihood 
if (x[1]+x[2]>=1 || x[1]<0 || x[2]<0 || x[3]<0.001||  x[4]<0.001 || abs(x[5]) > 0.999 ){
  NeglogLH = 9999
} else {
  for (i in 1:999) {
    sigmasqhat[i+1] = (1-x[1]*(1+x[5]^2)-x[2])*x[3]^2+x[1]*(ret[i]-x[5]*x[3])^2+x[2]*sigmasqhat[i]
  }
  f <-(1/(sqrt(2*pi*sigmasqhat)))*exp(-0.5*ret^2/sigmasqhat)
  NeglogLH = -sum(log(f))
}

return(NeglogLH)
}

#Optimization
Q5 =optim(c(Ini,0.01),fr5)
Q5








