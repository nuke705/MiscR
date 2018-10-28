dat <- read.table("E:/study/AMZN1012.csv", header = TRUE,sep=",")
logreturn <- dat[,8]
n <- length(logreturn)
plot(ecdf(logreturn))
library(moments)
sk <- skewness(logreturn,na.rm =T)
k <- kurtosis(logreturn,na.rm =T)
nbootstrap=5000
resample=rep(0,nbootstrap)
for (b in 1:nbootstrap)
{
  xstar=sample(logreturn,n,replace=T)
  resample[b]=kurtosis(xstar)
}
mean(resample)
sd(resample)
ci <- quantile(resample,probs=c(0.025,0.975))
summary(ci)

hist(resample, breaks=60,prob=TRUE)
d <- density(resample)
lines(d,col = "red", lwd=3)
#curve(density(resample),add =T, col = "red", lwd=3)
#lowci <- 2??? ci$1st Qu. 
#upci <- 2??? q???L

t <- 1/252;
initialvalue <- c(50,2,0,0)
theta <- rep(0,4)
x <- logreturn 
theta[1]<- 50
theta[2]<- 2
theta[3]<- 0
theta[4]<- 0
nig <- function(x,theta){theta[1]*theta[2]*t/pi*besselK(theta[1]*sqrt(theta[2]^2*t^2+
      (x-theta[3]*t)^2),1)/(sqrt(theta[2]^2*t^2+(x-theta[3]*t)^2))*exp(theta[2]*sqrt(theta[1]^2-theta[4]^2)*t
                                                                    +theta[4]*(x-theta[3]*t))}
result <- optim(initialvalue,fn=function(theta){-sum(log(nig(x,theta)))},method = "L-BFGS-B")
result

dlog <- density(logreturn)
hist(logreturn, breaks=100,prob=TRUE,ylim = c(0,40))
lines(dlog,col = "red", lwd=2)
dlog
curve(nig(x,result$par),-0.15,0.15,add = T,col = "blue", lwd=2,lty=2)



para <- rep(0,2)
para[1] <- 0
para[2] <- 0.005
laplacefn <- function(x,para){1/(2*para[2])*exp(-abs(x-para[1])/para[2])}
initialvalue2 <- c(0,0.005)
result2 <- optim(initialvalue2,fn=function(para){-sum(log(laplacefn(x,para)))},method = "L-BFGS-B")
result2

curve(laplacefn(x,result2$par),-0.15,0.15,add = T,col = "springgreen2", lwd=2)
aignig <- -2*(result$value-4)
aiglap =   -2*(result2$value-2)

