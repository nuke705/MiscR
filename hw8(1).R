##1(a)
loss<-c()
loss[1:10000]=0
for (trial in 1:10000){ 
  z=rnorm(100)
  u=pnorm(z)
  tau=-log(1-u)/0.02
  for (i in 1:80){
    if (tau[i]<1) {loss[trial]=loss[trial]+10}
  }
  for (i in 81:100){
    if (tau[i]<1) {loss[trial]=loss[trial]+20}
  }
}
hist(loss)
mean(loss)
(80*10+20*20)*0.02
##1(b)
