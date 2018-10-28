#Question1
#(a)
loss<-c()
for(i in 1:100000)
{
  x=runif(80)
  y=runif(20)
  loss[i]=(sum(x<0.02))*10+(sum(y<0.02))*20
}
hist(loss,breaks=19)
#(b)
lamda=-log(1-0.02)
loss1b<-c()
for(i in 1:100000)
{
  m=rnorm(1)
  x1=rep(m,80)
  x2=rnorm(80)
  x3=rep(m,20)
  x4=rnorm(20)
  z1=0.3^0.5*x1+(1-0.3)^0.5*x2
  z2=0.3^0.5*x3+(1-0.3)^0.5*x4
  loss1b[i]=sum((-log(1-pnorm(z1))/lamda)<1)*10+sum((-log(1-pnorm(z2))/lamda)<1)*20
}
hist(loss1b,breaks=30)
max(loss1b)

#c
joint_p<-c()
num<-c()
for(i in 1:100000)
{
  m=rnorm(1)
  x1=rep(m,100)
  x2=rnorm(100)
  z1=0.3^0.5*x1+(1-0.3)^0.5*x2
  num[i]=sum((-log(1-pnorm(z1))/lamda)<1)
  joint_p[i]=num[i]*(num[i]-1)/(100*99)
}
meanp=mean(joint_p)
def_cor=(meanp-0.02*0.02)/(0.02*0.02*0.98*0.98)^0.5

#d
joint_p<-c()
num<-c()
for(j in seq(0.25,0.26,0.0005))
{
  for(i in 1:100000)
  {
    m=rnorm(1)
    x1=rep(m,100)
    x2=rnorm(100)
    z1=j^0.5*x1+(1-j)^0.5*x2
    num[i]=sum((-log(1-pnorm(z1))/lamda)<1)
    joint_p[i]=num[i]*(num[i]-1)/(100*99)
  }
  meanp=mean(joint_p)
  def_cor=(meanp-0.02*0.02)/(0.02*0.02*0.98*0.98)^0.5
  print(c(j,def_cor))
}

#Question 2
#a
loss<-c()
for(i in 1:100000)
{
  x=runif(80)
  y=runif(20)
  loss[i]=(sum(x<0.02))*10+(sum(y<0.02))*20
}
ec=as.numeric(quantile(loss,0.999))-mean(loss)

#b
lamda=-log(1-0.02)
loss1b<-c()
for(i in 1:100000)
{
  m=rnorm(1)
  x1=rep(m,80)
  x2=rnorm(80)
  x3=rep(m,20)
  x4=rnorm(20)
  z1=0.3^0.5*x1+(1-0.3)^0.5*x2
  z2=0.3^0.5*x3+(1-0.3)^0.5*x4
  loss1b[i]=sum((-log(1-pnorm(z1))/lamda)<1)*10+sum((-log(1-pnorm(z2))/lamda)<1)*20
}
ec1=as.numeric(quantile(loss1b,0.999))-mean(loss1b)

#Question3
#a#b
loss<-c()
for(i in 1:100000)
{
  x=runif(80)
  y=runif(20)
  loss[i]=(sum(x<0.02))*10+(sum(y<0.02))*10+max((sum(y<0.02))*10-40,0)
}
hist(loss,breaks=10)
ec=as.numeric(quantile(loss,0.999))-mean(loss)

#c
lamda=-log(1-0.02)
loss1b<-c()
for(i in 1:100000)
{
  m=rnorm(1)
  x1=rep(m,80)
  x2=rnorm(80)
  x3=rep(m,20)
  x4=rnorm(20)
  z1=0.3^0.5*x1+(1-0.3)^0.5*x2
  z2=0.3^0.5*x3+(1-0.3)^0.5*x4
  loss1b[i]=sum((-log(1-pnorm(z1))/lamda)<1)*10+
    sum((-log(1-pnorm(z2))/lamda)<1)*10+max(sum((-log(1-pnorm(z2))/lamda)<1)*10-40,0)
}
hist(loss1b,breaks=15)
ec1=as.numeric(quantile(loss1b,0.999))-mean(loss1b)

#Question 4
#a
loss<-c()
for(i in 1:100000)
{
  x=runif(80)
  y=runif(20)
  rx=rbeta(20,2,2)
  ry=beta(20,2,2)
  loss[i]=(sum(x<0.02))*10+(sum(y<0.02))*10+max((sum(y<0.02))*10-40,0)
}
hist(loss,breaks=10)
ec=as.numeric(quantile(loss,0.999))-mean(loss)
