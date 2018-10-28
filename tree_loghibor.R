setwd("E:/study")
library(readxl)
library(rpart)
##new data
dat<-read_excel("new_tree.xlsx",sheet="new")
part1 =dat[1:289,]
part2 = dat[436:579,]
ret = c(part1$`Next 1W Return`,part2$`Next 1W Return`)
FF = c(part1$FF,part2$FF)
BUF = c(part1$buf,part2$buf)
HiborD = c(part1$HiborD,part2$HiborD)
ESI = c(part1$ESI,part2$ESI)

tc <- rpart.control(minsplit=50,minbucket=50,maxdepth=10,xval=5,cp=0.005)
tree=rpart(ret ~ FF +BUF +HiborD +ESI,method="anova",control=tc)
summary(tree)
plot(tree, uniform=TRUE, 
     main="Regression Tree testing 290-435 ")
text(tree, use.n=TRUE, all=TRUE, cex=.8)
post(tree, file = "E:/study/290435Tree.ps", 
     title = "Regression Tree testing 290-435 ")



value<-c()
num4=0
num3=0
num_3=0
num_4=0
num0=0
value[1] = 1
ret1 = ret[290:435]
for(i in 2:(length(ret1)-1))
{
  if ((is.na(HiborD[i-1]) != T)&(is.na(ESI[i-1]) != T)){
    
    if( (FF[i-1] >= -0.002691) & (ESI[i-1] < 11.82)){
      #3
      value[i]=value[i-1]*(0.33*1+0.17*(1+3*ret1[i-1])+0.5*(1+ret1[i-1]))
      num3=num3+1
      
    }else if(FF[i-1] < -0.002691){
      #2
      value[i]=value[i-1]*(0.5*1+0.5*(1+ret1[i-1]))
      
    }else if( (FF[i-1] >= -0.002691) & (ESI[i-1] < 11.82) & (HiborD[i-1] >= -0.01192) & (BUF < 5.669) ){
      #1
      value[i] = value[i-1]*(0.5*1+0.5*(1+ret1[i-1]))
      
    }else if( (FF[i-1] >= -0.002691) & (ESI[i-1] < 11.82) & (HiborD[i-1] >= -0.01192) & (BUF >= 5.669)) {
      #-1
      value[i]=value[i-1]*(0.17*1+0.33*(1-3*(ret1[i-1]))+0.5*(1+(ret1[i-1])))
      
    }else if( (FF >= -0.002691) &(ESI[i-1] >= 11.82)) {
      #-2
      value[i]=value[i-1]*(0.17*1+0.33*(1-3*(ret1[i-1]))+0.5*(1+(ret1[i-1])))
      
    }else {
      
      value[i]=value[i-1]*(0.33*1+0.17*(1-3*(ret1[i-1]))+0.5*(1+(ret1[i-1])))
      num0=num0+1
    }
  }else {
    value[i]=value[i-1]*(0.33*1+0.17*(1-3*(ret1[i-1]))+0.5*(1+(ret1[i-1])))
    num0=num0+1
    
  }
  
}

plot(value,type = "l")

Annulized_return=(value[length(value)]/1)^(1/ (145/52))-1
Annulized_return
return=NA
for(i in 2:length(value)){
  return[i]=value[i]/value[i-1]-1
}

Annulized_std=sd(return,na.rm=T)*sqrt(52)
Annulized_std
Sharpratio=Annulized_return/Annulized_std
Sharpratio

maxd = c(0)
for (i in 2:length(value)){
  maxd[i]=(value[i]-max(value[1:i]))/max(value[1:i])
}
min(maxd)

benchret = dat$`Next 1W Return`[290:435]
vbench = c(1)
for(i in 2:(length(benchret)-1)) {
  vbench[i]= vbench[i-1]*(1 +benchret[i])
  
}
lines(vbench,col = "blue")
legend(120,1, lty = 1,legend = c("tree ","bench"), col=c("black", "blue"))
Annulized_return2=(vbench[length(vbench)]/1)^(1/ (145/52))-1
Annulized_return2


Annulized_std2=sd(benchret,na.rm=T)*sqrt(52)
Annulized_std2
Sharpratio2=Annulized_return2/Annulized_std2
Sharpratio2

maxd2 = c(0)
for (i in 2:length(vbench)){
  maxd2[i]=(vbench[i]-max(vbench[1:i]))/max(vbench[1:i])
}
min(maxd2)
output = NULL
output$value = value
output$bench = vbench
output$ret1 = Annulized_return
output$sd1 = Annulized_std
output$sharpe1 = Sharpratio
output$ret2 = Annulized_return2
output$sd2 = Annulized_std2
output$sharpe2 = Sharpratio2
write.csv(output,"E:/study/tree_result_290-435.csv")

