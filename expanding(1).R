#expanding

library(rpart)
library(readxl)
library(plyr)
setwd("/Users/admin/Desktop/msfe/principal practicum/week11/hw11")
ndata<-read_excel("final_data.xlsx")
pretotal=NA
return=rep(0,48)
cutoff=matrix(data=0,nrow=48,ncol=4)

for (i in 1:48) {
  ndata_train = ndata[1:(384 + 4*(i-1)),]
  ndata_test = ndata[(385 + 4*(i-1)):(384 + 4*i),]
  tc <- rpart.control(minsplit=50,minbucket=(384+4*(i-1))/13,maxdepth=10,xval=5,cp=0.005)
  tree=rpart(`Next 1 W Index Return`~`Factor 1` + buf+HiborD+ESI,data=ndata_train,control=tc)
  #summary(tree)
  #plot(tree, uniform=TRUE, main="Regression Tree for Mileage ")
  #text(tree, use.n=TRUE, all=TRUE, cex=.8)
  pre=predict(tree,ndata_test)
  pretotal=c(pretotal,pre)
  
  pre_insample=predict(tree,ndata_train)
  v= table(pre_insample)
  pre_outcome=as.numeric(names(v))
  
  cutoff[i,1]=quantile(pre_outcome,0.8)
  cutoff[i,2]=quantile(pre_outcome,0.6)
  cutoff[i,3]=quantile(pre_outcome,0.4)
  cutoff[i,4]=quantile(pre_outcome,0.2)
  
  
}

#mse<-c()
#mae<-c()
#mmeu<-c()
#mmeo<-c()

pretotal=pretotal[2:length(pretotal)]

#for (i in 1:length(pretotal)){
#  mse[i]=(pretotal[i]-ndata$`Next 1 W Index Return`[423+i])^2
#  mae[i]=abs(pretotal[i]-ndata$`Next 1 W Index Return`[423+i])
#  if (pretotal[i]>ndata$`Next 1 W Index Return`[423+i]){
#    mmeu[i]=abs(pretotal[i]-ndata_test$`Next 1 W Index Return`[i])
#    mmeo[i]=sqrt(abs(pretotal[i]-ndata_test$`Next 1 W Index Return`[i]))
#  }
#  else{
#    mmeu[i]=sqrt(abs(pretotal[i]-ndata_test$`Next 1 W Index Return`[i]))
#    mmeo[i]=abs(pretotal[i]-ndata_test$`Next 1 W Index Return`[i])
#  }
#}

#mean(mse)
#mean(mae)
#mean(mmeu)
#mean(mmeo)



##
r<-c()
r[1]=0;
for (i in 1:192){
  row_index=ceiling(i/4)
  if(pretotal[i]>=cutoff[row_index,1]){
    r[i]=0.17*100+0.33*100*(1+3*ndata$`Next 1 W Index Return`[(385 + (i-1))])+0.5*100*(1+ndata$`Next 1 W Index Return`[(385 + (i-1))])
  }
  else if((pretotal[i]<cutoff[row_index,1])&(pretotal[i]>=cutoff[row_index,2])){
    r[i]=0.33*100+0.17*100*(1+3*ndata$`Next 1 W Index Return`[(385 + (i-1))])+0.5*100*(1+ndata$`Next 1 W Index Return`[(385 + (i-1))])
  }
  else if((pretotal[i]<cutoff[row_index,2])&(pretotal[i]>=cutoff[row_index,3])){
    r[i]=0.5*100+0.5*100*(1+ndata$`Next 1 W Index Return`[(385 + (i-1))])
  }
  else if((pretotal[i]<cutoff[row_index,3])&(pretotal[i]>=cutoff[row_index,4])){
    r[i]=0.33*100+0.17*100*(1-3*ndata$`Next 1 W Index Return`[(385 + (i-1))])+0.5*100*(1+ndata$`Next 1 W Index Return`[(385 + (i-1))])
  }
  else if((pretotal[i]<cutoff[row_index,4])){
    r[i]=0.17*100+0.33*100*(1-3*ndata$`Next 1 W Index Return`[(385 + (i-1))])+0.5*100*(1+ndata$`Next 1 W Index Return`[(385 + (i-1))])
  }
  
  
  
}
r=r/100
return=1
for (i in 1:192){
  return=return*r[i]
}

#(return-1)/192*52
return^(52/192)-1
sd(r)*sqrt(52)

length(pretotal)



real=ndata$`Next 1 W Index Return`[385:576]
hit=real*pretotal
length(hit)
hit_ratio=length(which(hit>0))/length(hit)
hit_ratio
