##orignial signal
setwd("E:/study")
library(readxl)
osig<-read_excel("signal.xlsx",sheet="Sheet1")
hit=c()
for (i in 1:727){
 if (osig$`Next 1 W Index Return`[i]>0) {hit[i]=1} else {hit[i]=0}
}
library(rpart)
data_train=osig[1:727,1:4]
tc <- rpart.control(minsplit=50,minbucket=50,maxdepth=10,xval=5,cp=0.005)
tree.both<-rpart(as.factor(hit)~ .,data=data_train,method='class',control = tc)
summary(tree.both)
tree.both$variable.importance
printcp(tree.both)
plotcp(tree.both,lwd=2)
##jsut a root

##original data


odata<-read_excel("signal.xlsx",sheet="Sheet2")
odata<-odata[1:727,]
tc <- rpart.control(minsplit=50,minbucket=50,maxdepth=10,xval=5,cp=0.005)
tree=rpart(odata$`Next 1 W Index Return`~odata$`Factor 1` +odata$`Factor 2`+odata$`Factor 3`+odata$`Factor 4`,method="anova",control=tc)
summary(tree)

plot(tree, uniform=TRUE, 
     main="Regression Tree for olddata ")
text(tree, use.n=TRUE, all=TRUE, cex=.8)
post(tree, file = "E:/study/tree3.ps", 
     title = "Regression Tree for Mileage ")
t<-predict(tree)
n=0
for (i in 1:727){
  if ( t[i]*odata$`Next 1 W Index Return`[i]>0) {n=n+1}
}
413/727
##not very good predict

##new signal
##bad


##new data
ndata<-read_excel("Principal_data_tree.xlsx",sheet="Sheet1")
ndata=ndata[1:727,]
tc <- rpart.control(minsplit=50,minbucket=50,maxdepth=10,xval=5,cp=0.005)
tree=rpart(odata$`Next 1 W Index Return`~ndata$buf+ndata$hibor+ndata$ESI,method="anova",control=tc)
summary(tree)
plot(tree, uniform=TRUE, 
     main="Regression Tree for new data ")
text(tree, use.n=TRUE, all=TRUE, cex=.8)
post(tree, file = "E:/study/tree4.ps", 
     title = "Regression Tree for Mileage ")

t<-predict(tree)
n=0
for (i in 1:599){
  if (t[i]*odata$`Next 1 W Index Return`[i]>0) {n=n+1}
}
#help("na.action")

