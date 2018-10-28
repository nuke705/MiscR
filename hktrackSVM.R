
data_etf <- read.table("E:/study/tracker_HS_2.csv", header = TRUE,sep=",")

library(e1071)
#library(kernlab)
library(rpart)
library(TTR)
n=length(data_etf[,2])
predict_days=5
rsidays=5
hs = data_etf[,2]
hs= hs[n:1]
rsi = RSI(hs,n = 5)
rsiweek = rsi[seq(6,2286,5)]
rweek = (hs[seq(6,n,5)]/hs[seq(1,n-5,5)]-1)

svm1 = svm(rsiweek,rweek)
summary(svm1)
new = predict(svm1,rweek)
plot(rsiweek,rweek)
points(rsiweek, new, col = 4)


pred <- predict(svm1, x, decision.values = TRUE)
attr(pred, "decision.values")[1:4,]

# visualize (classes by color, SV by crosses):
plot(cmdscale(dist(iris[,-5])),
     col = as.integer(iris[,5]),
     pch = c("o","+")[1:150 %in% svm1$index + 1])




x <- seq(0.1, 5, by = 0.05)
y <- log(x) + rnorm(x, sd = 0.2)

# estimate model and predict input values
m   <- svm(x, y)
new <- predict(m, x)

# visualize
plot(x, y)
points(x, log(x), col = 2)
points(x, new, col = 4)


pred <- predict(m, x, decision.values = TRUE)
attr(pred, "decision.values")[1:4,]

# visualize (classes by color, SV by crosses):
plot(cmdscale(dist(iris[,-5])),
     col = as.integer(iris[,5]),
     pch = c("o","+")[1:150 %in% m$index + 1])
