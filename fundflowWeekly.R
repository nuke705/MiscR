setwd("E:/study/")
dat <- read.table("E:/study/FF042613.csv", header = TRUE,sep=",")
ff = dat[,4]
dates <- as.Date(dat[,2])
isMonday = dat[,3]
ffweek = c()
dateweek = integer(0)
class(dateweek) <- "Date"
ci = 1
lastmonday = 1

for (i in 2:1188 ){
  if ( isMonday[i]>0 ) {
    ffweek[ci]= sum(ff[i:(lastmonday + 1)])
   dateweek[ci]=dates[(i)]
    ci=ci+1
    lastmonday = i
  }
}

output = NULL
output$date = dateweek
output$ffweek = ffweek
#output$change = c(ffweek[2:length(ffweek)]/ffweek[1:(length(ffweek)-1)]-1,0)

write.csv(output,"FFweekly.csv")

