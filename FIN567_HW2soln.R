
#There is some code in the script that has been commented. These are 
#alternative ways to do the same operations as above those lines
library(readxl)

#attach(mydata)

SP500 = read_excel("data-updated.xlsx",1)
FTSE = read_excel("data-updated.xlsx",2)
DAX = read_excel("data-updated.xlsx",3, na = "null")
#Some NA values are stored as "null" in the 3rd sheet
#this doesn't remove the null values, just converts them to NA datatype
GBP = read_excel("data-updated.xlsx",4, na = "NA()")
EUR = read_excel("data-updated.xlsx",5, na = "NA()")

data_update = merge(SP500,FTSE, by = "Date")
data_update = merge(data_update,DAX, by = "Date")

# note that number of rows in merged dataframe is lesser
# merge() automatically drops rows that aren't present in both dataframes
data_update = merge(data_update, GBP, by = "Date")
data_update = merge(data_update, EUR, by = "Date")
str(data_update)
#There are possibly more efficient ways to do this
old_data = read.csv("sample data.csv")
str(old_data)
#updating column names for the sake of uniformity
colnames(data_update) = colnames(old_data)
#making sure that the columns in the two dfs are of same type before merging
old_data[,1] = as.Date(old_data[,1], format = "%m/%d/%Y")
data_update[,1] = as.Date(data_update[,1], format = "%m/%d/%Y")
#reversing the order of data_updated and then adding the rows from 
#the older dataframe to ensure a chronological order
data = rbind.data.frame(data_update[nrow(data_update):1,], old_data )
#same as rbind, except it takes of some possible cases for dataframe binding
data = na.omit(data)
#omit all rows that have NA values in them

 
# iterate over FTSE and DAX columns and convert them to USD denominated prices
for( i in 3:4) {
  data[,i] = data[,i]*data[,(i+2)]
}
#Substituting the FTSE and DAX prices with their converted versions

#data[,3:4] = mapply(function(x,y) x*y, data[,3:4], data[,5:6])

#creating a log_returns dataframe with the same dates except the last date
log_returns = data.frame(data$Date[1:(nrow(data)-1)])
#computing continuously compounded returns of SP500, FTSE and DAX which are in column 2,3,and 4
for(i in 2:4){
  log_returns[,i] = log(data[1:nrow(data)-1,i])-log(data[2:nrow(data),i])
}
#diff() takes lag values of a vector
#log_returns[,2:4]=  sapply(data[,2:4], function(x) (-diff(log(x))))
colnames(log_returns) = c("Date", "S.P.500log","FT.SE.100log","DAXlog")
log_returns[nrow(data), ] = list(data$Date[nrow(data)], NA,NA,NA)
#To ensure same number of rows before merging we are adding a lastrow with NA values


#Computing simple returns
simp_returns = data.frame(data$Date[1:(nrow(data)-1)])
for(i in 2:4){
  simp_returns[,i] = (data[1:nrow(data) - 1, i]/data[2:nrow(data),i]) - 1
}
colnames(simp_returns) = c("Date", "S.P.500simp","FT.SE.100simp","DAXsimp")
#simp_returns[,2:4] = sapply(data[,2:4], function(x) (x[-length(x)] / x[-1] - 1))
simp_returns[nrow(data), ] = list(data$Date[nrow(data)], NA,NA,NA)

#Adding the returns to our original dataframe
data = merge(data,simp_returns, by = "Date", sort = FALSE)
data = merge(data,log_returns, by = "Date", sort = FALSE)


#obtaining the indices for the start and the end dates for computing means
ind_1 = row.names(data[data$Date == as.Date('2008-01-02'),])
#Taking the rowname of the row with the date we need
ind_2 = row.names(data[data$Date == as.Date("2017-12-29"),])
# 7th column is the one with simple returns of SP500
mean(data[ind_2:ind_1,7])
mean(data[ind_2:ind_1,8])
mean(data[ind_2:ind_1,9])


#calculating indices for the dates we will need in further calculations
ind_2 = as.integer(rownames(data[data$Date == as.Date('2016-12-30'),]))
ind_1 = as.integer(rownames(data[data$Date == as.Date('2017-12-28'),]))

#Historical VaR
mydata <- read_excel("E:/study/Fin580RM2016midtermpart2solutions.xlsx",header = TRUE,sep = ",")

value = 1000000
#Calculating total returns by summing the simple returns and multiplying the 
#position in each of the markets 
data$Tot_returns = (data[,7]+data[,8]+data[,9]) * value

#data$Tot_returns = apply(simp_returns[,2:4],1,sum) * value

hist_Var = 0
for(i in ind_1:ind_2){
  hist_Var[i+1-ind_1] = -quantile(data[i:(i+1999),13],p = 0.01)
}



#For each day, taking the returns for previous 2000 dates and calculating 
#the 1% quantile for each day


#hist_Var = sapply(ind_1:ind_2, function(x) (-quantile(tot_returns[x:(x+1999)],probs = 0.01)))

#Delta-Normal VaR



value1 = as.integer(rep(1e06,3))
cov_mat = matrix(nrow =(ind_2-ind_1+1),ncol = 9)
for(i in ind_1:ind_2){
  cov_mat[i+1-ind_1,] = (cov.wt(data[i:(i+1999),7:9], center = FALSE,method = "ML")$cov)
}
#we are using the cov.wt to set the means to be 0. The default method divides 
#the sumproducts by (n-1). "ML" method doesn't do that  
st_dev = sapply(1:nrow(cov_mat), function(x) (sqrt(value1%*%matrix(cov_mat[x,],3,3)%*%value1)))
#multiplying the covariance matrix by the positions we hold and taking squareroot 
DNVaR = -qnorm(0.01)*st_dev




# cov_mat = matrix(nrow = (ind_2-ind_1),ncol = 6)
# for(i in ind_1:ind_2){
#   m = 1
#   for(j in 7:9){
#     for(k in j:9){
#       cov_mat[i+1-ind_1,m] = sum(data[i:(i+1999),j]*data[i:(i+1999),k])/2000
#       m = m+1
#     }
#   }
# }
# cov_mat = data.frame(cov_mat)
# 
# colnames(cov_mat) = c("SP","SPFTSE","SPDAX","FTSE","FTSEDAX","DAX")
# 
# st_dev = numeric(16)
# for (i in ind_1:ind_2){
#   st_dev[i+1 - ind_1] = sqrt(1e12*(cov_mat$SP[i]+cov_mat$FTSE[i]+cov_mat$DAX[i] + 2*cov_mat$SPDAX[i] 
#                              + 2*cov_mat$SPFTSE[i] + 2*cov_mat$FTSEDAX[i]) )
# }
# DNVaR = -qnorm(0.01)*st_dev




#D-N VaR with exponentially weighted covariance estimator

lambda = 0.94
lag = numeric(101)
lag[1] = 0.06
for(i in 2:101){
  lag[i] = lambda*lag[i-1]
}
#Calculating lags for 100 days

cov_mat_wt = matrix(nrow = 2606,ncol = 6)
for(i in 1:2606){
  m = 1
  for(j in 7:9){
    for(k in j:9){
      cov_mat_wt[i,m] = sum(lag*data[i:(i+100),j]*data[i:(i+100),k])
      m = m+1
    }
  }
}
cov_mat_wt = data.frame(cov_mat_wt)
colnames(cov_mat_wt) = c("SP","SPFTSE","SPDAX","FTSE","FTSEDAX","DAX")
#calculating weighted covariance matrix based on previous 100 days
wt_st_dev = numeric(ind_2-ind_1+1)
for (i in ind_1:ind_2){
  wt_st_dev[i+1 - ind_1] = sqrt(1e12*(cov_mat_wt$SP[i]+cov_mat_wt$FTSE[i]+cov_mat_wt$DAX[i] + 2*cov_mat_wt$SPDAX[i] 
                             + 2*cov_mat_wt$SPFTSE[i] + 2*cov_mat_wt$FTSEDAX[i]) )
}
#Calculating st deviations
DNVaR_wt = -qnorm(0.01)*wt_st_dev


#tmp = sapply(1:100, function(x, lambda) ((lambda^x)*0.06),lambda)
#lag[2:101] = tmp
#cov_mat_wt = matrix(nrow = 16,ncol = 9)
#for(i in 255:270){
#  cov_mat_wt[i+1-255,] = (cov.wt(data[i:(i+100),7:9], wt = lag, center  = c(0,0,0),method = "ML")$cov)
#}
#cov_mat_wt = sapply(1:2606, function(x) (cov.wt(simp_returns[x:(x+100),2:4],wt = lag)))
#s_d_wt = sapply(1:nrow(cov_mat_wt), function(x) (sqrt(value1%*%matrix(cov_mat_wt[x,],3,3)%*%value1)))
#DNVaR_wt = -qnorm(0.01)*s_d_wt




#Historical Simulation using Rescaled returns

rescaled_ret = matrix(nrow = 2605,ncol = 3)
#Dividing simple returns by the previous day's std_deviations
rescaled_ret[,1] = data[1:2605,7]/sqrt(cov_mat_wt[2:2606,1])
rescaled_ret[,2] = data[1:2605,8]/sqrt(cov_mat_wt[2:2606,4])
rescaled_ret[,3] = data[1:2605,9]/sqrt(cov_mat_wt[2:2606,6])
Hist_rescale = 0

#rescaling returns by for each day multiply the past day returns with the 
#std_deviations of present day
for(i in ind_1:ind_2){
  temp = value*(sqrt(cov_mat_wt[i,1])*rescaled_ret[i:(i+1999),1]+
                sqrt(cov_mat_wt[i,4])*rescaled_ret[i:(i+1999),2]+
                sqrt(cov_mat_wt[i,6])*rescaled_ret[i:(i+1999),3])
  Hist_rescale[i+1-ind_1] = -quantile(temp,0.01)
}

#plotting with legend
plot(x = 1:241, y = hist_Var, type = 'l', ylim = c(min(DNVaR_wt),max(hist_Var)), col = "deepskyblue")
lines(DNVaR, col = "darkorange")
lines(DNVaR_wt, col = "firebrick")
lines(Hist_rescale, col = "forestgreen")
legend("topleft",legend  = c("historical","Delta-Norm","Weighted Delta-Norm", "Weighted Historical"),
       col = c("deepskyblue", "darkorange", "firebrick","forestgreen"), lty = 1, cex=0.7)




#### historrical simulation####
library(readxl)
dat <- read_excel("E:/study/Fin580RM2016midtermpart2solutions.xlsx",5)


#Calculating total returns by  simple returns and multiplying the 
#position in each of the markets 

Tot_returns = 1000*84.4*(exp(dat[,4])-1)+ 100.73*(exp(dat[,5])-1) * 1000
total = Tot_returns$`Return of Stock 1`
-quantile(total[2:length(total)],p = 0.05)
#data$Tot_returns = apply(simp_returns[,2:4],1,sum) * value

#hist_Var = 0
#for(i in ind_1:ind_2){
#  hist_Var[i+1-ind_1] = -quantile(data[i:(i+1999),13],p = 0.01)
#}










