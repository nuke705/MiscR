HW3_data <- read.csv("HW3_data.csv")

logrelatives = data.frame(date = HW3_data[2:nrow(HW3_data),1],
                          DJXret = log(HW3_data[2:nrow(HW3_data),2]/HW3_data[1:nrow(HW3_data)-1,2]),
                          DJXvolret = log(HW3_data[2:nrow(HW3_data),4]/HW3_data[1:nrow(HW3_data)-1,4]),
                          SPXret = log(HW3_data[2:nrow(HW3_data),3]/HW3_data[1:nrow(HW3_data)-1,3]),
                          SPXvolret = log(HW3_data[2:nrow(HW3_data),5]/HW3_data[1:nrow(HW3_data)-1,5])
)

lambda = 0.94
lag = sapply(1:300, function(x, lambda) ((lambda^(x-1))*0.06),lambda)  #create vector of weights
logrelatives = logrelatives[nrow(logrelatives):1,]
cov_matrix <- cov.wt(logrelatives[1:300,2:5],wt = lag)  #result is a list
tmp <- unlist(cov_matrix[1])                            #result is a vector
Sigma <- matrix(tmp, nrow=4) 
library(fOptions)

int_rate = 0.28689*10^-2 - 0.198833*10^-2
int_rate = int_rate/(63-28)
int_rate = int_rate* (30-28)
int_rate = int_rate+0.198833*10^-2
tau = 30/365
spx_y = 1.844812*10^-2
djx_y = 2.186723*10^-2
init_val = -50*100*GBSOption(TypeFlag = "c", S = 2079.61, X = 2080, Time = tau, r = int_rate, 
                         b = int_rate-spx_y, sigma = HW3_data[nrow(HW3_data),5],
                        title = '', description = '')@price -
           50*100*GBSOption(TypeFlag = 'p', S = 2079.61, X = 2080, Time = tau, 
                        r = int_rate, b = int_rate-spx_y, sigma = HW3_data[nrow(HW3_data),5],
                        title = '', description = '')@price + 
           550*100*GBSOption(TypeFlag = 'c', S = 173.4873, X = 173.000, Time = tau, 
                         r = int_rate, b = int_rate-djx_y, sigma = HW3_data[nrow(HW3_data),4],
                         title = '', description = '')@price +
           550*100*GBSOption(TypeFlag = 'p', S = 173.4873, X = 173.000, Time = tau, 
                         r = int_rate, b = int_rate-djx_y, sigma = HW3_data[nrow(HW3_data),4],
                         title = '', description = '')@price

library(MASS)
pf_value = 0
set.seed(42)
rand = mvrnorm(10000, mu = c(0,0,0,0), Sigma = Sigma)
tau = 29/365
for(i in 1:10000){
  pf_value[i] = -50*100*GBSOption(TypeFlag = "c", S = 2079.61 * exp(rand[i,3]), X = 2080, Time = tau, 
                              r = int_rate, b = int_rate-spx_y, sigma = HW3_data[nrow(HW3_data),5] * exp(rand[i,4]),
                              title = '', description = '')@price -
                50*100*GBSOption(TypeFlag = 'p', S = 2079.61 * exp(rand[i,3]), X = 2080, Time = tau, 
                             r = int_rate, b = int_rate-spx_y, sigma = HW3_data[nrow(HW3_data),5] * exp(rand[i,4]),
                             title = '', description = '')@price + 
                550*100*GBSOption(TypeFlag = 'c', S = 173.4873 * exp(rand[i,1]), X = 173, Time = tau, 
                              r = int_rate, b = int_rate-djx_y, sigma = HW3_data[nrow(HW3_data),4] * exp(rand[i,2]),
                              title = '', description = '')@price +
                550*100*GBSOption(TypeFlag = 'p', S = 173.4873 * exp(rand[i,1]), X = 173, Time = tau, 
                              r = int_rate, b = int_rate-djx_y, sigma = HW3_data[nrow(HW3_data),4] * exp(rand[i,2]),
                              title = '', description = '')@price
}

pf_pl = pf_value-init_val
VaR = -quantile(pf_pl, 0.05)
E_sf = -mean(pf_pl[pf_pl<=(-VaR)])

