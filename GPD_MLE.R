library(readxl)
dat <- read_excel("E:/study/Fin567C.s2017.midterm part 2.solutions.xlsx",6)
# use excess loss (- mu)
pl = dat$`Simulated Loan Portfolio Loss`-0.03

fgpd <- function(x) {  
  likelihood = c()
  #[1] [2]   
  # xi, b,  
  ##(negative) Log Likelihood 
  if (abs(x[1]) > 0.99 || abs(x[2]) > 0.99){
    NeglogLH = 9999
  } else {
    for (i in 1:length(pl)) {
     likelihood[i] = 1/x[2]*(1+x[1]*pl[i]/x[2])^(-1-1/x[1])
    }
    NeglogLH = -sum(log(likelihood))
  }
  
  return(NeglogLH)
}
opt =optim(c(0.01,0.01),fgpd)
opt