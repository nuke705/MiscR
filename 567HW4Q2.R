

b0 = c()
b1 = c()
sd0 = c()
sd1 = c()
s = c()
s[1] = 100
for( i in 1:1000) {
  
  r = rnorm(120, 0.01,0.08)

  for( j in 2:121){
    s[j] = s[j-1]*exp (r[j-1])
  }
  fit = lm(r ~ log(s[1:(length(s)-1)]))
  b0[i] = fit$coefficients[1]
  b1[i] = fit$coefficients[2]
  sd0[i] = coef(summary(fit))[, 2][1]
  sd1[i] = coef(summary(fit))[, 2][2]
}
mean(b0)
mean(b1)
sd(b0)
sd(b1)
se0=sd(b0)/sqrt(1000)
se1=sd(b1)/sqrt(1000)
se0
se1