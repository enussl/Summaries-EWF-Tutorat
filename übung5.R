# Aufgabe 1.d

set.seed(747)
N = 150
sigma2 = 3
b1 = numeric()
X = runif(N,-1,1)
for(i in 1:1000){
  
  Y = 0.2 + 1.2*X + rnorm(N,0,sqrt(sigma2))
  fit = lm(Y~X)
  
  b1[i] = coef(fit)[2]
}
hist(b1,breaks = 50)
abline(v = mean(b1), col = "tomato")

# Aufgabe 1.e
set.seed(747)
N = 150
sigma2 = 3
b1 = numeric()
reject = numeric()
X = runif(N,-1,1)
for(i in 1:1000){
  
  Y = 0.2 + 1.2*X + rnorm(N,0,sqrt(sigma2))
  fit = lm(Y~X)
  
  b1[i] = coef(fit)[2]
  
  tmp = summary(fit)
  reject[i] = abs((b1[i] - 1)/tmp$coefficients[2,2]) >= 1.96
}
mean(reject) # Güte
