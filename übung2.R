set.seed(1)
y = numeric()

for (i in 1:10000){
  
  x = rbinom(n = 1, size = 1, prob = 0.7)
  
  if (x == 1) {
    
    y[i] = rnorm(n = 1, mean = 2, sd = sqrt(3))
    
  } else {
    
    y[i] = rnorm(n = 1, mean = -0.5, sd = sqrt(3))
    
  }
  
}

hist(y, breaks = 20, prob = T)
lines(density(y), col = "red")
abline(v = mean(y), col = "blue")
mean(y)
