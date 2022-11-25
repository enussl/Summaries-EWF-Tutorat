# Übung 8

# 1.a
set.seed(1)

n =  1000
x = runif(n)
y = x + rnorm(n, mean=0, sd=0.1)
data = data.frame(y,x)
plot(x,y)
abline(lm(y~x), col='tomato', lwd = 2)

# 1.b
data_temp = data[data$x >= 0.5,]
plot(data$x, data$y, type='p', col='grey', xlab='x', ylab='y')
points(data_temp$x, data_temp$y, col='black')
abline(lm(y ~ x, data=data_temp), col='tomato')

# 1.c
cutoff = seq(0.1, 0.9, 0.1)
par(mfrow=c(3,3))
for (c in cutoff){
  data_temp = data[data$y >= c,]
  plot(data$x, data$y, type='p', col='grey', xlab='x', ylab='y')
  points(data_temp$x, data_temp$y, col='black')
  abline(lm(y ~ x, data=data_temp), col='tomato')
} 


# 2.a
par(mfrow=c(1,1))
n = 1000
x_true = runif(n)
y_true = 5*x_true + rnorm(n, mean = 0, sd=0.1)
x_measured = x_true + rnorm(n)
y_measured = y_true + rnorm(n)

plot(x_true, y_true)
abline(lm(y_true ~ x_true), col='tomato', lwd  = 3)

# 2.b
plot(x_true, y_true, col='grey')
abline(lm(y_true ~ x_true), col='tomato', lwd = 2)
points(x_true, y_measured)
abline(lm(y_measured ~ x_true), col='tomato', lwd = 2)
legend(0, 5, legend=c("Ohne Messfehler in Y", "Mit zufaelligem Messfehler in Y"), lty=c(2,1), col='tomato')

# 2.c
plot(x_true, y_true, col='grey')
abline(lm(y_true ~ x_true), col='tomato', lwd = 2)
points(x_measured, y_true)
abline(lm(y_true ~ x_measured), col='tomato', lwd = 2)
legend(0, 5, legend=c("Ohne Messfehler in X", "Mit zufaelligem Messfehler in X"), lty=c(2,1), col='tomato')


# 3.a
miete = read.csv('miete.csv')
fit1 = lm(rent ~ size + rooms + factor(region), data=miete)
summary(fit1)

# 3.b
fit2 = lm(size ~ rooms + factor(region), data=miete)
fit3 = lm(rent ~ fit2$residuals, data=miete)
summary(fit3)

# 3.c
fit4 = lm(rent ~ rooms + factor(region), data=miete)
fit5 = lm(fit4$residuals ~ fit2$residuals, data=miete)
summary(fit4)
summary(fit5)

# 3.d
plot(miete$size, miete$rent, ylim=c(-1500,8500), xlim=c(-50,250), col='grey', xlab = 'Size', ylab='Rent')
points(fit2$residuals, miete$rent, col='tomato')
points(fit2$residuals, fit4$residuals, col='blue')

abline(lm(miete$rent ~ miete$size), col='grey', lty=2)
abline(fit1, col='grey', lty=1)
abline(fit3, col='tomato', lty=1)
abline(fit5, col='blue', lty=1)
legend(120, 1100, legend=c("Originaldaten", "Size 'residualised'", "Rent und Size 'residualised'"), pch=c(1,1,1), col=c('grey', 'tomato', 'blue'))
legend(-50, 8500, legend=c("Originalregression", "Rent ~ Size", "Rent ~ Size_resid", "Rent_resid ~ Size_resid"), lty=c(1,2,1,1), col=c('grey', 'grey', 'tomato', 'blue'))
