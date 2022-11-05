################################################################################
# Aufgabe 2

# Working directory
setwd("C:/Users/eminu/OneDrive/Desktop/Tutorat/Übung 3")

# Daten 
daten = read.csv("nlsy.csv")

# a)
# Pairs plot
pairs(daten)

# Berechnen von Korrelationen
corr = matrix(0, ncol = ncol(daten), nrow = 1)
colnames(corr) = colnames(daten)
rownames(corr) = "Korrelation"

for (i in 1:ncol(daten)) {
  corr[1,i] = cor(daten[,"earn"], daten[,i])
}

x = seq(0, 10, l = 100)
plot(x = x, y = log(x), type = "line")

# c)

# Berechnen der Korrelationen
corr.white = cor(daten$earn[daten$white==1], daten$kww[daten$white==1])
corr.nonwhite = cor(daten$earn[daten$white==0], daten$kww[daten$white==0])

# Plot
plot(daten$kww[daten$white==1], daten$earn[daten$white==1],col=rgb(red=0.7, green=0.2, blue=0.2, alpha=0.8),pch = 16, xlab = "Knowledge-of-the-World-of-Work (KWW)", ylab="Gehalt")
points(daten$kww[daten$white==0], daten$earn[daten$white==0],col=rgb(red=0.1, green=0.6, blue=1.0, alpha=0.8),pch = 16)
legend("topleft", c("Whites", "Non-Whites"), pch=16, bty="n", col=c(rgb(red=0.7, green=0.2, blue=0.2, alpha=0.8), rgb(red=0.1, green=0.6, blue=1.0, alpha=0.8)))

################################################################################

# Aufgabe 4
# a)

# Explorativ
a = -17.5791

plot(cars$speed, cars$dist, xlab="Speed", ylab="Dist")
abline(a=a, b=2, col="skyblue")
abline(a=a, b=4, col="tomato")
abline(a=a, b=6, col="black")

dist_pred = a + 2*cars$speed
MSE_train_1 = mean((dist_pred - cars$dist)^2)

dist_pred = a + 4*cars$speed
MSE_train_2 = mean((dist_pred - cars$dist)^2)

dist_pred = a + 6*cars$speed
MSE_train_3 = mean((dist_pred - cars$dist)^2)

legend("topleft", c(paste("b = 2; MSE = ", round(MSE_train_1)), paste("b = 4; MSE = ", round(MSE_train_2)),
                    paste("b = 6; MSE = ", round(MSE_train_3))), col=c("skyblue","tomato","black"), lty=1, bty="n")

# Optimaler Steigungsparameter finden
b_grid = seq(0,10,by=0.1)
MSE_train = numeric()

for(i in 1:length(b_grid)){
  dist_pred = a + b_grid[i]*cars$speed
  MSE_train[i] = mean((dist_pred - cars$dist)^2)
}

plot(b_grid, MSE_train, type="l", main="Training-MSE für verschiedene Steigungsparameter b", ylab="Training-MSE",
     xlab = "Steigungsparameter b", lwd=2)

b_MSE_min = b_grid[which.min(MSE_train)]
abline(v=b_MSE_min, col="skyblue")

fit = lm(dist~speed,data=cars)
b_KQ = coef(fit)[2]
abline(v=b_KQ, col="tomato", lty="dashed")

legend("bottomleft",c(paste("b_MSE_min = ", round(b_MSE_min,3)), paste("b_KQ = ", round(b_KQ,3))), lty=c("solid","dashed"), col=c("skyblue", "tomato"), bty="n")
