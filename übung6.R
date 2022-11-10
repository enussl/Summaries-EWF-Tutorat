# Übung 6 (Softwareübung)

library(readr)
library(caret)
setwd("C:/Users/eminu/OneDrive/Desktop/Tutorat/Übung 6")

# Aufgabe 1
# 1.a)
luft = read_csv("luft.csv")

dat = data.frame(Y = luft$`Ozon (max)`, X = luft$`Luftfeuchte relativ`)
fit = lm(Y~., data=dat)
summary(fit)

plot(dat$X, dat$Y, xlab = "Luftfeuchte relativ", ylab = "Ozon (max)")
abline(fit, col = "tomato")

# 1.b
(pred = predict(fit, newdata = data.frame(Y=0, X=mean(dat$X, na.rm=T)),
                 interval = "confidence"))

# 1.c
(pred = predict(fit, newdata = data.frame(Y=0, X=mean(dat$X, na.rm=T)),
                 interval = "prediction"))
hist(fit$residuals)

# 1.d Vorhersageintervall ist breiter als das Konfidenzintervall, was Sinn macht

# 1.e
(pred = predict(fit, newdata = data.frame(Y=0, X=150),
                 interval = "prediction", level = 0.90))
summary(dat$X)
# Kann man nicht vertrauen


# Aufgabe 2
# 2.a
set.seed(420)
nlsy = read.csv("nlsy.csv")
dat = data.frame(Y = nlsy$lnearn, X = nlsy$educ)

id_test = sample(1:nrow(dat), round(nrow(dat)/3), replace = F)

# 2.b
fit = lm(Y~X, data = dat[-id_test,])

plot(dat[-id_test,2], dat[-id_test,1], xlab = "iq (train)", ylab = "lnearn (train)")
abline(fit, col="tomato")
# Linearität scheint erfüllt

# 2.c
MSE_train = sum((predict(fit) - dat[-id_test,1])^2) / (nrow(dat[-id_test,]) - 2)
MSE_test = sum((predict(
  fit, newdata = dat[id_test,]) - dat[id_test,1])^2) / (nrow(dat[id_test,]) - 2)

R2_train = 1 - (MSE_train * (nrow(dat[-id_test,]) - 2)) /
  sum((dat[-id_test,1] - mean(dat[-id_test,1]))^2)
R2_test = 1 - (MSE_test * (nrow(dat[id_test,]) - 2)) /
  sum((dat[id_test,1] - mean(dat[id_test,1]))^2)

table = c(MSE_train, MSE_test, R2_train, R2_test)
names(table) = c("Train MSE", "Test MSE", "Train R2", "Test R2")

print(table)
# Werte sind nahe beinander, was natürlich gut ist und dafür spricht,
# dass man im Trainings und Testdatensatz genügend Observationen hat.
# Man könnte zusätzlich den split öfters repetitieren, um das ganze robuster zu machen.

# 2.d
id_test = sample(1:nrow(dat), 885, replace = F)

fit = lm(Y~X, data = dat[-id_test,])

plot(dat[-id_test,2], dat[-id_test,1], xlab = "iq (train)", ylab = "lnearn (train)")
abline(fit, col="tomato")

MSE_train = sum((predict(fit) - dat[-id_test,1])^2) / (nrow(dat[-id_test,]) - 2)
MSE_test = sum((predict(
  fit, newdata = dat[id_test,]) - dat[id_test,1])^2) / (nrow(dat[id_test,]) - 2)

R2_train = 1 - (MSE_train * (nrow(dat[-id_test,]) - 2)) /
  sum((dat[-id_test,1] - mean(dat[-id_test,1]))^2)
R2_test = 1 - (MSE_test * (nrow(dat[id_test,]) - 2)) /
  sum((dat[id_test,1] - mean(dat[id_test,1]))^2)

table = c(MSE_train, MSE_test, R2_train, R2_test)
names(table) = c("Train MSE", "Test MSE", "Train R2", "Test R2")

print(table)
# Zu wenige Daten für das Schätzen vom Modell.

# Aufgabe 3
# 3.a
set.seed(748)
id_test = sample(1:nrow(dat), round(nrow(dat)/3), replace = F)

# 3.b
knn_5 = knnreg(Y ~ X, k=5, data=dat, subset=-id_test)
knn_20 = knnreg(Y ~ X, k=20, data=dat, subset=-id_test)
knn_50 = knnreg(Y ~ X, k=50, data=dat, subset=-id_test)
knn_200 = knnreg(Y ~ X, k=200, data=dat, subset=-id_test)

knn_list = list(knn_5, knn_20, knn_50, knn_200) # Für später in Teilaufgabe d)

# 3.c
pred_train_5 = predict(knn_5, newdata=data.frame(X=dat[-id_test,2]))
pred_train_20 = predict(knn_20, newdata=data.frame(X=dat[-id_test,2]))
pred_train_50 = predict(knn_50, newdata=data.frame(X=dat[-id_test,2]))
pred_train_200 = predict(knn_200, newdata=data.frame(X=dat[-id_test,2]))

plot(dat[-id_test,2], dat[-id_test,1], xlab = "iq (train)", ylab = "lnearn (train)")
tmp = order(dat[-id_test,2])
lines(dat[-id_test,2][tmp], pred_train_5[tmp], type="l", col="tomato")
lines(dat[-id_test,2][tmp], pred_train_20[tmp], type="l", col="deepskyblue3")
lines(dat[-id_test,2][tmp], pred_train_50[tmp], type="l", col="aquamarine3")
lines(dat[-id_test,2][tmp], pred_train_200[tmp], type="l", col="deeppink3")
legend("topleft",c("K=5","K=20","K=50","K=200"),lty=1, col=c("tomato","deepskyblue3","aquamarine3","deeppink3"), ncol=1, cex=0.75, bty="n")

# 3.d
pred_train = cbind(pred_train_5, pred_train_20, pred_train_50, pred_train_200)

table = matrix(NA,4,4)
rownames(table) = c("K=5", "K=20", "K=50", "K=200")
colnames(table) = c("Train MSE", "Test MSE", "Train R2", "Test R2")

for(j in 1:4){
  
  pred_test = predict(knn_list[[j]], newdata=data.frame(X=dat[id_test,2]))
  
  table[j,1] = sum((pred_train[,j] - dat[-id_test,1])^2) / (nrow(dat[-id_test,]) - 2)
  table[j,2] = sum((pred_test - dat[id_test,1])^2) / (nrow(dat[id_test,]) - 2)
  table[j,3] = 1 - (table[j,1] * (nrow(dat[-id_test,]) - 2)) /
    sum((dat[-id_test,1] - mean(dat[-id_test,1]))^2)
  table[j,4] = 1 - (table[j,2] * (nrow(dat[id_test,]) - 2))/
    sum((dat[id_test,1] - mean(dat[id_test,1]))^2)
  
}

print(table)
# Funktioniert etwa alles gleich gut

# 3.e
# Validierungsset erklären

# Aufgabe 4
# Bei einer linearen Beziehung lieber eine lineare Regression verwenden.