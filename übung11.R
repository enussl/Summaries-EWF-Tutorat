# Setup
setwd("C:/Users/eminu/OneDrive/Desktop/Tutorat/Übung 11")
library(caret)
library(sandwich)
library(mlbench)
library(ranger)

# 1.a
th <- read.table("Thanksgiving.txt", header=T)

fit1 <- lm(weight~weeks, data=th)
(s1 <- summary(fit1))

# 1.b
# Geht auch über factor(state)
fit2 <- lm(weight~weeks+d1+d2, data=th)
(s2 <- summary(fit2))

th$color[th$state=="G"] <- "tomato"
th$color[th$state=="V"] <- "skyblue"
th$color[th$state=="W"] <- "deeppink"
plot(th$weeks, th$weight, xlab="weeks", ylab="weight", col=th$color)
abline(a=coef(fit2)[1]+coef(fit2)[3], b=coef(fit2)[2], col="tomato")
abline(a=coef(fit2)[1]+coef(fit2)[4], b=coef(fit2)[2], col="skyblue")
abline(a=coef(fit2)[1], b=coef(fit2)[2], col="deeppink")

# 1.c
anova(fit1, fit2)

# 1.d
fit3 <- lm(weight~weeks + d1 + d2 + I(d1*weeks) + I(d2*weeks), data=th)
(s3 <- summary(fit3))

plot(th$weeks, th$weight, xlab="weeks", ylab="weight", col=th$color)
abline(a=coef(fit3)[1]+coef(fit3)[3], b=coef(fit3)[2]+coef(fit3)[5], col="tomato")
abline(a=coef(fit3)[1]+coef(fit3)[4], b=coef(fit3)[2]+coef(fit3)[6], col="skyblue")
abline(a=coef(fit3)[1], b=coef(fit3)[2], col="deeppink")

anova(fit2, fit3) # bringt nichts

# 2.a
werbung <- read.csv("werbung.csv")

fit <- lm(sales~TV + I(TV^2) + I(TV^3) + I(TV^4), data=werbung)
TV <- seq(0,300,length.out=1000)
y <- predict(fit,newdata = data.frame(TV,TV^2,TV^3,TV^4))
plot(werbung$TV, werbung$sales, xlab="TV'", ylab="sales")
lines(TV,y,col="tomato", lwd=2)

# 2.b
k_set <- c(2,10,20,50)

par(mfrow=c(2,2))
for(k in k_set){
  knn <- knnreg(sales ~ TV, k=k, data=werbung)
  pred <- predict(knn, newdata=werbung)
  plot(werbung[,1], werbung[,4], xlab="TV", ylab="Sales", main=paste("k =", k))
  tmp <- order(werbung[,1])
  lines(werbung[,1][tmp], pred[tmp], type="l", col="tomato", lwd=2)
}

# 2.c
# Kennen wir bereits aus Übung 4
par(mfrow=c(1,1))
fit <- lm(sales~sqrt(TV), data=werbung)
plot(sqrt(werbung$TV), werbung$sales, xlab="TV", ylab="sales")
abline(fit, col="tomato", lwd=2)

# 2.d
# Heteroskedastie
plot(fit)

# 2.e
# Sehr ähnlich
SF <- coef(summary(fit))[,2]
SF_HC3 <- sqrt(diag(vcovHC(fit)))

cbind(SF, SF_HC3)

# 3.a
data("PimaIndiansDiabetes")

PimaIndiansDiabetes$col[PimaIndiansDiabetes$diabetes=="neg"] <- "navyblue"
PimaIndiansDiabetes$col[PimaIndiansDiabetes$diabetes=="pos"] <- "firebrick"
plot(PimaIndiansDiabetes$glucose, PimaIndiansDiabetes$mass, col = PimaIndiansDiabetes$col,
     xlab="Plasma glucose concentration", ylab="BMI", pch=16)
legend("topleft", c("positive", "negative"), pch=16, col=PimaIndiansDiabetes$col, bty="n")

# 3.b
fit <- glm(diabetes ~ mass + glucose, data=PimaIndiansDiabetes, family = "binomial")
summary(fit)

# 3.c
phat <- predict(fit,type="response")
predpos <- as.factor(phat>0.5)
levels(predpos) <- c("neg","pos")

par(mfrow=c(1,2))
plot(PimaIndiansDiabetes$glucose, PimaIndiansDiabetes$mass, col = PimaIndiansDiabetes$col,
     xlab="Plasma glucose concentration", ylab="BMI", pch=16,
     main="Beobachtete Klassen")
abline(a=-(coef(fit)[1]/coef(fit)[2]), b=-(coef(fit)[3]/coef(fit)[2]), lty=2) # Klassifizierungsgrenze
legend("topleft", c("positive", "negative"), pch=16, col=PimaIndiansDiabetes$col, bty="n")
PimaIndiansDiabetes$colpred[predpos=="neg"] <- "navyblue"
PimaIndiansDiabetes$colpred[predpos=="pos"] <- "firebrick"
plot(PimaIndiansDiabetes$glucose, PimaIndiansDiabetes$mass, col = PimaIndiansDiabetes$colpred,
     xlab="Plasma glucose concentration", ylab="BMI", pch=16,
     main="Vorhergesagte Klassen")
abline(a=-(coef(fit)[1]/coef(fit)[2]), b=-(coef(fit)[3]/coef(fit)[2]), lty=2) # Klassifizierungsgrenze
legend("topleft", c("positive", "negative"), pch=16, col=PimaIndiansDiabetes$colpred, bty="n")

# 3.d
confusion <- table(PimaIndiansDiabetes$diabetes, predpos)
names(dimnames(confusion)) <- c("Beobachtet", "Vorhergesagt")
print(confusion)

# 3.e
(diff <- predict(fit, newdata=data.frame(mass = 37, glucose = 140), type="response") -
    predict(fit, newdata=data.frame(mass = 30, glucose = 140), type="response"))

# Bonus: Klassifizieren mit Random Forest funktioniert genial
fit_rf <- ranger(diabetes ~ pregnant + glucose + pressure + triceps + insulin +
                   mass + pedigree + age, data = PimaIndiansDiabetes)
pred_rf <- predict(fit_rf, data = PimaIndiansDiabetes)
predpos_rf <- pred_rf[["predictions"]] == "pos"

confusion_rf <- table(PimaIndiansDiabetes$diabetes, predpos_rf)
names(dimnames(confusion_rf)) <- c("Beobachtet", "Vorhergesagt")
print(confusion_rf)
