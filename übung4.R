# Aufgabe 1

# a)
setwd("C:/Users/eminu/OneDrive/Desktop/Tutorat/Übung 4")
daten = read.csv("werbung.csv")

# Modell laufen lassen
modell = lm(sales ~ TV, data = daten)
summary(modell)

# b)

# Plot 
par(mfrow=c(1,1))
plot(daten$TV, daten$sales, xlab = "TV", ylab = "sales")
abline(modell, col = "firebrick")

# c)
plot(modell$fitted.values, modell$residuals, xlab = "Vorhergesagte Werte",
     ylab = "Residuen")

# Neues Modell
modell2 = lm(sales ~ sqrt(TV), data = daten)
summary(modell2)

# Plot von altem und neuem Modell
par(mfrow = c(1,2))

plot(daten$TV, daten$sales, xlab = "TV", ylab = "sales")
abline(modell, col = "firebrick")

plot(sqrt(daten$TV), daten$sales, xlab = expression(sqrt(TV)), ylab = "Sales")
abline(modell2, col = "firebrick")

# Das Problem mit der nicht-konstanten Fehlervarianz besteht aber immer noch!
plot(modell2$fitted.values, modell2$residuals, xlab = "Vorhergesagte Werte",
     ylab = "Residuen")



