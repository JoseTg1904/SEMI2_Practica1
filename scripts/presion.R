install.packages("RColorBrewer")
library("RColorBrewer")
color <- brewer.pal(9, "Pastel1")
presion <- read.csv("PresionSanguineaEdad.csv", header=TRUE)

#exploración de datos
pairs(presion)
cor(presion)

#regresion lineal
regresion = lm(presion$Systolic.Blood.Pressure  ~ presion$Edad)
plot(presion$Edad, presion$Systolic.Blood.Pressure, main="relacion edad - presion", xlab="edad", ylab="presion")
abline(regresion, col="red")

#verificacion
verificacion = function(edad) {
    98.7147 + 0.9709 * edad
}
