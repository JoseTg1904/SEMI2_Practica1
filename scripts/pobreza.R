install.packages("RColorBrewer")
library("RColorBrewer")
color <- brewer.pal(9, "Pastel1")
pobreza <- read.csv("PobrezaDesempleoAsesinatos.csv", header=TRUE)

#exploración de datos
pairs(pobreza)
cor(pobreza)

#regresion lineal pobreza - desempleo
regresionPobrezaDesempleo = lm(pobreza$Porcentaje.desempleado ~ pobreza$Porcentaje.con.ingresos.debajo.de.5000)
plot(pobreza$Porcentaje.con.ingresos.debajo.de.5000, pobreza$Porcentaje.desempleado, main = "relacion pobreza - desempleo", xlab="pobreza", ylab="desempleo")
abline(regresionPobrezaDesempleo, col="red")

#verificacion pobreza - desempleo
pobrezaDesempleo = function(desempleo) {
    0.9486 + 0.3036 * desempleo
}

#regresion lineal pobreza - asesinatos
regresionPobrezaAsesinatos = lm(pobreza$Asesinatos.por.1000000.habitantes ~ pobreza$Porcentaje.con.ingresos.debajo.de.5000)
plot(pobreza$Porcentaje.con.ingresos.debajo.de.5000, pobreza$Asesinatos.por.1000000.habitantes, main = "relacion pobreza - asesinatos", xlab="pobreza", ylab="asesinatos")
abline(regresionPobrezaAsesinatos, col="red")

#verificacion pobreza - asesinatos
pobrezaAsesinatos = function(asesinatos) {
    -29.901 + 2.559 * asesinatos
}