install.packages("RColorBrewer")
library("RColorBrewer")
color <- brewer.pal(9, "Pastel1")
ventas <- read.csv("VENTAS.csv", header=TRUE)

#filtrado de paises centroamericanos
ventasCA <- ventas[ventas$Country == "Guatemala" | ventas$Country == "Belice" | ventas$Country == "El Salvador" | ventas$Country == "Honduras" | ventas$Country == "Nicaragua" | ventas$Country == "Costa Rica" | ventas$Country == "Panama", ]

#generacion de tabla de frecuencias
frecuenciaAbsoluta <- table(ventasCA$Country)
frecuenciaAcumulada <- cumsum(frecuenciaAbsoluta)
frecuenciaRelativa <- prop.table(frecuenciaAbsoluta)
frecuenciaRelativaAcumulada <- cumsum(frecuenciaRelativa)
tablaDeFrecuencias <- cbind(frecuenciaAbsoluta, frecuenciaAcumulada, frecuenciaRelativa, frecuenciaRelativaAcumulada)
tablaDeFrecuencias <- data.frame(tablaDeFrecuencias)

#grafica de barras
barplot(frecuenciaAbsoluta, main = "Ventas por pais centroamericano", las = 3, ylab = "Ventas", col=color)

#histograma
hist(x = frecuenciaAcumulada, main = "Frecuencia Acumulada ventas por pais centroamericano", xlab = "Paises centroamericanos", ylab = "ventas", col=color)