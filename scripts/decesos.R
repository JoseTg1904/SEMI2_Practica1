install.packages("RColorBrewer")
library("RColorBrewer")
color <- brewer.pal(9, "Pastel1")
muertes <- read.csv("MuertesSexoEdad.csv", header=TRUE)

#aplicando filtro
muertes <- muertes[muertes$Age != "Total" ,]
muertes <- muertes[muertes$Sex != "Total" ,]

#creacion de tabla de frecuencias
frecuenciaAbsoluta <- table(muertes$Age)
frecuenciaAcumulada <- cumsum(frecuenciaAbsoluta)
frecuenciaRelativa <- prop.table(frecuenciaAbsoluta)
frecuenciaRelativaAcumulada <- cumsum(frecuenciaRelativa)
tablaDeFrecuencias <- cbind(frecuenciaAbsoluta, frecuenciaAcumulada, frecuenciaRelativa, frecuenciaRelativaAcumulada)
tablaDeFrecuencias <- data.frame(tablaDeFrecuencias)

#histograma
hist(x = frecuenciaAbsoluta, main = "Frecuencia absoluta de decesos por edad", xlab = "Edades", ylab = "Decesos", col=color)

#poligono de frecuencias
polygon(x=frecuenciaAbsoluta)

#diagrama frecuencia acumulada
hist(x = frecuenciaAcumulada, main = "Frecuencia Acumulada de decesos por edad", xlab = "Edades", ylab = "Decesos", col=color)