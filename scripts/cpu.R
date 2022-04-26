install.packages("RColorBrewer")
library("RColorBrewer")
color <- brewer.pal(9, "Pastel1")
cpu <- read.csv("DesempenioCPU.csv", header=TRUE)

#filtrado de cpu amdahl
cpuAmdahl <- cpu[cpu$Compania == "amdahl",]

#generacion de arreglo de promedio, minimo, maximo
promedio <- sum(cpuAmdahl$PRP) / length(cpuAmdahl$PRP)
minimo <- min(cpuAmdahl$PRP)
maximo <- max(cpuAmdahl$PRP)
rendimientoAmdahl <- c("minimo"=minimo, "promedio"=promedio, "maximo"=maximo)

#grafica de barras cpu amdahl
barplot(rendimientoAmdahl, main = "Rendimiento de CPU marca Amdahl", las = 3, ylab = "PRP", col=color)

#generacion de valores promedios de rendimiento por empresa
marcas <- unique(cpu$Compania)
promedios <- c()
for (i in 1:length(marcas)){
    cpuAux <- cpu[cpu$Compania == marcas[i],]
    promedioAux <- sum(cpuAux$PRP) / length(cpuAux$PRP)
    promedios <- c(promedios, promedioAux)
    names(promedios)[i] <- marcas[i]
}

#generacion de minimo y maximo de promedios
promedios <- sort(promedios)
rendimientos <- c(promedios[1], promedios[length(promedios)])

#grafica de rendimiento promedio por compañia
barplot(rendimientos, main = "Rendimiento minimo y maximo por compañia", las = 3, ylab = "PRP", col=color)