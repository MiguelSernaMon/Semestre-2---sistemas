library(readxl)
datos <- read_excel("D:/subilo/resubir/Todoo/TAREA/Semestre 1 sistemas/Colas/UNIDAD 7/Trabajo Unidad 7/Trabajo evaluativo unidad 7.xlsx", 
                                          sheet = "Puntos 4 y 5")
View(datos)
#punto 4
#h0 -> θ <= 170
#h1 -> θ > 170
# n grande, varianza desconocida, no normal - Caso 2

hombre<-0
medidasHombre <- c()
mediaHombre <- 0
#almacenamos los datos de los hombres en un vector y sumamos para la mediana

for(y in 1:99){
  if(datos[y,2] == "Hombre"){
  hombre = hombre+1
  medidasHombre[hombre] = as.double(datos[y,1])
  mediaHombre = mediaHombre + as.double(datos[y,1])
  
  }
  
}
#media
mediaHombre = mediaHombre/hombre

#varianza
varianzaHombre <- var(medidasHombre)

#desviacion
desviacionH <- sqrt(varianzaHombre)

u0 <- 170
#z-teorico
za <- 1.64

#z-Calculado
zCalculado <- (mediaHombre-u0)/((desviacionH)/(sqrt(87)))


