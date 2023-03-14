#ejemplo de tabla de frecuencia clase 2
datos <- c(25,31,42,22,36,31,45,
           50,52,35,33,32,37,28,
           20,28,35,28,39,43,48,
           51,40,37,36,29,31,25,
           39,23,26,40,47,44,55,
           30,28,38,28)
datos;

#Minimo 
minimo <- min(datos) ; minimo
#Maximo
maximo <- max(datos) ; maximo
#Rango
rango <- maximo - minimo ; rango
#numero de datos
n <- length(datos) ; n
#Numero de intervalos
intervalos <- 1 + (3.32 * log10(n)) ; intervalos
#Amplitud del intervalo
amplitud <- (rango/round(intervalos))
round(amplitud)
#se tiene que cumplir la condicion Amplitud * Intervalos > Rango
round(intervalos)
if(round(amplitud)*round(intervalos) > rango){
  print("esta bien")
} else {
  print("no ta bien, sumele al intervalo pai xd")
}
#definimos cotas superiores e inferiores para cada intervalo
cotaInferior <- c(20,26,32,38,44,50)
cotaSuperior <- c(26,32,38,44,50,56)
#Marca de clase------------------------------------
marcaDeClase <- (cotaSuperior + cotaInferior)/2
marcaDeClase
#--------------------------------------------------

#Numero de datos que hay en un intervalo (fi)

intervalos2 <- round(intervalos)
datos[datos >= 12 & datos < 18]
length(datos[datos >= 12 & datos < 18])
i <- 1
#creo vector de frecuencias absolutas
fi <- rep(0,intervalos2)
#pivote que se usa en el while para asignar datos mas legiblemente xd
pivot <- 0
while (i <= intervalos2) {
  print(paste("frecuencia entre ", cotaInferior[i], " y ", cotaSuperior[i]))
  print(length(datos[datos >= cotaInferior[i] & datos < cotaSuperior[i]]))
  pivot = length(datos[datos >= cotaInferior[i] & datos < cotaSuperior[i]])
  fi[i] = pivot
  i = i+1
}
#Frecuencia relativa
fr <- fi/n 

#porcentaje de frecuencia relativa
frp <- fr*100

#ACUMULADAS-----------------------------------
#Frecuencia acumulada
FI <- cumsum(fi)

#Frecuencia relativa acumulada
FRA <- FI/n

#Porcentaje relativa acumulada
FRAP <- FRA*100

#-----------------Tabla-----
tabla <- matrix(c(marcaDeClase,fi,fr,frp,FI,FRA,FRAP),ncol = 7)
colnames(tabla) <- c("Mi","fi","fri","fri%","FI","FRI","FRI%")
rownames(tabla) <- c("20-26","26-32","32-38","38-44","44-50","50-56")
View(tabla)




