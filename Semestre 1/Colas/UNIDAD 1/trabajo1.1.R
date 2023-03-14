
Dataset <- 
  readXL("C:/Users/Miguel/Desktop/Todoo/TAREA/Semestre 1 sistemas/Colas/UNIDAD 1/Datos trabajo descriptiva.xlsx",
   rownames=FALSE, header=TRUE, na="", sheet="creditos_de_consumo_otorgados", stringsAsFactors=TRUE)
summary(Dataset)
str(Dataset)
names(Dataset)[c(11)] <- c("fecha_de_beneficio_año")
local({
  .Table <- with(Dataset, table(barrio_vereda))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
Dataset <- within(Dataset, {
  barrio_vereda <- Recode(barrio_vereda, 
  '"antonio nariÃ±o" = "antonio nariño"; "belen vicuÃ±a" = "belen vicuña"; "campiÃ±as abiertas" = "campiñas abiertas";
  "cataluÃ±a" = "cataluña"; "robledo la campiÃ±a" = "robledo la campiña"; "robledo villa campiÃ±a" = "robledo villa campiña";
  "san cristÃ³bal" = "san cristóbal"; "san javier antonio nariÃ±o" = "san javier antonio nariño"; "san jose de la montaÃ±a" = "san jose de la montaña"',
   as.factor=TRUE)
})
Dataset <- within(Dataset, {
  comuna <- Recode(comuna, '"12-la amÃ©️rica" = "12-la américa"; "16-belÃ©️n" = "16-belén"', as.factor=TRUE)
})
Dataset <- within(Dataset, {
  caracterizacion_ciudadano <- Recode(caracterizacion_ciudadano, 
  '"cootranscataluÃ±a" = "cootranscataluña"', as.factor=TRUE)
})
Dataset <- within(Dataset, {
  sexo <- Recode(sexo, '"femenino" = "Mujer"; "Femenino" = "Mujer"; "masculino" = "Hombre"; "Masculino" = "Hombre";', as.factor=TRUE)
})
names(Dataset)[c(2)] <- c("SEXO")
Dataset <- within(Dataset, {
  estrato <- as.factor(estrato)
  fecha_de_beneficio_año <- as.factor(fecha_de_beneficio_año)
  fecha_de_beneficio_dia <- as.factor(fecha_de_beneficio_dia)
  fecha_de_beneficio_mes <- as.factor(fecha_de_beneficio_mes)
})
str(Dataset)
summary(Dataset)
datos <- Dataset[c(1)]
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
save(datos,file ="myvector.rda")
edad <- subset(Dataset, select=c(edad))
write.table(edad, "C:/Users/Miguel/Desktop/Todoo/TAREA/Semestre 1 sistemas/Colas/UNIDAD 1/edad.csv", sep=",", col.names=TRUE, row.names=TRUE, quote=TRUE, na="NA")
write.table(edad, 
  "C:/Users/Miguel/Desktop/Todoo/TAREA/Semestre 1 sistemas/Colas/UNIDAD 1/edad.csv", sep=",",
   col.names=FALSE, row.names=FALSE, quote=TRUE, na="NA")
write.table(edad, 
  "C:/Users/Miguel/Desktop/Todoo/TAREA/Semestre 1 sistemas/Colas/UNIDAD 1/edad.csv", sep=",",
   col.names=FALSE, row.names=FALSE, quote=TRUE, na="NA")
write.table(edad, 
  "C:/Users/Miguel/Desktop/Todoo/TAREA/Semestre 1 sistemas/Colas/UNIDAD 1/edad.csv", sep=",",
   col.names=TRUE, row.names=FALSE, quote=TRUE, na="NA")
with(edad, Hist(edad, scale="frequency", breaks="Sturges", col="darkgray"))
Boxplot( ~ edad, data=edad, id=list(method="y"))

