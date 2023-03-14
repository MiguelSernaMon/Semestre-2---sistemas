datos <- c(68,65,12,22,79,31,
           63,43,32,43,27,28,
           42,25,49,27,22,25,
           27,74,38,49,23,45,
           30,51,42,28,24,12,
           36,36,27,23,25,57,
           28,42,31,19,44,51,
           32,28,50,46,30,43,
           12,38,21,16,24,69,
           47,23,49)

datos

minimo <- min(datos) ; minimo
maximo <- max(datos); maximo
rango <- maximo - minimo ; rango
num_intervalos <- 1 + (3.32*log10(57))  #log=ln != log10

Amplitud <- rango/round(num_intervalos)
round(Amplitud)

# marca_de_clase
inferior <- c(12,22,32,42,52,62,72)
superior <- c(22,32,42,52,62,72,82)
marca_de_clase <-  (superior+inferior)/2
marca_de_clase

#Conteo de frecuencias
datos_orden <- sort(datos)
datos_orden[datos_orden < 42 & datos_orden >= 32]
length(datos_orden[datos_orden < 42 & datos_orden >= 32])

length(datos[datos< 42 & datos >= 32])

datos[datos < 42 & datos >= 32]

length(datos_orden[datos_orden < 52 & datos_orden >= 42])

# Frecuencias absolutas acumuladas
frec_abs_acum <- cumsum(c(6,22,6,16,1,4,2))
frec_rela_acum <- frec_abs_acum/57
frec_rela_acum *100

frec_abs <- c(6,22,6,16,1,4,2)

sum(marca_de_clase*frec_abs)/57

sqrt(sum(((marca_de_clase-37.70)^2)*frec_abs)/57)

mediana <- datos_orden[(57+1)/2]
