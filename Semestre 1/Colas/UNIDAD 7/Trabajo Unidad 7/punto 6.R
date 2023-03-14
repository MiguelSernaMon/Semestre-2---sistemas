

#vector mujeres 
vectorM <- c(38.9, 61.2, 73.3, 21.8, 63.4, 64.6, 48.4, 48.8, 48.5)
#vector hombres
vectorH <- c(67.8, 60, 63.4, 76, 89.4, 73.3, 67.3, 61.3, 62.4)
vectorM = sort(vectorM)
#test de saphiro para hombres
shapiro.test(vectorH)
#suma de los valores de los vectores
lasum1 <- sum(vectorM)
lasum2 <- sum(vectorH)
#vector al cuadrado de las mujeres
vectorM2 = vectorM^2
lasuma = sum(vectorM2)
#valores a para saphiro de las mujeres
valoresA <- c(0.5888,0.3244,0.1976,0.0947)

contador <- 9
index <- 1
valoresResta <- c()
#se multiplican los valores extremos de el vector de mujeres
for(i in 1:4){
  
  valoresResta[index] = vectorM[contador]-vectorM[i]
  index = index+1
  contador = contador-1
  
}
#se multiplican los vectores
multiplicarse = valoresResta*valoresA

sumamulti = sum(multiplicarse)

#medias
mediaPrimer = lasum1/9
mediaSegundo = lasum2/9
varVecorM2 <- lasuma-9*(mediaPrimer)^2

#varianza de los vectores
varVectorM <- var(vectorM)
varVectorH <- var(vectorH)

#se calcula el wc que en este caso tiene que ser mayor a wt = 0.829 
#para que sea normal

wc = ((sumamulti)^2)/((varVecorM2))
wt = 0.829

#De esto obtenemos que las dos muestras se distribuyen normalmente

#Calculamos si las varianzas son iguales o distintas
fisher <- qf(0.025,8,8, lower.tail = F)
fc = (varVectorM/varVectorH)
f1 = fisher #= 4.43
f2 = 1/fisher #=0.22

#Como fc < f1 y fc > f2, no podemos rechazar H0 por lo cual se concluye que 
#las varianzas son iguales

#Ahora el caso que le toca es el 3 con varianzas 

#Ho-> El peso promedio de las mujeres es igual al de los hombres
#H1-> El peso promedio de las mujeres es diferente al peso promedio de los hombres

Sp = sqrt(((9-1)*varVectorM+(9-1)*varVectorH)/16)

Tc =(mediaPrimer-mediaSegundo)/(Sp*sqrt((1/9)+(1/9))) # = -2.78
Ts = 2.1199

#Como tenemos una restricción de dos colas decimos que si |Tc| > Ts
#Vemos que la condición de rechazo se cumple, entonces
#rechazamos H0, así el peso promedio de peso de las mujeres es distinto al del los hombres.

#Ahora con el valor P
Vp = 2*pt(2.78,8,lower.tail = F) #= 0.023 

#Como P es menor a α=0.05, rechazamos Ho
#punto 7

pchisq(3.2414,2,lower.tail = F) # Vp = 0.19 > 0.05 entonces Ho no se rechaza




