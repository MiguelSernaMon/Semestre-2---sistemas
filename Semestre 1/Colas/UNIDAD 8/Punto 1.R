#punto 1 
#λ = 12, µ = 15 llamadas/hora

#a) Tiempo promedio que debe esperar el cliente para tomar su pedido
    #Wq = λ/(µ(µ-λ))

Wq = 12/(15*(15-12))
  
#b)  número promedio de personas que llaman y esperan para un pedido
   #Lq = λ^2/(µ(µ-λ))
Lq = 12^2/(15*(15-12))

#c)¿contratar otro empleado?
  
  #Po = (1/(∑ 1/n! * (λ/µ)^n)) + 1/m * (λ/µ)^m * (m*µ)/(m*µ-λ)

  #Wq = (µ(λ/µ)^m)/(m-1)!*(mµ-λ)^2
#m = 2
aux <- 0
valoresN <- c(0,1)
for (i in valoresN) {
    
  aux = aux + ((1/1) * (12/15)^i)
  
}

Po = 1/(aux +(1/2) * (12/15)^2 * (2*15)/(2*15-12))

Wq2 = ((15*(12/15)^2)/((2-1)*(2*15-12)^2)) * Po

#Costos con un trabajador
c1 <- 10+12*Wq*50
#Costos con dos trabajadores
c2 <- 20+12*Wq2*50

Ls = Lq + 12/15

#Dinero ahorrado
Dinero<-c1-c2

#costo total = 4*Wq3*50 + 10*n
4*Wq*50+10

trabajadores <- c(1:20)
#define data to plot
x <- 1:10
y1 <- c(2, 4, 4, 5, 7, 6, 5, 8, 12, 19)
y2 <- c(2, 2, 3, 4, 4, 6, 5, 9, 10, 13)

#plot first line
plot(x, y1, type='l', col='red', xlab='x', ylab='y')

#add second line to plot
lines(x, y2, col='blue')




