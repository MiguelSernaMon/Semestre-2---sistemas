#GRAFICO 1
probabilidades <- c((1/6),(1/6),(1/6),(1/6),(1/6),(1/6))
barplot(probabilidades,col="cyan" , ylim = c(0, 0.4), names.arg =c(
  "1","2","3","4","5","6"))

#GRAFICO 2
dados <- c(1:6)
print(dados)
sumdados <- rep(0,36)
aux <- 1
piv <- 0
for (i in 1:6) {
  for (j in 1:6) {
    piv= i + j
    sumdados[aux] =piv
    aux = aux+1
  }
}
print(sumdados)
tabla <- matrix(c(sumdados[1:6],sumdados[7:12],sumdados[13:18],sumdados[19:24],
         sumdados[25:30],sumdados[31:36]),ncol = 6)
colnames(tabla) <- c("1","2","3","4","5","6")
rownames(tabla) <- c("1","2","3","4","5","6")
View(tabla)

#GRAFICO 3
vesperado <-median(dados)
desviacion <- sd(dados)
PS <- c(1:6, 5:1)/36
print(PS)
barplot(PS,
        ylim = c(0, 0.2), 
        xlab = "Media", 
        ylab = "Probability", 
        col = "steelblue", 
        space = 0, 
        main = "Sum of Two Dice Rolls",
        names.arg =c("1","-","2","-","3","-","4","-","5","-","6"))

#GRAFICO 4

sample5 <- c()

#tomar 10000 muestras con dados = 3
n = 10000
data <- c(1:6)
for (i in 1:n){
  sample5[i] = mean(sample(data, 3, replace=TRUE))
}
#----
histogram(table(sample5)/length(sample5))
plot(table(sample5)/length(sample5), xlab = 'Cara de dados',
     ylab = 'Relative Frequency', 
     main = '3 dados muestra aleatoria',
     type = "h",
     ylim = c(0,0.2),
     col = "cyan", lwd = 5,
     pch = 2)
     
#Grafico 5----------------------------------------
sample5 <- c()
#tomar 10000 muestras con dados = 4
n = 10000
data <- c(1:6)
for (i in 1:n){
  sample5[i] = mean(sample(data, 4, replace=TRUE))
}
#----
histogram(table(sample5)/length(sample5))
plot(table(sample5)/length(sample5), xlab = 'Cara de dados',
     ylab = 'Relative Frequency', 
     main = '3 dados muestra aleatoria',
     type = "h",
     ylim = c(0,0.2),
     col = "cyan", lwd = 5,
     pch = 2)





