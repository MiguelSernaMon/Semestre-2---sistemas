#Punto 2 M/D/1
# λ = 4, µ = 6 cafes por min

#a) Número de personas promedio en espera
#Lq = λ^2/((2*µ)*(µ-λ))

Lq <- 4^2/((2*6)*(6-4)) 


#b) Número de personas promedio en el sistema
#L = Lq+ λ/µ
L = Lq + 4/6


#c) Tiempo promedio en cola
#Wq = λ/((2*µ)*(µ-λ))

Wq = 4/((2*6)*(6-4))
