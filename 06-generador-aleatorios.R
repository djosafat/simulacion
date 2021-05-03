# Generador de números aleatorios
m <- 100
x0 <- 11
M <- 37
a <- 5
c <- 0
##################################
aux <- numeric(m)
aux[1] <- x0
for(i in 2:m){
    aux[i] <-  (aux[i-1]*a+c)%%M
}
aux ### sucesión de Lehmer
u <- aux/M ### posibles números aleatorios
#############################################
plot(u,pch=16,cex=2)
hist(u)
mean(u)
var(u)

numalea <- function(m,x0,a,c,M){
    aux <- numeric(m)
    aux[1] <- x0
    for(i in 2:m){
        aux[i] <-  (aux[i-1]*a+c)%%M
    }
    #aux
    aux/M
}

muestra <- numalea(1000,2^5,13.3,0.5,2^7)
muestra
plot(muestra)
hist(muestra)
mean(muestra)
var(muestra)

muestra <- numalea(1000,2021,7^5,0,2^31-1)
muestra
plot(muestra)
hist(muestra)
mean(muestra)
var(muestra)

muestra <- numalea(20,2,1,7,13)
muestra
plot(muestra)
hist(muestra)
mean(muestra)
var(muestra)
