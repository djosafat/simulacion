#Generando una
# permutación
##################################
permutacion <- function(m,x0,a,c){
    aux <- numeric(m)
    aux[1] <- x0
    for(i in 2:m){
        aux[i] <-  (aux[i-1]*a+c)%%m
    }
    aux
}

muestra <- permutacion(1000,500,21,103)
library(FRACTION)
gcd(1000,103)
muestra
plot(muestra)

############################################
#  Ejemplo    
# Generador de números aleatorios
m <- 1000
x0 <- 11
M <- 1511
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
par(mfrow=c(2,1),mar=c(0,0,0,0))
plot(u,pch=16,cex=1.2)
plot(u[1:999],u[2:1000],pch=16,cex=1.2)
#############################################
library(randtests)
runs.test(u) # randtests package needed
unew <- u[muestra] 
plot(unew,pch=16,cex=1.2)
plot(unew[1:999],unew[2:1000],pch=16,cex=1.2)
runs.test(unew) # randtests package needed