# Generador de números aleatorios
m <- 2000
x0 <- 0
x1 <- 1
M <- 5*(98) # debe ser mayor a 3
##################################
aux <- numeric(m)
aux[1] <- x1
aux[2] <- (x1+x0)%%M
for(i in 3:m){
    aux[i] <-  (aux[i-1]+aux[i-2])%%M
}
aux ### sucesión de Lehmer
plot(0:m,c(0,aux),pch=16,cex=1.2)
abline(v=c(0,1680))
plot(aux[1:399],aux[2:400],pch=16,cex=1.2)
#############################################
u <- aux/M ### posibles números aleatorios
plot(u,pch=16,cex=2)
hist(u)
mean(u)
var(u)

