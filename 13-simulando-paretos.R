#Caso Pareto con parámetros a=3, b=1
FF <- function(x){1-(1+x)^(-3)}
curve(FF(x),0,3,lwd=4,col='blue')
abline(h=0,v=0,lwd=4)
abline(h=1,lty=2,col=2,lwd=4)
#inversa
FFinv <- function(x){ (1-x)^(-1/3)-1 }
curve(FFinv(x),0,0.99,lwd=4,col='magenta',add=T)
curve(1*(x),add=T)
########################################
## Generamos una muestra tamaño 10000 ##
########################################
u <- runif(10000)
Z <- FFinv(u)
hist(Z,50,xlim=c(0,8),freq = F,col=rainbow(8))
f <- function(x) { # función de densidad
    3/(1+x)^4
}
curve(f(x),add = T, col = 4, lwd=4)
plot(ecdf(Z),col = 6,lwd=5)
#plot(ecdf(rexp(10000))) # ecdf = empirical cumulative distribution function
curve(FF(x),add=T,col=1,lwd=2,xlim = c(0,40))
