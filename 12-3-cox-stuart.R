numeros_aleatorios <- c(0.3,.025,.17,.98,0.23,
                        0.82,0.08,0.72,0.07,0.45,
                        0.54,0.16,0.07,0.26,0.35,
                        0.12,0.99,0.06,0.2,0.97)
plot(numeros_aleatorios,pch=16,cex=1.3,col=2)
#graf. de dispersion
abline(h=0,v=0)
plot(numeros_aleatorios[1:19],numeros_aleatorios[2:20])
acf(numeros_aleatorios)#correlograma
l <- 10
m <- length(numeros_aleatorios)
aux <- 1*(numeros_aleatorios[1:l]<numeros_aleatorios[(1+l):m])
aux
empates <- sum((numeros_aleatorios[1:l]==numeros_aleatorios[(1+l):m]))
n <- l-empates
TT <- sum(aux)
aux2 <- dbinom(TT,n,1/2)
probasbinom <- dbinom(0:n,n,1/2)
pvalue <- sum(probasbinom[probasbinom<=aux2])
pvalue
# Si pvalue > 0.05 entonces "No se rechaza H0 y por tanto, 
# no existe evidencia de tendencia
####
muestra <- runif(20)
plot(muestra,pch=16,cex=1.3)#graf. de dispersion
abline(h=0,v=0)
plot(muestra[1:19],muestra[2:20])
acf(muestra)#correlograma
l <- 10
m <- length(muestra)
aux <- 1*(muestra[1:l]<muestra[(1+l):m])
aux
empates <- sum((muestra[1:l]==muestra[(1+l):m]))
n <- l-empates
TT <- sum(aux)
aux2 <- dbinom(TT,n,1/2)
probasbinom <- dbinom(0:n,n,1/2)
pvalue <- sum(probasbinom[probasbinom<=aux2])
pvalue
# Si pvalue > 0.05 entonces "No se rechaza H0 y por tanto, 
# no existe evidencia estadística de tendencia"
####
