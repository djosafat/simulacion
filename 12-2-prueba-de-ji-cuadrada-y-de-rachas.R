####################################
#Prueba Ji cuadrada              ###
#sugerida para muestras grandes  ###
####################################
set.seed(123)
algunos_datos <- runif(500)
#algunos_datos <- c(0.3,.025,.17,.98,0.23,
                      0.82,0.08,0.72,0.07,0.45,
                      0.54,0.16,0.07,0.26,0.35,
                      0.12,0.99,0.06,0.2,0.97)
hist(algunos_datos,col=rainbow(11))
#hist(qnorm(algunos_datos),freq = F); curve(dnorm(x),add=T)
m <- length(algunos_datos)
alpha <- 0.05 #significancia
plot(algunos_datos[1:(m-1)],algunos_datos[2:m])
plot(algunos_datos)
abline(h=0:10/10,col=rainbow(11),lwd=4)
a <- hist(algunos_datos,breaks = 0:10/10,col=rainbow(11))
Oj <- a$counts
### Cálculo sin hist ##################################
O <- numeric(10)
O[1] <- sum(algunos_datos<=1/10)
O[2] <- sum((algunos_datos>1/10)*(algunos_datos<=2/10))
O[3] <- sum((algunos_datos>2/10)*(algunos_datos<=3/10))
O[4] <- sum((algunos_datos>3/10)*(algunos_datos<=4/10))
O[5] <- sum((algunos_datos>4/10)*(algunos_datos<=5/10))
O[6] <- sum((algunos_datos>5/10)*(algunos_datos<=6/10))
O[7] <- sum((algunos_datos>6/10)*(algunos_datos<=7/10))
O[8] <- sum((algunos_datos>7/10)*(algunos_datos<=8/10))
O[9] <- sum((algunos_datos>8/10)*(algunos_datos<=9/10))
O[10] <- sum((algunos_datos>9/10)*(algunos_datos<=1))
sum(Oj-O) #cero si coincide totalmente con las frecu. del hist
#######################################################
Ej <- rep(m/10,10)
TT <- sum((Oj-Ej)^2/Ej)
Criterio <- (TT>qchisq(1-alpha,9))
if(Criterio == TRUE){
    'no son uniformes'
    }else{
        'hay evidencia de uniformidad'
}             
pvalue <- 1-pchisq(TT,9) 
pvalue
#rechazar la uniformidad si pvalue<alpha
##################################################

#comparación con ks.test
plot(ecdf(algunos_datos))
curve(punif(x),col=2,add=T)
ks.test(algunos_datos,'punif')
library(randtests)
runs.test(algunos_datos) # randtests package needed
#################
## A mano:
#################
plot(algunos_datos)
med <- median(algunos_datos)
med
abline(h=med)
n1 <- length(algunos_datos[algunos_datos<med])
n2 <- length(algunos_datos[algunos_datos>med])
indicadoras <- 1
for(i in 2:m){
    aux1 <- 1*(algunos_datos[i-1] < med)
    aux2 <- 1*(algunos_datos[i] < med)
    indicadoras[i] <-  1*(aux1!=aux2)
}
Rachas <- sum(indicadoras)
ER <- (2*n1*n2)/(n1+n2)+1
VarR <- (2*n1*n2*(2*n1*n2-n1-n2))/((n1+n2)^2*(n1+n2-1)) 
Z <- (Rachas-ER)/sqrt(VarR)
pvalue <- 2*(1-pnorm(Z))
