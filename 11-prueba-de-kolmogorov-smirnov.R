set.seed(425)
algunos_datos <- runif(20,0,1)
m <- length(algunos_datos)
plot(ecdf(algunos_datos),
     lwd=3,xlim=c(0,1),col='blue')
sort(algunos_datos)
abline(h=0,v=0)
curve(punif(x,0,1),add = T,col=2,lwd=3)
#?ks.test
ks.test(algunos_datos,"punif") ##TT
#cálculo a mano
aux <- sort(algunos_datos) #estadísticos de orden
points(aux,rep(0,m),pch=16,col='magenta',cex=1.2)
points(aux,0:(m-1)/m,cex=1.2)
alturas_linea_roja <- punif(aux)
alturas_linea_negra <- 1:m/m
distancias_izq <- abs(alturas_linea_negra-alturas_linea_roja)
distancias_der <- abs(0:(m-1)/m-alturas_linea_roja)
TT <- max(distancias_izq,distancias_der)
TT
aux2 <- which(c(distancias_izq,distancias_der)==TT)
if(aux2>m){
    points(aux[aux2-m],(aux2-m-1)/m,pch=16,col='green',cex=2)
}else{
    points(aux[aux2],aux2/m,pch=16,col='green',cex=2)
}

########################################
# Cálculo del p-value
########################################
# Distribución nula (del estadístico de prueba)
G <- function(x,n){
    lim <- floor(n*(1-x))
    S <- 0
    for(j in 0:lim){
        S <- S + choose(n,j)*(1-x-(j/n))^(n-j)*(x+(j/n))^(j-1)
    }
    1-x*S
}
####################################################
valorcritico <- 1.36/sqrt(m+sqrt(m/10)) #si m>40
valorcritico #0.2938921 para m=20, alpha=0.05
2*G(valorcritico,m)-1 # debe ser parecido a 0.95
                      # corroborando el valor crítico
2*G(0.2945,m)-1 # corrección si m < 40
G(0.2945,m)^2 #aproximación 
pvalue <- 2*(1-G(TT,m)) #2*Pv(TT)
TT
pvalue
ks.test(algunos_datos,'punif')

