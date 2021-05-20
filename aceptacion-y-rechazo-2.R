####################################
## Método de aceptación y rechazo ##
####################################
# Función de densidad objetivo.
f <- function(x){(6/5)*(x^2+x)}    ### 0<x<1
curve(f(x),0,1,col='green',lwd=3)
abline(h=0,v=c(0,1))
# cota superior
abline(h=f(1),col=2)
# Candidatas a función de densidad proposal (propuesta)
curve(dunif(x),0,1,col='blue',lwd=3,add=T)
#curve(dbeta(x,1,2),col='purple',lwd=3,add=T)
curve(dbeta(x,2,1),col='purple',lwd=3,add=T)


####################################################
####################################################
### Algoritmo
### generar valores desde f:
# Función de densidad objetivo.
f <- function(x){(6/5)*(x^2+x)}    ### 0<x<1

#Paso 1: Encontra un valor C y una función g (llamada proposal) que cumplan:
#         f(x)<=C*g(x)  para todos los valores del soporte de f
#         de preferencia el valor mínimo de C que se pueda usar
##################################################################
#Caso uniforme para g: 
##################################################################
C <- 3
g <- function(x){dunif(x)}
curve(C*g(x),0,1,col='red',lwd=3,ylim=c(0,3.5))
curve(f(x),0,1,col='green',lwd=3,add=T)
abline(h=0,v=c(0,1))
text(0.5,2,'esta es la densidad objetivo', col = 'green',cex=1.2)
text(0.5,3.2,'esta es la densidad proposal por C', col = 'red',cex=1.2)
#Paso 2: entrar al ciclo siguiente que se ejecutará
#        tantas veces como sea necesario para completar la muestra
#        deseada tamaño n.
n <- 1000
i <- 0
X <- 0
rechazo <- 0
while(i<n){
        Y <- runif(1) #genero valor aleatorio desde la proposal
        U <- runif(1) #genero valor aleatorio U(0,1)
        if(f(Y)/g(Y)>=C*U){ #se acepta Y
                i <- i + 1
                X[i] <- Y
        }else{rechazo <- rechazo + 1} #se rechaza Y
}
hist(X,freq = F,col='yellow')
curve(f(x),col=2,add = T,lwd=3)

##################################################################
#Caso uniforme para g mejorado: 
##################################################################
C <- f(1) #2.4 the best
g <- function(x){dunif(x)}
curve(C*g(x),0,1,col='red',lwd=3,ylim=c(0,3))
curve(f(x),0,1,col='green',lwd=3,add=T)
abline(h=0,v=c(0,1))
text(0.5,2,'esta es la densidad objetivo', col = 'green',cex=1.2)
text(0.5,2.6,'esta es la densidad proposal por C', col = 'red',cex=1.2)
#Paso 2: entrar al ciclo siguiente que se ejecutará
#        tantas veces como sea necesario para completar la muestra
#        deseada tamaño n.
n <- 1000
i <- 0
X <- 0
rechazo <- 0
while(i<n){
        Y <- runif(1) #genero valor aleatorio desde la proposal
        U <- runif(1) #genero valor aleatorio U(0,1)
        if(f(Y)/g(Y)>=C*U){ #se acepta Y
                i <- i + 1
                X[i] <- Y
        }else{rechazo <- rechazo + 1} #se rechaza Y
}
hist(X,freq = F,col='yellow')
curve(f(x),col=2,add = T,lwd=3)


##################################################################
#Caso beta 
##################################################################
C <- 1.5
g <- function(x){dbeta(x,2,1)}
curve(C*g(x),0,1,col='red',lwd=3,ylim=c(0,3))
curve(f(x),0,1,col='green',lwd=3,add=T)
abline(h=0,v=c(0,1))
text(0.6,0.5,'esta es la densidad objetivo', col = 'green',cex=1.2)
text(0.5,2.2,'esta es la densidad proposal por C', col = 'red',cex=1.2)
#Paso 2: entrar al ciclo siguiente que se ejecutará
#        tantas veces como sea necesario para completar la muestra
#        deseada tamaño n.
n <- 1000
i <- 0
X <- 0
rechazo <- 0
while(i<n){
        Y <- rbeta(1,2,1) #genero valor aleatorio desde la proposal
        U <- runif(1) #genero valor aleatorio U(0,1)
        if(f(Y)/g(Y)>=C*U){ #se acepta Y
                i <- i + 1
                X[i] <- Y
        }else{rechazo <- rechazo + 1} #se rechaza Y
}
hist(X,freq = F,col='yellow')
curve(f(x),col=2,add = T,lwd=3)


##################################################################
#Caso beta mejorado
##################################################################
C <- f(1)/g(1)
g <- function(x){dbeta(x,2,1)}
curve(C*g(x),0,1,col='red',lwd=3,ylim=c(0,3))
curve(f(x),0,1,col='green',lwd=3,add=T)
abline(h=0,v=c(0,1))
text(0.6,0.5,'esta es la densidad objetivo', col = 'green',cex=1.2)
text(0.5,2.2,'esta es la densidad proposal por C', col = 'red',cex=1.2)
#Paso 2: entrar al ciclo siguiente que se ejecutará
#        tantas veces como sea necesario para completar la muestra
#        deseada tamaño n.
n <- 1000
i <- 0
X <- 0
rechazo <- 0
while(i<n){
        Y <- rbeta(1,2,1) #genero valor aleatorio desde la proposal
        U <- runif(1) #genero valor aleatorio U(0,1)
        if(f(Y)/g(Y)>=C*U){ #se acepta Y
                i <- i + 1
                X[i] <- Y
        }else{rechazo <- rechazo + 1} #se rechaza Y
}
hist(X,freq = F,col='yellow')
curve(f(x),col=2,add = T,lwd=3)

