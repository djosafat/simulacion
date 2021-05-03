M <- 31
a <- 11 ## a>0 y entero 
##############
vq <- 0
a1M <- 0.5
i <- 0
while(a1M!=floor(a1M)){
    i <- i + 1
    a1M <- (a^i-1)/M
    vq[i] <- a1M==floor(a1M)
}
vq
i

#########################################################
#########################################################
#########################################################

M <- 17
periodos <- numeric(M)
##############
for(aj in 1:M){
    a1M <- 0.5
    i <- 0
    while(a1M!=floor(a1M)){
        i <- i + 1
        a1M <- (aj^i-1)/M
    }
    periodos[aj] <- i
}
plot(periodos)
abline(h=M-1)
periodos
##########
numalea <- function(m,x0,a,c,M){
    aux <- numeric(m)
    aux[1] <- x0
    for(i in 2:m){
        aux[i] <-  (aux[i-1]*a+c)%%M
    }
    aux
    #aux/M
}

muestra <- numalea(M,13,aux,0,M)
muestra
plot(muestra)
muestra*M

#################################################
M <- 31
q_ <- M-1
menor <- 0
primitivas1 <- 1
primitivas2 <- 1
i <- 0
j <- 0
for(a in 2:(M-1)){
    xx <- (a^q_-1)/M
    if(xx==floor(xx)){
        j <- j + 1
        primitivas2[j] <- a
        aux2 <- (a^(1:(M-1))-1)/M
        menor <- sum(aux2==floor(aux2))
        if(menor==1){
            i <- i + 1
            primitivas1[i] <- a
        }
    }
}

