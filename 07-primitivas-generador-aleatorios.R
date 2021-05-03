M <- 31
q_ <- M-1
#######################################
#######################################
menor <- 0
aux <- 2
xx <- (aux^q_-1)/M
while(menor!=1){
    if(xx!=floor(xx)){
        aux <- aux + 1
        xx <- (aux^q_-1)/M
    }else{
        aux2 <- (aux^(1:(M-1))-1)/M
        menor <- sum(aux2==floor(aux2))
        if(menor!=1){aux <- aux + 1}
    }
}
############################################
############################################
#aux <- 2
(aux^q_-1)/M
aux
##############
vq <- 0
for(i in 2:M){
    aq1M <- (aux^i-1)/M
    vq[i] <- aq1M==floor(aq1M)
}
vq
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
primitivas1
