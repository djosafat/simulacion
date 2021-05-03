dbinom(0,10,1/2)
dbinom(1,10,1/2)
dbinom(2,10,1/2)
dbinom(3,10,1/2)
dbinom(0:10,10,1/2)
100000*dbinom(0:10,10,1/2)
aux <- floor(100000*dbinom(0:10,10,1/2))
#sum(floor(100000*dbinom(0:10,10,1/2)))
muestra <- c(rep(0,aux[1]),rep(1,aux[2]),rep(2,aux[3]),
             rep(3,aux[4]),rep(4,aux[5]),rep(5,aux[6]),
             rep(6,aux[7]),rep(7,aux[8]),rep(8,aux[9]),
             rep(9,aux[10]),rep(10,aux[11]))
#c(8,8,8,8) # equivalente a:
#rep(8,4) #función 'repetir'
hist(muestra)
hist(rbinom(99993,10,1/2),col='yellow',add=T)
plot(muestra)
plot(rbinom(99993,10,1/2),col='yellow')
mean(muestra)
mean(rbinom(99993,10,1/2))
var(muestra)
var(rbinom(99993,10,1/2))
