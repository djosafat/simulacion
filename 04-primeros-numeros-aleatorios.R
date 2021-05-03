numeros_aleatorios <- c(0.3,.025,.17,.98,0.23,
                        0.82,0.08,0.72,0.07,0.45,
                        0.54,0.16,0.07,0.26,0.35,
                        0.12,0.99,0.06,0.2,0.97)
plot(numeros_aleatorios,pch=16,cex=3)#graf. de dispersion
abline(h=0,v=0)
####
muestra <- runif(20)
plot(muestra,pch=16,cex=3)#graf. de dispersion
abline(h=0,v=0)
####
par(mfrow=c(2,1),mar=c(2,3,2,3))
##################################
# La media poblacional es 1/2 = 0.5
# La varianza poblacional es 1/12= 0.08333
#################################
# Cálculo de la media y varianza muestral
mean(numeros_aleatorios)
mean(muestra)
#
var(numeros_aleatorios)
var(muestra)
###################################

###################################
los_numeros <- c(38,21,46,68,65,
                 13,4,22,2,18,
                 23,16,78,49,50,
                 56,8,54,95,3)
numeros_aleatorios_2 <- los_numeros/100
plot(numeros_aleatorios_2,pch=16,cex=3)#graf. de dispersion
abline(h=0,v=0)
####
muestra <- runif(20)
plot(muestra,pch=16,cex=3)#graf. de dispersion
abline(h=0,v=0)

####
mean(numeros_aleatorios_2)
var(numeros_aleatorios_2)
####
mean(muestra)
var(muestra)
####
# La media poblacional es 1/2 = 0.5
# La varianza poblacional es 1/12= 0.08333

par(mfrow=c(2,2),mar=c(1,1,1,1))
####
muestra <- runif(100)
plot(muestra,pch=16,cex=3)#graf. de dispersion
abline(h=0,v=0)
####
muestra <- runif(100)
plot(muestra,pch=16,cex=3)#graf. de dispersion
abline(h=0,v=0)
####
muestra <- runif(100)
plot(muestra,pch=16,cex=3)#graf. de dispersion
abline(h=0,v=0)
####
muestra <- runif(100)
plot(muestra,pch=16,cex=3)#graf. de dispersion
abline(h=0,v=0)
