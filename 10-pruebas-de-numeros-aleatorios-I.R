m <- 1000
muestra <- runif(m)
#muestra <- c(0.3,.025,.17,.98,0.23,
#  0.82,0.08,0.72,0.07,0.45,
#  0.54,0.16,0.07,0.26,0.35,
#  0.12,0.99,0.06,0.2,0.97)
#m <- length(muestra)
?runif
hist(muestra) #frecuencias
hist(muestra, freq = F) #probabilidades (area igual a uno)
curve(dunif(x,0,1),add = T, col='red',lwd=4)
plot(muestra)
plot(muestra[1:(m-1)],muestra[2:m])
