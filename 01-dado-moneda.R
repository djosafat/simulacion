#Valor exacto
ve <- 7/4
#Simulando
m <- 1000
resultados <- numeric(m)
i <- 0
repeat{
        i <- i + 1
        x <- sample(1:6,1) # lanz. de dado
        y <- rbinom(1,x,1/2) # lanz. de moneda
        resultados[i] <- y
        if(i == m){break}
}
aprox <- mean(resultados)
aprox
tray_prom <- cumsum(resultados)/1:m
plot(tray_prom,type = 'l',col='red',lwd=8)
title(paste('aproximación =',aprox,' exacto =',ve))
abline(h=ve,col='blue',lty=3,lwd=5)
rang <- max(tray_prom)-min(tray_prom)
legend(m/2,min(tray_prom)+0.35*rang,
       c('aproxim','exacto'),
       border = 'white', lwd=3, lty = c(1,3),
       col=c('red','blue'))
