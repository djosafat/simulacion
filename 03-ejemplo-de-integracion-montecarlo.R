#solución forma numérica
f <- function(x){exp(-x^2)}
integrate(f,0,1)
# con error de 8.3 por 10^{-15}
as.numeric(integrate(f,0,1)[1])
#
as.numeric(integrate(f,0,Inf)[1])
as.numeric(integrate(f,-Inf,Inf)[1])
#
#solución Montecarlo
u <- runif(10000)
mean(exp(-u^2))


f <- function(x){sin(x)}
integrate(f,0,1)
# con error de 5.1 por 10^{-15}
as.numeric(integrate(f,0,1)[1])
#
#solución Montecarlo
u <- runif(10000)
mean(sin(u))
1-cos(1)
