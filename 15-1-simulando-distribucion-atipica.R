###############################################
Fx <- function(x){ 
        #función de distribución propuesta
        (x<0)*(0*x) + 
        (x>=0 && x<1)*(x^2/2) +
        (x>=1 && x<2)*(x/4+1/4) + 
        (x>=2)*(1-exp(2-x)/4)
}
curve(Fx,0,1,ylim=c(0,1.1),xlim=c(-0.1,7),lwd=3)
abline(h=0,v=0)
abline(h=0.5,v=1,lty=2) #lty =1,2,3,4
curve(Fx,1,1.99,add=T,col=2,lwd=3)
abline(h=0.75,v=2,lty=2)
curve(Fx,2,7,add=T,col=4,lwd=3)
abline(h=1,lty=2)

FxInv <- function(p){
        (p>=0 && p<=1/2)*(sqrt(2*p)) + 
        (p>1/2 && p<=0.75)*(4*p-1) +
        (p>0.75 && p<=1)*(2-log(4-4*p))
}
##############################
m <- 10000
U <- runif(m)
X <- 1
for(i in 1:m){
        X[i] <- FxInv(U[i])
}
#############################

hist(X,100,freq = F,col='yellow')
fx <- function(x){
                (x>=0 && x<1)*(x) +
                (x>=1 && x<2)*(1/4) + 
                (x>=2)*(exp(2-x)/4)
}
curve(fx(x),0,1,col=2,lwd=3,add=T)
curve(fx(x),1,1.99,col=2,lwd=3,add=T)
curve(fx(x),2,8,col=2,lwd=3,add=T)
#
#curve(fx(x),0,8,add=T) ¿Cómo hacerlo directo?
#
plot(ecdf(X),lwd=6)
curve(Fx,0,1,ylim=c(0,1.1),xlim=c(-0.1,7),add=T,col=5,lwd=3)
abline(h=0,v=0)
abline(h=0.5,v=1,lty=2)
curve(Fx,1,1.99,add=T,col=2,lwd=3)
abline(h=0.75,v=2,lty=2)
curve(Fx,2,12,add=T,col=3,lwd=3)
abline(h=1,lty=2)
