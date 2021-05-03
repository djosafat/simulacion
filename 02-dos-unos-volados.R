m <- 10000
lanzamientos <- numeric(m)
for(j in 1:m){
        i <- 1
        a <- 0
        b <- rbinom(1,1,1/2)
        while(a+b<2){
                a <- b
                b <- rbinom(1,1,1/2)
                i <- i + 1
        }        
        lanzamientos[j] <- i
}
resultado <- mean(lanzamientos)
