pred <- function(x_p){
  sapply(x_p, 
         function(x__p){
           if(x__p < 0) {
             0
           }
           else{
             n*(sum(x)/(x__p + sum(x)))^n*1/(x__p + sum(x))
           }
         }
  )
}

x <- rexp(100, rate = 1)

n <- 100

integrate(pred, 0, Inf)

curve(pred, 0, 2)

plot(density(pred(rexp(100, 1))))

esp <- function(x_p){
  n*(sum(x)/(x_p + sum(x)))^n*1/(x_p + sum(x)) * x_p
}

integrate(esp, 0, Inf)

avg2 <- function(x_p){
  n*(sum(x)/(x_p^2 + sum(x)))^n*1/(x_p^2 + sum(x)) * x_p^2
}

integrate(avg2, 0, Inf)$value-integrate(avg, 0, Inf)$value^2

gam <- rgamma(1000, n, 1/sum(x))

qgamma(c(.05, .95), n, 1/sum(x))

rexp(10, gam[1:10])
