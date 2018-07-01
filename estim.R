# Função preditiva

pred <- function(x_p, sx, n){
  den <- x_p + sx
  (x_p >= 0)*((sx/den)^n * (n/den))
}

curve(pred(x, 500, 20), xlim = c(-10, 500))

# Amostra da f(x|theta)

n <- 100
x <- rexp(n, rate = 2)
sx <- sum(x)

integrate(function(x) pred(x, sx, n), 0, Inf)

# Esperança x

esp <- function(x_p){
  n*(sum(x)/(x_p + sum(x)))^n*1/(x_p + sum(x)) * x_p
}

integrate(esp, 0, Inf)

# Esperança x^2

esp2 <- function(x_p){
  n*(sum(x)/(x_p^2 + sum(x)))^n*1/(x_p^2 + sum(x)) * x_p^2
}

# Variância

integrate(esp, 0, Inf)$value - integrate(esp, 0, Inf)$value^2

# Estimando moda e mediana da preditiva

posteriori <- rgamma(1000, shape = n, scale = 1/sx)
yp <- rexp(1000, posteriori)

quant <- quantile(yp)

dens <- density(yp)

moda <- dens$x[which(dens$y == max(dens$y))]

x_axis <- seq(-1, 8, len = 1000)
pred1 <- pred(x_axis, sx, n)
plot(x_axis, pred1, col = "red", lty = 2, type = "l")
lines(density(yp))

density(yp)


###

mu <- 0
vari <- 1

post <- function(x, mu, vari, n, sx){
  exp(-n*x-(log(x)-mu)^2/(2*vari))*x^(sx-1)
}

theta <- rlnorm(1000, mu, vari)

pred_pois <- rpois(1000, theta)

curve(dnorm(x, mean = mean(pred_pois), sd = sd(pred_pois)), from = 0, to = 12)

lines(prop.table(table(pred_pois)), type = "h")
