### Exponencial

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
  n*(sum(x)/(x_p + sum(x)))^n*1/(x_p + sum(x)) * x_p^2
}

# Variância

integrate(esp, 0, Inf)$value - integrate(esp, 0, Inf)$value^2

# Estimando moda e quantis da preditiva

posteriori <- rgamma(1000, shape = n, scale = 1/sx)
yp <- rexp(1000, posteriori)

quant <- quantile(yp); quant # Quantis

dens <- density(yp)

moda <- dens$x[which(dens$y == max(dens$y))]; moda # Moda

plot(dens)
abline(v = moda, h = max(dens$y))

# Solução analítica vs simulação

x_axis <- seq(-1, 8, len = 1000)
pred1 <- pred(x_axis, sx, n)
plot(x_axis, pred1, col = "red", lty = 2, type = "l")
lines(density(yp))
legend("topright", legend = c("Analítico", "Simulação"), lty = c(2, 1), col = c("red", "black"))

### log-normal

# Parâmetros da priori

mu <- 0 
vari <- 1

theta <- rlnorm(1000, mu, vari) # Simulando valores da priori

# Simulando valores da preditiva

pred_pois <- rpois(1000, theta) 

x <- seq(from = 0, to = 12, l = 1000)

# Curva da aproximação normal

plot(dnorm(x, mean = mean(pred_pois), sd = sd(pred_pois)) ~ x, t = "l", ylim = c(0, .4),
     ylab = "")

# Valores simulados

lines(prop.table(table(pred_pois)), type = "h", col = "red", lty = 3)

# Discretização

lines(dnorm(c(0:12), mean = mean(pred_pois), sd = sd(pred_pois)) ~ c(0:12), type = "h", col = "green")

legend("topright", col = c("black", "red", "green"), lty = c(1, 3, 1), 
       legend = c("Aproximação normal", "Simulação", "Discretização"))
