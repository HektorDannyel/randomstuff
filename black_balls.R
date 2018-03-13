black_balls <- function(n, m, x, theta = rep(1/n, times = n + 1), plot = TRUE){
  y <- numeric(n + 1)
  if(x <= n & m <= x){
    for(i in (x - m + 1):(n - m + 1)){
      if(i - x + 1 <= 0){
        max_n <- 1
      }
      else{
        max_n <- i - x + 1
      }
      
      if(n - x + 1 < 0){
        max_d <- 0
      }
      else{
        max_d <- n - x + 1
      }
      prob_n <- seq(i, max_n, -1)
      prob_d <- seq(n, max_d, -1)
      y[i] <- prod(prob_n)/prod(prob_d)
    }
  } else {
    stop("You can't extract more balls than you have, and you can't observe more non-black balls than you extracted")
  }
  bayes <- y*theta/sum(y*theta)
  dist <- data.frame(id = 0:c(n),
                     priori = theta,
                     posteriori = bayes)
  if(plot == TRUE){
    plot(y = dist$priori, x = dist$id - .1, t = "h", col = "red", lty = 3, xlim = c(0, (n + .1)),
         ylim = c(0, dist[sort(which(dist$posteriori > 0), TRUE)[1],3] + .1),
         ylab = expression(paste("P(", theta,"|X)")), xlab = expression(theta))
    lines(dist$id + .1, dist$posteriori, t = "h", lty = 1)
    legend("topleft", legend = c("Priori", "Posteriori"), col = c("red", "black"), lty = c(3, 1))
  }
  return(dist)
}

black_balls(25, 15, 19)
