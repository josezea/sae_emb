
mean.estimation <- function(x, weights){
  sum(x * weights) / sum( weights)
}


ratio.estimation <- function(y, z, weights){
  sum(y * weights) / sum( z * weights)
}


estvar.mean <- function(x, weights, nsim){
  
  est_mean <- numeric(nsim) 
  
  for(i in 1:nsim){
    set.seed(i)
    pi_k <-  1/weights
    sel <- sample(length(x), replace = T, prob = pi_k) 
    est_mean[i] <- sum(x[sel] * weights[sel]) / sum(weights[sel])
  }
  est_var <- var(est_mean)
  est_var
}


estvar.ratio <- function(y, z, weights, nsim){
  
  est_ratio <- numeric(nsim) 
  
  for(i in 1:nsim){
    set.seed(i)
    pi_k <-  1/weights
    sel <- sample(length(y), replace = T, prob = pi_k) 
    est_ratio[i] <- sum(y[sel] * weights[sel]) / sum(z[sel] * weights[sel])
  }
  est_var <- var(est_ratio)
  est_var
}
