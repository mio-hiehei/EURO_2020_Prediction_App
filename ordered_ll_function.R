ll_ologit <- function(theta, Z, X){
  K <- ncol(X)
  J <- ncol(Z)
  
  # Subset theta
  beta <- theta[1:K]
  tau <- theta[(K + 1):(K + J -1)] #because we have 1 tau less than categories, that is why - 1
  
  # linear predictor
  U <- X %*% beta
  
  # Probabilities in a matrix
  probs <- matrix(nrow = length(U), ncol = J)
  
  # Calculate the Probabilities for each category according to Tau values
  
  # First category
  probs[,1] <- 1 / (1 + exp(-(tau[1] - U)))
  
  # in-between categories
  for(j in 2:(J - 1)){
    probs[, j] <- (1 / (1 + exp(-(tau[j] - U))) - 1 / (1 + exp(-(tau[j - 1] - U))))
  }
  
  # last category
  probs[,J] <-1 - 1 / (1 + exp(-(tau[J - 1] - U)))
  
  # sum up all the probabilities for Log Likelihood
  ll <- sum(log(probs[Z]))
  
  return(ll)
}
