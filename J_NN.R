
#Cost function for Neural Network when we have K >= 1 output units

J_NN <- function(H, Y, theta, lambda = 0){
  
  m <- dim(H)[1]
  Y <- as.matrix(Y)
  J <- 0
  
  for(i in 1:m){
    J <- J - t(Y[i,])%*%log(H[i,]) - t(1 - Y[i,])%*%log(1 - H[i,]) 
  }
  
  J <- (1/m)*(J + (lambda/2)*sum(theta^2))
  
  return(J)
}

