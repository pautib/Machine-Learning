# Forward propagation for Neural Networks
  # L = Number of layers (including input and output layer, so L > 2)
  # sl = number of units in each hidden layer
ForwardProp <- function(X, L, Theta){

    A <- list()
    A[[1]] <- X
    for(i in 2:(L-1)){ 
      A[[i]] <- sigmoid( A[[i-1]]%*%t(Theta[[i-1]]) )
      A[[i]] <- as.matrix(data.frame(1, A[[i]]))
    }
    A[[L]] <- sigmoid( A[[L-1]]%*%t(Theta[[L-1]]) )
    return(A)
}