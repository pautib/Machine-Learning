# Back Propagation for Neural Networks

BackProp <- function(A, Y, L, alpha, lambda, Theta){
  
  D <- list()
  Delta <- list()
  m <- dim(Y)[1]
  
  D[[L]] <- A[[L]] - Y
  Delta[[L-1]] <- apply(X = simplify2array(lapply(X = 1:m, function(j) D[[L]][j,]%*%t(A[[L-1]][j,]))), MARGIN = c(1,2), FUN = sum)
  Theta[[L-1]] <- Theta[[L-1]] - alpha*(1/m)*Delta[[L-1]]
  Theta[[L-1]][,-1] <- Theta[[L-1]][,-1] - alpha*(1/m)*lambda*Theta[[L-1]][,-1]
  
  for(l in (L-1):2){
    D[[l]] <- (D[[l+1]]%*%Theta[[l]])*A[[l]]*(1 - A[[l]])
    D[[l]] <- D[[l]][,-1]
    Delta[[l-1]] <- apply(X = simplify2array(lapply(X = 1:m, function(j) D[[l]][j,]%*%t(A[[l-1]][j,]))), MARGIN = c(1,2), FUN = sum)
    Theta[[l-1]] <- Theta[[l-1]] - alpha*(1/m)*Delta[[l-1]]
    Theta[[l-1]][,-1] <- Theta[[l-1]][,-1] - alpha*(1/m)*lambda*Theta[[l-1]][,-1]
  }
  
  return(Theta)

}
  