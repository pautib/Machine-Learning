NeuralNetwork <- function(X, Y, alpha, lambda, L, sl, max_iter = 500 ,threshold = 0.001){
  
  X <- as.matrix(data.frame(1,X))
  Y <- as.matrix(Y)
  K <- dim(Y)[2]
  n <- dim(X)[2]
  
  if(dim(X)[1]==dim(Y)[1] && L >= 3){
    
    Theta <- list()
    Theta[[1]] <- matrix(data = runif(sl*n,-sqrt(6/(n + sl)), sqrt(6/(n + sl))), nrow = sl, ncol = n)
    
    if(L>3){ 
      for(i in 2:(L-2)){ Theta[[i]] <- matrix(data = runif(sl*(sl + 1),-sqrt(6/(2*sl + 1)), sqrt(6/(2*sl + 1))), nrow = sl, ncol = sl + 1)}
      Theta[[L-1]] <- matrix(data = runif(K*(sl + 1),-sqrt(6/(K + sl + 1)), sqrt(6/(K + sl +1))), nrow = K, ncol = sl + 1)
    }
    else{ Theta[[2]] <- matrix(data = runif(K*(sl + 1),-sqrt(6/(K + sl + 1)), sqrt(6/(K + sl +1))), nrow = K, ncol = sl + 1)}
    
    convergence = F
    J <- 0
    iter <- 0

    while(convergence == F){
    
      A <- ForwardProp(X, L, Theta)
      
      alpha_iter <- alpha/(1 +2*iter/max_iter)
    
      Theta <- BackProp(A, Y, L, alpha_iter, lambda, Theta)
    
      if(abs(J - J_NN(A[[L]], Y, unlist(Theta), lambda)) < threshold && iter >= max_iter ){convergence=T}
      if((abs(J - J_NN(A[[L]], Y, unlist(Theta), lambda)) > threshold && iter >= max_iter) || is.nan(abs(J - J_NN(A[[L]], Y, unlist(Theta), lambda)))){
        print("El algoritmo NO converge o no funciona")
        return(list(MSE = J, Theta = Theta))
      }
      J = J_NN(A[[L]], Y, unlist(Theta), lambda)
      iter = iter + 1
    
      print(iter)
      print(J)
    }
    print("El algoritmo converge")
    return(list(MSE = J, Theta = Theta))
  } else{return(print("Error: Las dimensiones de los parámetros no concuerdan o has puesto L < 3"))}
}