LinearReg_GD <- function(X, Y, theta, lambda = 0, alpha, max_iter, threshold){
  
  if(dim(X)[1]==length(Y) && length(theta)==dim(X)[2]) {
    X <- as.matrix(X)
    #plot(x = NULL, xlim=c(0,n_iter), ylim=c(1000,2000), ylab="J", xlab="iteration number")
    convergence = F
    J <- 0
    iter <- 0
    while(convergence == F){
      theta <- Gradescent_iter(X,Y,theta,alpha,lambda)
      if(abs(J - J_linearreg(X,Y,theta,lambda)) < threshold && iter >= max_iter ){convergence=T}
      if(abs(J - J_linearreg(X,Y,theta,lambda)) > threshold && iter >= max_iter){
        print("El algoritmo NO converge")
        return(list(MSE = J, theta = theta))
        }
      J = J_linearreg(X,Y,theta,lambda)
      iter = iter + 1
      #points(i, J, type = "p", add = T)
    }
    print("El algoritmo converge")
    return(list(MSE = J, theta = theta))
  } else{return(print("Error: Las dimensiones de los parámetros no concuerdan"))}
}