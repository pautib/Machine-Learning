# Gradient descent iteration for linear regression

Gradescent_iter <- function(X, Y, theta, alpha, lambda = 0){
  m <- dim(X)[1]
  theta <- theta - (alpha/m)*(t(X)%*%(X%*%theta - Y))
  theta[-1] <- theta[-1] - (alpha*lambda/m)*theta[-1]
  return(theta)
}
