# Gradient descent iteration for logistic regression 

Gradescent_log_iter <- function(X,Y,theta,alpha,lambda = 0){
  m <- dim(X)[1]
  theta <- theta - (alpha/m)*t(X)%*%(sigmoid(X%*%theta) - Y)
  theta[-1] <- theta[-1] - (alpha*lambda/m)*theta[-1] 
  return(theta)
}