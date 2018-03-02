# Mean squared error function
J_linearreg <- function(X,Y,theta,lambda=0){
    m <- dim(X)[1]
    J <- t(X%*%theta - Y)%*%(X%*%theta - Y)/(2*m) + (lambda/(2*m))*t(theta)%*%theta
    return(J)
}