# Mean squared error function for logistic regression

J_logisticreg <- function(X,Y,theta,lambda){
    m <- dim(X)[1]
    J <- (-t(Y)%*%log(sigmoid(X%*%theta)) - t(1 - Y)%*%log(1 - sigmoid(X%*%theta)))/m + (lambda/(2*m))*t(theta)%*%theta
    return(J)
}