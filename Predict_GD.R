Predict_GD <- function(X, theta, lambda, compare = F ,Y = NULL, algorithm, L = NULL){
  
  X <- as.matrix(data.frame(1,X))
  
  if(algorithm == "lm"){
    pred <- X%*%theta
    if(compare == T){
      J_error <- J_linearreg(X,Y,theta,lambda)
      return(list(Scores = pred, J_error = J_error))
    }
  }
  if(algorithm == "glm"){
    pred <- as.numeric(sigmoid(X%*%theta) >= 0.5)
    if(compare == T){
      Mean_error <- sum(pred != Y)/length(pred)
      J_error <- J_logisticreg(X, Y, theta, lambda)
      TP <- sum(Y[pred == 1])
      FP <- sum(!(Y[pred == 1]))
      FN <- sum(Y[pred == 0])
      Precision <- TP/(TP + FP)
      Recall <- TP/(TP + FN)
      F1Score <- 2*Precision*Recall/(Precision + Recall)
      return(list(Scores = pred, Mean_error = Mean_error, J_error = J_error, Precision = Precision, Recall = Recall, F1Score = F1Score))
    }
  }
  if(algorithm == "nnet"){
    
    A <- ForwardProp(X, L, theta)
    pred <- as.numeric(A[[L]]>= 0.5)
    if(compare == T){
      
      Mean_error <- sum(pred != Y)/length(pred)
      J_error <- J_NN(A[[L]], Y, unlist(theta), lambda)
      TP <- sum(Y[pred == 1])
      FP <- sum(!(Y[pred == 1]))
      FN <- sum(Y[pred == 0])
      Precision <- TP/(TP + FP)
      Recall <- TP/(TP + FN)
      F1Score <- 2*Precision*Recall/(Precision + Recall)
      return(list(Scores = pred, Mean_error = Mean_error, J_error = J_error, Precision = Precision, Recall = Recall, F1Score = F1Score))
    }
  }
  return(pred)
}