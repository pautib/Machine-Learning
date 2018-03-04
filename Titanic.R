Datatest <- read.csv2(file = "/Users/entimos/Desktop/test.csv", header = T, sep = ",", na.strings = c(""))
Datatrain <- read.csv2(file = "/Users/entimos/Desktop/train.csv", header = T, sep = ",", na.strings = c(""))
Outputtest <- read.csv2(file = "/Users/entimos/Desktop/gender_submission.csv", header = T, sep = ",")

sapply(Datatrain,function(x) sum(is.na(x)))

Datatrain <- subset(x = Datatrain, select = c(2,3,5,6,7,8,10,12))

Datatrain$Age[is.na(Datatrain$Age)] <- mean(x = Datatrain$Age, na.rm=T)

Datatrain <- Datatrain[!is.na(Datatrain$Embarked),]

levels(Datatrain$Sex) <- c(0,1) # female = 0, male = 1
levels(Datatrain$Embarked) <- c(0,1,2) # C = 0, Q = 1, S = 2
Datatrain <- apply(X = data.frame(1,Datatrain), MARGIN = 2, function(x) as.numeric(x))
as.matrix(Datatrain)
Training <- data.frame(Datatrain[,c(1,2)],scale(x = Datatrain[,-c(1,2)], center = T, scale = T))

###Data test


sapply(Datatest,function(x) sum(is.na(x)))

Datatest <- subset(x = Datatest, select = c(2,4,5,6,7,9,11))

Datatest$Age[is.na(Datatest$Age)] <- mean(x = Datatest$Age, na.rm=T)

Datatest$Fare[is.na(Datatest$Fare)] <- mean(x = Datatest$Fare, na.rm=T)

levels(Datatest$Sex) <- c(0,1) # female = 0, male = 1
levels(Datatest$Embarked) <- c(0,1,2) # C = 0, Q = 1, S = 2
Datatest <- apply(X = data.frame(1,Datatest), MARGIN = 2, function(x) as.numeric(x))
as.matrix(Datatrain)
Testset <- data.frame(Datatest[,1],scale(x = Datatest[,-1], center = T, scale = T))
save(... = Training, Testset, file = "CleanData.RData")

### Cross-validation
crosslabel <- sample(x = 1:dim(Training)[1], size = 177, replace = F)
DataCross <- Training[crosslabel,]
DataTrain <- Training[-crosslabel,]
DataCross <- apply(X = DataCross, MARGIN = 2, function(x) as.numeric(x))

###MIA
lambda <- seq(from = 0, to = 10, by = 0.3)
alpha <- seq(from =0.2, to = 2, by = 0.1)
Grid <- expand.grid(lambda,alpha)

p <- qplot(main = "TrainError", xlab = "lambda", ylab = "J")

err <- sapply(X = lambda, function(x){
  res <- LogisticReg_GD(X = DataTrain[,-2], Y = DataTrain[,2], theta =c(0,runif(1,0,1),runif(1,0,1),runif(1,0,1),runif(1,0,1),runif(1,0,1),runif(1,0,1),runif(1,0,1)),lambda = x, alpha = 1.5, max_iter = 400, threshold = 0.001)
  crosserror <- J_logisticreg(X = DataCross[,-2], Y = DataCross[,2], theta = res$theta, lambda = 0)
  return(c(res[[1]], crosserror))
} )

err <- t(err)
p <- p + geom_point(mapping = aes(x = lambda , y = err[,1]), color = "green")
p <- p +  geom_point(mapping = aes(x = lambda , y = err[,2]))

res <- LogisticReg_GD(X = DataTrain[,-2], Y = DataTrain[,2], theta =c(0,runif(1,0,1),runif(1,0,1),runif(1,0,1),runif(1,0,1),runif(1,0,1),runif(1,0,1),runif(1,0,1)),lambda = 0, alpha = 1.5, max_iter = 400, threshold = 0.001)
outputcross <- Predict_GD(X = DataCross[,-2], theta = res$theta, lambda = 0, compare = T, Y = DataCross[,2], algorithm = "glm")
outputest <- Predict_GD(X = Testset, theta = res$theta, lambda = 0, compare = F, Y = NULL, algorithm = "glm")
Titanic <- data.frame(Outputtest[,1],outputest)
colnames(Titanic) <- c("Passenger Id", "Survived")
write.table(x = Titanic, file = "Titanic.csv", sep = ",", row.names = F, col.names = c("PassengerId", "Survived"))


###GLM
model <- glm(formula = Datatrain2[,2] ~ ., family = binomial(link = "logit"), data = Datatrain2[,-c(1,2)])
out <- predict.glm(object = model, newdata = Datatest2[,-c(1,2)], type = "response")
Titanicglm <- data.frame(Outputtest[,1], out >= 0.5)
write.table(x = Titanicglm, file = "Titanic2.csv", sep = ",", row.names = F, col.names = c("PassengerId", "Survived"))

###gradDescent package











