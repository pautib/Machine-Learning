Datatest <- read.csv2(file = "/Users/entimos/Desktop/test.csv", header = T, sep = ",", na.strings = c(""))
Datatrain <- read.csv2(file = "/Users/entimos/Desktop/train.csv", header = T, sep = ",", na.strings = c(""))

Outputtrain <- Datatrain[,2]
Datatrain <- Datatrain[,-2]
Datos <- rbind(Datatrain,Datatest)

sapply(Datos, function(x) sum(is.na(x)))


Datos[,3] <- as.character(x = Datos[,3])
Datos$Age <- as.numeric(Datos$Age)
Datos$Fare <- as.numeric(Datos$Fare)

#####Detect people title
chars <- gsub('(.*, )|(\\..*)', '', Datos[,3])
table(chars)
#Mr = 1, Mrs = Mme = 2, Miss = Mlle = Ms = 3, Master = 4, Dr = 5,
#Capt = Col = Don = Jonkheer = Lady = Major = Rev = Sir = the Countess = 6

chars[chars == c("Mme")] <- "Mrs"
chars[chars %in% c("Mlle","Ms")] <- "Miss"
chars[chars %in% c("Capt", "Col", "Don","Dona", "Jonkheer", "Lady", "Major", "Rev", "Sir", "the Countess")] <- "Rare"

##Add variable $Mother


#We say and individual is a mother if it's a female, age > = 18 , parch > 0 doesn't have the title Miss !
Datos$Mother <- 0 ## 0 = Not mother
Datos$Mother[Datos$Sex == "female" & Datos$Age >= 18 & Datos$Name != "Miss" & Datos$Parch > 0] <- 1 # 1= Mother

#We change the names with numbers
table(chars)
chars[chars == "Mr"] <- 1
chars[chars == "Mrs"] <- 2
chars[chars == "Miss"] <- 3
chars[chars == "Master"] <- 4
chars[chars == "Dr"] <- 5
chars[chars == "Rare"] <- 6

Datos[,3] <- as.numeric(chars)

#####Family size
Datos$FamilySize <- Datos$SibSp + Datos$Parch + 1 


#####Missing values
library("ggplot2")
library("ggthemes")
library("scales")

##Missing Fare

Datos[1044,]
aux <- Datos[which(Datos$Pclass == 3 & Datos$Embarked == "S" & Datos$Sex =="male" & Datos$FamilySize ==1 & Datos$Age > 60 ),]

Plot <- ggplot(main = "Density plot", xlab = "Fare", ylab = "Density")
Plot <- Plot + geom_density(mapping = aes(x = aux$Fare))
Plot <- Plot + geom_vline(aes(xintercept=mean(aux$Fare,na.rm=T)),colour='red', linetype='dashed', lwd=2)

Datos[1044,"Fare"] <- 187

##Missing Embarked
Datos[is.na(Datos$Embarked),]

Aux <- Datos[which(Datos$Sex == "female" & Datos$FamilySize == 1 & Datos$Cabin == "B"),]

Plot <- ggplot(main = "Boxplot", xlab = "Embarked", ylab = "Fare")
Plot <- Plot + geom_boxplot(mapping = aes(x = Aux$Embarked , y = Aux$Fare, fill = factor(Aux$Pclass)))
Plot <- Plot + geom_hline(aes(yintercept=227),colour='red', linetype='dashed', lwd=2)

Datos[is.na(Datos$Embarked),"Embarked" ] <- "S"

## Missing Age
sapply(Datos, function(x) sum(is.na(x)))

levels(Datos$Sex) <- c(0,1) # female = 0, male = 1
levels(Datos$Embarked) <- c(0,1,2) # C = 0, Q = 1, S = 2

Agetrain <- Datos[!is.na(Datos$Age), c("Pclass","Name","Sex","SibSp","Parch","Fare","Embarked")]

Agetrain <- apply(X = Agetrain, MARGIN = 2, function(x) as.numeric(x))
Agetrain <- cbind(Datos[!is.na(Datos$Age),"Age"], scale(x = Agetrain, center = T, scale = T))

#With LM
Agemodel <- lm(formula = Agetrain[,1]~.,data = data.frame(Agetrain[,-1]) )
summary(Agemodel)

Agetest <- Datos[is.na(Datos$Age), c("Pclass","Name","Sex","SibSp","Parch","Fare","Embarked")]
Agetest <- apply(X = Agetest, MARGIN = 2, function(x) as.numeric(x))
Agetest <-data.frame(scale(x = Agetest, center = T, scale = T))

resAge <- predict.lm(object = Agemodel, newdata = Agetest, type = "response")

#With LinearReg_GD
theta <- c(0, runif(1,0,1),runif(1,0,1),runif(1,0,1),runif(1,0,1),runif(1,0,1),runif(1,0,1),runif(1,0,1))

Agemodel2 <- LinearReg_GD(X = data.frame(1,Agetrain[,-1]), Y = Agetrain[,1], theta = theta, lambda = 0, alpha= 0.1, max_iter = 500, threshold = 0.001 )
resAge <- Predict_GD(X = data.frame(1,Agetest), theta = Agemodel2$theta, lambda = 0, compare = F, algorithm = "lm")
Datos$Age[is.na(Datos$Age)] <- as.numeric(resAge) 
Datos$Age[is.na(Datos$Age)] <- mean(x = Datos$Age, na.rm = T)

# Add variable Child
#We say an individual is a child if it has age < 18.
Datos$Child <- as.numeric(Datos$Age < 18)


sapply(Datos, function(x) sum(is.na(x)))

Datos <- Datos[,-c(1,8,10)]


####Train model with NN
TrainingSet <- Datos[1:891,]
TrainingSet <- apply(X = TrainingSet, MARGIN = 2, function(x) as.numeric(x))
TrainingSet <- data.frame(scale(x = TrainingSet, center = T, scale = T))
Model <- NeuralNetwork(X = TrainingSet, Y = Outputtrain, L = 4, sl = 5, lambda = 0, alpha = 1.6, max_iter = 2000, threshold = 0.001)

###Predict
TestSet <- Datos[892:1309,]
TestSet <- apply(X = TestSet, MARGIN = 2, function(x) as.numeric(x))
TestSet <- data.frame(scale(x = TestSet, center = T, scale = T))

Res <- Predict_GD(X = TestSet, theta = Model$Theta, lambda = 0, compare = F, Y = NULL, algorithm = "nnet", L = 4)
Titanic <- data.frame(892:1309, Res)
write.table(x = Titanic, file = "Titanic.csv", sep = ",", row.names = F, col.names = c("PassengerId", "Survived"))


### Usando diferentes alpha
alpha <- seq(from =0.01, to = 2, by = 0.2)

p <- qplot(main = "TrainError", xlab = "lambda", ylab = "J")

errtrain <- sapply(X = alpha, function(x){
  cv_index <- sample(x = 1:891, size = 179, replace = F)
  mtrain <- setdiff(1:891,cv_index)
  TrainingSet <- Datos[mtrain,]
  TrainingSet <- apply(X = TrainingSet, MARGIN = 2, function(x) as.numeric(x))
  TrainingSet <- data.frame(scale(x = TrainingSet, center = T, scale = T))
  restrain <- NeuralNetwork(X = TrainingSet, Y = Outputtrain[mtrain], L = 5, sl = 3, lambda = 0, alpha = x, max_iter = 2000, threshold = 0.001)
  
  CrossSet <- Datos[cv_index,]
  CrossSet <- apply(X = CrossSet, MARGIN = 2, function(x) as.numeric(x))
  CrossSet <- data.frame(scale(x = CrossSet, center = T, scale = T))
  A <- ForwardProp(X = as.matrix(data.frame(1,CrossSet)), L = 5, Theta = restrain[[2]])
  
  rescross <- J_NN(H = A[[5]], Y = Outputtrain[cv_index], theta = unlist(restrain[[2]]), lambda = 0 )
  return(c(restrain[[1]],rescross))
} )

simplify2array(errtrain)

p <- p + geom_point(mapping = aes(x = lambda , y = as.vector(errtrain[1,]))) + 
    geom_point(mapping = aes(x = lambda , y = as.vector(errtrain[2,])))


###NeuralNet Function

library("neuralnet")

n <- names(TrainingSet)
f <- as.formula(paste("Outputtrain ~", paste(n[!n %in% "Outputtrain"], collapse = " + ")))
nn <- neuralnet(formula = f, data = TrainingSet, hidden = c(3,3,3), threshold = 0.005, stepmax = 2000, rep = 500 , learningrate = 0.1 , err.fct = 'ce', algorithm = 'backprop', linear.output = F, lifesign = 'full')

###GLM
Model2 <- glm(formula = Outputtrain ~ ., family = binomial(link = "logit"), data = TrainingSet[,-1])
out <- predict.glm(object = Model2, newdata = TestSet[,-1], type = "response")

Plot <- ggplot(main = "Density plot", xlab = "Probs", ylab = "Density")
Plot <- Plot + geom_density(mapping = aes(x = out))


Titanicglm <- data.frame(892:1309, as.numeric(out >= 0.70))
write.table(x = Titanicglm, file = "Titanic2.csv", sep = ",", row.names = F, col.names = c("PassengerId", "Survived"))


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

###svm function
Model <- svm(formula = Outputtrain~., data = TrainingSet, kernel = "polynomial", degree = 4)
error <- sqrt(mean((as.numeric(predict(Model, TrainingSet) > 0.5) - Outputtrain)^2))
Res <- as.numeric(predict(Model, TestSet) > 0.5)
Titanic <- data.frame(892:1309, Res)
write.table(x = Titanic, file = "Titanic.csv", sep = ",", row.names = F, col.names = c("PassengerId", "Survived"))
