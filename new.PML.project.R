rm(list = ls())
training <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
testing <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")

#processing <- c(12:36, 50:59, 69:83, 87:101, 103:112, 125:139, 141:150)
#processing <- c(processing, 1:7)
x1 <- (training == "")
y1 <- apply(x1, 2, sum)
y1 <- y1/19622
colindex1 <- (y1 > 0.5)
colindex1 <- which(colindex1 == TRUE)
x <- is.na(training)
y <- apply(x, 2, sum)
y <- y/19622
colindex <- (y > 0.5)
colindex <- which(colindex == TRUE)
col1 <- union(colindex, colindex1)


training <- training[, -col1]
testing <- testing[, - col1]

training <- training[, -c(1:7)]
testing <- testing[, -c(1:7)]

set.seed(1313)
library(caret)
trainIndex <-  createDataPartition(training$classe, p = 0.8, list = FALSE)
train1 <- training[trainIndex, ]
test1 <- training[-trainIndex, ]

#using trees
controltrain <- trainControl(method = "cv", number = 5)
model1 <- train(classe ~ ., data = train1, method = "rpart", trControl = controltrain)
predict1 <- predict(model1, test1[,-53])
accuracy1 <- confusionMatrix(predict1, test1$classe)

#random forests 
controltrain <- trainControl(method = "cv", number = 5)
model2 <- train(classe ~ ., data = train1, method = "rf", trControl = controltrain, ntree = 16)
predicttrain <- predict(model2, train1[,-53])
predict2 <- predict(model2, test1[,-53])
accuracy2 <- confusionMatrix(predict2, test1$classe)

#gradient boosting 
controltrain <- trainControl(method = "repeatedcv", number = 5, repeats = 1)
#worked
#model3 <- train(classe ~ ., data = train1, method = "gbm", verbose = TRUE)
model3 <- train(classe ~ ., data = train1, method = "gbm", trControl = controltrain, verbose = TRUE)
predict3 <- predict(model3, test1[,-53])
accuracy3 <- confusionMatrix(predict3, test1$classe)





