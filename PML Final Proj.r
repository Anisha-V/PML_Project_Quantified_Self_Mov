###Installing required packages
install.packages("randomForest")
install.packages("rpart")
install.packages("rpart.plot")

###Loading libraries
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)

###Reading in the datasets

modeldata<-read.csv("pml-training.csv",na.strings = c("NA", ""))
modeldataTest<-read.csv("pml-testing.csv",na.strings = c("NA", ""))

###Partitioning the Training dataset into 70% training and 30% testing sets
inTrain <- createDataPartition(y=modeldata$classe, p=0.7, list=FALSE)
Training2 <- modeldata[inTrain, ]
Testing2 <- modeldata[-inTrain, ]

###Cleaning the dataset to remove unwanted variables and thereby improving computing efficiency
summary(modeldata)
summary(modeldata$classe)

###Removing variables which has missing values for all the rows
rm_var = sapply(Training2, function(x) {sum(is.na(x))})
table(rm_var)
rm_columns = names(rm_var[rm_var==13436]) ###100 of the columns have 13436 rows with missing values - this was the result of the table function
Training2 = Training2[, !names(Training2) %in% rm_columns]
str(Training2)

###Removing other unwanted numeric variables such as timestamp

Training2<-Training2[c(-1,-3,-4,-5)]

###Predicting using ML packages
###Decision Trees
set.seed(333)
modelfitDT <- rpart(classe ~ ., data=Training2, method="class")
rpart.plot(modelfitDT)
predictDT<- predict(modelfitDT,Testing2,type = "class")
confusionMatrix(predictDT, Testing2$classe)

###Random Forests - with cross validation
modelfitRF<-train(Training2$classe ~ ., method="rf", trControl=trainControl(method = "cv", number = 4), data=Training2)
print(modelfitRF, digits=3)
predictRF<- predict(modelfitRF,newdata = Testing2)
confusionMatrix(predictRF, Testing2$classe)

###Since Random forest algorithm is returning a higher accuracy, I'll  be using that to predict the final outcome from the pml-testing dataset
predictFinal<- predict(modelfitRF,newdata = modeldataTest)

###Script for creating text files for submission
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predictFinal)








