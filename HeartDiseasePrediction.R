library(tables)
library(caTools)
library(class)
library(ROCR)
library(pROC)
library(ROSE)
library(PRROC)
library(randomForest)
library(caret)
library(e1071)
library(MLmetrics)
library(dplyr)
library(rpart.plot)

#read in data
heart <- read.csv(file = "./framingham.csv",encoding="UTF-8")

#show dimension of the dataset
dim(heart)

#first 10 rows of the dataset
head(heart, 10)

#show summary
str(heart)

#convert to appropriate data types
heart$male <- as.factor(heart$male)
heart$education <- as.factor(heart$education)
heart$currentSmoker <- as.factor(heart$currentSmoker)
heart$BPMeds <- as.factor(heart$BPMeds)
heart$prevalentStroke <- as.factor(heart$prevalentStroke)
heart$prevalentHyp <- as.factor(heart$prevalentHyp)
heart$diabetes <- as.factor(heart$diabetes)
heart$TenYearCHD <- as.factor(heart$TenYearCHD)
levels(heart$TenYearCHD) <- 
  make.names(levels(factor(heart$TenYearCHD)))

#drop irrelevant column
heart <- heart[-3]

#check for missing records
sapply(heart, function(x) sum(is.na (x)))

#return the dataset with listwise deletion of missing value
heart.complete <- na.omit(heart)
sapply(heart, function(x) sum(is.na (x)))

#copy the complete dataset to the normalize dataset
heart.norm <- heart.complete

#normalize the dataset using z-score normalization
heart.norm[,c(2,4,9:14)] <- as.data.frame(
  apply(heart.norm[,c(2,4,9:14)], 2, function(x) scale(x)))

#split the dataset in to train and test subsets
set.seed(2020)
split = sample.split(heart.norm, SplitRatio = 0.8)
train = subset(heart.norm, split == TRUE)
test = subset(heart.norm, split == FALSE)

#check for proportion of each target variables
prop.table(table(train$TenYearCHD))

#train the model using random forest with feature selection
control <- trainControl(method = "cv", 
                        number = 10,
                        sampling = "down",
                        verboseIter = FALSE,
                        classProbs = TRUE, 
)

HeartRF <- train(TenYearCHD ~ age + sysBP + diaBP + totChol + BMI
                  + glucose + heartRate + cigsPerDay + prevalentHyp + male, 
                  data = train, 
                  method = 'rf',
                  metric = "Kappa",
                  preProcess = c("scale", "center"), 
                  trControl = control
)

#train the model using K-Nearest Neighbor with feature selection
HeartKNN <- train(TenYearCHD ~ age + sysBP + diaBP + totChol + BMI
                   + glucose + cigsPerDay + prevalentHyp + male, 
                   data = train, 
                   method = 'rf',
                   metric = "Kappa",
                   preProcess = c("scale", "center"), 
                   trControl = control
)

#train the model using Decision Tree with feature selection
HeartDT <- train(TenYearCHD ~ age + sysBP + diaBP + totChol + BMI
                  + glucose + heartRate + cigsPerDay + prevalentHyp + male, 
                  data = train, 
                  method = 'rpart',
                  metric = "Kappa",
                  preProcess = c("scale", "center"), 
                  trControl = control
)

#predict usnig Random Forest on the test subset
RFPredict <- data.frame(actual = test$TenYearCHD, 
                        predict(HeartRF, 
                                type="prob", 
                                newdata = test))

#calculate the Area Under the Precision-Recall Curve
plot(pr.curve(scores.class0 = RFPredict$X1, 
              scores.class1 = RFPredict$X0, curve = T))

#predict with the probability threshold set at 0.5
RFPredict$predict <- ifelse(RFPredict$X1 > 0.5, "X1", "X0")

confusionMatrix(table(RFPredict$predict, test$TenYearCHD),
                positive = "X1", mode = "prec_recall")


#predict using K-Nearest Neighbor on the test subset
KNNPredict <- data.frame(actual = test$TenYearCHD, 
                         predict(HeartKNN, 
                                 type="prob", 
                                 newdata = test))

  
#calculate the Area Under the Precision Recall curve
plot(pr.curve(scores.class0 = KNNPredict$X1, 
                scores.class1 = KNNPredict$X0, curve = T))
  
#predict with the probability threshold set at 0.5
KNNPredict$predict <- ifelse(KNNPredict$X1 > 0.5, "X1", "X0")

confusionMatrix(table(KNNPredict$predict, test$TenYearCHD), 
                positive = "X1", mode = "prec_recall")

#Predict on test subset using the Decision Tree
DTPredict <- data.frame(actual = test$TenYearCHD, 
                        predict(HeartDT, 
                                type="prob", 
                                newdata = test))

#Calculate the Area Under the Precision Recall curve
plot(pr.curve(scores.class0 = DTPredict$X1, 
              scores.class1 = DTPredict$X0, curve = T))

#predict with the probability threshold set at 0.5
DTPredict$predict <- ifelse(DTPredict$X1 > 0.5, "X1", "X0")

confusionMatrix(table(DTPredict$predict, test$TenYearCHD), 
                positive = "X1", mode = "prec_recall")


