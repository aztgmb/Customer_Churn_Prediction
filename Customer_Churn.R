###installing packages and libraries
install.packages("randomForest")
install.packages("unbalanced")
install.packages("sqldf")
install.packages("ROCR")
install.packages("caret")
install.packages("AUC")
install.packages("mlbench")
install.packages("splitstackshape")
install.packages("mlr")
install.packages("corrplot")
install.packages("gridExtra")
install.packages("ggthemes")
install.packages("party")
install.packages("e1071")

library(pROC)
library(readr)
library(e1071)
library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(party)
library(unbalanced)
library(sqldf)
library(MASS)
library(ROCR)
library(caret)
library(AUC)
library(mlbench)
library(splitstackshape)
library(mlr)

###load data
data <- read.csv('https://raw.githubusercontent.com/aztgmb/Customer_Churn_Random_Forest/master/Customer-Churn.csv')
head(data)
str(data)

### make categorical variables factors if necessary
sapply(data, function(x) sum(is.na(x)))
data <- data[complete.cases(data),]

unique(data['OnlineSecurity'])
f2_col <- c(10:15)
for (i in 1:ncol(data[, f2_col])) {
  data[, f2_col][, i] <- as.factor(mapvalues(data[, f2_col][, i], from = c("No internet service"), to = c("No")))
}

data$MultipleLines <- as.factor(mapvalues(data$MultipleLines, from = c("No phone service"), to = c("No")))
data$SeniorCitizen <- as.factor(mapvalues(data$SeniorCitizen, from = c("0", "1"), to = c("No", "Yes")))

#may be create even folds with other intervals
tenure_folds <- function(tenure){
  if(tenure >= 0 & tenure <= 12){
    return('0 - 12 Month')
  }else if(tenure > 12 & tenure <= 24){
    return('12 - 24 Month')
  }else if(tenure > 24 & tenure <= 36){
    return('24 - 36 Month')
  }else if(tenure > 36 & tenure <= 60){
    return('36 - 60 Month')
  }else if(tenure > 60){
    return('> 60 Month')
  }
}

data$tenure_folds1 <- sapply(data$tenure, tenure_folds)
data$tenure_folds1 <- as.factor(data$tenure_folds1)

### EDA
hist(data$MonthlyCharges)
hist(data$TotalCharges)
hist(data$tenure)
boxplot(data$MonthlyCharges)
boxplot(data$TotalCharges)
summary(data$MonthlyCharges)
summary(data$TotalCharges)
summary(data$Churn)
summary(data$tenure_folds1)
summary(pa)
summary(data)

set.seed(100)

### split dataset 
train.index <- createDataPartition(data$Churn, p = .7, list = FALSE)
train <- data[ train.index,]
test  <- data[-train.index,]
trainLabel <- data$Churn[train.index]
testLabel <- data$Churn[-train.index]


summary(train$Churn)
summary(test$Churn)

### drop unneccessary columns
train$customerID <- NULL
train$tenure <- NULL
#train$TotalCharges<-NULL
test$customerID <- NULL
test$tenure <- NULL
#test$TotalCharges<-NULL

str(train)
str(test)

cols <- c(1:18, 20)
x1<-train[,cols]
y1<-train[,19]

### tune parameters on train data set
mtry_opt <- tuneRF(x1, y1, stepFactor=1.5, improve=1e-5, ntree=1000)
print(mtry_opt)

random_forest <- randomForest(x = x1,y = y1, ntree = 1000, mtry = 4, sampsize=c(45,55),strata=y1,metric="Accuracy",replace=T)
random_forest1 <- randomForest(x = x1,y = y1, ntree = 1000, mtry = 4,metric="Accuracy",replace=T)


### make prediction on test data set
print(random_forest)
print(random_forest1)
plot(random_forest)
table(testLabel, predict(object = random_forest, newdata = test))

table(testLabel, predict(object = random_forest1, newdata = test))

varImpPlot(random_forest)

### plot ROC
rf_prediction <- predict(random_forest, test, type = "prob")
ROC_rf <- roc(test$Churn, rf_prediction[,2])
plot(ROC_rf, col = "red", main = "ROC For Random Forest")
print(ROC_rf)

rf_prediction1 <- predict(random_forest1, test, type = "prob")
ROC_rf1 <- roc(test$Churn, rf_prediction1[,2])
plot(ROC_rf1, col = "red", main = "ROC For Random Forest")
print(ROC_rf1)