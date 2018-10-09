#  SVM Letter Recognisingg R Programming ########
#####################################################################################

# Major Steps to Follow 

#  1. Business Understanding
#  2. Data Understanding
#  3. Data Preparation
#  4. Model Building 
#     4.1 Linear kernel
#     4.2 RBF Kernel
# 5 Hyperparameter tuning and cross validation
##################################################################

# 1. Business Understanding: 

#The objective is to identify each of a large number of black-and-white
#rectangular pixel displays as one of the 0-9 digit.

#####################################################################################

# 2. Data Understanding: 
# Number of Instances: 60,000 (Train data)
# Number of Instances: 10,000 (Test data)

# Number of Attributes: 785 

#3. Data Preparation: 

###########========================================##################################
##Loading Neccessary libraries

library(kernlab)
library(readr)
library(caret)
library(caTools)
library(dplyr)
library(ggplot2)
library(gridExtra)

# Data Loading

mnist_train <- read.csv("mnist_train.csv", header = F, stringsAsFactors = F)
mnist_test <- read.csv("mnist_test.csv", header=F, stringsAsFactors = F)

#Making  target class to factor

mnist_test$V1 <-factor(mnist_test$V1)
mnist_train$V1 <-factor(mnist_train$V1)

#Understanding Dimensions
dim(mnist_train)

#Structure of the dataset
str(mnist_train)

#printing first few rows
head(mnist_train)

#Exploring the data
summary(mnist_train)

#checking missing value
sapply(mnist_train, function(x) sum(is.na(x)))

# Data set is big for my labtop , so initally choose 15% of data for Training  purpose.
# Later this may need to improve by having larger Training  data. (if any overfitting found ??)
set.seed(1)
train.indices = sample(1:nrow(mnist_train), 0.15*nrow(mnist_train))
mnist_train15 = mnist_train[train.indices, ]

## Check the column name of both test and Training datasets. Both should match
x<-colnames(mnist_test)
length(x)
y<-colnames(mnist_train15)
length(y)
setdiff(x,y)
setdiff(y,x)

#--------------------------------------------------------------------
# 4.1 Linear model 
#####################################################################

#Using Linear 
Lnr_Model <- ksvm(V1~ ., data = mnist_train15, scale = FALSE, kernel = "vanilladot")
linear_ev<- predict(Lnr_Model, mnist_test)

#confusion matrix - Linear 
confusionMatrix(linear_ev,mnist_test$V1)


#Confusion Matrix and Statistics
#   Accuracy : 0.9166         
#   95% CI : (0.911, 0.9219)

#Statistics by Class:
#                         Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
# Sensitivity            0.9827   0.9850   0.9147   0.8901   0.9348   0.8722   0.9520   0.9134   0.8501   0.8593
# Specificity            0.9936   0.9935   0.9893   0.9875   0.9880   0.9885   0.9940   0.9919   0.9918   0.9893


#--------------------------------------------------------------------
# Linear model with Cost = 5
#####################################################################

#Using Linear 
Lnr_Model_c5 <- ksvm(V1~ ., data = mnist_train15, scale = FALSE, C=5)
linear_ev_c5<- predict(Lnr_Model_c5, mnist_test)

#confusion matrix - Linear 
confusionMatrix(linear_ev_c5,mnist_test$V1)


#Confusion Matrix and Statistics
#  Accuracy : 0.966           
#  95% CI : (0.9623, 0.9695)

#Statistics by Class:

#                       Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
#Sensitivity            0.9898   0.9894   0.9632   0.9624   0.9644   0.9428   0.9781   0.9572   0.9497   0.9584
#Specificity            0.9965   0.9974   0.9958   0.9957   0.9965   0.9960   0.9972   0.9965   0.9959   0.9948

## Looks like it may be ""overfitting scenario""" ? becasue training data set is only 15% ? 
# So Better to build the RBF model then .. 


#--------------------------------------------------------------------
# 4.2 Kernal   model 
#####################################################################


#Using RBF Kernel
model_krb <- ksvm(V1~ ., data = mnist_train15, scale = FALSE, kernel = "rbfdot")
eval_krb<- predict(model_krb, mnist_test)

#confusion matrix - RBF Kernel
confusionMatrix(eval_krb,mnist_test$V1)

#Overall Statistics

# Accuracy : 0.9587          
# 95% CI : (0.9546, 0.9625)
# P-Value [Acc > NIR] : < 2.2e-16 (SIGMA Value)

#Statistics by Class:

#                 Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
#Sensitivity      0.9878   0.9894   0.9496   0.9535   0.9603   0.9372   0.9718   0.9397   0.9476   0.9455
#Specificity      0.9962   0.9971   0.9942   0.9957   0.9956   0.9948   0.9962   0.9960   0.9947   0.9937


#####################################################################
#Hyperparameter tuning and Cross Validation - Non-Linear - SVM 
######################################################################

# We will use the train function given data sent to form crossvalidation

trainControl <- trainControl(method="cv", number=5)
# Number - Number of folds 
# Method - cross validation

metric <- "Accuracy"

set.seed(100)

# Making grid of "sigma" and C values. 
grid <- expand.grid(.sigma=seq(0.01, 0.05, by=0.01), .C=seq(1, 5, by=1))

# Performing 5-fold cross validation
fit.svm_radial <- train(V1~., data=mnist_train15, method="svmRadial", metric=metric,tuneGrid=grid, trControl=trainControl)

# Printing cross validation result
print(fit.svm_radial)

# Plotting model results
plot(fit.svm_radial)

######################################################################
# Checking overfitting - Non-Linear - SVM
######################################################################

# Validating the model results on mnist_test data
evaluate_non_linear<- predict(fit.svm_radial, mnist_test)
confusionMatrix(evaluate_non_linear, mnist_test$V1)



