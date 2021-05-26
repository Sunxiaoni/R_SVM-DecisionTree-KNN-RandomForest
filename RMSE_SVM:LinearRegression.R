#2. Problem
#• Compare the SVM regression with a linear regression.
#• Visualize your results.
#• For SVM and linear model, Compute theRMSE, root meansquare error:

# Import data
data<- read.csv("/Users/xiaoni/Downloads/exdata.csv")

######################## Fit linear model #######################
model=lm(Y~X,data)
plot(data, main ="Scatter Plot")
abline(model)

## Scatter plot displaying actual values and predicted values 
#Scatter Plot 
plot (data, pch=16)
#Predict Y using Linear Model
predY <- predict (model, data)
#Overlay Predictions on Scatter Plot
points (data$X, predY, col = "blue", pch=16)

## RMSE Calculation for linear model
library(Metrics)
rmse(data$Y, predict(model, data))

#######################    SVM     #######################

## Fit SVR model and visualize using scatter plot
#Load Library
library(e1071)
#Scatter Plot
plot(data)
#Regression with SVM
modelsvm = svm(Y~X,data)
#Predict using SVM regression
predYsvm = predict(modelsvm, data)
#Overlay SVM Predictions on Scatter Plot
points(data$X, predYsvm, col = "red", pch=16)

## RMSE for SVR Model
#Calculate RMSE
library(Metrics)
RMSEsvm=rmse(data$Y, predYsvm)
RMSEsvm
