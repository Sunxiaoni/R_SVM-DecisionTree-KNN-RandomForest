#1. Problem
#• Choose a Problem fromAnalytic 2or from an internetrepositories.
#• Compute a SVM and a model from Analytic 1 or 2. Compare both analysis. Compare the confusion matrices.Differences?
#• Present your results.

#Comparing classification techniques on iris dataset
#Loading dataset and libraries
library(ggplot2)
library(rpart)
library(rpart.plot)
library(gmodels)
library(e1071)
library(gridExtra)
library(randomForest)
data(iris)
summary(iris)

# Data Pre-processing
temp = as.data.frame(scale(iris[,1:4]))
temp$Species = iris$Species
summary(temp)

# Creating training and testing dataset
smp_size =  100
set.seed(123)
train_ind = sample(seq_len(nrow(temp)), size = smp_size)
train = temp[train_ind, ]
test = temp[-train_ind, ]

#################### Classification Techniques #################
#################### 1. Decision Tree ####################
model.rpart = rpart(Species ~ . ,data =train)
preds.rpart = predict(model.rpart,newdata = test,type = "class")

#CrossTable: Confusion Matrix
CrossTable(test$Species,preds.rpart,chisq = F,prop.r = F,prop.c = F,prop.t = F,prop.chisq = F)

# Accuracy of Decision trees
((16+20+12)/nrow(test))

#################### 2. k-Nearest Neighbours ####################
library(class)
cl = train$Species
set.seed(1234)
preds.knn = knn(train[,1:4],test[,1:4],cl,k=3)

#CrossTable: Confusion Matrix
CrossTable(preds.knn,test$Species,chisq = F,prop.r = F,prop.c = F,prop.t = F,prop.chisq = F)
# Accuracy of k-Nearest Neighbours
((16+18+12)/nrow(test))

#################### 3. Support Vector Machine(SVM) ####################
model.svm = svm(Species ~ . ,data = train)
preds.svm = predict(model.svm,newdata = test)

#CrossTable: Confusion Matrix
CrossTable(preds.svm,test$Species,chisq = F,prop.r = F,prop.c = F,prop.t = F,prop.chisq = F)

# Accuracy of SVM
((16+20+13)/nrow(test))

#################### 4.Random Forest ####################
set.seed(100)
model.rf = randomForest(Species ~ .,data = train)
preds.rf = predict(model.rf,newdata = test)
#CrossTable: Confusion Matrix
CrossTable(preds.rf,test$Species,chisq = F,prop.r = F,prop.c = F,prop.t = F,prop.chisq = F)

# Accuracy of Random Forest
((16+19+13)/nrow(test))

#################### Accuracy comparison ####################
models = data.frame(Technique = c("Decision Tree","kNN","SVM","Random Forest"),Accuracy_Percentage = c(0.96,0.92,0.98,0.96))
models


