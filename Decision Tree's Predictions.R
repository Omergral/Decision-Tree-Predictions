#cleaning the data enviroment
rm(list = ls())
options(scipen = 999)

#lodaing the data
setwd("C:/Users/Omer Gralnik/Desktop/Business Analysis/Datasets")
getwd()
titanic_hw <-  read.csv("titanic.csv", stringsAsFactors = F)

#cheking the data's summary and structure
summary(titanic_hw)
str(titanic_hw)

#transforming variables into factors
titanic_hw$Pclass <-  as.factor(titanic_hw$Pclass)
titanic_hw$Sex <-  as.factor(titanic_hw$Sex)
titanic_hw$Embarked <-  as.factor(titanic_hw$Embarked)

#cheking the new data
summary(titanic_hw)
str(titanic_hw)


#creating test and train data sets
set.seed(123)
sampleSize <-  floor( 0.7 * nrow(titanic_hw) )


train.index <-  sample(nrow(titanic_hw), size = sampleSize, replace = F ) 

train <-  titanic_hw[train.index, ]
test <-  titanic_hw[-train.index, ]


#calculation of the survival rate
survival_Rate <-  sum(train$Survived) / nrow(train)
survival_Rate

library(data.table)

#coverting the train data frame into data table
train <-  data.table(train)

#creation of N variable
N <-  nrow(train)

#how many objects for each type to class
nfirstclass <-  nrow(subset(train, Pclass == "1") )
nsecondclass<-  nrow(subset(train, Pclass == "2") )
nthirdclass<-  nrow(subset(train, Pclass == "3") )

#calculation with data.table
train.SurvivalByClass <-  train[ ,.( survivalnum = sum(Survived),
                                     survivalRate = sum(Survived)/(.N)
), by = Pclass]

#creating a simple prediction by class
test$PredSurvivalByClass <-  0
test$PredSurvivalByClass[test$Pclass =="1"] <-  1

#checking the accuracy
Accuracy_simple <-  sum(test$PredSurvivalByClass == test$Survived )/nrow(test)
Accuracy_simple

#Creating family size variable
train$family_size <- train$SibSp+train$Parch+1
test$family_size <- test$SibSp+test$Parch+1


library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#validation of train's names
names(train)

#building the prediction model
tree <-  rpart(Survived ~ Sex + Age+SibSp+Parch+Fare+Embarked+family_size,
               data = train, method = "class",parms = list(split = "information"))

fancyRpartPlot(tree)

#variable's importance
tree$variable.importance["family_size"]
plot( factor(names(tree$variable.importance)), tree$variable.importance,
      main = "Variable's Importance", xlab = 'Variable', ylab = 'Importance')

#creation of predict on the test dataset
test$PredSurvival_tree = predict(tree, newdata = test, type = "class")

#Checking the accuracy of the prediction model
AccuracyTree = sum(test$PredSurvival_tree == test$Survived)/nrow(test)
AccuracyTree


#Difference between the simple model and the tree model
AccuracyTree-Accuracy_simple


