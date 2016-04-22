##Titanic Kaggle

library(ggplot2)
library(randomForest)
library(dplyr)
library(ISLR)
library(stats)
library(corrplot)
library(scales)
library(gridExtra)
library(lattice)
library(questionr)
library(mice)
library(nnet)
library(ROCR)
library(tree)
library(rpart)
library(pROC)
library(party)
library(e1071)


titanic <- read.csv("~/ensiie_S4/projet_MOST/train.csv")
test<-read.csv("~/ensiie_S4/projet_MOST/test.csv")
gendermodel<-read.csv("~/ensiie_S4/projet_MOST/gendermodel.csv")

