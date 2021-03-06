# current: change weight of text and category variables
# predict using 90% category, and 10% text variables
# pick top x variables using importance(rf) / varImpPlot(rf)
# had higher AUC than random forest on all vars with 30% split on train data
# But, performed worse on Kaggle than plain random forest on all vars
# model was overfit, remove some predictors?

#clear session
rm(list = ls())

library(ROCR)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
library(e1071)
library(caTools)

#Prep data for Random Forest. Read variables as factors. Make training & testing levels same for productline factor
eBayTrain2 = read.csv("eBayiPadTrain.csv") #without stringsAsFactors=FALSE
eBayTrain2$sold=as.factor(eBayTrain2$sold)
eBayTrain2$biddable=as.factor(eBayTrain2$biddable)

#Get all the text variables
source("KCompetition_TextData.R") #sparseThreshold =.99
DescriptionWordsTrain2=DescriptionWordsTrain
DescriptionWordsTrain2$sold=NULL
colnames(DescriptionWordsTrain2)[13]="condition2"
eBayTrain3=cbind(eBayTrain2,DescriptionWordsTrain2)

#Split Training data: eBayTrain3 into Traintrain & Traintest
## set.seed(123)
# spl = sample.split(eBayTrain3$sold,SplitRatio = 0.7)
# eBayTraintrain = subset(eBayTrain3, spl==TRUE)
# eBayTraintest = subset(eBayTrain3, spl==FALSE)
# eBayTraintrain$description = NULL
# eBayTraintest$description = NULL
# eBayTest=eBayTraintest
## end training

eBayTraintrain = eBayTrain3
eBayTraintrain$description=NULL
#create dataframe of category vars and randomForest
trainDiscreteVars = eBayTraintrain[,1:10]
rf1 = randomForest(sold~biddable+startprice,data=trainDiscreteVars)

#create dataframe of text vars and randomForest
trainTextVars = eBayTraintrain[,-1:-8]
rf2 = randomForest(sold~ X100+scratch+ipad+screen+screen+use+condit,data=trainTextVars)

#Compute test values & submit
eBayTest2 = read.csv("eBayiPadTest.csv") #without stringsAsFactors=FALSE
eBayTest2$biddable=as.factor(eBayTest2$biddable)
levels(eBayTest2$productline) <- levels(eBayTrain2$productline) #train has more levels than test

DescriptionWordsTest2=DescriptionWordsTest
colnames(DescriptionWordsTest2)[13]="condition2"
eBayTest=cbind(eBayTest2,DescriptionWordsTest2)

Predrf1 = predict(rf1,newdata=eBayTest, type="prob")[,2]
Predrf2 = predict(rf2,newdata=eBayTest, type="prob")[,2]

# validation start - used for testing when splitting training
# Predrf1 = predict(rf1,newdata=eBayTraintest, type="prob")[,2]
# Predrf2 = predict(rf2,newdata=eBayTraintest, type="prob")[,2]
# ROCRrf = prediction(((.6*Predrf1)+(.4*Predrf2))/2,eBayTraintest$sold)
# rf.auc = as.numeric(performance(ROCRrf,"auc")@y.values)
# rf.auc
# validation end

MySubmissionrf = data.frame(UniqueID = eBayTest2$UniqueID, Probability1 = ((.5*Predrf1)+(.5*Predrf2)/2))
write.csv(MySubmissionrf, "rf.csv", row.names=FALSE)
