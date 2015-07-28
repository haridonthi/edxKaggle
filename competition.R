# Submissions score
# 1. GLM sold~.-Description-UniqueID, AUC = 0.838792
# 2. RF with same formula as above. Performed better than GLM on Kaggle. AUC = 0.8432041
# 3a. RF using columns and text, AUC with sparseness thresh.995 =  0.8331331
# 3b. RF using columns and text, AUC with sparseness thresh.99 =   0.8362532  (kaggle AUC 0.82432, was better than RF with discrete vars)  
# abandoned: GLM with cross-validation instead of RF - abandon - rf should be better than glm for this?
# bad: Mean of rf/variables, and glm/text - worse that RF using ALL variables  0.82319
# randomForest with manually selected columns performs AUC 0.827009
# startprice+condition+cellular+carrier+color+storage+productline+box+brand+case+clean+condit+condition2+cosmet+crack+dent+excellent+excel+functional+good+great+minor+mint+origin+perfect+scratch+scratches+scuff+tear+wear+wifi+use+used
# current: change weight of text and category variables

# Submit current if AUC for manually selected vars is higher

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

#eBayTrain3=eBayTrain2
#Split eBayTrain3 into Traintrain & Traintest
set.seed(123)
spl = sample.split(eBayTrain3$sold,SplitRatio = 0.7)
eBayTraintrain = subset(eBayTrain3, spl==TRUE)
eBayTraintest = subset(eBayTrain3, spl==FALSE)

#GLM for numeric/categorical variables (without text)
eBayTraintrain$description = NULL
eBayTraintest$description = NULL
glm1 = glm(sold~.-UniqueID,data=eBayTraintrain,family=binomial)
Predglm1 = predict(glm1,newdata=eBayTraintest, type="response")
ROCRglm1 = prediction(Predglm1,eBayTraintest$sold)
glm1.auc = as.numeric(performance(ROCRglm1,"auc")@y.values)

#randomForest for numeric/categorical variables (without text)
eBayTraintrain$description = NULL
eBayTraintest$description = NULL
rf1 = randomForest(sold~.-UniqueID,data=eBayTraintrain)
Predrf1 = predict(rf1,newdata=eBayTraintest, type="prob")[,2]
ROCRrf1 = prediction(Predrf1,eBayTraintest$sold)
rf1.auc = as.numeric(performance(ROCRrf1,"auc")@y.values)


rf2 = randomForest(sold~startprice+condition+cellular+carrier+color+storage+productline+box+brand+case+clean+condit+condition2+cosmet+crack+dent+excellent+excel+functional+good+great+minor+mint+origin+perfect+scratch+scratches+scuff+tear+wear+wifi+use+used,data=eBayTraintrain)
Predrf2 = predict(rf2,newdata=eBayTraintest, type="prob")[,2]
ROCRrf2 = prediction(Predrf2,eBayTraintest$sold)
rf2.auc = as.numeric(performance(ROCRrf2,"auc")@y.values)

#forest=randomForest(sold~.-description-UniqueID,data=eBayTraintrain, keep.forest=TRUE)
PredForest = predict(forest1, newdata=eBayTraintest, type="prob")[,2]

#auc on training set
ROCRpred=prediction(PredForest,eBayTraintest$sold)
as.numeric(performance(ROCRpred,"auc")@y.values)

#Compute test values & submit
eBayTest2 = read.csv("eBayiPadTest.csv") #without stringsAsFactors=FALSE
eBayTest2$biddable=as.factor(eBayTest2$biddable)
levels(eBayTest2$productline) <- levels(eBayTrain2$productline) #train has more levels than test

PredTestdf = as.data.frame(PredTest)

MySubmissionrf3 = data.frame(UniqueID = eBayTest2$UniqueID, Probability1 = (PredForest1[,2]+PredTestdf$PredTest)/2)
write.csv(MySubmissionrf3, "rf3.csv", row.names=FALSE)


