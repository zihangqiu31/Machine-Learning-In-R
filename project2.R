##################################
###### STAT 557 (Project 2) ######
##################################




##read the data
setwd("~/R")
library(readxl)
originaldata=read_excel("default of credit card clients.xls")
cccfull=originaldata[,-1]
colnames(cccfull)[24]=c("dpnm")

##seperate data into train and test
set.seed(1)
train=sample(nrow(cccfull),0.75*nrow(cccfull))
ccctrain=cccfull[train,]
ccctest=cccfull[-train,]

##logistic regression
glm.fit=glm(dpnm~.,data=ccctrain,family=binomial)
glm.pred=predict(glm.fit,newdata=ccctest[,-24],type="response")
glm.pred[glm.pred<0.5]=0
glm.pred[glm.pred>=0.5]=1

#accuracy of lg
length(glm.pred[glm.pred==ccctest$dpnm])/length(glm.pred)

  #subset selection
  library(leaps)
  regfit.full=regsubsets(dpnm~.,cccfull,nvmax=23)
  reg.summary=summary(regfit.full)
  which.min(reg.summary$cp)
  ss=reg.summary$which[15,]
  names(ss[ss=="TRUE"])
  #glm.fit=glm(dpnm~LIMIT_BAL+SEX+EDUCATION+MARRIAGE+AGE+PAY_0+PAY_2+PAY_3+PAY_5+BILL_AMT1+BILL_AMT2+PAY_AMT1+PAY_AMT2+PAY_AMT4+PAY_AMT5,data=ccctrain,family=binomial)
  #do not use because of no obvious improvement for accuracy for lg

#draw ROC curve


##tree structure classifier
library(tree)
tree.fit=tree(as.factor(dpnm)~.,ccctrain)
plot(tree.fit)
text(tree.fit,pretty=0)

tree.pred=predict(tree.fit,ccctest[,-24],type="class")
#accuracy of tree
length(tree.pred[tree.pred==ccctest$dpnm])/length(tree.pred)#0.8177333


#random forest
library(randomForest)
rf.ccc=randomForest(as.factor(dpnm)~.,data=ccctrain,mtry=4,importance=TRUE)
yhat.rf=predict(rf.ccc,newdata=ccctest[,-24])
#accuracy of rf
length(yhat.rf[yhat.rf==ccctest$dpnm])/length(yhat.rf)#0.8156


##Boosting
library(gbm)
boost.ccc=gbm(dpnm~.,data=ccctrain,distribution="gaussian",n.trees=5000,interaction.depth=4)
#summary(boost.ccc)

yhat.boost=predict(boost.ccc,newdata=ccctest[,-24],n.tree=5000)
mean((yhat.boost-ccctest$dpnm)^2)#0.1461722


##SVM
library(e1071)
svmfit=svm(dpnm~.,data=ccctrain,kernel="linear",cost=10,scale=FALSE)

tune.out=tune(svm,dpnm~.,data=ccctrain,kernel="linear",ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)
svmpred=predict(tune.out$best.model,newdata=ccctest[,-24])
table(predict=svmpred,truth=ccctest$dpnm)


svmfit2=svm(dpnm~.,data=ccctrain,kernel="radial",gamma=1,cost=10)
tune.out2=tune(svm,dpnm~.,data=ccctrain,kernel="radial",ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100),gamma=c(0.5,1,2,3,4)))
svmpred2=predict(tune.out2$best.model,newdata=ccctest[,-24])
table(true=ccctest$dpnm,pred=svmpred2)



#################
library(ROCR)
rocrpred <- prediction(pred[,2], data$bad_econ)
# print AUC value
as.numeric(performance(rocrpred, "auc")@y.values)
# plots the ROC curve with colors where the splits are.
plot(performance(rocrpred, "tpr", "fpr"), colorize = TRUE) 











