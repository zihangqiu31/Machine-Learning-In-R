##################################
###### STAT 557 (Project 1) ######
##################################


rm(list=ls()) ## To clear your environment

## Read the data
xTrain=read.csv("ecoli_xTrain.csv")
yTrain=read.csv("ecoli_yTrain.csv")
xTest=read.csv("ecoli_xTest.csv")
yTest=read.csv("ecoli_yTest.csv")


#### Part 1 ####
logProd <- function(x){
  lp<-sum(x)
  return(lp)
}


logSum <- function(x){
  ls<-log(sum(exp(x)))
  return(ls)
}



#### Part 2 ####
prior <- function(yTrain){
  x<-table(yTrain)
  p<-matrix(0,length(x),1)
  for (i in 1: length(x)){
    p[i]<-x[i]/nrow(yTrain)
  }
  return(p)
}


likelihood <- function(xTrain, yTrain){
  Z<-cbind2(xTrain,yTrain)
  M<-matrix(0,5,5)
  V<-matrix(0,5,5)
  
  Z1=Z[Z[,6]==1,]
  Z2=Z[Z[,6]==2,]
  Z3=Z[Z[,6]==3,]
  Z4=Z[Z[,6]==4,]
  Z5=Z[Z[,6]==5,]
  
  M[1,]<-sapply(Z1[,1:5],mean)
  M[2,]<-sapply(Z2[,1:5],mean)
  M[3,]<-sapply(Z3[,1:5],mean)
  M[4,]<-sapply(Z4[,1:5],mean)
  M[5,]<-sapply(Z5[,1:5],mean)
  
  V[1,]<-sapply(Z1[,1:5], var)
  V[2,]<-sapply(Z2[,1:5], var)
  V[3,]<-sapply(Z3[,1:5], var)
  V[4,]<-sapply(Z4[,1:5], var)
  V[5,]<-sapply(Z5[,1:5], var)
  
  return(matrix(c(M,V),5,10))
}
#because the function returns only one thing, using codes below to output M and V seperately
#MV<-likelihood(xTrain,yTrain)
#M<-MV[,1:5]
#V<-MV[,6:10]


naiveBayesClassify <- function(xTest, M, V, p){
  ClassifyProb<-matrix(0,nrow(xTest),5)
  for (i in 1:nrow(xTest)){
    for (j in 1:5){
    p1<-(1/sqrt(2*pi*V[j,]))*exp(-(xTest[i,]-M[j,])^2/(2*V[j,]))
    ClassifyProb[i,j]<-prod(p1)*p[j]   
    }
  }
  outcome<-matrix(max.col(ClassifyProb),nrow=nrow(xTest))
  return(outcome)
  # length(outcome[outcome==yTest,])/length(outcome)
  #[1] 0.8333333
  #calculate prob of right estimate
}



## Read the data
#overlay the previous data
xTrain=read.csv("ecoli_new.xTrain.csv")
yTrain=read.csv("ecoli_new.yTrain.csv")
xTest=read.csv("ecoli_new.xTest.csv")
yTest=read.csv("ecoli_new.yTest.csv")

##############
#prob_pi<-length(yTrain[yTrain[]==1])/nrow(yTrain)
#NewTrain<-cbind2(xTrain,yTrain)
#
#mean0<-matrix(colMeans(NewTrain[NewTrain[,7]==0,][,2:6]))
#mean1<-matrix(colMeans(NewTrain[NewTrain[,7]==1,][,2:6]))
#
#variance<-matrix(0,5,1)
#for (i in 1:5){
#  variance[i]<-var(NewTrain[,2:6])[i,i]
#}
#
#w0<-log((1-prob_pi)/prob_pi)+sum((mean1*mean1-mean0*mean0)/(2*variance))
#w<-matrix(c(w0,(mean0-mean1)/variance))
#
#p_y1x<-matrix(0,length(xTrain),1)
#for (i in 1:nrow(xTrain)){
#  p_y1x[i]<-1/(1+exp(sum(w*xTrain[i,])))
#}
#outcome<-matrix(sapply(p_y1x[]>=0.5,as.numeric),ncol=1)
#length(outcome[outcome==yTrain,])/length(outcome)
##############


#### Part 3 ####
#w should be f*1 size matrix
sigmoidProb <- function(y, x, w){
  if (y==1){ 
    p_yx<-1/(1+exp(sum(x*w)))
  }
  else{
    p_yx<-exp(sum(x*w))/(1+exp(sum(x*w)))
  }
  return(p_yx)
}


#w0 should be f*1 size matrix
logisticRegressionWeights <- function(xTrain, yTrain, w0, nIter){
  w<-w0
  for (i in 1: nIter){
    for (j in 1:6) {
      w[j]<-w[j]+0.1*(t(yTrain)-exp(t(w)%*%t(xTrain))/(1+exp(t(w)%*%t(xTrain))))%*%xTrain[,j]
    }
  
  }
  return(w)
}


logisticRegressionClassify <- function(xTest, w){
  
  p_y1x<-matrix(exp(t(w)%*%t(xTest))/(1+exp(t(w)%*%t(xTest))),nrow(xTest),1)
  return(p_y1x)
  #prob of right estimate
  #outcome<-matrix(sapply(p_y1x[]>=0.5,as.numeric),ncol=1)
  #length(outcome[outcome==yTest,])/length(outcome)
}