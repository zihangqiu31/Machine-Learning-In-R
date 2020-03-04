rm(list = ls())
library(mclust)
#### Read Data ####
#setwd("E:/Courses in PSU/STAT 557 Data Mining/HW/Project 3/Anuran Calls (MFCCs)")
data = read.csv("Frogs_MFCCs.csv", header = TRUE)
sum(is.na(data))#check if data missing

#### Pre-processing ####
Frogdata = as.data.frame(data)
Frogdata[,23] = as.numeric(Frogdata[,23])
Frogdata.attributes = as.data.frame(Frogdata[,1:22])
Frogdata.attributes = as.matrix(Frogdata.attributes)

#### K-means ####
#Question 1: Different Initializations
class = Frogdata[,23]
set.seed(5)
adjrand=c()
for (i in 1:50){
  km.out=kmeans(Frogdata.attributes,4,nstart=i)
  temp=adjustedRandIndex(km.out$cluster,class)
  adjrand=c(adjrand,temp)
}
plot(c(1:50),adjrand,type='l',ylim=c(0,1),main="Different Initializations",ylab="adjusted rand index",xlab="initialization")
points(which.max(adjrand),adjrand[which.max(adjrand)])
print(c(DifferentInitMaxARI=adjrand[which.max(adjrand)]))
#identify(adjrand)
#Question 2: Total Sum of Squares within cluster under different K values
aver.ari=c()
set.seed(6)
for (j in 1:15){
  ari=c()
  for (i in 1:20){
    km.out=kmeans(Frogdata.attributes,j,nstart=i)
    temp=adjustedRandIndex(km.out$cluster,class)
    ari=c(ari,temp)
  }
  aver.ari=c(aver.ari,mean(ari))
}
plot(c(1:15),aver.ari,type='l',ylim=c(0,1),main="Different K",xlab ="parameter k",ylab="average adjusted rand index")
points(which.max(aver.ari),aver.ari[which.max(aver.ari)])
print(c(DifferentKMaxARI=aver.ari[which.max(aver.ari)]))
#### PCA ####
pr.out = prcomp(Frogdata.attributes,scale = TRUE)
# pr.var = pr.out$sdev^2
# pve = pr.var/sum(pr.var)
# plot(pve,xlab="Principal Component",ylab="Proportion of Variance Explained",ylim=c(0,1),type='b')
# plot(cumsum(pve),xlab="Principal Component",ylab="Proportion of Variance Explained",ylim=c(0,1),type='b')

#Dimension reduction
pc1 = as.matrix(as.numeric(pr.out$rotation[,1]))
pc2 = as.matrix(as.numeric(pr.out$rotation[,2]))
col1 = Frogdata.attributes%*%pc1
col2 = Frogdata.attributes%*%pc2
Newdata = cbind(col1,col2)
km.out = kmeans(Newdata,4,nstart=20)
plot(Newdata,col=(km.out$cluster+1),main="K-Means Clustering",xlab="",ylab="",pch=20,cex=2)
AdjustedRandIndex = adjustedRandIndex(km.out$cluster,class)
print(AdjustedRandIndex)


#### GMM ####
mod1 = Mclust(Frogdata.attributes,G=4,modelNames = c("VII","VEI","VVI","VVV")) 
table(class,mod1$classification)
adjustedRandIndex(mod1$classification,class)
#GMM on 2 dimensions
mod2 = Mclust(Newdata,G=4,modelNames = c("VII","VEI","VVI","VVV"))
table(class,mod2$classification)
adjustedRandIndex(class,mod2$classification)
plot(mod2,what='classification')