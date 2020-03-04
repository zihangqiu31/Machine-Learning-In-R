setwd("~/R")

#import data#
mydata=read.csv(file="Frogs_MFCCs.csv",header=TRUE)
#process data#
frogdata=subset(mydata,select=-c(23:26))
standardized.X=scale(frogdata)
mydata[,23]=as.numeric(mydata[,23])#turn characters into numbers



#k-Means with different initializations
library(clues)
adjrand=c()
set.seed(15)
for (i in 1:50){
  km.out=kmeans(frogdata,4,nstart=i)
  temp=adjustedRand(km.out$cluster,mydata[,23])
  temp=as.data.frame(temp)
  adjrand=c(adjrand,temp[2,])
}
plot(c(1:50),adjrand,type='l',ylab="adjusted rand index",
     xlab="initialization")
points(which.max(adjrand),adjrand[which.max(adjrand)])




#k-Means with different parameters
aver.ari=c()
set.seed(6)
for (j in 1:15){
  ari=c()
  for (i in 1:20){
    km.out=kmeans(frogdata,j,nstart=i)
    temp=adjustedRand(km.out$cluster,mydata[,23])
    temp=as.data.frame(temp)
    ari=c(ari,temp[2,])
  }
  aver.ari=c(aver.ari,mean(ari))
}
plot(c(1:15),aver.ari,type='l',xlab ="parameter k",ylab="average adjusted rand index")
points(which.max(aver.ari),aver.ari[which.max(aver.ari)])



#Guassian mixture model
library(mclust,quietly = TRUE)
gmm.fit=Mclust(frogdata,G=4)
adjustedRand(gmm.fit$classification,mydata[,23])
library(sBIC)
gMix=GaussianMixtures(maxNumComponents=15,phi=1,restarts = 100)


#PCA
states=row.names(standardized.X)
#prcomp performs principal components analysis/no scale needed
set.seed(6)
pr.out=prcomp(standardized.X)
#biplot(pr.out,scale=0)
pr.var=pr.out$sdev^2
pve=pr.var/sum(pr.var)
#plot pve
plot(pve,xlab="principal component",ylab="proportion of
  variance explained",ylim=c(0,1),type='b')
plot(cumsum(pve),xlab="principal component",ylab="cumulative 
  proportion of vaiance explained",ylim=c(0,1),type='b')












