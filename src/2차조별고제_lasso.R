# 2차 과제 lasso로 변수 찾기 
library(dplyr)
library(glmnet)
library(e1071)
library(adabag)
library(class)
library(Epi)

setwd("C:/Users/fkaus/OneDrive/문서/주문파일")

signature<-read.csv("Signature.Table.by.Group1.csv",header=T,stringsAsFactors = F)
signature<-na.omit(signature)

signature<-signature[,-c(2,3,4)]
View(signature)
yvec<-signature[,10]
signature<-signature[,-10]
xmat<-as.matrix(signature[1:10])
fit.lasso<-glmnet(x=xmat,y=yvec,alpha=1,nlambda=100)
fit.lasso.cv<-cv.glmnet(x=xmat,y=yvec,nfolds=10,alpha=1,lambda = fit.lasso$lambda)
plot(fit.lasso.cv)

fit.lasso.param<-fit.lasso.cv$lambda.min
fit.lasso.tune<-glmnet(x=xmat,y=yvec,alpha=1,lambda=fit.lasso.param)
coef(fit.lasso.tune)

# SVM 실패

# signature<-read.csv("Signature.Table.by.Group1.csv",header=T,stringsAsFactors = F)
# signature<-na.omit(signature)
# 
# signature<-signature[,-c(2,3,4)]
# 
# t_index<-sample(1:nrow(signature),size=nrow(signature)*0.7)
# train<-signature[t_index,]
# test<-signature[-t_index,]
# 
# m<-svm(factor(금액기준.배송완료.소요기간.일.)~SapID+DeliveryCode+SoldToParty+RDC+배송량.개.+배송총액.위안.+배송기간.일.+차수+주문총액.위안.+평균배송횟수,data=train,gamma=2,const=16,kernel="linear")
# auto<-read.csv("autoparts.csv",header=T)

# knn

signature<-read.csv("Signature.Table.by.Group1.csv",header=T,stringsAsFactors = F)
signature<-na.omit(signature)
signature<-signature[,-c(2,3,4)]

t_index<-sample(1:nrow(signature),size=nrow(signature)*0.7)
train<-signature[t_index,]
test<-signature[-t_index,]
head(train)

xmat.test<-as.matrix(test[1:9])

xmat.train<-as.matrix(train[1:9])
y.train<-train$금액기준.배송완료.소요기간.일.

yhat_test<-knn(xmat.train,xmat.test,as.factor(y.train),k=5)
table<-table(real=test$금액기준.배송완료.소요기간.일.,predict=yhat_test)
table
ROC(test=yhat_test,stat=test$금액기준.배송완료.소요기간.일.,plot="ROC",AUC=T,main="KNN")


# SVR

signature<-read.csv("Signature.Table.by.Group1.csv",header=T,stringsAsFactors = F)
signature<-na.omit(signature)
signature<-signature[,-c(2,3,4)]

t_index<-sample(1:nrow(signature),size=nrow(signature)*0.7)
train<-signature[t_index,]
test<-signature[-t_index,]

m<-svm(금액기준.배송완료.소요기간.일.~.,data=train,gamma=2,cost=16)


