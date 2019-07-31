# 20190722 

getwd()
setwd("D:/BigDataCampus")
autoparts<-read.csv("autoparts.csv",header=TRUE)
autoparts1<-autoparts[autoparts$prod_no=="90784-76001",c(2:11)]
autoparts2<-autoparts1[autoparts1$c_thickness<1000,]
autoparts2$y_faulty<-ifelse((autoparts2$c_thickness<20)|(autoparts2$c_thickness>32),1,0)
t_index<-sample(1:nrow(autoparts2),size=nrow(autoparts2)*0.7)
train<- autoparts2[t_index,]
test<-autoparts2[-t_index,]

nrow(train);
install.packages("party")
install.packages("rpart")
#install.packages("tree")
library(tree)
m<-tree(factor(y_faulty)~fix_time+a_speed+b_speed+separation+s_separation+rate_terms+mpa+load_time+highpressure_time,data=train)
plot(m)
text(m)



prune.m<-prune.tree(m,method="misclass")
plot(prune.m)
prune.m<-prune.tree(m,best=9)

yhat_test<-predict(prune.m,test,type="class")
table<-table(real=test$y_faulty,predict=yhat_test); table
(table[1,1]+table[2,2])/sum(table)

library(Epi)
ROC(test=yhat_test,stat=test$y_faulty,plot="ROC",AUC=T,main="TREE")



library(rpart)
library(caret)
library(party)

rpartmod<-rpart(factor(y_faulty)~ fix_time + a_speed + b_speed + separation + s_separation +
                  rate_terms + mpa + load_time + highpressure_time , data = train, method="class")
plot(rpartmod)
text(rpartmod)

printcp(rpartmod)
plotcp(rpartmod)
ptree<-prune(rpartmod, cp= rpartmod$cptable[which.min(rpartmod$cptable[,"xerror"]),"CP"])
plot(ptree)
text(ptree)

rpartpred<-predict(ptree, test, type='class')
confusionMatrix(table(rpartpred, test$y_faulty),positive = "1")

# Error
#new.data<-data.frame(fix_time=87,a_speed=0.609,b_speed=1.715,separatin=242.7,s_separation=657.5,rate_terms=95,mpa=78,load_time=18.1,highpressure_time=82)

new.data<-data.frame(fix_time=c(87,85.6),a_speed=c(0.609,0.472),b_speed=c(1.715,1.685),separation=c(242.7,243.4),s_separation=c(657.5,657.9),rate_terms=c(95,95),mpa=c(78,28.8),load_time=c(18.1,18.2),highpressure_time=c(82,60))
predict(m,newdata = new.data)

new.data<-data.frame(fix_time=test$fix_time,a_speed=test$a_speed,b_speed=test$b_speed,separation=test$separation,s_separation=test$s_separation,rate_terms=test$rate_terms,mpa=test$mpa,load_time=test$load_time,highpressure_time=test$highpressure_time)
predict(m,newdata=new.data)


autoparts2$g_class<-as.factor(ifelse(autoparts2$c_thickness<20,1,ifelse(autoparts2$c_thickness<32,2,3)))
t_index<-sample(1:nrow(autoaprts2),size=nrow(autoparts2)*0.7)
train<-autoparts2[t_index,]
test<-autoparts2[-t_index,]

yhat_test<-predict(m,test,type="class")
table<-table(real=test$g_class,predict=yhat_test);table
(table[1,1]+table[2,2]+table[3,3])/sum(table)



m<-tree(c_thickness~fix_time+a_speed+b_speed+separation+s_separation+rate_terms+mpa+load_time+highpressure_time,data=train)
plot(m)
text(m)

yhat_test<-predict(m,test)
head(yhat_test)
head(test$c_thickness)

# MSE계산하기! 
mse<-mean((yhat_test-test$c_thickness)^2)
mse #결과값 4.500051

#------------------------------------------------------------------------------------------------------------------
#k-NN

autoparts<-read.csv("autoparts.csv",header=TRUE)
autoparts1<-autoparts[autoparts$prod_no=="90784-76001",c(2:11)]
autoparts2<-autoparts1[autoparts1$c_thickness<1000,]
autoparts2$y_faulty<-ifelse((autoparts2$c_thickness<20)|(autoparts2$c_thickness>32),1,0)

t_index<-sample(1:nrow(autoaprts2),size=nrow(autoparts2)*0.7)
train<-autoparts2[t_index,]
test<-autoparts2[-t_index,]

# 훈련데이터 행렬과 종속변수
xmat.train<-as.matrix(train[1:9])
# 별도로 저장 
y_faulty.train<-train$y_faulty

xmat.test<-as.matrix(test[1:9])
head(xmat.test)

#install.packages("class")
library(class)

# knn()함수는 모델을 만들지 않고, 바로 예측을 한다. 
yhat_test<-knn(xmat.train,xmat.test,as.factor(y_faulty.train),k=3)
yhat_test

table<-table(real=test$y_faulty,predict=yhat_test)
table
(table[1,1]+table[2,2])/sum(table)
# 95%의 정확도를 보입니다. 

library(e1071)
# 최적의 k 값을 구합니다. 
# 최적의 k값은 5입니다. 
tune.out<-tune.knn(x=xmat.train,y=as.factor(y_faulty.train),k=1:10); tune.out
plot(tune.out)
# k가 1~10까지의 error정도를 plot으로 보여줍니다. 

yhat_test<-knn(xmat.train,xmat.test,y_faulty.train,k=5)
table<-table(real=test$y_faulty,predict=yhat_test)
table
accuarcy<-(table[1,1]+table[2,2])/sum(table)
accuarcy

ROC(test=yhat_test,stat=test$y_faulty,plot="ROC",AUC=T,main="KNN")
# AUC: 0.876

# 데이터 1개를 예측합니다. 
new.data<-data.frame(fix_time=87,a_speed=0.609,b_speed=1.715,separation=242.7,s_separation=657.5,rate_terms=95,mpa=78,load_time=18.1,highpressure_time=82)

knn(xmat.train,new.data,y_faulty.train,k=5)
# 결과: 0 (정상)

new.data<-data.frame(fix_time=c(87,85.6),a_speed=c(0.609,0.472),b_speed=c(1.715,1.685),separation=c(242.7,243.4),s_separation=c(657.5,657.9),rate_terms=c(95,95),mpa=c(78,28.8),load_time=c(18.1,18.2),highpressure_time=c(82,60))
knn(xmat.train,new.data,y_faulty.train,k=5)
# 첫번째 새로운 데이터는 0 정상 , 두번째 데이터는 1 불량 

autoparts<-read.csv("autoparts.csv",header=TRUE)
autoparts1<-autoparts[autoparts$prod_no=="90784-76001",c(2:11)]
autoparts2<-autoparts1[autoparts1$c_thickness<1000,]
autoparts2$g_class<-as.factor(ifelse(autoparts2$c_thickness<20,1,ifelse(autoparts2$c_thickness<32,2,3)))

t_index<-sample(1:nrow(autoparts2),size=nrow(autoparts2)*0.7)
train<-autoparts2[t_index,]
test<-autoparts2[-t_index,]

xmat.train<-as.matrix(train[1:9])
c_thickness.train<-train$c_thickness
xmat.test<-as.matrix(test[1:9])

library(FNN)
yhat_test<-knn.reg(xmat.train,xmat.test,c_thickness.train,k=5)
mse<-mean((yhat_test$pred-test$c_thickness)^2)
mse

new.data<-data.frame(fix_time=87,a_speed=0.609,b_speed=1.715,separation=242.7,s_separation=657.5,rate_terms=98,mpa=78,load_time=18.1,highpressure_time=82)
knn.reg(xmat.train,new.data,c_thickness.train,k=3)

new.data<-data.frame(fix_time=c(87,88),a_speed=c(0.609,0.68),b_speed=c(1.715,2.1),separation=c(242.7,250.1),s_separation=c(657.5,667),rate_terms=c(98,99),mpa=c(78,79),load_time=c(18.1,19.5),highpressure_time=c(82,81))
knn.reg(xmat.train,new.data,c_thickness.train,k=3)

#----------------------------------------------인공신경망---------------------------------------------------------------------


autoparts<-read.csv("autoparts.csv",header = T)
autoparts1<-autoparts[autoparts$prod_no=="90784-76001",c(2:11)]  
autoparts2<-autoparts1[autoparts1$c_thickness<1000,]
autoparts2$g_class<-as.factor(ifelse(autoparts2$c_thickness<20,1,ifelse(autoparts2$c_thickness<32,2,3)))

t_index<-sample(1:nrow(autoparts2),size=nrow(autoparts2)*0.7)
train<-autoparts2[t_index,]
test<-autoparts2[-t_index,]

install.packages("nnet")
library(nnet)

# size=10은 은닉층을 10개라고 지정합니다. 
m<-nnet(g_class~fix_time+a_speed+b_speed+separation+s_separation+rate_terms+mpa+load_time+highpressure_time,data=train,size=10)

yhat_test<-predict(m,test,type="class")
table<-table(real=test$g_class,predict=yhat_test)
table

x1<-c(0,0,0)
table<-cbind(x1,table)
table
(table[1,1]+table[2,2]+table[3,3])/sum(table)
# 결과: 86.4%

as.factor(yhat_test)


new.data<-data.frame(fix_time=c(87,85.6),a_speed=c(0.609,0.472),b_speed=c(1.715,1.685),separation=c(242.7,243.4),s_separation=c(657.5,657.9),rate_terms=c(95,95),mpa=c(78,28.8),load_time=c(18.1,18.2),highpressure_time=c(82,60))
predict(m,newdata=new.data,type="class")
# "2" "2" 를 반환한다. 즉, 2개의 새로운 데이터들은 정상으로 예측했다. 



# Occupancy.test=read.csv("occupancy_test.csv",header = T)
# Occupancy.train=read.csv("occupancy_train.csv",header=T)
# head(Occupancy.train)
# xmat<-as.matrix(Occupancy.train[2:5])
# yvec<-Occupancy.train$Occupancy
# 
# library(glmnet)
# fit.lasso<-glmnet(x=xmat,y=yvec,alpha=1,nlambda = 100)
# fit.lasso.cv<-cv.glmnet(x=xmat,y=yvec,nfolds=10,alpha=1,lambda=fit.lasso$lambda)
# plot(fit.lasso.cv)
# 
# fit.lasso.param<-fit.lasso.cv$lambda.min
# fit.lasso.tune<-glmnet(x=xmat,y=yvec,alpha = 1,lambda=fit.lasso.param)
# coef(fit.lasso.tune)
# 
# m<-tree(factor(Occupancy)~Temperature+Humidity+Light+CO2+HumidityRatio,data=Occupancy.train)
# plot(m)
# text(m)
# 
# yhat_test<-predict(m,Occupancy.test,type="class")
# table<-table(real=Occupancy.test$Occupancy,predict=yhat_test)
# table
# ROC(test=yhat_test,stat=Occupancy.test$Occupancy,plot="ROC",AUC=T,main="TREE")
# # AUC: 0.983
# 
# yhat_test<-knn(Occupancy.train,Occupancy.test,as.factor(Occupancy.train),k=3)
# yhat_test
# 
# table<-table(real=Occupancy.test$Occupancy,predict=yhat_test);table
# (table[1,1]+table[2,2])/sum(table) # 정확도 0.9786116
# 


# 데이터를 불러옵니다. 
occupancy_all <- read.csv("occupancy_all.csv")
occupancy_train <- read.csv("occupancy_train.csv")
occupancy_test <- read.csv("occupancy_test.csv")

head(occupancy_train,0)

# 데이터를 체크해봅니다. 
omat <- as.matrix(occupancy_train[2:5])
#head(omat)
ovec <- occupancy_train$Occupancy
#head(ovec)

library(glmnet)

# 데이터의 종속변수는 1과 0 이므로 Level은 2개이다. 
fit.lasso <- glmnet(x= omat, y = ovec, alpha=1, nlambda = 100, family = "binomial")
fit.lasso.cv <- cv.glmnet(x= omat, y = ovec, nfolds = 10, alpha = 1, lambda = fit.lasso$lambda, family = "binomial")
plot(fit.lasso.cv)

fit.lasso.param <- fit.lasso.cv$lambda.1se # 이걸 개인적으로 선호
fit.lasso.param2 <- fit.lasso.cv$lambda.min

fit.lasso.tune <- glmnet(x=omat, y = ovec, alpha =1, lambda = fit.lasso.param, family ="binomial")

coef(fit.lasso.tune)

# 사용할 train데이터를 뽑습니다. 
occupancy_train <- occupancy_train[,c(2,4,5,7)]

head(occupancy_train)

library(tree)

# Occupancy를 의사결정 트리로 만들어 냅니다. 
m <- tree(factor(Occupancy)~ Temperature + Light + CO2 , data = occupancy_train)
plot(m)
text(m)

prune.m <- prune.tree(m, method = "misclass")
plot(prune.m)


## 위를 바탕으로 다시 피팅

prune.m <- prune.tree(m, best = 2)
plot(prune.m)
text(prune.m)

yhat_test <- predict(prune.m, occupancy_test, type="class")
table <- table(real = occupancy_test$Occupancy, predict = yhat_test); table
(table[1,1]+table[2,2])/sum(table)

library(Epi)
ROC(test = yhat_test, stat = occupancy_test$Occupancy, plot="ROC", AUC = T, main = "TREE")

# 연습문제 2

xmat.train <- as.matrix(occupancy_train[1:3])
occupancy.train <- occupancy_train$Occupancy

xmat.test <- as.matrix(occupancy_test[c(2,4,5)])
head(xmat.test)

library(class)

yhat_test <- knn(xmat.train, xmat.test, as.factor(occupancy.train),k=3)

table <- table(real = occupancy_test$Occupancy, predict = yhat_test);table
(table[1,1]+table[2,2])/sum(table)

ROC(test = yhat_test, stat = occupancy_test$Occupancy, plot="ROC", AUC = T, main = "TREE")

library(e1071)

tune.out <- tune.knn(x=xmat.train, y = as.factor(occupancy.train), k = 1:10); tune.out
plot(tune.out)

yhat_test <- knn(xmat.train, xmat.test, as.factor(occupancy.train),k=7)

table <- table(real = occupancy_test$Occupancy, predict = yhat_test);table
(table[1,1]+table[2,2])/sum(table)


