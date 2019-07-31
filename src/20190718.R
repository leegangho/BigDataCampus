setwd("D:/BigDataCampus")
autoparts<-read.csv("autoparts.csv",header=T)
autoparts1<-autoparts[autoparts$prod_no=="90784-76001",c(2:11)]
autoparts2<-autoparts1[autoparts1$c_thickness<1000,]

# SVM은 분류모형이므로 종속변수를 1과 0으로 바꿉니다. 
autoparts2$y_faulty<-ifelse((autoparts2$c_thickness<20) | (autoparts2$c_thickness > 32),1,0)
head(autoparts2)

t_index<-sample(1:nrow(autoparts2),size=nrow(autoparts2)*0.7)

# 훈련데이터 
train<-autoparts2[t_index,]

# 검증데이터
test<-autoparts2[-t_index,]

# 데이터 분리가 되었는지 확인
nrow(train); nrow(test)

#install.packages("e1071")

library(e1071)

# tune.svm() 함수는 gamma와 cost의 최적값을 알려줍니다. 
# gamma는 초평면의 기울기, cost는 과적합에 따른 비용입니다. 
tune.svm(factor(y_faulty)~fix_time + a_speed + b_speed + separation + s_separation + rate_terms + mpa + load_time + highpressure_time,data=autoparts2,gamma=2^(-1:1),cost=2^(2:4))
# gamma:1 cost:16 

m<-svm(factor(y_faulty)~fix_time + a_speed + b_speed + separation + s_separation + rate_terms + mpa + load_time + highpressure_time,data=train,gamma=1,cost=16)

# 모델에서는 train 데이터를 사용하고 
# 이후 yhat_test<-predict(m,test)에서는 test 데이터를 사용합니다. 

yhat_test<-predict(m,test)
table<-table(real=test$y_faulty,predict=yhat_test)

# 정분류율은 약 96%를 보여줍니다. 
(table[1,1]+table[2,2])/sum(table)


# kernel="linear" 로 하였을때, 96% -> 92% 로 떨어집니다.
m<-svm(factor(y_faulty)~fix_time + a_speed + b_speed + separation + s_separation + rate_terms + mpa + load_time + highpressure_time,data=train,gamma=1,cost=16,kernel="linear")

yhat_test<-predict(m,test)
table<-table(real=test$y_faulty,predict=yhat_test)
(table[1,1]+table[2,2])/sum(table)


# 기본 gamma와 cost일 경우 91%입니다. 
m<-svm(factor(y_faulty)~fix_time + a_speed + b_speed + separation + s_separation + rate_terms + mpa + load_time + highpressure_time,data=train)
yhat_test<-predict(m,test)
table<-table(real=test$y_faulty,predict=yhat_test)
(table[1,1]+table[2,2])/sum(table)


#install.packages("Epi")
library(Epi)

# gamma: 1, cost: 16 으로 모델의 ROC & AUC 값을 보입니다. 
# AUC: 0.897 입니다. 
m<-svm(factor(y_faulty)~fix_time + a_speed + b_speed + separation + s_separation + rate_terms + mpa + load_time + highpressure_time,data=train,gamma=1,cost=16)
yhat_test<-predict(m,test)
ROC(test=yhat_test,stat=test$y_faulty,plot="ROC",AUC=T,main="SVM")

# gamma: 1, cost:16, kernel="linear"
# AUC: 0.725 입니다. 
m<-svm(factor(y_faulty)~fix_time + a_speed + b_speed + separation + s_separation + rate_terms + mpa + load_time + highpressure_time,data=train,gamma=1,cost=16,kernel="linear")

yhat_test<-predict(m,test)
ROC(test=yhat_test,stat=test$y_faulty,plot="ROC",AUC=T,main="SVM")


# 기본 gamma와 cost일 경우 91%입니다. 
# AUC: 0.713 입니다. 
m<-svm(factor(y_faulty)~fix_time + a_speed + b_speed + separation + s_separation + rate_terms + mpa + load_time + highpressure_time,data=train)
yhat_test<-predict(m,test)
ROC(test=yhat_test,stat=test$y_faulty,plot="ROC",AUC=T,main="SVM")

# 정확도가 96%인 모형을 가지고, 새로운 데이터를 분류해 봅니다.
new.data<-data.frame(fix_time=87,a_speed=0.609,b_speed=1.715,separation=242.7,s_separation=657.5,rate_terms=95,mpa=78,load_time=18.1,highpressure_time=82)
predict(m,newdata=new.data)

# 1
# 0 
# levels: 0 1 

# 위의 출력결과, 1개의 새로운 데이터는 정상 (0) 이다. 

new.data<-data.frame(fix_time=c(87,85.6),a_speed=c(0.609,0.472),b_speed=c(1.715,1.685),separation=c(242.7,243.4),s_separation=c(657.5,657.9),rate_terms=c(95,95),mpa=c(78,28.8),load_time=c(18.1,18.2),highpressure_time=c(82,60))
predict(m,newdata = new.data)

# 2개의 데이터를 입력하였더니, 2번째는 불량 (1)으로 분류한다. 

new.data<-data.frame(fix_time=test$fix_time,a_speed=test$a_speed,b_speed=test$b_speed,separation=test$separation,s_separation=test$s_separation,rate_terms=test$rate_terms,mpa=test$mpa,load_time=test$load_time,highpressure_time=test$highpressure_time)
predict(m,newdata=new.data)

#----------------------------------------------------------------------------

autoparts<-read.csv("autoparts.csv",header=T)
autoparts1<-autoparts[autoparts$prod_no=="90784-76001",c(2:11)]
autoparts2<-autoparts1[autoparts1$c_thickness<1000,]
autoparts2$y_faulty<-ifelse((autoparts2$adc_thickness<20)|(autoparts2$c_thickness>32),1,0)

t_index<-sample(1:nrow(autoparts2),size=nrow(autoparts2)*0.7)
train<-autoparts2[t_index,]
test<-autoparts2[-t_index,]
nrow(train); nrow(test)
head(train)

# Support Vector Machine Regression 이므로 연속형 데이터 타입을 예측한다.
# Cross table이 아닌 MSE로 비교해야한다. 
m<-svm(c_thickness~fix_time + a_speed + b_speed + separation + s_separation + rate_terms + mpa + load_time + highpressure_time,data=train,gamma=1,cost=16)
summary(m)

yhat_test<-predict(m,test)
plot(x=test$c_thickness,y=yhat_test,main="SVR")
mse<-mean((yhat_test-test$c_thickness)^2); mse

# 다중회귀
m2<-lm(c_thickness~fix_time+a_speed+b_speed+separation+s_separation+rate_terms+mpa+load_time+highpressure_time,data=train)
summary(m2)
r
yhat_test<-predict(m2,test)
plot(x=test$c_thickness,y=yhat_test,main="lm",xlab = "실제값",ylab="예측값")
mse<-mean((yhat_test-test$c_thickness)^2)
mse


autoparts
autoparts1<-autoparts[autoparts$prod_no=="90784-76001",c(2:11)]
autoparts2<-autoparts1[autoparts1$c_thickness<1000,]
autoparts2$y_faulty<-ifelse((autoparts2$c_thickness<20)|(autoparts2$c_thickness>32),1,0)

# 종속변수는 factor형이어야 하므로 형변환을 한다. 
autoparts2$y_faulty<-as.factor(autoparts2$y_faulty)
table(autoparts2$y_faulty)

m<-glm(y_faulty~fix_time+a_speed+b_speed+separation+s_separation+rate_terms+mpa+load_time+highpressure_time,data=autoparts2,family=binomial(logit))

# 통계적 유의도를 살펴봅니다. 
summary(m)

t_index<-sample(1:nrow(autoparts2),size=nrow(autoparts2)*0.7)
train<-autoparts2[t_index,]
test<-autoparts2[-t_index,]
nrow(train); nrow(test)

m<-glm(y_faulty~fix_time+a_speed+b_speed+separation+s_separation+rate_terms+mpa+load_time+highpressure_time,data=train,family=binomial(logit))

# m$fitted.values는 승산비를 보여준다. 
m$fitted.values

# 이부분의 1과 0의 구분 기준을 정해서 입력해야한다. 
yhat<-ifelse(m$fitted.values >= 0.5,1,0)

# 교차 테이블을 만들고, ROC로 활용한다. 
table<-table(real=train$y_faulty,predict=yhat)
table


test<-data.frame(fix_time=test$fix_time,a_speed=test$a_speed,b_speed=test$b_speed,separation=test$separation,s_separation=test$s_separation,rate_terms=test$rate_terms,mpa=test$mpa,load_time=test$load_time,highpressure_time=test$highpressure_time)
yhat_test<-predict(m,test,type="response")

library(Epi)
ROC(test = yhat_test, stat=yhat_test$y_faulty, plot="ROC",AUC=T,main="Logistics Regression")


autoparts2$g_class<-as.factor(ifelse(autoparts2$c_thickness<20,1,ifelse(autoparts2$c_thickness<32,2,3)))

# 1과 3은 불량, 2는 정상 입니다. 
table(autoparts2$g_class)

t_index<-sample(1:nrow(autoparts2),size=nrow(autoparts2)*0.7)
train<-autoparts2[t_index,]
test<-autoparts2[-t_index,]
head(train)

#install.packages("nnet")
library(nnet)

# 로지스틱 다항회귀 
# g_class를 종속변수로 한다. data는 train을 사용한다.  
m<-multinom(g_class~fix_time+a_speed+b_speed+separation+s_separation+rate_terms+mpa+load_time+highpressure_time,data=train)

# 1 분류가 안보이는 이유는 2,3분류를 제외한 값이므로, 따로 보여주지 않는다. 
summary(m)
head(m$fitted.values)

yhat_test<-predict(m,test)
table<-table(real=test$g_class,predict=yhat_test)
table

# 약 95%의 정확도를 보임 
(table[1,1]+table[2,2]+table[3,3])/sum(table)

OCCUPANCY.ALL<-read.csv("occupancy_all.csv",sep=",",header=T,stringsAsFactors = F)
OCCUPANCY.TEST<-read.csv("occupancy_test.csv",sep = ",",header = T,stringsAsFactors = F)
OCCUPANCY.TRAIN<-read.csv("occupancy_train.csv",sep=",",header=T,stringsAsFactors = F)

nrow(OCCUPANCY.ALL)
nrow(OCCUPANCY.TRAIN)
nrow(OCCUPANCY.TEST)
str(OCCUPANCY.ALL$Occupancy)

xmat<-as.matrix(OCCUPANCY.TRAIN[2:6])
yvec<-OCCUPANCY.TRAIN$Occupancy

fit.lasso<-glmnet(x=xmat,y=yvec,alpha=1,nlambda=100,family="binomial")
fit.lasso.cv<-cv.glmnet(x=xmat,y=yvec,nfolds=10,alpha=1,lambda=fit.lasso$lambda,family="binomial")
plot(fit.lasso.cv)

fit.lasso.param<-fit.lasso.cv$lambda.min
fit.lasso.tune<-glmnet(x=xmat,y=yvec,alpha=1,lambda = fit.lasso.param,family = "binomial")
coef(fit.lasso.tune)

# Temperature, Light, CO2, HumidityRatio 4개 변수 사용한다. 

OCCUPANCH.TRAIN$Occupancy<-as.factor(OCCUPANCY.TRAIN$Occupancy)
table(OCCUPANCY.TRAIN$Occupancy)

# 0은 6414 1은 1729

m<-glm(Occupancy~Temperature+Light+CO2+HumidityRatio,data=OCCUPANCH.TRAIN,family = binomial(logit))
# 확인용으로 사용합니다. 
summary(m)

m$fitted.values
yhat<-ifelse(m$fitted.values>=0.5,1,0)
yhat
table<-table(real=OCCUPANCH.TRAIN$Occupancy,predict=yhat)
table

yhat_test<-predict(m,OCCUPANCY.TEST,type="response")
head(yhat_test,n=20)

ROC(test=yhat_test,stat=OCCUPANCY.TEST$Occupancy,plot="ROC",AUC=T,main="LOGISTICS REGRESSION")


m<-svm(Occupancy~Temperature+Light+CO2+HumidityRatio,data=OCCUPANCH.TRAIN,cost=10,kernel="linear")
yhat_test<-predict(m,OCCUPANCY.TEST)
table<-table(real=OCCUPANCY.TEST$Occupancy,predict=yhat_test)
table
(table[1,1]+table[2,2])/sum(table)

ROC(test=yhat_test,stat=OCCUPANCY.TEST$Occupancy,plot="ROC",AUC=T,main="SVM")

















