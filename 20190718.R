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
























