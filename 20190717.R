#20190717 
library(reshape2)



setwd("D:/BigDataCampus")
autoparts<-read.csv("autoparts.csv")
dim(autoparts)

is.na(autoparts)
sum(is.na(autoparts))

head(french_fries)
french_fries[!complete.cases(french_fries),]

head(autoparts)
head(autoparts,n=10)

autoparts1<-autoparts[autoparts$prod_no=="90784-76001",c(2:11)]
head(autoparts1)

summary(autoparts1)
boxplot(autoparts1)
boxplot(autoparts1$c_thickness)
autoparts2<-autoparts1[autoparts1$c_thickness<1000,]
dim(autoparts2)
boxplot(autoparts2$c_thickness)

str(women)
m<-lm(weight~height,data=women)
summary(m)

plot(women$weight,women$height)
abline(m,col="red")

new.data<-data.frame(height=75)
predict(m,newdata=new.data)

new.data<-data.frame(height=c(75,76))
predict(m,new.data=new.data)

new.data<-data.frame(height=c(75,76))
predict(m,newdata=new.data,interval="confidence")

head(autoparts2)
m<-lm(c_thickness~fix_time,data=autoparts2)
new.data<-data.frame(fix_time=86.1)
predict(m,newdata=new.data)


autoparts<-read.csv("autoparts.csv",header=T)
autoparts1<-autoparts[autoparts$prod_no=="90784-76001",c(2:11)]
autoparts2<-autoparts1[autoparts1$c_thickness <1000,]

head(autoparts2)
summary(autoparts2)

m<-lm(c_thickness~fix_time+a_speed,data=autoparts2)
m<-lm(c_thickness~.,data=autoparts2)
summary(m)


new.data<-data.frame(fix_time=86.1,a_speed=0.610,b_speed=1.718,separation=241.9,s_separation=657.3,rate_terms=95,mpa=78.2,load_time=18.1,highpressure_time=74)
head(new.data)
predict(m,newdata=new.data)

new.data<-data.frame(fix_time=c(86.1,86.1),a_speed=c(0.610,0.603),b_speed=c(1.718,1.704),separation=c(241.9,242.5),s_separation=c(657.3,657.3),rate_terms=c(95,95),mpa=c(78.2,77.9),load_time=c(18.1,18.2),highpressure_time=c(74,56))

predict(m,newdata=new.data)

# 신뢰구간은 95%으로 기본값으로 설정 되어있다. 
predict(m,newdata=new.data,interval="confidence")

head(autoparts)


swiss
head(swiss)
dim(swiss)
m<-lm(Fertility~.,data=swiss)
summary(m)

# 전진선택법 
step(m,direction = "forward")
step(m,direction = "backward")
step(m,direction = "both")

autoparts<-read.csv("autoparts.csv",header=T)
autoparts1<-autoparts[autoparts$prod_no=="90784-76001",c(2:11)]
autoparts2<-autoparts1[autoparts1$c_thickness<1000,]

m<-lm(c_thickness~.,data=autoparts2)
step(m,direction="forward")
step(m,direction = "backward")
step(m,direction = "both")

#-----------------------------

# 교차검증 
# 현재 가지고 있는 데이터를 훈련데이터와 검증데이터로 나누어, 모델은 훈련데이터로 만들고,
# 나머지는 검증데이터로서 미래에 만나게 도리 데이터로 간주한다. 

# k-fold CV
# 데이터를 K개의 집합으로 나누어 한 개의 집합을 제외하고 나머지를 이용해서 모형을 적합한다ㅣ.
# 제외했던 한개의 집합으로 검정오차를 계산한다.



autoparts<-read.csv("autoparts.csv",header=TRUE)
autoparts1<-autoparts[autoparts$prod_no=="90784-76001",c(2:11)]
autoparts2<-autoparts1[autoparts1$c_thickness<1000,]

t_index<-sample(1:nrow(autoparts2),size=nrow(autoparts2))
split_index<-split(t_index,1:10)
head(split_index)


mse<-c()
for(i in 1:5){
  test<-autoparts2[split_index[[i]],]
  train<-autoparts2[split_index[[i]],]
  
  m<-lm(c_thickness~.,data=train)
  m_pred<-predict(m,test)
  mse[i]<-mean((test$c_thickness-m_pred)^2)
}
mse
mean(mse)

# 교차검증을 하는 이유? 
# 신뢰도 검사.


t_index<-sample(1:nrow(autoparts2),size=nrow(autoparts2)*0.7)
train<-autoparts2[t_index,]
test<-autoparts2[-t_index,]

nrow(train)
nrow(test)

m<-lm(c_thickness~.,data=train)
m_pred<-predict(m,test)
mean((test$c_thickness-m_pred)^2)
#------------------------------------------------------------------------------

xmat<-as.matrix(autoparts2[1:9])
yvac<-autoparts2$c_thickness

#install.packages("glmnet")

library(glmnet)

# alpha=1 로 했을때를 LASSO라고 한다. 
# alpha는 비용이라고 생각하면 편하다. 비용이 클수록 오차가 커진다.(MSE)

# 람다에 따라 과대,과소 적합을 판단 할 수 있다. 

fit.lasso<-glmnet(x=xmat,y=yvac,alpha=1,nlambda=100)
fit.lasso.cv<-cv.glmnet(x=xmat,y=yvac,nfolds=10,alpha=1,lambda=fit.lasso$lambda)
plot(fit.lasso.cv)

fit.lasso.param<-fit.lasso.cv$lambda.min
fit.lasso.tune<-glmnet(x=xmat,y=yvac,alpha=1,lambda=fit.lasso.param)
coef(fit.lasso.tune)

#-------------------------연습문제.... ---------------------------------------------------------

# LASSO 중요합니다. !!!!! 
lasso<-read.csv("Occupancy_train.csv",sep=",",header=T,stringsAsFactors = F)

# 독립변수들을 matrix로 만들고
# 종속변수를 저장합니다. 

lasso.xmat<-as.matrix(lasso[2:6])
lasso.yvec<-lasso$Occupancy


# lambda를 100개를 생성합니다.
fit.lasso<-glmnet(x=lasso.xmat,y=lasso.yvec, alpha=1, nlambda = 100)

# k=10 개인 k-fold를 생성합니다.  
flt.lasso.cv<-cv.glmnet(x=lasso.xmat,y=lasso.yvec,nfolds=10,alpha=1,lambda = fit.lasso$lambda)
plot(fit.lasso.cv)

# lambda의 가장 작은 값을 선택합니다.
fit.lasso.param<-fit.lasso.cv$lambda.min

# 가장 작은 lambda를 구합니다. 
fit.lasso.tune<-glmnet(x=lasso.xmat,y=lasso.yvec,alpha=1,lambda=fit.lasso.param)
coef(fit.lasso.tune)
  
fit.lasso<-glmnet(x=lasso.xmat,y=lasso.yvec,alpha=1,nlambda = 100,family="binomial")
fit.lasso.cv<-cv.glmnet(x=lasso.xmat,y=lasso.yvec,nfolds=10,alpha=1,lambda = fit.lasso$lambda,family="binomial")
fit.lasso.param<-fit.lasso.cv$lambda.1se
fit.lasso.tune<-glmnet(x=lasso.xmat,y=lasso.yvec,alpha=1,lambda=fit.lasso.param,family = "binomial")
coef(fit.lasso.tune)





































































