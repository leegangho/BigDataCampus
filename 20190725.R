#20190725 

str(AirPassengers)
AirPassengers
plot(AirPassengers)

apts<-ts(AirPassengers,frequency = 12)
f<-decompose(apts)
plot(f)

#install.packages("tseries")
#install.packages("forecast")
library(tseries)
library(forecast)
plot(stl(AirPassengers,s.window="periodic"))

adf.test(diff(log(AirPassengers)),alternative="stationary",k=0)
adf.test(AirPassengers,alternative = "stationary",k=0)

tsdiag(auto.arima(diff(log(AirPassengers))))

auto.arima(AirPassengers)
#(2,1,1)(0,1,0)
#(p,d,q)(p,d,q)

fit<-arima(AirPassengers,order=c(2,1,1),list(order=c(0,1,0),period=12))
fore<-predict(fit,n.ahead=24)
U<-fore$pred+2*fore$se
L<-fore$pred-2*fore$se
ts.plot(AirPassengers,fore$pred,U,L,col=c(1,2,4,4),lty=c(1,1,2,2))
legend("topleft",c("Actual","Forecast","Error Bounds (95% Confidence)"),col=c(1,2,4),lty=c(1,1,2))
#---------------------------------------------------
# 특정 컬럼을 제외하고 plot을 볼 수 있습니다. 
ldeaths
plot(ldeaths)
ldeaths.decompose<-decompose(ldeaths)
ldeaths.decompose$seasonal
plot(ldeaths.decompose)
ldeaths.decompose.adj<-ldeaths-ldeaths.decompose$seasonal
plot(ldeaths.decompose.adj)


Nile
plot(Nile)
Nile.diff1<-diff(Nile,differences=1)
plot(Nile.diff1)
Nile.diff2<-diff(Nile,differences=2)
plot(Nile.diff2)
acf(Nile.diff2,lag.max = 20)
acf(Nile.diff2,lag.max = 20,plot=F)
pacf(Nile.diff2,lag.max = 20)
pacf(Nile.diff2,lag.max = 20,plot=F)

library(forecast)
auto.arima(Nile)
Nile.arima<-arima(Nile,order=c(1,1,1))
Nile.arima
Nile.forecasts<-forecast(Nile.arima,h=10)
Nile.forecasts
plot(Nile.forecasts)


airquality
plot(airquality)

airquality.decompose<-decompose(airquality$Ozone)


airquality.Ozone<-airquality$Ozone
plot(airquality.Ozone)

airquality.Ozone.diff1<-diff(airquality$Ozone,differences = 1)
plot(airquality.Ozone.diff1)

auto.arima(airquality)
#--------------------------------------------------------------------------

install.packages("adabag")
library(adabag)
data(iris)
iris.bagging<-bagging(Species~.,data=iris,mfinal=10)
iris.bagging$importance
plot(iris.bagging$trees[[10]])
text(iris.bagging$trees[[10]])

pred<-predict(iris.bagging,newdata=iris)
table(pred$class,iris[,5])

boo.adabag<-boosting(Species~.,data=iris,boos=TRUE,mfinal=10)
boo.adabag$importance

plot(boo.adabag$trees[[10]])
text(boo.adabag$trees[[10]])

pred<-predict(boo.adabag,newdata=iris)
tb<-table(pred$class,iris[,5])
tb

error.rpart<-1-(sum(diag(tb))/sum(tb))
error.rpart

#install.packages("ada")
library(ada)
data(iris)
iris<-iris[iris$Species != "setosa",]
n<-dim(iris)[1]
n

trind<-sample(1:100,floor(.6*100),F)
teind<-setdiff(1:100,trind)
iris[,5]<-as.factor(((levels(iris[,5])[2:3])[as.numeric(iris[,5])-1]))

gdis<-ada(Species~.,data=iris[trind,],iter=20,nu=1,type="discrete")
gdis<-addtest(gdis,iris[teind,-5],iris[teind,5])
gdis

plot(gdis,TRUE,TRUE)
varplot(gdis)
pairs(gdis,iris[trind,-5],maxvar=4)


install.packages("randomFrorest")
library(randomForest)
stagec

train<-sample(nrow(stagec), nrow(stagec)*0.7)
trainData<-na.omit(stagec[train, ])
testData<-na.omit(stagec[-train, ])
rf <- randomForest(ploidy ~., data=trainData, ntree=100, proximity=TRUE)
table(predict(rf), trainData$ploidy)
print(rf)
plot(rf)

importance(rf)
varImpPlot(rf)

rf.pred<-predict(rf,newdata=testData)
table(rf.pred,testData$ploidy)
plot(margin(rf))

library(Epi)

ROC(test=rf.pred,stat = testData$ploidy,plot="ROC",AUC=T,main="randomForest")













