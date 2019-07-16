#20190716 수업 EDA 탐색적 데이터 분석 

setwd("D:/BigDataCampus")
autoparts=read.csv("autoparts.csv",header=TRUE)

autoparts1<-autoparts[autoparts$prod_no=="90784-76001",c(2:11)]
autoparts1
dim(autoparts1)

# 행에 결측치가 있는지를 알려준다.  is.na() 와 비슷하다. 
#?complete.cases
autoparts1[!complete.cases(autoparts1),]
autoparts1[is.na(autoparts1),]

# 기초 통계량을 알려줍니다. 
# highpressure_time 과 c_thickness 의 max값과 평균값의 차이가 큽니다. 
# 이상치가 존재함을 예상해 볼 수 있습니다. 

summary(autoparts1)

# summary를 boxplot으로 시각화 해서 보여줍니다. 
boxplot(autoparts1)
# ?boxplot

# 각각 하나의 열을 시각화 합니다. 
boxplot(autoparts1$separation)
boxplot(autoparts1$rate_terms)

# 이상치를 제거합니다. 
boxplot(autoparts1$c_thickness)

# 조건을 주어서 이상치를 제거합니다. 
# 1000보다 큰 이상치가 12개 입니다. 
autoparts2<-autoparts1[autoparts1$c_thickness<1000,]
dim(autoparts2)
boxplot(autoparts2$c_thickness)


# plot를 통해서 변수의 분포를 대략적으로 판단할 수 있습니다. 
# c_thickness의 20~30값이 주로 분포 되어있습니다. 
# 나머지 범위는 정상이 아닙니다. 
plot(autoparts2$c_thickness)

# 구간별 데이터 분포를 알아보고자 할 때, histogram을 사용합니다. 
# breaks 옵션을 주면 사용자가 구간 수를 설정 할 수 있습니다. 
hist(autoparts2$c_thickness,breaks = 100)

x<-autoparts2$c_thickness
library(moments)
# skewness (왜도)가 양의 값이면 왼쪽으로 쏠려있고, 음의값이면 오른쪽으로 쏠려있습니니다. 
skewness(x)
# kurtosis (첨도)는 데이터의 뾰족한 정도, 즉 분산정도를 알 수 있습니다. 
kurtosis(x)

# 확률 밀도 함수를 그립니다.
plot(density(x))
# 정규분포 그래프를 그립니다.
# 정규분포 그래프는 빨간색입니다. 
den.norm<-function(x) dnorm(x,mean=mean(x),sd=sd(x))
curve(den.norm,col="red",add=TRUE,lty=2)

# 확률밀도 그래프의 평균값의 위치에 파란색 수직선을 그립니다. 
abline(v=mean(x),col="blue",lty=2)

#-----------------------연습문제--------------------------

library(dplyr)

DATA=read.csv("train.csv",sep=",",header=T,stringsAsFactors = T)
head(DATA)

DATA_MALE=DATA %>% filter(Sex=="male")
DATA_FEMALE=DATA %>% filter(Sex=="female")

DATA_MALE_SURVIVED=mean(DATA_MALE$Survived)
DATA_FEMALE_SURVIVED=mean(DATA_FEMALE$Survived)

# Fare와 Pclass가 Survived와 상관관계가 얼마나 있는지 수치적으로 알 수 있다. 
cor(DATA$Survived,DATA$Fare) # 0.2573065
cor(DATA$Survived,DATA$Pclass) # -0.338481

#DATA_MALE_SURVIVED   #CHECK용 
#DATA_FEMALE_SURVIVED #CHECK 용 


PIMA=read.csv("pima-indians-diabetes.csv",sep=",",header=T,stringsAsFactors = T)
str(PIMA)
# 데이터 프레임 형태
# 768행과 9개 변수 
# BMI,DiabetesPedigreeFunction은 num 형태, 나머지 변수는 int data-type입니다. 

summary(PIMA)

# 인슐린의 이상치가 굉장히 많다. 
boxplot(PIMA)
hist(PIMA$Pregnant)
hist(PIMA$Glucose)


PIMA %>% filter(Outcome==1) %>% boxplot()
PIMA %>% filter(Outcome==0) %>% boxplot()

#------------------------------------------------------------------

# 획득 점수입니다. 
score<-c(85,90,93,86,82)

# 평균, 중앙값, 분산, 표준편차 결과를 반환 
mean(score)
median(score)
var(score)
sd(score)


# 자유도 t분포.. 

# result<-matrix()
# 
# 
# A_result<-(1200-1000)/200
# A_result<-1*10+50
# A_result

# rnorm 100만개의 정규분포를 만든다. 
height<-rnorm(n=1000000,mean=168,sd=7)

#?hist

hist(height,breaks=1000,probability = T)

height<-rnorm(n=1000000,mean=168,sd=7)
hist(height,breaks=30,probability = T)
lines(density(height,bw=0.5),col="red",lwd=1)

getwd()
score<-read.csv("tdata.csv",header = T)
str(score)

result<-t.test(score$성적,alternative=c("greater"),mu=75)
# p-value 값이 0.254입니다.
# 0.05보다 크므로 귀무가설을 선택합니다. 
#result

score2<-read.csv("tdata2.csv",header=T)
score2

# p-value 값이 0.001707입니다. 
# 0.05보다 크므로 귀무가설을 기각하고,
# 연구가설을 선택합니다.
result.score2<-t.test(score2$성적,alternative=c("greater"),mu=75)
result.score2

fruits1<-read.csv("love_fruits.csv",header=T)
fruits1

# 빈도를 계산합니다. 
round(prop.table(table(fruits1$선호과일))*100,2)

table(fruits1$선호과일)

count<-c(table(fruits1$선호과일))
pct<-c(round(prop.table(table(fruits1$선호과일))*100,2))
love_fruits<-data.frame(건수=count,비율=pct)
love_fruits

# bar차트와 pie 로 데이터를 시각화 합니다. 
barplot(love_fruits$건수,names.arg=c("바나나","복숭아","사과","체리","포도"),ylim=c(0,15),col=rainbow(5))
pie(love_fruits$건수,col=rainbow(5),init.angle = 90,label=love_fruits$비율)

# legend는 범례로서 제목을 보여줍니다.
legend(0.8,1,c("바나나","복숭아","사과","체리","포도"),cex=0.7,fill=rainbow(5))

install.packages("gmodels")
library(gmodels)
study<-read.csv("pass_cross.csv",header = T)

# p-vaule=0.0832 이무로  다른 ㅂㄴ수들}
CrossTable(study$공부함,study$합격,chisq=T)

x<-c93,5,8,11,13

x<-c( 110,120,130,140,150)
y<-c(100,105,128,115,142)

plot(x,y,pch=20,col="red")

line<-lm(y~x)
line
abline(line,col="blue")


score<-read.table("score.txt",header=T,sep=",")
#score
attach(score)
#?attach

# lm 예측 모형 
lm1<-lm(성적~IQ)
y<- -5.2918+0.6714*125
#y
plot(IQ,성적,pch=20,col="red")
abline(lm1,col="blue")

coef(lm1)
#?coef

predict(lm1,newdata=data.frame(x=c(125,125,125,125,125,125,125,125,125,125)))

# 다중 회귀 분석 
lm3<- lm(성적~IQ+다니는학원수+게임하는시간+TV시청시간)
lm3

y=23.2992+(0.4684*130)+(0.7179*3)-(0.8390*2)-(1.3854*1)
# 예상 점수 입니다. 
y


























