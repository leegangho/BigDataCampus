#20190710 

x=c(6.5,4.0,7.1,8.3,5.4,7.6,9.0,15.7,16.7,6.4,5.0,8.5,5.7,7.7,7.2,12.4,7.1,5.5,9.7,4.4,7.0,6.3,8.3,6.9,5.7,7.6,7.9,7.9,6.0,8.2,10.4,9.9,3.9,9.8,8.2,5.6,7.9,6.4,7.4,7.0,13.0,8.7,6.4,6.7,7.4)
x.num=length(x)
x.num
x.classNum=round(x.num^(1/3))
x.classNum
x.classInterval=round((max(x)-min(x))/x.classNum)
x.classInterval

x.floor=floor(min(x))
rest=ceiling(max(x))%%x.classInterval
supplement=x.classInterval-rest
x.ceiling=ifelse(rest==0,ceiling(max(x)),ceiling(max(x))+supplement)
x.cut=cut(x,breaks=seq(x.floor,x.ceiling,by=x.classInterval),rignt=FALSE)
x.levels=gsub(",","~",levels(x.cut))
x.cut=cut(x,breaks=seq(x.floor,x.ceiling,by=x.classInterval),right=FALSE)
table(x.cut)


x.table=table(x.cut)
round(prop.table(x.table),digits=3)
sum(round(prop.table(x.table),digits=3))


x=c(6.5,4.0,7.1,8.3,5.4,7.6,9.0,15.7,16.7,6.4,5.0,8.5,5.7,7.7,7.2,12.4,7.1,5.5,9.7,4.4,7.0,6.3,8.3,6.9,5.7,7.6,7.9,7.9,6.0,8.2,10.4,9.9,3.9,9.8,8.2,5.6,7.9,6.4,7.4,7.0,13.0,8.7,6.4,6.7,7.4)
plot(x)
boxplot(x)
hist(x)
table(x)
barplot(x,border=TRUE,title("Hi!!!!"))


install.packages("moments")
library(moments)
skewness(x)
kurtosis(x)



plot(density(x))
den.norm=function(x) dnorm(x,mean=mean(x),sd=sd(x))
curve(den.norm,col="red",add=TRUE,lty=2)
abline(v=mean(x),col="blue",lty=2)
mean(x)



#Titanic histogram 표현
titanic_practice=read.csv("titanic.csv",header=TRUE,sep="\t",stringsAsFactors = F)
titanic_practice

#Age 부분에서 왜도,첨도,히스토그램 표현
AGE=titanic_practice[,"Age"]
hist(AGE)
stem(AGE)
skewness(AGE,na.rm=TRUE) #왜도
kurtosis(AGE,na.rm=TRUE) #첨도

#PCLASS 부분에서 왜도,첨도,히스토그램 표현
PCLASS=titanic_practice[,"Pclass"]
hist(PCLASS)
stem(PCLASS)
skewness(PCLASS)
kurtosis(PCLASS)

#FARE 부분에서 왜도,첨도,히스토그램 표현
FARE=titanic_practice[,"Fare"]
hist(FARE)
stem(FARE)
skewness(FARE)
kurtosis(FARE)



#------------- 연습-----------
#정규본포 표 그리기 
x=seq(-5,5,length=1000)
y=dnorm(x,mean=0,sd=1)
plot(x,y,type="l",lwd=1)

den.norm=function(x) dnorm(x,mean=0,sd=2)
curve(den.norm,col="red",add=TRUE,lty=2)
den.norm=function(x) dnorm(x,mean=0,sd=3)
curve(den.norm,col="blue",add=TRUE,lty=2)

x=c(1:10)
sample(x,5)
sample(x,5,replace=TRUE)

?sample

#가중치는 단지 확률이지 정렬되서 return 되지 않음 
x=c("A","B","C","D","E")
prob=c(6,1,5,1,3)
sample(x,5,replace = FALSE,prob=prob)



#층화 임의 추출 
# TEST데이터를 확보하는데 활용할 수 있는 함수(?)
install.packages("sampling")
library(sampling)
iris
#자동으로 Species 종류를 나눠주는거 같다
#종류와 size의 개수가 다르면 error
strata(c("Species"),size=c(2,3,4),method="srswor",data=iris)




#독립성 검정 
#예제 
library(MASS)
data("survey")
head(survey)

str(survey)
survey

xt=xtabs(~Sex+Exer,data=survey)
xt
chisq.test(xt)


#데이터가 편향되어있는 경우 피셔의 검정을 사용한다.
#글씨 쓰는 손과 박수칠때 위로 올라오는 손고 관계가 있다 
xt=xtabs(~W.Hnd+Clap,data=survey)
chisq.test(xt)
fisher.test(xt)

#카이사와 피셔는 데이터 테이블 형태를 인수로 받는다. 

#맥니마 검정은 matrix로 인수를 받는다. 
#어떤 사건의 전후로 응답자의 성형 등을 조사할 때 사용한다. 
#1차때 승인한 사람들이 2차때도 승인을 하는가?  2x2행렬의 (0,0)셀 
#귀무가설: 1차승인과 2차 승인의 관계가 없을것이다. 
#대립가설: 1차승인과 2차 승인이 관계가 있을것이다. 

Performence=matrix(c(794,86,150,570),nrow=2,dimnames=list("1st Survey"=c("Approve","Disapprove"),"2nd Survey"=c("Approve","Disapprove")))
Performence
mcnemar.test(Performence)
# p-value=4.11e-05 이므로 0.05보다 작다. 
# 귀무가설을 기각한다. 

?chisq.test

xt=table(survey$W.Hnd)
xt
chisq.test(xt,p=c(0.3,0.7))
?table
?rnorm
x=rnorm(1000)
x
shapiro.test(x)
# p-value=0.3649 이므로 0.05보다 크므로 데이터가 정규분포를 따른다. 

# 두개의 생성된 난수들은 같은 분포를 띈다 
# because p-value=0.6982 이므로 0.05이상이다. 
# 따라서 귀무가설을 채택한다. 

ks.test(rnorm(100),rnorm(90))


setwd("D:/BigDataCampus")

titanic_practice=read.csv("titanic.csv",header=TRUE,sep="\t",stringsAsFactors = F)
titanic_practice
Data=as.factor(titanic_practice$Survived)
Data
str(Data)

xt=xtabs(~Sex+Survived,data=titanic_practice)
xt
chisq.test(xt)
# p-value=1.7e-12이므로 0,05보다 작다. 따라서 대립가설을 채택한다. 
# 성별과 생존여부가 관련있다. 

tr=xtabs(~Ticket+Survived,data=titanic_practice)
tr
chisq.test(tr)
fisher.test(tr)
# P-VALUE=는 2.2e-16이므로 0.05보다 작다. 따라서 대립가설을 채택한다. 
# 좌석등급과 관련있다. 

nr=xtabs(~Name+Survived,data=titanic_practice)
nr
chisq.test(nr)


group=c("A","A","B","B")
cancer=c("1.Yes","2.No","1.Yes","2.No")
count=c(2,28,5,25)
dat=data.frame(group,cancer,count)
tab=xtabs(count~group+cancer,data=dat)
tab


iris
# 상관관계
cor(iris$Sepal.Width,iris$Sepal.Length)

x1=c(1,3,6,8,10)
x2=c(-2,3,8,5,9)
cor(x1,x2)

x=iris[,1:4]
head(x)
cor(x)
symnum(cor(x))

#install.packages("corrgram")
library(corrgram)
corrgram(x)
corrgram(iris,upper.panel = panel.conf)
?corrgram


kor=c(85,90,87,92,95)
eng=c(88,89,68,84,91)
m=matrix(c(kor,eng),ncol=2)
m
cor(m,method = "spearman")
# 국어와 영어의 순위 상관관계는 0.5 정도 





