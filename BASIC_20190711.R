#20190711


iris
#상관분석은 양적관계에서만 적용되며
#상관분석 이후 인과관계까지 가야 의미가 있다.
cor(iris$Sepal.Width,iris$Petal.Length)

x1=c(1,3,6,8,10)
x2=c(-2,3,8,5,9)
cor(x2,x1)

x=iris[,1:4]
head(x)
cor(x)
symnum(cor(x))

library(corrgram)
corrgram(x)
#상관계수의 수치를 보여준다. upper.panel=panel.conf
# -> 어차피 행렬로 대칭으로 보여주기때문에 위쪽에 수치를 보여줄때 사용한다. 
corrgram(iris,upper.panel=panel.conf)


kor=c(85,90,87,92,95)
eng=c(88,89,68,84,91)
m=matrix(c(kor,eng),ncol=2)
cor(m,method="spearman")

# 앞서 결과를 뽑아낸 상관관계의 값들이 유효한지 판단한다.
# 대립가설=alternative hypothesis: true correlation is not equal to 0

cor.test(x1,x2) # pearson's
cor.test(x1,x2,method="spearman") #Spearman's

setwd("D:/BigDataCampus")
titanic=read.csv("titanic.csv",sep="\t",header=T,stringsAsFactors = T)
pclass=titanic[,"Pclass"]
fare=titanic[,"Fare"]

cor(pclass,fare)
# 타이타닉 지료에서 좌석과 운임료의 상관관계를 알아본다.
#-0.6072558 이며,음의 값이므로 관계가 없다. 

for_corr=data.frame(titanic$Pclass,titanic$Fare)
colnames(for_corr)=c("Pclass","Fare")
for_corr$Sex=as.integer(titanic$Sex)
for_corr$Survived=as.integer(titanic$Survived)
for_corr
str(for_corr)
cor(for_corr)

corrgram(for_corr,upper.panel = panel.conf)
?data.frame
#--------복습 끝-----------------

alternative1=c("two.sided")#,"less","greater")
mu=0
#정규분포를 따르는 랜덤 값들을 만들어라 x=rnorm(30)
x=rnorm(30)
x #check용
#일표본평균
#하나의 모집단으로부터 표본을 추출하여 평균 신뢰구간을 구한다. 
t.test(x)


sleep
str(sleep)
head(sleep)
tail(sleep)
sleep2=sleep[,c(1,2)]
head(sleep2)

#p-value가 0.05보다 크므로 귀무가설을 채택한다. 
#모분산이 같다 
var.test(extra~group,sleep2)

#P-VALUE가 0.07919이므로 귀무가설을 채택한다.
#유의한 차이가 없다. 
t.test(extra~group,data=sleep2,paired=FALSE,var.equal=TRUE)

#p-value가 0.05보다 작으므로 귀무가설:'모평균의 차이가 0이다'를 기각한다.
#따라서 대립가설인 "모평균과 차이가 있다."
t.test(sleep$extra[sleep$group==1],sleep$extra[sleep$group==2],paired=TRUE)

#100중 42명의 찬성을 얻을경우, 찬성비율을 50%라고 말하는것이 가능한가? 
#p-value=0.1336 > 0.05 이므로 귀무가설을 채택하고 
# 따라서 50%라고 말할 수 있다. 

prop.test(42,100)
prop.test(42,100,0.5)
?prop.test

binom.test(42,100)
binom.test(42,100,0.5)

#이표본 비율
#두 표본으로부터 모집단의 비율을 추정 및 가설 검정할 때 사용한다. 
prop.test(c(45,55),c(100,90))

#이제 전처리 시작-----------------------------------------------------------

#특성공학
# 1. scaling 1-0사이의 값들로 변환 즉, 자료의 재표현.


getwd()
setwd("D:/BigDataCampus")
scailing_pratice=read.csv("Scailing.csv",sep=',',header=T,stringsAsFactors = T)
scailing_pratice
pampas=scailing_pratice[1]
milk=scailing_pratice[2]
tissue=scailing_pratice[3]
pampas
milk
tissue

str(scailing_pratice)

#plot(scailing_pratice$癤퓈ampas,type="o",col="blue")
#lines(scailing_pratice$milk,type="o",pch=21,lty=2,col="red")
#lines(scailing_pratice$tissue,type="o",pch=22,lty=3,col="green")


iris
str(iris)
head(iris)
sc=scale(iris[1:4]); head(sc)
#1:4의 열들은 num형이다. 따라서 df.frame으로 만들고 Species를 합치기 위해 data.frame으로 만들어 준다. 
#df=as.data.frame(sc); head(df)

#Species와 나머지 열들을 합친다. 
cb=cbind(df,iris$Species); head(cb)
cb



getwd()
data=read.csv("agemoney.csv",header=T,stringsAsFactors = T)
age=c("")
for (i in 1:5){
  age=mean(data[data$age >=i*10 $ data$age<(i+1)*10,2])
}

age10=mean(data[data$age>=10 & data$age < 20,2])
age20=mean(data[data$age>=20 & data$age < 30,2])
age30=mean(data[data$age>=30 & data$age < 40,2])
age40=mean(data[data$age>=40 & data$age < 50,2])
age50=mean(data[data$age>=50 & data$age < 60,2])
age60=mean(data[data$age>=60 & data$age < 70,2])


data=read.csv("creating_feature.csv",header=T,stringsAsFactors = T,sep=',')
str(data)

data[,"판매일"]
#test1=c("상품명","A001","A002","A003","A004")
#test2=c("성별","남","여","여","여")
#test3=c("판매일","2016-11-01")

date.txt=c("2016-11-01","2016-11-01","2016-11-03","2016-11-05")
date.txt
class(date.txt)

date.as.date=as.Date(date.txt)
date.as.date
class(date.as.date)


date.as.date.week.full=format(date.as.date,format="%Y-%m-%A")
date.as.date.week.full

date.as.date.week.only=format(date.as.date,format="%A")
date.as.date.week.only

?format



v=factor(c("A","B","A","A","C"))
v
df=data.frame(v); df
str(df)

dv=model.matrix(~v,data=df)[,-1]; dv


#--------------------------------------------------
date.txt=c("2016-11-01","2017-11-01","2018-11-03","2019-11-05")
date1=as.Date(date.txt)
date1=format(date1,format="%A")
date1

date2=as.Date(date.txt)
date2=format(date2,format="%y-%m")
date2

data=read.csv("titanic.csv",sep="\t",header=T,stringsAsFactors = T)
str(data)



titanic_age20=mean(data[data$Age>=10 & data$Age<20,2],na.rm=TRUE)
titanic_age30=mean(data[data$Age>=20 & data$Age<30,2],na.rm=TRUE)
titanic_age40=mean(data[data$Age>=30 & data$Age<40,2],na.rm=TRUE)
titanic_age50=mean(data[data$Age>=40 & data$Age<50,2],na.rm=TRUE)
titanic_age60=mean(data[data$Age>=50 & data$Age<60,2],na.rm=TRUE)
titanic_age70=mean(data[data$Age>=60 & data$Age<70,2],na.rm=TRUE)


#------------------------------------------------------------------
#DataFrame 형태로 만들 수 있다. 
getwd()
test=read.csv("test.csv",sep=",",header=T,stringsAsFactors = T)
class(test)
str(test)
x=c(1,2,3,4,5)
y=c(10,20,30,40,50)
z=c("M","M","M","F","F")
d=data.frame(x,y,z)

id=c(1,2,3,4,5)
factory=c("평택지부1","평택지부2","안산지부","인천지부","원주지부")
type=c("공장","공장","오피스","오피스","오피스")
sales=c(70,90,80,85,87)
d=data.frame(id,factory,sales,type,stringsAsFactors = F); d
str(d)

d$type=as.factor(d$type)
d
str(d)


id=1:7
name=c("김원경","박찬웅","조해선","김선영","이화영","양영욱","최필선")
gender=c("F","M","F","F","F","M","M")
sales=c(1000,2000,1500,2200,1700,2000,2200)
d=data.frame(id,name,gender,sales,stringsAsFactors = F)
d.col=d$name
d
d.col=d[,2]
d
d.col=d[,"name"]
d


d.cols=d[,2:4]
d.cols

d.row=d[2,]
d.element=d[2,3]
d.row
d.element

#   d[d$gender=="M", ]   무조건 행과 열의 구분을 위해 , 를 입력해줘야 한다. ! 
d.man=d[d$gender=="M",]
d.man

d.woman=d[d$gender=='F',]
d.woman


d[d$sales>2000,]
d[(d$gender=="F")&(d$sales>2000),]
d[(d$gender=="F")|(d$sales>2000),]

#which는 index를 가져온다
d.man=d[which(d$gender=="M"),]
d.man

d.woman=d[which(d$gender=="F"),]
d.woman

d.man=subset(d,gender=="M")
d.man
subset(d,gender=="F",select=c(name,gender))

subset(d,sales>2000)
subset(d,gender=="F"&sales>2000)
subset(d,gender=="F"|sales>2000)
subset(d,gender=="F"|sales>2000,select=c(name,gender))


#install.packages("dplyr")
library(dplyr)
d.man=d%>%filter(gender=="M")
d.man

d %>% filter(sales>2000)
d %>% filter(gender=="F"&sales>2000)
d %>% filter(gender=="F"|sales>2000)


#---- 실습--- 
test
basic1=test[test$sales>5,]
basic1

basic2=test[test$price>=300 & test$sales>=5,]
basic2

basic3=subset(basic2,select=c(item))
basic3

d
head(d)
View(d)

x1=c(1,2,3)
x2=c(4,5,6)
x3=c(7,8,9)
d=data.frame(x1,x2,x3); d
x4=c(10,11,12)
d2=cbind(d,x4); d2
x5=c("James","Mary","Tony")
d3=cbind(d2,x5,stringsAsFactors=F); d3



x1=c(1,2,3)
x2=c(4,5,6)
x3=c(7,8,9)
d=data.frame(x1,x2,x3); d
x4=c(10,11,12)
d$x4=x4;d



x1=c(1,2,3)
x2=c(4,5,6)
x3=c(7,8,9)
d=data.frame(x1,x2,x3); d
d$sum=d$x1+d$x2+d$x3; d
d$pass=ifelse(d$sum>15,"pass","fail");d


a=data.frame(id=c(1,2,3,4,5),mid=c(30,40,50,60,70))
b=data.frame(id=c(6,5,4,3,2,1),final=c(80,70,90,100,90,80))
a;b
library(dplyr)
left_join(a,b,by="id")

old1=c(1,2,3)
old2=c(4,5,6)
old3=c(7,8,9)
d=data.frame(old1,old2,old3); d

d=rename(d,new1=old1)
d
d[-1]
d

data=read.csv("titanic.csv",sep="\t",header=T,stringsAsFactors = T)
data
data_male=data[data$Sex=="male",]
data_female=data[data$Sex=="fmale",]

