#20190715 경희대 R을 활용한 빅데이터 전문가 양성 과정 

install.packages("car")
library("mtcars")
setwd('D:/BigDataCampus')
getwd()

install.packages("DMwR")
mtcars
tapply(mtcars$hp,mtcars$gear,sum)
#------------------------------------------------

x<-c(1,2,3,4,5)
y<-c(10,20,30,40,50)
d=data.frame(x,y)
d
# 새롭게 행을 추가합니다. (병합) 벡터와 벡터, 같은 종류일때 
rbind(d,c(6,60))

d<-rbind(iris,c(1,2,3,"setosa"))
#integer 형이었던 column들이 char 형으로 바뀐다.... 

#새로운 data.frame을 만들고,합친다. 
d1<-data.frame(Sepal.Length=1,Sepal.Width=2,Petal.Length=3,Petal.Width=4,Species="setosa")
d2=rbind(iris,d1)
d2

#분석에 이용되는 데이터 테이블을 만들때 유용하게 사용한다. rbind



#iris
iris.sepal.lw<-cbind(iris$Sepal.Length,iris$Sepal.Width)
head(iris.sepal.lw)
class(iris.sepal.lw)
#cbind는 기본적으로 matrix형태를 만든다. 행렬

x<-c(1,2,3,4,5)
y<-c(10,20,30,40,50)
z<-c("M","M","M","F","F")
d=data.frame(x)
str(d)
d=cbind(d,y,z)
str(d)
d=cbind(d,y,z,stringAsFactors=F)
d
str(d)

# $를 이용하면 data type을 유지시키며 data.frame에 추가할 수 있다. 
x=c(1,2,3,4,5)
y=c(10,20,30,40,50)
z=c("M","M","M","F","F")
data.frame(x)
d$y=y
d$z=z
str(d)

iris
str(iris)
# 다른 data type을 묶어야 하므로 $를 이용하여 data.frame형태를 완성시킴 
iris_practice=data.frame(iris$Sepal.Length,iris$Sepal.Width,iris$Species)
str(iris_practice)



# merge()는 공통 컬럼을 기준으로 통합합니다. 
name<-c("James","Mary","John")
math<-c(70,80,90)
d1<-data.frame(name,math)
d1

name<-c("James","John","Mary")
english<-c(80,70,90)
d2=data.frame(name,english)
d2

# 이름을 기준으로 통합합니다. 결과는 데이터프레임을 반환합니다. 
what=merge(d1,d2)
str(what)

#merge와 cbind는 다르다. cbind는 공통된 컬럼도 출력한다. 
#cbind(d1,d2) #check용 


name<-c("James","Mary","John")
age<-c(72,73,74)
math<-c(70,80,90)
d1<-data.frame(name,age,math)

name<-c("James","John","Mary")
age<-c(72,74,73)
english<-c(80,70,90)
d2<-data.frame(name,age,english)
#d1 #check
#d2 #check

merge(d1,d2,by="age")
#---------------------------------------------

# 합쳤을때, 공통된게 없으면 해당 행을 보여주지 않는다. 
# 다만, merge(,, all=TRUE) 를 하면 전부 보여준다. 
# by= , 을 하면 열을 기준으로 바꾼다. 
name=c("James","Mary","John","Lion")
age=c(72,73,74,89)
math=c(70,80,90,100)
d1=data.frame(name,age,math)

name=c("James","John","Mary")
age=c(72,74,73)
english=c(80,70,90)
d2=data.frame(name,year,english)
#merge(d1,d2,all=TRUE)

#연습 문제 

Titanic=read.csv("titanic.csv",sep='\t',header=T)

selected=data.frame(Titanic$Pclass,Titanic$Sex,Titanic$Age,Titanic$Parch,Titanic$Fare,Titanic$Embarked)
selected
#------------------------------------------
# summary()는 데이터의 최소값, 1사분위값, 중앙값,평균,3사분위값, 최대값을 출력한다. 
summary(iris)
summary(iris$Sepal.Length)

install.packages("doBy")
library(doBy)

# Sepal.Length 해당 컬럼에 대하여 Species별로 요약한다. 
summaryBy(Sepal.Length~Species,iris)

# 2개의 column에 대하여 species별로 요약한다. 
summaryBy(Sepal.Length+Sepal.Width~Species,iris)

#tapply와 유사하게 사용할 수 있다. 

#--------------------------------------------------
#정렬 

x<-c(1,30,22,27,51,38,45)
sort(x)
sort(iris$Sepal.Length)
sort(iris$Sepal.Length,decreasing = T)
#----------------------------------------------------
#order는 index를 반환한다.

iris$Sepal.Length
order(iris$Sepal.Length)
i<-order(iris$Sepal.Length)
iris$Sepal.Length[i]

# 정렬하고 index를 반환하여 data.frame을 재구성한다. 
index=iris[order(iris$Sepal.Width),]
# 정렬 기준이 2개의 column이다. 역시 index를 반환합니다. 
iris[order(iris$Sepal.Width,iris$Petal.Width),]

# wit는 $표시 없이 바로 column이름을 사용할 수 있는 장점이 있다. 
# 하지만 똑같다.... 

with(iris,sum(Sepal.Length))
sum(iris$Sepal.Length)

# 조건문을 활용하여 결과값을 넣을 수 있다. 새로운 COLUMN을 추가 
with(iris,ifelse(Sepal.Length>5.0,"High","Low"))
iris$Sepal.Length.Judge<-with(iris,ifelse(Sepal.Length>5.0,"High","LOW"))

#head(iris) #CHECK 용 

data(iris)
with(iris,ifelse(Sepal.Length>5.0,"High","Low"))

#within은 직접 데이터를 수정합니다. 
data(iris)
within(iris,ifelse(Sepal.Length>5.0,"High","Low"))

data(iris)
# 지정된 컬럼의 내용을 변경한다. 직접 데이터가 수정되는 것을 볼 수 있다. 
within(iris,Sepal.Practice<-ifelse(Sepal.Length>5.0,"High","Low"))


# 결측치를 새로운 값으로 대체합니다. 
data(iris)
#임의의 결측치를 [1,1]에 만듭니다. 
iris[1,1]=NA; head(iris)

?split
# Species를 기준으로 length를 구분합니다. 
per_species<-split(iris$Sepal.Length,iris$Species)
per_species

# sapply를 적용하여 mean, 즉 평균을 구합니다. 
median_per_species<-sapply(per_species,median,na.rm=TRUE)
# 각 종별로 평균값을 만들어 냅니다. 
median_per_species

class(median_per_species)
head(iris)

#결측치를 대체하는 코드입니다. 
#lfelse를 적용시키고, is.na로 na를 검색하고, true이면 평균치를 대입, false면 그대로 length값을 대체합니다.
iris<-within(iris,Sepal.Length<-ifelse(is.na(Sepal.Length),median_per_species[Species],Sepal.Length))
head(iris)

#with는 복수 명령을 할 수 있다. 코드 블록을 사용하여 명령문을 작성한다. 
with(iris,{
  print(mean(Sepal.Length))
  print(mean(Sepal.Width))
})


# mtcars
# # 정렬하여 보여준다. 
# minimam=sort(mtcars$mpg,decreasing = F)
# i<-order(mtcars$mpg)
# for (i in i){
#   mtcars[i,]
# }
# 
# 
# maximam=sort(mtcars$hp,decreasing = F)
# i<-order(mtcars$hp)
# for (i in i){
#   mtcars[i,]
# }

# rev.mtcars=t(mtcars)
# rev.mtcars=as.data.frame(rev.mtcars)
# gear.id=order(mtcars$gear,decreasing = T)
# gear.names=names(rev.mtcars[gear.id])
# mear.names=names(rev.mtcars[gear.id])
# cyl.id=order(mtcars$cyl,decreasing = T)
# cyl.names=names(rev.mtcars[cyl.id])
# df1=data.frame("names"=gear.names,"rank"=c(1:32))
# df2=data.frame("names"=cyl.names,"rank"=c(1:32))
# df3=merge(df1,df2,by="names")
# ranksum=lapply(df3$rank.x+df3$rank,sum)
# ranksum.df=as.data.frame(ranksum)
# order(ranksum.df)
# df3[11,1]

getwd()
#trian 데이터가.. 다르다... 
# 
# testdata1=read.csv("test.csv",sep=",",header=T,stringsAsFactors = T)
# testdata1
# 
# 
# 
# 
# 
# selected1=data.frame(testdata1[,"Pclass"],testdata1$Sex,testdata1$Age,testdata1$SibSp,testdata1$Parch,testdata1$Fare,testdata1$Embarked)
# #selected1
# testdata2=read.csv("train.csv",sep="\t",header=T,stringsAsFactors = F)
# #testdata2
# selected2=data.frame(testdata2$Pclass,testdata2$Sex,testdata2$Age,testdata2$SibSp,testdata2$Parch,testdata2$Fare,testdata2$Embarked)
# #selected2
# 
# all_data=rbind(selected1,selected2)
# all_data
# 
# summary(all_data)
# 
# train=read.csv("train.csv",sep=",",header=T,stringsAsFactors = T)
# train
# selected=with(train,cbind(Pclass,Sex,Age,SibSp,Parch,Fare,Embarked))
# 
# test=read.csv("test.csv",sep=",",header=T,stringsAsFactors = T)
# selected_test=with(test,cbind(Pclass,Sex,Age,SibSp,Parch,Fare,Embarked))
# d1=data.frame(selected)
# d2=data.frame(selected_test)
# all_data=rbind(d1,d2)
# all_data
# 
# summary(all_data)

# train 데이터의 각 열들을 data.frame으로 바꿉니다. 
selected<-data.frame(train$Pclass,train$SibSp,train$Sex,train$Parch,train$Fare,train$Age,
                     train$Embarked)

#str(selected) #구조 확인 

# 열의 이름들을 바꿉니다. 
colnames(selected)<-c("Pclass","SibSp","Sex","Parch","Fare","Age","Embarked")

selected_test<-data.frame(test$Pclass,test$SibSp,test$Sex,test$Parch,test$Fare,test$Age,
                          test$Embarked)

#str(selected_test) #구조 확인

#열 이름을 바꿉니다. 
colnames(selected_test)<-c("Pclass","SibSp","Sex","Parch","Fare","Age","Embarked")

#selected 데이터의 na 인덱스를 반환합니다. 
which(is.na(selected))


all_data<-rbind(selected,selected_test)
all_data
summary(all_data)

mtcars
str(mtcars)
sort(mtcars$hp,decreasing = T)
sort(mtcars$mpg,decreasing = F)

within(mtcars,HP_plus<-sort(mtcars$hp,decreasing = T))

rev.mtcars=t(mtcars)
final.rev.mtcars=as.data.frame(rev.mtcars)
mm=rbind(mtcars$cyl,mtcars$gear)
tt=t(mm)
tt=as.data.frame(tt)
ll=as.data.frame(tt)
ll=lapply(tt$V1+tt$V2,sum)
ll=as.data.frame(ll)
save=c()
k=1
for (i in 1:32){
  if(ll[i]==max(ll)){
    save[k]=i
    k=k+i
  }
}
names(final.rev.mtcars[save[]])


#-----------------------------------------------------------
강원<-c(200,500,400)
경남<-c(400,300,500)
충북<-c(600,300,400)

# 데이터를 분석할 수 있도록 바꾸는 함수 stack()
# 이러한 형태를 Tidy table 이라고 한다. 

sales<-data.frame(강원,경남,충북)
sales
sales<-stack(sales); sales


# values의 값을 ind로 summary한다. 
summaryBy(values~ind,sales)

# unstack을 하게 되면 다시 분석 할 수 없는 테이블로 만든다. 
sales<-unstack(sales,values~ind)
sales

sales<-data.frame(강원,경남,충북)
library(reshape2)
sales<-melt(sales); sales
summaryBy(value~variable,sales)

sales<-dcast(sales,value~variable);
sales

head(french_fries)
head(melt(french_fries,id.vars=1:4))

head(melt(french_fries,id.vars=1:4,na.rm=TRUE))
?melt()
#----------------------연습문제---------
Apple<-c(6,10,13)
Banana<-c(2,8,10)
Peach<-c(7,3,5)
Berry<-c(8,15,11)
Year<-c(2000,2001,2002)
Fruit<-data.frame(Apple,Banana,Peach,Berry,Year)


Fruit<-stack(Fruit)
Fruit
#str(Fruit)
# year은 제외해야한다. 
summaryBy(values~ind,Fruit)

Fruit
#summary(melt(Fruit,id.vars=1:4))


#-------------------------------------------------

name<-c("James","Mary","Kevin")
age<-c(22,23,24)
df<-data.frame(name,age)
df

library(data.table)


name<-c("James","Mary","Kevin")
age<-c(22,23,24)
dt<-data.table(name,age)
dt

iris.dt<-as.data.table(iris)
iris.dt
class(dt)
class(df)
str(dt)
str(df)

# data.table 접근 방식 
iris.dt<-as.data.table(iris);
iris.dt
iris.dt[1,]
iris.dt[,1]

# column명에 따옴표가 없으면 벡터로 결과를 산출합니다. 
iris.dt[,Sepal.Length]

# 데이터 테이블로 결과를 산출합니다. 기본적으로 데이터 테이블 형태를 유지시켜 사용하는게 좋다. 
iris.dt[,"Sepal.Length"]
iris.dt[,"Sepal.Width"]

iris.dt[,c(Sepal.Length,Petal.Length)]
iris.dt[,c("Sepal.Length","Petal.Length")]

iris.dt[1,Species]
iris.dt[1,"Species"]
# 다른 데이터 타입이면 Species가 1.0으로 출력한다. 
iris.dt[1,c(Sepal.Length,Sepal.Width,Species)]

# column들을 "" 표시하면 데이터타입이 다르더라도 원래대로 출력합니다. 
iris.dt[1,c("Sepal.Length","Sepal.Width","Species")]


iris.dt[iris.dt$Petal.Length>1.5]
iris.dt[Petal.Length>1.5]


aggregate(Sepal.Width~Species,iris.dt,mean)
tapply(iris.dt$Sepal.Length,iris.dt$Species,mean)
iris.dt[,mean(Sepal.Length),by=Species]

# list로 감싸면 컬럼의 이름을 만들 수 있다. 
iris.dt[,list(my.length=mean(Sepal.Length)),by=Species]

install.packages("dplyr")
library(dplyr)
iris
head(iris)
dim(iris)
iris %>% head()
#iris

x<-c(1,2,3,4,5)
x2<-as.data.frame(x)
class(x2)

x<-c(1,2,3,4,5)
class(as.data.frame(x))

x<-c(1,2,3,4,5)
x %>% as.data.frame() %>% class()

iris[,c("Sepal.Length","Species")]

#select() 함수는 첫번쨰 인자로는 데이터 프레임을, 그 다음은 원하는 column을 인자로 받는다. 
select(iris,Sepal.Length,Species)

# %>% 는 연산자가 뒤로 넘어감을 의미합니다. 
iris %>% select(Sepal.Length,Species)

class(iris)

# 조건식을 보기 쉽게 알 수 있다. 

iris %>% filter(Species=="setosa")
iris %>% filter(Sepal.Length<5.0)
iris %>% filter(Species=="setosa" & Sepal.Length<5.0)

with(iris,mean(Sepal.Length))
summarize(iris,mean(Sepal.Length))

# 계산 결과값의 열의 이름을 부여할 수 있다. 
# 평균 값이 Sepal.Avg 라는 이름의 열로 들어갑니다. 
summarize(iris,Sepal.Avg=mean(Sepal.Length))

# summarize(iris,Sepal.Avg=mean(Sepal.Length)) 와 같다
iris %>% summarize(Sepal.Avg=mean(Sepal.Length))

iris %>% group_by(Species) %>% summarize(Sepal.Avg=mean(Sepal.Length))

# group_by를 통해서 Species로 먼저 나누고, Species를 기준으로 length의 평균을 구한다. 
#iris %>% summarize(Sepal.Avg=mean(Sepal.Length)) %>% group_by(Species) 

#부총별로 group_by를 사용할 수 있다. 

# group_by와 같은 결과값을 도출한다. 
tapply(iris$Sepal.Length,iris$Species,mean)

x=c(1,7,5,9,3)
sort(x)
order(x)

sort(x,decreasing=TRUE)
order(x,decreasing = T)

# order이므로 맨 옆에 index를 알 수 있다. 
head(iris[order(iris$Sepal.Length),])

# 새롭게 만들어진 행번호를 보여준다 .
head(iris %>% arrange(Sepal.Length))



PRACTICE=read.csv("pima-indians-diabetes.csv",sep=",",header=T,stringsAsFactors = F)
PRACTICE
str(PRACTICE)
mean(PRACTICE$BMI)

PRACTICE1=PRACTICE %>% filter(outcome==1 & BMI) %>% summarize(BMI평균=mean(BMI))
PRACTICE1

PRACTICE %>% group_by(outcome) %>% summarize(수축기평균=mean(수축기))
PRACTICE %>% group_by(outcome) %>% summarize(수축기평균=mean(이완기))

#PRACTICE[sort(PRACTICE$BMI,decreasing = TRUE),]
sort(PRACTICE$BMI,decreasing = T)
order(PRACTICE$BMI,decreasing = T)
#str(order(PRACTICE$BMI,decreasing = T))
head(PRACTICE[order(PRACTICE$BMI,decreasing = T),])


# 데이터셋을 데이터 테이블로 변환
#PRACTICE_table=as.data.table(PRACTICE)
library(data.table)
PRACTICE=read.csv("pima-indians-diabetes.csv",sep=",",header=T,stringsAsFactors = F)
PRACTICE.dt<-as.data.table(PRACTICE)
PRACTICE.dt
PRACTICE.dt[100,]

PRACTICE.dt[,mean(인슐린),by=outcome]













































