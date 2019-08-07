#20190806 

library(RColorBrewer)
library(treemap)
library(MASS)
library(ggplot2)

x <- c(1,2,2,1,3,3,1,5)
x2 <- table(x)
bp <- barplot(x2)
percent <- round(x2/sum(x2)*100,digits=1)
text(x=bp,y=x2-0.1,labels=percent)

pal <- brewer.pal(12,"Set3") 
bp<-barplot(x2,col=pal)
percent<-round(x2/sum(x2)*100,digits=1)

data("GNI2014")
str(GNI2014)

treemap(GNI2014,index=c("continent","iso3"),vSize = "population",vColor="GNI",type="value",bg.labels = "yellow")
GNI2014$GNI.total=GNI2014$population*GNI2014$GNI
head(GNI2014)

GNI2014.a=aggregate(GNI2014[,4:6],by=list(GNI2014$continent),sum)
GNI2014.a$GNI.percap<-GNI2014.a$GNI.total/GNI2014.a$population
head(GNI2014.a)

treemap(GNI2014.a,index=c("Group.1"),vSize = "population",vColor="GNI.percap",type="value",bg.labels = "yellow")


# 연습문제 1번 2번 
setwd("D:/BigDataCampus/practiceData/file")
data=read.csv("국회의원_선거구_유권자수.csv",header=T)
treemap(data,index=c("시도","선거구명"),vSize="선거인.수",vColor="선거인.수",type="value",bg.labels="black")
data.a<-aggregate(data[5], by=list(data$시도), sum)
data.a
treemap(data.a, index=c("Group.1"), vSize="선거인.수"
        ,vColor="선거인.수",type="value", bg.labels="yellow")
#---------------------------------------------------------------------



#install.packages("MASS")
radius <- sqrt(UScrime$Pop)
symbols(UScrime$U2,UScrime$y,circles=radius,inches=0.4,fh="white",bg="lightgray",lwd=1.5,
        xlab="unemploymnet 35-39 males",ylab="crime rate",main="UScrime Data")

text(UScrime$U2,UScrime$y,1:nrow(UScrime),cex=0.8,col="brown")



ds=read.csv("Seoul_temp_2017.csv",header = T)
head(ds)
summary(ds$avg_temp) # y축 범위를 설정할 수 있다. 

boxplot(ds$avg_temp,col="yellow",ylim=c(-20,40),xlab="서울 1년 기온",ylab="기온")

month.avg=aggregate(ds$avg_temp,by=list(ds$month),median)[2]
odr<-rank(-month.avg)
boxplot(avg_temp~month,col=heat.colors(12)[odr],ylim=c(-20,40),
        ylab="기온 ",xlab="월 ",main="서울 월별 기온 분포")

x <- c(1,2,3,4,5,6)
y <- c(2,5,7,12,15,20)
plot(x,y,xlab="time",ylab="effect")

qplot(x,y)
qplot(x,y,xlab="time",ylab="effect")

ggplot(data=iris,aes(x=Petal.Length,y=Petal.Width))+geom_point()
ggplot(data=iris,aes(x=Petal.Length,y=Petal.Width))+geom_point(aes(color=Species,shape=Species),alpha=0.5,size=2)



#---------------------------------------------------------------------------------------
# 1번
poll=read.csv("국회의원_선거구_유권자수.csv",header=T)
head(poll)
treemap(poll,index=c("시도","선거구명"),vSize="선거인.수",vColor="선거인.수",type="value",bg.labels="black")

# 2번 
poll.a<-aggregate(poll[5], by=list(poll$시도), sum)
poll.a
treemap(poll.a, index=c("Group.1"), vSize="선거인.수"
        ,vColor="선거인.수",type="value", bg.labels="yellow")



head(airquality)
summary(airquality)
air_temp <- aggregate(airquality[4], by=list(airquality$Month), median)[2]
odr <- rank(-air_temp)
odr
boxplot(Temp~Month, data=airquality, col=heat.colors(5)[odr],  ylim=c(50,100), ylab="Temp", xlab="Month Number", main="Different boxplots for each month")



air_wind <- aggregate(airquality[3], by=list(airquality$Month), median)[2]
odr <- rank(-air_wind)
boxplot(Wind~Month, data=airquality, col=heat.colors(5)[odr],  ylim=c(0,25), ylab="Wind", xlab="Month Number", main="Different boxplots for each month")

air_Oz <- aggregate(airquality[1], by=list(airquality$Month), median)[2]
odr <- rank(-air_Oz)
boxplot(Ozone~Month, data=airquality, col=heat.colors(5)[odr],  ylim=c(0,170), ylab="Ozone", xlab="Month Number", main="Different boxplots for each month")



belief=c('no belief','no belief','no belief','no belief','belief','belief','belief','belief','belief','belief','no belief','no belief','belief','belief','no belief','no belief')
sibling=c('order brother','order brother','order brother','order sister',
          'no other sibling','no other sibling','no other sibling',
          'order sister','order brother','order sister','order brother','order sister',
          'no other sibling','order sister','order brother','no other sibling')
santa<-data.frame(belief,sibling)
santa<-table(santa)
mosaicplot(santa,color = c("red","green","blue"))

data<-data.frame(state.x77)
pop<-sqrt(data$Population)
symbols(data$Income,data$Illiteracy,circle=pop,inches = 0.3,
        fg="white",bg="lightgray",lwd=1.5)
data$state<-rownames(data)

text(data$Income,data$Illiteracy,data$state,cex=0.5)


data(HairEyeColor)
mosaicplot(HairEyeColor)

belief=c('no belief','no belief','no belief','no belief','belief','belief','belief','belief','belief','belief','no belief','no belief','belief','belief','no belief','no belief')
sibling=c('order brother','order brother','order brother','order sister',
          'no other sibling','no other sibling','no other sibling',
          'order sister','order brother','order sister','order brother','order sister',
          'no other sibling','order sister','order brother','no other sibling')
santa<-data.frame(belief,sibling)
santa<-table(santa)
mosaicplot(santa,color = c("red","green","blue"))



#5-1
head(state.x77)
state<-as.data.frame(state.x77)
ggplot(state, aes(x=state$Illiteracy,y=state$Income))+geom_point()
#5-2
head(mtcars)
mtcars_gear<-table(mtcars$gear)
mtcars_gear
gear<-barplot(mtcars_gear)
#5-3
am <- as.numeric(airmiles)
am
ggplot(,aes(y=airmiles, x=c(1937:1960)))+geom_line()
#5-4
head(iris)
summary(iris)
boxplot(Petal.Width~Species, data=iris, 
        ylim=c(0,3), ylab="Petal.Width", xlab="Species",
        main="Different boxplots for each Species")





#5-1
head(state.x77)
state<-as.data.frame(state.x77)
ggplot(state, aes(x=state$Illiteracy,y=state$Income))+geom_point()
#5-2
head(mtcars)
mtcars_gear<-data.frame(table(mtcars$gear))
mtcars_gear
ggplot(mtcars_gear, aes(x=Var1,y=Freq))+geom_bar(stat="identity", width=0.5, fill="steelblue")
#5-3
am <- as.numeric(airmiles)
am
ggplot(,aes(y=airmiles, x=c(1937:1960)))+geom_line()
#5-4
head(iris)
summary(iris)
boxplot(Petal.Width~Species, data=iris, 
        ylim=c(0,3), ylab="Petal.Width", xlab="Species",
        main="Different boxplots for each Species")
#----------------------------------------------------------------------------------------------

time=c(1,2,3,4,5,6)
effect=c(2,5,7,12,15,20)
effect2<-c(1,3,5,8,11,18)
df<-data.frame(time,effect,effect2)
ggplot(df,aes(x=time,y=effect))+geom_line()

ggplot(df,aes(x=time,y=effect))+geom_line(size=2.7)
ggplot(df,aes(x=time,y=effect))+geom_line(size=2.7,color="red")
ggplot(df,aes(x=time,y=effect))+geom_line(size=2.7,color="red")+geom_point(size=2.7,color="white")

hwdata<-read.csv("heightweight.csv",header=T)
head(hwdata)

ggplot(hwdata,aes(x=hwdata$ageYear,y=hwdata$heightIn,colour=hwdata$sex))+geom_line()
ggplot(hwdata,aes(x=hwdata$ageMonth,y=hwdata$heightIn,colour=hwdata$sex))+geom_line()


ggplot(hwdata,aes(x=hwdata$ageYear,y=hwdata$heightIn,colour=hwdata$sex))+geom_point()



df=data.frame(table(mtcars$carb))
df
ggplot(df,aes(x=Var1,y=Freq))+geom_bar(stat="identity",width=0.7,fill="steelblue")
  
ggplot()+geom_boxplot(data=iris,aes(x=Species,y=Petal.Length,fill=Species))


data <- read.csv(file="uspopage.csv",header=T)
head(data)
ggplot(data,aes(x=Year,y=Thousands))
ggplot(data,aes(x=Year,y=Thousands,fill=AgeGroup))+geom_area()

data<-read.csv("hit.csv",header = T)
avg<-data$average
name<-data$name
barplot(avg,names.arg=name)
ggplot(data,aes(x=avg,y=name))+geom_point()





































