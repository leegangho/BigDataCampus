# k-means

library(dplyr)
library(ggplot2)
library(ggmap)
library(raster)
library(viridis)
library(rgeos)
library(maptools)
library(rgdal)

library(caret)
library(NbClust)
set.seed(1712)
#install.packages("NbClust")


setwd("C:/Users/fkaus/OneDrive/바탕 화면/데이터캠퍼스 프로젝트/데이터/전국도시별_위도_경도")

Gyeongnam <- read.csv("gyeongnam.csv",header=T)
Gyeongnam <- Gyeongnam[,c(3,4,9)]

inTrain <- createDataPartition(y=Gyeongnam$시군구,p=0.7,list=F)
training <- Gyeongnam[inTrain,]
testing <- Gyeongnam[-inTrain,]


training.data <- training[,-3]
#training.data <- scale(training[-3])
summary(training.data)
head(training.data)

Gyeongnam.kmeans <- kmeans(training.data,centers =20,iter.max = 10000)

training$cluster <- as.factor(Gyeongnam.kmeans$cluster)
qplot(long,lat,colour=cluster,data=training)

Gyeongnam.kmeans$centers

test <- Gyeongnam.kmeans$centers

#-------------------------------------------------------------------
register_google(key="AIzaSyDb6CtknFf0WsNEHDErgOZZM_pTPWMfPbs")
setwd("C:/Users/fkaus/OneDrive/바탕 화면/데이터캠퍼스 프로젝트/데이터")

korea <- getData('GADM',country='kor',level=2)
korea <- shapefile('TL_SCCO_SIG.shp')

Map<-get_map(location = 'south korea',zoom=7,maptype='roadmap',color='bw')

result <- read.csv("경상남도군집.csv",header=T)

ggmap(Map)+geom_point(result,mapping=aes(x=long,y=lat))



#----------------------------------



Busan <- read.csv("busan.csv",header=T)
Busan<- Busan[,c(3,4,9)]

inTrain <- createDataPartition(y=Busan$시군구,p=0.7,list=F)
training <- Busan[inTrain,]
testing <- Busan[-inTrain,]


training.data <- training[,-3]

Busan.kmeans <- kmeans(training.data,centers =10,iter.max = 10000)

training$cluster <- as.factor(Busan.kmeans$cluster)
qplot(long,lat,colour=cluster,data=training)

test <- Busan.kmeans$centers

result2 <- read.csv("부산군집.csv",header=T)

ggmap(Map)+geom_point(result2,mapping=aes(x=long,y=lat))+geom_point(result,mapping=aes(x=long,y=lat))

#-----------------------------------------------------------------------------

Daegu <- read.csv("daegu.csv",header=T)
Daegu<- Daegu[,c(3,4,9)]

inTrain <- createDataPartition(y=Daegu$시군구,p=0.7,list=F)
training <- Daegu[inTrain,]
testing <- Daegu[-inTrain,]


training.data <- training[,-3]

Daegu.kmeans <- kmeans(training.data,centers =3,iter.max = 10000)

training$cluster <- as.factor(Daegu.kmeans$cluster)
qplot(long,lat,colour=cluster,data=training)
test <- Daegu.kmeans$centers

write.csv(test,file="대구군집.csv")
result3 <- read.csv("대구군집.csv",header=T)
result3 <- result3[,-1]

ggmap(Map)+geom_point(result,mapping=aes(x=long,y=lat))+geom_point(result2,mapping=aes(x=long,y=lat))+geom_point(result3,mapping=aes(x=long,y=lat))


#-----------------------------------------------------------------------------------

Gyeongbuk <- read.csv("gyeongbuk.csv",header=T)
Gyeongbuk <- Gyeongbuk[,c(3,4,9)]

inTrain <- createDataPartition(y=Gyeongbuk$시군구,p=0.7,list=F)
training <- Gyeongbuk[inTrain,]
testing <- Gyeongbuk[-inTrain,]


training.data <- training[,-3]

Gyeongbuk.kmeans <- kmeans(training.data,centers = 10,iter.max = 10000)

training$cluster <- as.factor(Gyeongbuk.kmeans$cluster)
qplot(long,lat,colour=cluster,data=training)
test <- Gyeongbuk.kmeans$centers

write.csv(test,file="경북군집.csv")
result4 <- read.csv("경북군집.csv",header=T)
result4 <- result4[,-1]


ggmap(Map)+geom_point(result,mapping=aes(x=long,y=lat))+geom_point(result2,mapping=aes(x=long,y=lat))+geom_point(result3,mapping=aes(x=long,y=lat))+geom_point(result4,mapping=aes(x=long,y=lat))


#--------------------------------------------------------------------------------------

Ulsan <- read.csv("ulsan.csv",header=T)
Ulsan <- Ulsan[,c(3,4,9)]

inTrain <- createDataPartition(y=Ulsan$시군구,p=0.7,list=F)
training <- Ulsan[inTrain,]
testing <- Ulsan[-inTrain,]


training.data <- training[,-3]

Ulsan.kmeans <- kmeans(training.data,centers = 10,iter.max = 10000)

training$cluster <- as.factor(Ulsan.kmeans$cluster)
qplot(long,lat,colour=cluster,data=training)
test <- Ulsan.kmeans$centers

write.csv(test,file="울산군집.csv")
result5 <- read.csv("울산군집.csv",header=T)
result5 <- result5[,-1]
#result5 <- as.data.frame(result5)
result5

ggmap(Map)+geom_point(result,mapping=aes(x=long,y=lat),color="red")+geom_point(result2,mapping=aes(x=long,y=lat),color="blue")+geom_point(result3,mapping=aes(x=long,y=lat),color="brown")+geom_point(result4,mapping=aes(x=long,y=lat),color="#FF33FF")+geom_point(result5,mapping=aes(x=long,y=lat))






































