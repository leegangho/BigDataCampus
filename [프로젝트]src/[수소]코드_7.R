#20190823

library(dplyr)
library(ggplot2)
library(ggmap)
library(raster)
library(viridis)
library(rgeos)
library(maptools)
library(rgdal)

setwd("C:/Users/fkaus/OneDrive/바탕 화면/데이터캠퍼스 프로젝트/데이터")
register_google(key="AIzaSyDb6CtknFf0WsNEHDErgOZZM_pTPWMfPbs")
test1 <- read.csv("흥덕구.csv",header=T)
test1 <- test1[-c(1,2,3,4),]

test1 <- test1[test1$X.2=="흥덕구",]
test1 <- test1[,c(2,3,4,5,6)]

test1$X.4 <- as.character(test1$X.4)
test1

ge<-geocode(enc2utf8(test1$X.4))
ge <- ge[-c(136,160),]
View(ge)

result <- read.csv("chungbuk.csv",header=T)
result <- result[result$시군구=="청주시흥덕구",]
result <- result[,c(3,4,9)]
chungbuk.huengduk<- result
chungbuk.huengduk


# 지도에 찍고있어연!! 
ggplot()+geom_polygon(data=chungbuk.huengduk,aes(x=long,y=lat),fill="white",color="black")+geom_point(data=ge,aes(x=lon,y=lat))+geom_point(data=mart,aes(x=lon,y=lat),color="red")+geom_point(data=chungbuk.school,aes(x=lon,y=lat),color="green")

mart <- c()
mart$lon=c(127.4261698,127.4226807)
mart$lat=c(36.6447762,36.6368296)

mart <- data.frame(mart$lon,mart$lat)
names(mart)[1] <- "lon"
names(mart)[2] <- "lat"
#-----------------------------------------------------------------------------------------------------------------------

#충청북도 학교 
test2 <- read.csv("충북흥덕구학교.csv",header=T)
View(test2)
test2 <- test2[,4]
test2 <- as.character(test2)
chungbuk.school<-geocode(enc2utf8(test2))
chungbuk.school <- data.frame(test2,chungbuk.school)
chungbuk.school

#-----------------------------------------------------------------------------------------------------------------------

test3 <- read.csv("대피소.csv",header=T)
head(test3)

# 

ggplot()+geom_polygon(data=chungbuk.huengduk,aes(x=long,y=lat),fill="white",color="black")+geom_point(data=ge,aes(x=lon,y=lat))+geom_point(data=mart,aes(x=lon,y=lat),color="red")+geom_point(data=chungbuk.school,aes(x=lon,y=lat),color="green")+geom_point(data=test3,aes(x=경도,y=위도),color="yellow")



test4 <- read.csv("kid.csv",header=T)
head(test4)

ggplot()+geom_polygon(data=chungbuk.huengduk,aes(x=long,y=lat),fill="white",color="black")+geom_point(data=ge,aes(x=lon,y=lat),size=5,alpha=0.1)+geom_point(data=mart,aes(x=lon,y=lat),color="red")+geom_point(data=chungbuk.school,aes(x=lon,y=lat),color="green",size=5,alpha=0.1)+geom_point(data=test3,aes(x=경도,y=위도),color="yellow",size=5,alpha=0.1)+geom_point(data=test4,aes(x=경도,y=위도),color="blue",size=5,alpha=0.1)

location <- c(lon=127.40645,lat=36.6360912)
Hiresult <- ggmap(get_map(location = location,zoom=12,maptype = 'roadmap'))+geom_hex(binwidth=c(0.1/12,0.08/12), alpha = 0.3)+geom_point(data=ge,aes(x=lon,y=lat),size=5,alpha=0.1)+geom_point(data=mart,aes(x=lon,y=lat),color="red")+geom_point(data=chungbuk.school,aes(x=lon,y=lat),color="green",size=5,alpha=0.1)+geom_point(data=test3,aes(x=경도,y=위도),color="yellow",size=5,alpha=0.1)+geom_point(data=test4,aes(x=경도,y=위도),color="blue",size=5,alpha=0.1)
Hiresult

library(hexbin)

Lon <- rnorm(5000, 127.4847, 0.01)
Lon <- c(Lon, rnorm(5000, 127, 0.008))
Lat <- rnorm(5000,36.725353,0.01)
Lat <- c(Lat, rnorm(5000, 37.57, 0.005))
Loc <- data.frame(Lon, Lat)
test.test <- ggplot(Loc, aes(x=Lon, y=Lat)) + geom_hex(binwidth=c(0.1/12,0.08/12), alpha = 0.3)











