#20190823

library(dplyr)
library(ggplot2)
library(ggmap)
library(raster)
library(viridis)
library(rgeos)
library(maptools)
library(rgdal)
library(leaflet)


setwd("C:/Users/fkaus/OneDrive/바탕 화면/데이터캠퍼스 프로젝트/데이터")
register_google(key="AIzaSyDb6CtknFf0WsNEHDErgOZZM_pTPWMfPbs")

# 아파트
test1 <- read.csv("흥덕구.csv",header=T)
test1 <- test1[-c(1,2,3,4),]

test1 <- test1[test1$X.2=="흥덕구",]
test1 <- test1[,c(2,3,4,5,6)]

test1$X.4 <- as.character(test1$X.4)

ge<-geocode(enc2utf8(test1$X.4))
ge <- ge[-c(136,160),]

#write.csv(ge,file="충북_흥덕구_아파트.csv")



# 충청북도에서 흥덕구 경계 위도 경도만 뽑아옵니다. 
result <- read.csv("chungbuk.csv",header=T)
result <- result[result$시군구=="청주시흥덕구",]
result <- result[,c(3,4,9)]
chungbuk.huengduk<- result
chungbuk.huengduk


# 지도에 찍고있어연!! 
#ggplot()+geom_polygon(data=chungbuk.huengduk,aes(x=long,y=lat),fill="white",color="black")+geom_point(data=ge,aes(x=lon,y=lat))+geom_point(data=mart,aes(x=lon,y=lat),color="red")+geom_point(data=chungbuk.school,aes(x=lon,y=lat),color="green")

# 흥덕구 백화점은 2곳입니다. 
mart <- c()
mart$lon=c(127.4261698,127.4226807)
mart$lat=c(36.6447762,36.6368296)

mart <- data.frame(mart$lon,mart$lat)
names(mart)[1] <- "lon"
names(mart)[2] <- "lat"
#-----------------------------------------------------------------------------------------------------------------------

#충청북도 학교 
test2 <- read.csv("충북흥덕구학교.csv",header=T)
#View(test2)
test2 <- test2[,4]
test2 <- as.character(test2)
chungbuk.school<-geocode(enc2utf8(test2))
chungbuk.school <- data.frame(test2,chungbuk.school)
chungbuk.school
write.csv(chungbuk.school,file="충북_흥덕구_학교.csv")
#-----------------------------------------------------------------------------------------------------------------------

setwd("C:/Users/fkaus/OneDrive/바탕 화면/데이터캠퍼스 프로젝트/데이터/흥덕 좌표")
test3 <- read.csv("대피소.csv",header=T)
head(test3)

LNG_LAT <- read.csv("충북_흥덕구_어린이집_대피소_아파트_학교.csv",header=T)
head(LNG_LAT)





test4 <- read.csv("kid.csv",header=T)
head(test4)

ggplot()+geom_polygon(data=chungbuk.huengduk,aes(x=long,y=lat),fill="white",color="black")+geom_point(data=ge,aes(x=lon,y=lat),size=5,alpha=0.1)+geom_point(data=mart,aes(x=lon,y=lat),color="red")+geom_point(data=chungbuk.school,aes(x=lon,y=lat),color="green",size=5,alpha=0.1)+geom_point(data=test3,aes(x=경도,y=위도),color="yellow",size=5,alpha=0.1)+geom_point(data=test4,aes(x=경도,y=위도),color="blue",size=5,alpha=0.1)

location <- c(lon=127.40645,lat=36.6360912)
Hiresult <- ggmap(get_map(location = location,zoom=12,maptype = 'roadmap'))+geom_hex(binwidth=c(0.1/12,0.08/12), alpha = 0.3)+geom_point(data=ge,aes(x=lon,y=lat),size=5,alpha=0.1)+geom_point(data=mart,aes(x=lon,y=lat),color="red")+geom_point(data=chungbuk.school,aes(x=lon,y=lat),color="green",size=5,alpha=0.1)+geom_point(data=test3,aes(x=경도,y=위도),color="yellow",size=5,alpha=0.1)+geom_point(data=test4,aes(x=경도,y=위도),color="blue",size=5,alpha=0.1)
Hiresult

ggmap(get_map(location = location,zoom=12,maptype = 'roadmap'))+addCircles(ge$lon,ge$lat,radius = 100,color="red")+addCircles(mart$lon,mart$lat,color="green",radius=100)+addCircles(chungbuk.school$lon,chungbuk.school$lat,color="yellow")

#leaflet() %>% addTiles() %>% setView(127.40645,36.6360912, zoom = 12) %>%  addCircles(LNG_LAT$lon,LNG_LAT$lat, weight = 1, radius = 500)

?addCircles






