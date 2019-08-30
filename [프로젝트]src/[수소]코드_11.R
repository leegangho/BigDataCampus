# 충북 교통량 히트맵
library(dplyr)
library(ggplot2)
library(ggmap)
library(raster)
library(viridis)
library(rgeos)
library(maptools)
library(rgdal)
library(leaflet)

library(tidyverse)
library(sf)
register_google(key="AIzaSyDb6CtknFf0WsNEHDErgOZZM_pTPWMfPbs")


setwd("C:/Users/fkaus/OneDrive/바탕 화면/데이터캠퍼스 프로젝트/데이터/자동차유입시설")

one <- read.csv("상당구_자동차유입시설.csv")
two <- read.csv("음성군_자동차유입시설.csv")
three <- read.csv("충주시_자동차유입시설.csv")
four <- read.csv("흥덕구_자동차유입시설.csv")

all <- rbind(one,two,three,four)
#write.csv(all,file="자동차유입_전체_포인트.csv")
leaflet() %>% addTiles() %>% setView(127.40645,36.6360912, zoom = 12) %>%  addCircles(all$lon,all$lat,weight=10,radius = 100,color="green")


setwd("C:/Users/fkaus/OneDrive/바탕 화면/데이터캠퍼스 프로젝트/데이터/충청북도_반경")

gu1_아파트 <- read.csv("상당구아파트_위도경도.csv",header=T)
gu1_어린이집 <- read.csv("상당구어린이집_위도경도.csv",header=T)
gu1_대피소 <- read.csv("상당구대피소_위도경도.csv",header=T)
gu1_학교 <- read.csv("상당구학교_위도경도.csv",header=T)


all.1 <- rbind(gu1_대피소,gu1_아파트,gu1_어린이집,gu1_학교)


gu2_아파트 <- read.csv("음성아파트.csv",header=T)
gu2_어린이집 <- read.csv("음성어린이.csv",header=T)
gu2_대피소 <- read.csv("음성대피소.csv",header=T)
gu2_학교 <- read.csv("음성학교.csv",header=T)

all.2 <- rbind(gu2_대피소,gu2_아파트,gu2_어린이집,gu2_학교)

gu3_아파트 <- read.csv("충주아파트_위도경도.csv",header=T)
gu3_어린이집 <- read.csv("충주어린이집_위도경도.csv",header=T)
gu3_대피소 <- read.csv("충주대피소_위도경도.csv",header=T)
gu3_학교 <- read.csv("충주학교_위도경도.csv",header=T)

all.3 <- rbind(gu3_대피소,gu3_아파트,gu3_어린이집,gu3_학교)

all.4 <- read.csv("흥덕구_전체.csv",header=T)

all <- rbind(all.1,all.2,all.3,all.4)
#write.csv(all,file="충북_피해야할포인트.csv")

leaflet() %>% addTiles() %>% setView(127.40645,36.6360912, zoom = 12) %>%  addCircles(all$lon,all$lat,weight=0.01,radius = 200,color="red")


setwd("C:/Users/fkaus/OneDrive/바탕 화면/데이터캠퍼스 프로젝트/데이터")
red <- read.csv("red.csv",header=T,stringsAsFactors = F)
green <- read.csv("green.csv",header = T,stringsAsFactors = F)

str(red)
str(green)

setwd("C:/Users/fkaus/OneDrive/바탕 화면/데이터캠퍼스 프로젝트/데이터")
df <- read.csv("교통량위도경도도.csv",header=T)
pal<-colorNumeric(palette="Purples",domain = df$TotalCar,reverse = F)


leaflet() %>% addTiles() %>% setView(127.40645,36.6360912, zoom = 12) %>% addCircles(data=df,df$lon,df$lat,weight=1,radius = 800,fillColor = ~pal(df$TotalCar),fillOpacity = 0.5) %>%  addCircles(red$lon,red$lat,weight=1,radius = 150,color="red") %>% addCircles(green$lon,green$lat,weight=1,radius = 500,color="green")
leaflet() %>% addTiles() %>% setView(127.40645,36.6360912, zoom = 12) %>% addCircles(data=df,df$lon,df$lat,weight=1,radius = 800,fillColor = ~pal(df$TotalCar),fillOpacity = 0.5) %>%  addCircles(red$lon,red$lat,weight=1,radius = 150,color="red") %>% addCircles(green$lon,green$lat,weight=1,radius = 300,color="green")
leaflet() %>% addTiles() %>% setView(127.40645,36.6360912, zoom = 12) %>% addCircles(data=df,df$lon,df$lat,weight=1,radius = 800,fillColor = ~pal(df$TotalCar),fillOpacity = 0.5) %>%  addCircles(red$lon,red$lat,weight=1,radius = 150,color="red") %>% addCircles(green$lon,green$lat,weight=1,radius = 200,color="green")



location <- c(lon=127.40645,lat=36.6360912)
map <- get_map(location=location,zoom=12,maptype="roadmap")
ggmap(map)+addCircles(red$lon,red$lat,weight=0.01,radius = 100,color="red")+addCircles(green$lon,green$lat,weight=0.01,radius = 100,color="green")




#-------------------------------------------------------------------------------

?leaflet

leaflet() %>% 
  addTiles() %>% 
  setView(127.40645,36.6360912,zoom = 12) %>%  
  addCircles(data=df,df$lon,df$lat,weight=1,radius = 800,fillColor = ~pal(df$TotalCar),
             fillOpacity = 0.5) 


























