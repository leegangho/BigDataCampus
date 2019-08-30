# 충북지역 피해야할 지역 좌표 다 합칩니다. 
library(dplyr)
library(leaflet)

setwd("C:/Users/fkaus/OneDrive/바탕 화면/데이터캠퍼스 프로젝트/데이터")

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
leaflet() %>% addTiles() %>% setView(127.40645,36.6360912, zoom = 12) %>%  addCircles(all$lon,all$lat,weight=0.01,radius = 200,color="red")
