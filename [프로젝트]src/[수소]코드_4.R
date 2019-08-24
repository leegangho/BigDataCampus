# 충북 스타벅스 드라이브 스루 

library(dplyr)
library(ggplot2)
library(ggmap)
library(leaflet)



setwd("C:/Users/fkaus/OneDrive/바탕 화면/데이터캠퍼스 프로젝트/데이터")
register_google(key="AIzaSyDb6CtknFf0WsNEHDErgOZZM_pTPWMfPbs")

Chungbuk <- read.csv("충북_스타벅스_드라이브.csv",header=T)
Chungbuk$충북스타벅스주소 <- as.character(Chungbuk$충북스타벅스주소)


ge<-geocode(enc2utf8(Chungbuk$충북스타벅스주소))

Chungbuk.drive <- data.frame(Chungbuk,ge)
write.csv(Chungbuk.drive,file="Chungbuk.drive.csv")

Chungbuk.drive <- read.csv("Chungbuk_drive.csv",header=T)
Chungbuk.drive


lft=leaflet()
lft=addTiles(lft)
lft=addMarkers(lft,lng=Chungbuk.drive$lon,lat=Chungbuk.drive$lat,popup="DT점")
lft
#----------------------------------------------------------------------------------------------

Chungbuk.highpass <- read.csv("충북 JC.csv",header=T)
Chungbuk.highpass$주소 <- as.character(Chungbuk.highpass$주소)
Chungbuk.highpass.ge<-geocode(enc2utf8(Chungbuk.highpass$주소))
Chungbuk.highpas
Chungbuk.highpass <- data.frame(Chungbuk.highpass,Chungbuk.highpass.ge)
head(Chungbuk.highpass)

write.csv(Chungbuk.highpass,file="충북고속도로.csv")
