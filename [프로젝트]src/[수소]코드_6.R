# 20190821

test <- read.csv("충청북도_주민등록인구.csv",header=T)
test <- test[1:43,]
test
write.csv(test,file="충청북도_주민등록인구.csv")


setwd("C:/Users/fkaus/OneDrive/바탕 화면/데이터캠퍼스 프로젝트/데이터/전국도시별_위도_경도")

Chungbuk <- read.csv("chungbuk.csv",header=T)

Chungbuk.상당구 <- Chungbuk[Chungbuk$시군구=="청주시상당구",]
head(Chungbuk.상당구)

MAX <- max(Chungbuk.상당구$long)
MIN <- min(Chungbuk.상당구$long)
center.long <- MAX-MIN
center.long
distance <- center.long/20


center.long <- 127+center.long

MAX <- max(Chungbuk.상당구$lat)
MIN <- min(Chungbuk.상당구$lat)
center.lat <- MAX-MIN
center.lat <- 36+center.lat

center.long
center.lat


Chungbuk.충주시 <- Chungbuk[Chungbuk$시군구=="충주시",]
Chungbuk.충주시

long <- mean(Chungbuk.충주시$long)
lat <- mean(Chungbuk.충주시$lat)

long;lat
