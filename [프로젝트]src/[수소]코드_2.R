library(dplyr)
library(ggplot2)
library(ggmap)
library(raster)
library(viridis)
library(rgeos)
library(maptools)
library(rgdal)

register_google(key="AIzaSyDb6CtknFf0WsNEHDErgOZZM_pTPWMfPbs")
setwd("C:/Users/fkaus/OneDrive/바탕 화면/데이터캠퍼스 프로젝트/데이터")

# 전국 시군구 차량등록->시각화

#test<-read.csv("시군구_위도경도.csv",header=T)
test1<-read.csv("전국시군구_차량등록.csv",header=T)
# test1$차량등록 <- as.integer(test1$차량등록)
# test1$시군구 <- as.character(test1$시군구)
# str(test1)
names(test1)[3]<-"id"
head(test1)

korea <- getData('GADM',country='kor',level=2)
korea <- shapefile('TL_SCCO_SIG.shp')
#ggplot() + geom_polygon(data=korea, aes(x=long, y=lat, group=group), fill='white', color='black')

head(test1)
head(korea)

korea <- korea[,c(1,2,3,7,8,9)]
korea <- fortify(korea,region='SIG_CD')
korea <- merge(korea,test1,by="id")



seoul <- korea[korea$id <= 11740,]
busan <- korea[korea$id >= 26110 & korea$id <=26710,]
daegu <- korea[korea$id >= 27110 & korea$id <=27710,]
incheon <- korea[korea$id >= 28110 & korea$id <=28720,]
gwangju <- korea[korea$id >= 29110 & korea$id <=29200,]
daejeon <- korea[korea$id >= 30110 & korea$id <=30230,]
ulsan <- korea[korea$id >= 31110 & korea$id <=31710,]
gyeonggi <- korea[korea$id >= 41110 & korea$id <=41830,]
gangwon <- korea[korea$id >= 42110 & korea$id <=42830,]
chungbuk <- korea[korea$id >= 43110 & korea$id <=43800,]
chungnam <- korea[korea$id >= 44130 & korea$id <=44830,]
jeonbuk <- korea[korea$id >= 45110 & korea$id <=45800,]
jeonnam <- korea[korea$id >= 46110 & korea$id <=46910,]
gyeongbuk <- korea[korea$id >= 47110 & korea$id <=47940,]
gyeongnam <- korea[korea$id >= 48120 & korea$id <=48890,]
jeju <- korea[korea$id >= 50110 & korea$id <=50130,]

write.csv(seoul,file="seoul.csv")
write.csv(busan,file="busan.csv")
write.csv(daegu,file="daegu.csv")
write.csv(incheon,file="incheon.csv")
write.csv(gwangju,file="gwangju.csv")
write.csv(daejeon,file="daejeon.csv")
write.csv(ulsan,file="ulsan.csv")
write.csv(gyeonggi,file="gyeonggi.csv")
write.csv(gangwon,file="gangwon.csv")
write.csv(chungbuk,file="chungbuk.csv")
write.csv(chungnam,file="chungnam.csv")
write.csv(jeonbuk,file="jeonbuk.csv")
write.csv(jeonnam,file="jeonnam.csv")
write.csv(gyeongbuk,file="gyeongbuk.csv")
write.csv(gyeongnam,file="gyeongnam.csv")
write.csv(jeju,file="jeju.csv")





ggplot() + geom_polygon(data=jeju,aes(x=long,y=lat,group=group,fill=차량등록))



Map<-get_map(location = 'south korea',zoom=7,maptype='roadmap',color='bw')

ggmap(Map)+geom_polygon(data=jeju,aes(x=long,y=lat,group=group,fill=차량등록),alpha=0.75)+theme_void()



































