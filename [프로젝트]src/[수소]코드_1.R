# 주제: 수소차 충전소 위치선정

library(dplyr)
library(ggplot2)
library(ggmap)
library(raster)
library(viridis)
library(rgeos)
library(maptools)
library(rgdal)
#install.packages("raster")
#install.packages("viridis")
#install.packages("rgeos")
# install.packages(maptools)
# install.packages(rgdal)


register_google(key="AIzaSyDb6CtknFf0WsNEHDErgOZZM_pTPWMfPbs")
setwd("C:/Users/fkaus/OneDrive/바탕 화면/데이터캠퍼스 프로젝트/데이터")

H.station=read.csv("수소충전소주소.csv",header=F)
H.station <- H.station[,-c(1,5,6,7,8)]
H.station$V4<-as.character(H.station$V4)


head(H.station)
str(H.station)
H.station



# 충전소 주소명으로 위도, 경도 추출  # 수소차 충전소 위치만 지도에 보입니다. 
ge<-geocode(enc2utf8(H.station$V4))
ge[is.na(ge$lon),]<-126.788372
ge[4,2]<-35.2137005
#write.csv(ge,file="현재수소충전소위도경도.csv")


# 우리나라 전체 지도에서 수소 충전소 위치 표시 
Map<-get_map(location = 'south korea',zoom=7,maptype='roadmap',color='bw')
Eye.point<- ggmap(Map)+geom_point(data=ge,aes(x=lon,y=lat),color="red")
Eye.point

#-----------------------------------------------------------------------------------------

now.station<-read.csv("전국주유소주소1.csv",header=T)
now.station2<-read.csv("전국주유소주소2.csv",header=T)
now.station2

#now.station<-now.station[,-1]
now.station2<-now.station2[,-5]
head(now.station)
head(now.station2)
names(now.station2)[1]<-c("판매업의종류")

result.station<-rbind(now.station,now.station2)
result.station$구분<-as.character(result.station$구분)
.libPaths()


# 현재 사업중인 주유소만 선별 #9095개 주유소
result.station<-result.station[result.station$구분!="페업",]
result.station<-result.station[result.station$구분!="폐업",]
result.station<-result.station[result.station$구분!="폐지",]
result.station<-result.station[result.station$구분!="등록취소",]
result.station<-result.station[result.station$구분!="휴업",]

# 전국 주유소 위도 경도 추가 
# result.station$소재지<-as.character(result.station$소재지)
# result.station.ge<-geocode(enc2utf8(result.station$소재지))
# result.station<-cbind(result.station,result.station.ge)
#write.csv(result.station,file="전국주유소주소.csv",row.names=F)

# 주소를 얻지 못한 주유소 51곳 -> 제거하고 실시 
result.station<-result.station[!is.na(result.station$lon),]
result.station.ge<-result.station.ge[!is.na(result.station.ge$lon),]

head(result.station)
head(result.station.ge)

Eye.point<- ggmap(Map)+geom_point(data=result.station.ge,aes(x=lon,y=lat),color="skyblue",alpha=0.5)+geom_point(data=ge,aes(x=lon,y=lat),color="red")
Eye.point
# 전국 주유소-> blue, 수소충전소->red 

#-----------------------------------------------------------------------------------------

H.register<-read.csv("연도별수소차량등록.csv",header=T)
H.register<-H.register[,-21]
H.register$울산 <- as.integer(H.register$울산)

View(H.register)

#-----------------------------------------------------------------------------------------

# 스타벅스 상호명은 전부 다릅니다.
test1<-read.csv("전국_스타벅스_주소.csv",header=T)
Map<-get_map(location = 'south korea',zoom=7,maptype='roadmap',color='bw')
ggmap(Map)+geom_point(data=test1,aes(x=경도,y=위도),color="#006633",alpha=0.3)

#-----------------------------------------------------------------------------------------


H.register2<-read.csv("연도별수소차량등록2.csv",header=T)
H.register2$울산 <- as.integer(H.register2$울산)

View(H.register2)

# 53개월 자료가 존재 
# test.test1 <- colSums(each.year[[52]][,4:20])
# test.test2 <- colSums(each.year[[53]][,4:20])
# test.test1
# test.test2
# test <- rbind(test.test1,test.test2)
# test
each.year <- H.register2 %>% split(H.register2$조회년월)

test.result<-data.frame()
for ( i in 1:53){
  test1 <- colSums(each.year[[i]][,4:20])
  test.result <- rbind(test.result,test1)
}
test.result

colnames(test.result)=c("서울특별시","부산광역시","대구광역시","인천광역시","광주광역시","대전광역시","울산광역시","세종특별자치시","경기도","강원도","충청북도","충청남도","전라북도","전라남도","경상북도","경상남도","제주특별자치도")
test.result
result <- colSums(test.result[,1:17])
View(result)
write.csv(real,file="지역수소차증가.csv")

real <- read.csv("수소차등록대수.csv",header=T)
real <- real[1,]

#-----------------------------------------------------------------------------------------

real <- read.csv("지역수소차증가.csv",header=T)
real
# korea <- getData('GADM',country='kor',level=1)
# ggplot()+geom_polygon(data=korea,aes(x=long,y=lat,group=group),fill='white',color='black')
# 
# head(korea)

korea <- shapefile("TL_SCCO_CTPRVN.shp")
korea <- spTransform(korea,CRS("+proj=longlat"))
korea_map <- fortify(korea)
merge_result <- merge(korea_map,real,by='id')
head(merge_result)


korea_map <- unique(korea_map)
test.result

View(test.result)
p <- ggplot()+geom_polygon(data=merge_result,aes(x=long,y=lat,group=group,fill=수소차))+labs(fill="수소차 누적 등록대수")
p <- p+theme_void()

ggmap(Map)+geom_polygon(data=merge_result,aes(x=long,y=lat,group=group,fill=수소차),alpha=0.75)+labs(fill="수소차 누적 등록대수")+geom_point(data=test1,aes(x=경도,y=위도),color="#006633",alpha=0.3)+geom_point(data=ge,aes(x=lon,y=lat),color="red")
ggmap(Map)+geom_point(data=test1,aes(x=경도,y=위도),color="#006633",alpha=0.3)+geom_point(data=ge,aes(x=lon,y=lat),color="red")

#ggmap(Map)+geom_point(data=test1,aes(x=경도,y=위도),color="#006633",alpha=0.3)

#-----------------------------------------------------------------------------------------





































