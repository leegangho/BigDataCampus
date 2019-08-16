#20190807 
library(ggplot2)
library(ggmap)
library(treemap)
library(MASS)

register_google(key="AIzaSyDb6CtknFf0WsNEHDErgOZZM_pTPWMfPbs")

setwd("D:/BigDataCampus/practiceData/file")
data <- read.csv("hit.csv",header=TRUE)
avg<-data$average
name<-data$name
ggplot(data,aes(x=avg,y=name))+geom_point()
ggplot(data,aes(x=avg,y=reorder(name,avg)))+geom_point(size=3)

# reorder함수는 avg를 기준으로 이름을 정렬합니다. 
ggplot(data,aes(x=avg,y=reorder(name,avg)))+geom_point(size=3)+geom_segment(aes(yend=name),xend=0)
ggplot(data,aes(x=avg,y=reorder(name,avg)))+geom_point(size=3)+geom_segment(aes(yend=name),xend=0)+ylab("name")

#연습 
# data<-read.csv("hit2.csv",header=TRUE)
# avg<-data.$Avg
# name<-data$Name
# ggplot((data,aes(x=avg,y=reorder(name,avg)))+geom_point(size=3)+geom_segment(aes(yend=name),xend=0)+ylab("name")

fire<-read.csv("fire.csv")
head(fire)
lat<-fire$LAT
lon<-fire$LON
y2011<-fire$y2011
data<-data.frame(lat,lon,y2011)
data

map<-get_map("Seoul",zoom=10,maptype = "roadmap")
map<-ggmap(map)
map+geom_point(aes(x=lon,y=lat,color=y2011,size=y2011),data=data)

ge<-geocode(enc2utf8("용인"))
cen<-as.numeric(ge)
Map <- get_googlemap(center=cen)
ggmap(Map)

gc<-geocode(enc2utf8("설악산"))
cen<-as.numeric(gc)
Map<-get_googlemap(center=cen,zoom=8,size=c(640,640),maptype = "hybrid")
ggmap(Map)

cen <- c(-118.233248,34.085015)
map<-get_googlemap(center=cen)
ggmap(map)


names<-c("용두암","성산일출봉","정방폭포","중문관광단지","한라산 1100고지","차귀도")
addr<-c("제주시 용두암길 15","서귀포시 성산읍 성산리","서귀포시 동홍동 299-3","서귀포시 중문동 2624-1","서귀포시 색달동 산1-2","제주시 한경면 고산리 125")
gc<-geocode(enc2utf8(addr))
af<-data.frame(name=names,lon=gc$lon,lat=gc$lat)
head(af)


cen<-c(mean(af$lon),mean(af$lat))
map<-get_googlemap(center=cen,maptype="roadmap",zoom=10,size=c(640,640),marker=gc)
gmap<-ggmap(map)
gmap+geom_text(data=af,aes(x=lon,y=lat),size=5,label=af$name)


data("quakes")
head(quakes)

df <- head(quakes,100)
cen <- c(mean(df$long),mean(df$lat))
gc<-data.frame(lon=df$long,lat=df$lat)

# 구글맵에 사용하는 경도 값으로 변환합니다. 
gc$lon<-ifelse(gc$lon > 180, -(360-gc$lon),gc$lon)

map <- get_googlemap(center=cen,maptype="roadmap",zoom=4,marker=gc)
ggmap(map)+theme(axis.title.x=element_blank(),
                 axis.text.x=element_blank(),
                 axis.ticks.x=element_blank(),
                 axis.title.y=element_blank(),
                 axis.text.y=element_blank(),
                 axis.ticks.y=element_blank())

map<-get_googlemap(center=cen,maptype = "roadmap",zoom=5)
gmap<-ggmap(map)
gmap+geom_point(data=df,aes(x=long,y=lat,size=mag),alpha=0.5)



data<-read.csv("국회의원_선거구_유권자수.csv",header=T)
gsum<-aggregate(data[,5],by=list(data$시도),sum)
treemap(gsum,index=c("Group.1"),vSize="x",vColor="x",type="value",bg.labels="yellow")

st<-data.frame(state.x77)

# 버블차트 그리기 
symbols(st$Income,st$Illiteracy,circles=st$Population,inches=0.4,fg="white",bg="red",lwd=1.5,xlab="Income",ylab="llliteracy",main="US State data")
text(st$Income,st$Illiteracy,rownames(st),cex=0.5,col="blue")


# boxplot 그리기! 
data("airquality")
boxplot(Temp~Month,data=airquality,xlab="Month Number",ylab="Temp",main="Different boxplots for each month",col="orange")
boxplot(Wind~Month,data=airquality,xlab="Month Number",ylab="Wind",main="Different boxplots for each month",col="yellow")
boxplot(Ozone~Month,data=airquality,xlab="Month Number",ylab="Ozone",main="Different boxplots for each month",col="lightgray")


data("HairEyeColor")
str(HairEyeColor)
mosaicplot(~Eye+Hair,data=HairEyeColor,color=c("red","green"),off=1)

st<-data.frame(state.x77)
ggplot(data=st,aes(x=Income,y=Illiteracy))+geom_point()



cnt<-table(mtcars$gear)
gear<-c(3,4,5)
data<-cbind(cnt,gear)
data<-data.frame(data)
ggplot(data,aes(x=gear,y=cnt))+geom_bar(stat="identity",width=0.7,fill="steelblue")


data("airmiles")
str(airmiles)
head(airmiles)
year<-1937:1960
data<-cbind(year,airmiles)
air<-data.frame(data)
ggplot(air,aes(x=year,y=airmiles))+geom_line(color="red",size=1)
data

ggplot()+geom_boxplot(data=iris,aes(x=Species,y=Petal.Width,fill=Species))

#--------------------------------------------------------------------------------------

gc<-geocode(enc2utf8("서울시청"))
cen<-as.numeric(gc)
Map<-get_googlemap(center=cen,zoom=8,size=c(640,640),maptype = "roadmap")
ggmap(Map)


gc<-geocode(enc2utf8("금강산"))
cen<-as.numeric(gc)
Map<-get_googlemap(center=cen,zoom=8,size=c(640,640),maptype = "roadmap")
ggmap(Map)


cen=c(103.867881,1.331017)
map<-get_googlemap(center=cen,maptype = "roadmap",zoom=9)
ggmap(map)



names <- c("강서구청", "강동구청", "강남구청", "성북구청", "서울중구청", "은평구청", "금천구청", "광진구청", "서대문구청",
               
               "중랑구청", "강북구청", "관악구청", "구로구청", "영등포구청", "마포구청", "종로구청", "도봉구청", "용산구청",
               
               "동작구청", "서초구청", "송파구청", "노원구청", "성동구청", "양천구청","동대문구청")

addr<-c("서울특별시 강서구 화곡로 302 강서구청","서울특별시 강동구 성내로 25 강동구","서울특별시 강남구 학동로 426 강남구청",
        "서울특별시 성북구 보문로 168 성북구","서울특별시 중구 창경궁로 17 중구청","서울특별시 은평구 은평로 195 은평구","서울특별시 금천구 시흥대로73길 70 금천구 종합청사",
        "서울특별시 광진구 자양로 117 광진구","서울특별시 서대문구 연희로 248 서대문구청","서울특별시 중랑구 봉화산로 179 중랑구청","서울특별시 강북구 도봉로89길 13 강북구청",
        "서울특별시 관악구 관악로 145","서울특별시 구로구 가마산로 245 구로","서울특별시 영등포구 당산로 123 영등포구청","서울특별시 마포구 월드컵로 212 마포구청",
        "서울특별시 종로구 삼봉로 43 종로구청","","서울특별시 도봉구 마들로 656 도봉구청","서울특별시 용산구 녹사평대로 150 용산구종합행정타운","서울특별시 동작구 장승배기로 161 동작구청",
        "서울특별시 서초구 남부순환로 2584 서초구청","서울특별시 송파구 올림픽로 326 송파구청","서울특별시 노원구 노해로 437 노원구청",
        "서울특별시 성동구 고산자로 270 성동구청","서울특별시 양천구 목동동로 105 양천구청")

gc<-geocode(enc2utf8(addr))
gc$lon<-ifelse(gc$lon > 180, -(360-gc$lon),gc$lon)
df<-data.frame(name=names,lon=gc$lon,lat=gc$lat)

df<-df[-17,]

cen<-c(mean(df$lon),mean(df$lat))
map<-get_googlemap(center=cen,maptype = "roadmap",zoom=10,size=c(640,640),marker=gc)
ggmap(map)


names<-c("서울특별시","부산광역시","대구광역시","인천광역시","광주광역시","대전광역시","울산광역시")
addr<-c("서울특별시","부산광역시","대구광역시","인천광역시","광주광역시","대전광역시","울산광역시")
gc <- geocode(enc2utf8(addr))
df<-data.frame(name=names,lon=gc$lon,lat=gc$lat)
cen<-c(mean(df$lon),mean(df$lat))
map<-get_googlemap(center=cen,maptype = "roadmap",zoom=7,size=c(640,640),marker=gc)
ggmap(map)



















