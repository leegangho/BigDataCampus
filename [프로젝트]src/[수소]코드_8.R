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



setwd("C:/Users/fkaus/OneDrive/바탕 화면/데이터캠퍼스 프로젝트/데이터/흥덕 좌표")
register_google(key="AIzaSyDb6CtknFf0WsNEHDErgOZZM_pTPWMfPbs")

#충북 중앙 지점
location <- c(lon=127.40645,lat=36.6360912)


one <- read.csv("kid.csv",header=T)
two <- read.csv("대피소.csv",header=T)
three <- read.csv("충북_흥덕구_아파트.csv",header=T)
four <- read.csv("충북_흥덕구_학교.csv",header=T)
five <- read.csv("충북_흥덕구_백화점.csv",header=T)

all <- read.csv("충북_흥덕구_어린이집_대피소_아파트_학교.csv",header=T)
write.csv(all,file="흥덕구_전체.csv")


# 대략 반경 250m
leaflet() %>% addTiles() %>% setView(127.40645,36.6360912, zoom = 12) %>%  addCircles(all$lon,all$lat,weight=0.01,radius = 100,color="red")
leaflet() %>% fitBounds(127.40645,36.6360912)

#------------------------------------------------------------------------------------
setwd("C:/Users/fkaus/OneDrive/바탕 화면/데이터캠퍼스 프로젝트/데이터")
emd_shp <- st_read("TL_SCCO_EMD.shp")
emd_shp$EMD_KOR_NM <- iconv(emd_shp$EMD_KOR_NM,from="CP949", to="UTF-8",sub=NA,mark=TRUE,toRaw=FALSE)

chungbuk_emd_shp <- emd_shp %>% mutate(SIGUNGU=case_when(str_detect(EMD_CD,"^43111")~"상당구",
                                                         str_detect(EMD_CD,"^43112")~"서원구",
                                                         str_detect(EMD_CD,"^43113")~"흥덕구",
                                                         str_detect(EMD_CD,"^43114")~"청원구",
                                                         str_detect(EMD_CD,"^43130")~"충주시",
                                                         str_detect(EMD_CD,"^43150")~"제천시",
                                                         str_detect(EMD_CD,"^43720")~"보은군",
                                                         str_detect(EMD_CD,"^43730")~"옥천군",
                                                         str_detect(EMD_CD,"^43740")~"영동군",
                                                         str_detect(EMD_CD,"^43745")~"증평군",
                                                         str_detect(EMD_CD,"^43750")~"진천군",
                                                         str_detect(EMD_CD,"^43760")~"괴산군",
                                                         str_detect(EMD_CD,"^43770")~"음성군",
                                                         str_detect(EMD_CD,"^43800")~"단양군",)) %>% filter(!is.na(SIGUNGU))

View(chungbuk_emd_shp)


chungbuk_emd_shp %>% group_by(SIGUNGU) %>% summarise('동수'=n()) %>% plot()

st_write(chungbuk_emd_shp,"chungbuk_emd_shp.shp",delete_dsn = T)
st_crs(emd_shp)
emd_shp

#---------------------------------------------------------------------------------------------------------
chungbuk_emd_shp.흥덕 <- chungbuk_emd_shp %>% filter(SIGUNGU=="흥덕구")
chungbuk_emd_shp.흥덕[,1] %>% plot()
chungbuk_emd_shp.흥덕[,1]

chungbuk_emd_shp.흥덕 <- as.data.frame(chungbuk_emd_shp.흥덕)
chungbuk_emd_shp.흥덕$EMD_CD <- as.data.frame(chungbuk_emd_shp.흥덕$EMD_CD)
?fortify
#chungbuk_emd_shp.흥덕 <- spTransform(chungbuk_emd_shp.흥덕,proj4string("+proj=longlat"))

chungbuk_emd_shp <- as(chungbuk_emd_shp,'Spatial')
chungbuk_emd_shp <- fortify(chungbuk_emd_shp.흥덕)
View(chungbuk_emd_shp)
View(chungbuk_emd_shp)

chungbuk_emd_shp %>% ggplot(aes(x=long,y=lat,group=group))+geom_path(color='blue',size=.2)+coord_fixed(1.3)
chungbuk_emd_shp %>% ggplot(aes(x=long,y=lat,group=group))+geom_polygon(color='white',fill='skyblue')+coord_fixed(1.3)+guides(fill=FALSE)+theme_void()



chungbuk_df_shp <- emd_shp %>% mutate(SIGUNGU=case_when(str_detect(EMD_CD,"^43111")~"상당구",
                                                         str_detect(EMD_CD,"^43112")~"서원구",
                                                         str_detect(EMD_CD,"^43113")~"흥덕구",
                                                         str_detect(EMD_CD,"^43114")~"청원구",
                                                         str_detect(EMD_CD,"^43130")~"충주시",
                                                         str_detect(EMD_CD,"^43150")~"제천시",
                                                         str_detect(EMD_CD,"^43720")~"보은군",
                                                         str_detect(EMD_CD,"^43730")~"옥천군",
                                                         str_detect(EMD_CD,"^43740")~"영동군",
                                                         str_detect(EMD_CD,"^43745")~"증평군",
                                                         str_detect(EMD_CD,"^43750")~"진천군",
                                                         str_detect(EMD_CD,"^43760")~"괴산군",
                                                         str_detect(EMD_CD,"^43770")~"음성군",
                                                         str_detect(EMD_CD,"^43800")~"단양군",)) %>% filter(!is.na(SIGUNGU))
chungbuk_emd_shp <- as(chungbuk_df_shp,'Spatial')
chungbuk_emd_df <- fortify(chungbuk_emd_shp)
chungbuk_emd_df %>% ggplot(aes(x=long,y=lat,group=group))+geom_path(color='blue',size=.2)+coord_fixed(1.3)
chungbuk_emd_df %>% ggplot(aes(x=long,y=lat,group=group))+geom_polygon(color="white",fill="black")+coord_fixed(1.3)+guides(fill=F)+theme_void()






#_------------------------------------

location <- c(lon=127.40645,lat=36.6360912)
Hiresult <- ggmap(get_map(location = location,zoom=12,maptype = 'roadmap'))+geom_hex(binwidth=c(0.1/12,0.08/12), alpha = 0.3)+geom_point(data=ge,aes(x=lon,y=lat),size=5,alpha=0.1)+geom_point(data=mart,aes(x=lon,y=lat),color="red")+geom_point(data=chungbuk.school,aes(x=lon,y=lat),color="green",size=5,alpha=0.1)+geom_point(data=test3,aes(x=경도,y=위도),color="yellow",size=5,alpha=0.1)+geom_point(data=test4,aes(x=경도,y=위도),color="blue",size=5,alpha=0.1)







chungbuk_emd_shp.흥덕 <- as(chungbuk_emd_shp.흥덕,'Spatial')
chungbuk_emd_shp.흥덕 <- fortify(chungbuk_emd_shp.흥덕)
View(chungbuk_emd_shp)


chungbuk_emd_shp.흥덕 %>% ggplot(aes(x=long,y=lat,group=group))+geom_path(color='blue',size=.2)+coord_fixed(1.3)
chungbuk_emd_shp.흥덕 %>% ggplot(aes(x=long,y=lat,group=group))+geom_polygon(color='white',fill='skyblue',alpha=0.4)+coord_fixed(1.3)+guides(fill=FALSE)+theme_void()

ggplot(data=chungbuk_emd_shp.흥덕,aes(x=long,y=lat,group=group))+geom_polygon(color='white',fill='skyblue',alpha=0.4)+coord_fixed(1.3)+guides(fill=FALSE)+theme_void()
head(chungbuk_emd_shp.흥덕)

ggmap(get_map(location=location,zoom=12,maptype="roadmap"))


map <- get_map(location=location,zoom=12,maptype="roadmap")
ggmap(map)+geom_polygon(data=chungbuk_emd_shp.흥덕,aes(x=long,y=lat,group=group),alpha=0.5)


#-----------------------------------------------------------------------------------
setwd("C:/Users/fkaus/OneDrive/바탕 화면/데이터캠퍼스 프로젝트/데이터/전국도시별_위도_경도")
a <- read.csv("chungbuk.csv",header=T)

korea <- shapefile("TL_SCCO_EMD.shp")
korea
korea <- spTransform(korea,CRS("+proj=longlat"))
korea_map <- fortify(korea)
View(korea_map)


































