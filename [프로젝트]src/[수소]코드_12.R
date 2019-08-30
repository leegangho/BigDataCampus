#201890826
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


setwd("C:/Users/fkaus/OneDrive/바탕 화면/데이터캠퍼스 프로젝트/데이터")

korea <- getData('GADM',country='kor',level=3)
ggplot()+geom_polygon(data=korea,aes(x=long,y=lat,group=group),fill="white",color="black")


tdf <- st_read("TL_SCCO_EMD.shp")
str(tdf)
head(tdf)

tdf$EMD_CD <- as.character(EMD_CD)
tdf$EMD_ENG_NM <- as.character(EMD_ENG_NM)
tdf$EMD_KOR_NM <- iconv(emd_shp$EMD_KOR_NM,from="CP949", to="UTF-8",sub=NA,mark=TRUE,toRaw=FALSE)

tdf %>% 
  as.tibble %>%
  mutate( 
    EMD_CD = as.character(EMD_CD), 
    EMD_ENG_NM = as.character(EMD_ENG_NM),
    EMD_KOR_NM = iconv(EMD_KOR_NM, localeToCharset(), "UTF-8")
  ) -> tdf_EMD



                                                         
tdf_EMD %>% filter(substr(EMD_CD, start = 1, stop = 2) == "43")  -> tdf_chung 

str(tdf_EMD)
str(tdf_chung)


ggplot(tdf_chung)
#devtools::install_github("tidyverse/ggplot2")
#-------------------------------------------------------------------------------------------

devtools::install_github("cardiomoon/Kormaps")
install.packages("units")
library(Kormaps)
library(tmap)
library(raster)

require(Kormaps)
require(leaflet)

  









