library(dplyr)
library(stringr)
library(lubridate)
setwd("C:/Users/fkaus/OneDrive/문서/China")

DATA1<-read.csv("Order1501.csv",sep=",",header=T,stringsAsFactors = F)
DATA2<-read.csv("Order1502.csv",sep=",",header=T,stringsAsFactors = F)
DATA3<-read.csv("Order1503.csv",sep=",",header=T,stringsAsFactors = F)
DATA4<-read.csv("Order1504.csv",sep=",",header=T,stringsAsFactors = F)
DATA5<-read.csv("Order1505.csv",sep=",",header=T,stringsAsFactors = F)
DATA6<-read.csv("Order1506.csv",sep=",",header=T,stringsAsFactors = F)
DATA7<-read.csv("Order1507.csv",sep=",",header=T,stringsAsFactors = F)
DATA8<-read.csv("Order1508.csv",sep=",",header=T,stringsAsFactors = T)
DATA9<-read.csv("Order1509.csv",sep=",",header=T,stringsAsFactors = F)
DATA10<-read.csv('Order1510.csv',sep=",",header=T,stringsAsFactors = F)
DATA11<-read.csv("Order1511.csv",sep=",",header=T,stringsAsFactors = F)
DATA12<-read.csv("Order1512.csv",sep=",",header=T,stringsAsFactors = F)

DATA1=DATA1[,-c(3,5,8,10,12,13,14,16)]
DATA2=DATA2[,-c(3,5,8,10,12,13,14,16)]
DATA3=DATA3[,-c(3,5,8,10,12,13,14,16)]
DATA4=DATA4[,-c(3,5,8,10,12,13,14,16)]
DATA5=DATA5[,-c(3,5,8,10,12,13,14,16)]
DATA6=DATA6[,-c(3,5,8,10,12,13,14,16)]
DATA7=DATA7[,-c(3,5,8,10,12,13,14,16)]
DATA8=DATA8[,-c(3,5,8,10,12,13,14,16)]
DATA9=DATA9[,-c(3,5,8,10,12,13,14,16)]
DATA10=DATA10[,-c(3,5,8,10,12,13,14,16)]
DATA11=DATA11[,-c(3,5,8,10,12,13,14,16)]
DATA12=DATA12[,-c(3,5,8,10,12,13,14,16)]

# DATA8의 타입이 다르므로, 타입을 맞춘다. 
DATA8$OrderDate<-as.character(DATA8$OrderDate)
DATA8$ItemType<-as.character(DATA8$ItemType)
DATA8$ItemNo<-as.character(DATA8$ItemNo)
DATA8$MarketPriceAmt<-as.numeric(DATA8$MarketPriceAmt)
DATA8$OrderStatus<-as.character(DATA8$OrderStatus)
DATA8$OrderQty<-as.integer(DATA8$OrderQty)

# Fdealer을 묶습니다. 
FDATA1<-read.csv("Fdealer1501.csv",sep=",",header=T,stringsAsFactors = F)
FDATA2<-read.csv("Fdealer1502.csv",sep=",",header=T,stringsAsFactors = F)
FDATA3<-read.csv("Fdealer1503.csv",sep=",",header=T,stringsAsFactors = F)
FDATA4<-read.csv("Fdealer1504.csv",sep=",",header=T,stringsAsFactors = F)
FDATA5<-read.csv("Fdealer1505.csv",sep=",",header=T,stringsAsFactors = F)
FDATA6<-read.csv("Fdealer1506.csv",sep=",",header=T,stringsAsFactors = F)
FDATA7<-read.csv("Fdealer1507.csv",sep=",",header=T,stringsAsFactors = F)
FDATA8<-read.csv("Fdealer1508.csv",sep=",",header=T,stringsAsFactors = F)
FDATA9<-read.csv("Fdealer1509.csv",sep=",",header=T,stringsAsFactors = F)
FDATA10<-read.csv('Fdealer1510.csv',sep=",",header=T,stringsAsFactors = F)
FDATA11<-read.csv("Fdealer1511.csv",sep=",",header=T,stringsAsFactors = F)
FDATA12<-read.csv("Fdealer1512.csv",sep=",",header=T,stringsAsFactors = F)

ALLFDATA=bind_rows(FDATA1,FDATA2,FDATA3,FDATA4,FDATA5,FDATA6,FDATA7,FDATA8,FDATA9,FDATA10,FDATA11,FDATA12)
ALLFDATA<-ALLFDATA[,-c(2:8,10)]

ALLDATA=bind_rows(DATA1,DATA2,DATA3,DATA4,DATA5,DATA6,DATA7,DATA8,DATA9,DATA10,DATA11,DATA12)
ALLDATA=ALLDATA %>% filter(ItemType=="TAN" & is.na(CancelCode))
#-----------------------------------------------------------------------------------------------


ORDER.NUMBER<-data.frame(ALLDATA %>% group_by(SoldToParty) %>% summarise(주문건수=length(table(SapID))))
#ORDER.NUMBER

SALE.MONEY<- data.frame(ALLDATA %>% group_by(SoldToParty) %>% summarise(연매출액=sum(MarketPriceAmt)))
#SALE.MONEY

signiture.table<-merge(ORDER.NUMBER,SALE.MONEY,by="SoldToParty")
#signiture.table

#-----------------------------------------------------------------------------------------------

DATE<-read.csv("date.csv",sep=",",header=T,stringsAsFactors = F)
DATE<-DATE[c(1:29417),]

names(DATE)[names(DATE)=="대리점코드"]<-c("SoldToParty")
#names(DATE)

# 특이사항 발견
# 주문건수는 있지만 입사일자와 퇴사일자가 없는 행이 존재함 56개의 대리점 
# 56개는 제외 합니다. 

signiture.table<-merge(signiture.table,DATE,by="SoldToParty",all = FALSE)
signiture.table
#-----------------------------------------------------------------------------------------------

COMPUTE.DATE<-data.frame(입사일자=signiture.table$입사일자,퇴사일자=signiture.table$퇴사일자)
COMPUTE.DATE[1]<-as.Date(COMPUTE.DATE$입사일자)
COMPUTE.DATE[2]<-as.Date(COMPUTE.DATE$퇴사일자)


# 퇴사일자가 없는 곳은 2015-12-31로 정함
# 2015년에 근무개월을 계산해야 하므로, 퇴사일자가 2015-12-31 이상이면 2015-12-31로 함 
COMPUTE.DATE[is.na(COMPUTE.DATE[2]),2]<-as.Date("2015-12-31")
COMPUTE.DATE[COMPUTE.DATE[[2]]>as.Date("2015-12-31"),2]<-as.Date("2015-12-31")

# 입사일자가 2015년 이전이면 2015-01-01로 하여, 2015년 근무한 날짜만 계산하도록 합니다. 
COMPUTE.DATE[is.na(COMPUTE.DATE[1]),1]<-as.Date("2001-04-21")
COMPUTE.DATE[COMPUTE.DATE[[1]]<as.Date("2015-01-01"),1]<-as.Date("2015-01-01")
COMPUTE.DATE$근속개월<-1
COMPUTE.DATE$근속일<-COMPUTE.DATE[,2]-COMPUTE.DATE[,1]
COMPUTE.DATE$근속개월<-COMPUTE.DATE$근속일/30
COMPUTE.DATE

COMPUTE.DATE$근속일<-COMPUTE.DATE[2]-COMPUTE.DATE[1]
COMPUTE.DATE$근속일<-as.numeric(COMPUTE.DATE$근속일)
COMPUTE.DATE$근속일<-COMPUTE.DATE[3]/30



signiture.table<-signiture.table[,-c(6,7)]
signiture.table$근속개월<-1
signiture.table$근속개월<-COMPUTE.DATE$근속일

signiture.table$근속개월<-COMPUTE.DATE$근속개월
signiture.table$근속개월<-as.numeric(signiture.table$근속개월)
head(signiture.table)

#-----------------------------------------------------------------------------------------------

#이탈여부 구하기(0:이탈안함, 1:이탈)
date<-read.csv("date.csv")
date<-date[1:29417, ]
date[2]<-as.Date(date[[2]])
date[3]<-as.Date(date[[3]])
date[is.na(date[[3]]), 3]<-as.Date("2020-10-10")
date[date[[3]]>=as.Date("2016-01-01"), 3]<-NA
test1 <- is.na(date[3])
date$이탈여부[test1]<-0
date[is.na(date[4]),4]<-1
head(date)

names(date)[names(date)=="대리점코드"]<-c("SoldToParty")


signiture.table<-merge(signiture.table,date,by="SoldToParty",all = FALSE)
signiture.table<-signiture.table[,-c(7,8)]
names(signiture.table)[names(signiture.table)=="입사일자.x"]<-c("입사일자")
names(signiture.table)[names(signiture.table)=="퇴사일자.x"]<-c("퇴사일자")
head(signiture.table)
View(signiture.table)


names(ALLFDATA)[names(ALLFDATA)=="FdealerCode"]<-c("SoldToParty")

table(ALLFDATA)
ALLFDATA<-ALLFDATA[order(unique(ALLFDATA$SoldToParty)),]



signiture.table<-merge(signiture.table,ALLFDATA,by="SoldToParty",all=F)
signiture.table<-signiture.table[order(unique(signiture.table$SoldToParty)),]
signiture.table
# signiture.table<-merge(signiture.table,ALLFDATA,by="SoldToParty",all.x=FALSE)
# signiture.table





































