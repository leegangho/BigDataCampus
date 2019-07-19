# 중국 화창품 기업 데이터 Handling

# Goal: 19개 부총의 1월~12월까지의 매출액 분석

# Data Handling Process 
# 1. dealerpriceamt, c53amt,code는 고려하지 않는다. 
# 2. itemtype에서 TAN만 고려한다. 
# 3. Cancel Code가 존재하면, 해당 레코드는 제외한다. 

library(dplyr)

getwd()
setwd("C:/Users/fkaus/OneDrive/문서/China")

ESCAPE<-read.csv("Escape.csv",sep=",",header=T,stringsAsFactors = F)
ESCAPE$퇴사일자[ESCAPE$퇴사일자=="NULL"]<-"2016-01-01"


ESCAPE$입사일자<-as.Date(ESCAPE$입사일자)
ESCAPE$퇴사일자<-as.Date(ESCAPE$퇴사일자)
#ESCAPE$퇴사일자[1]-ESCAPE$퇴사일자[1]


# 먼저 NA값은 입사월이 NULL인 경우 발생한다. 이는 엄청 오래된 대리점을 의미한다. 일단 그대로 두기 
# 날짜를 계산했는데 음수값이 나오면 입사월이 퇴사월보다 나중인 경우이다. 따라서 이 부분은 0값으로 대체한다. 
WORKYEAR<-(ESCAPE$퇴사일자-ESCAPE$입사일자)/30
WORKYEAR[which(WORKYEAR[]<0)]<-0
WORKYEAR






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

# 변수는 9개, 필터 이후의 행의 개수는 2050505개 

#-------------------------------------------------------------------------

# 먼저 지점별로 나눌 수 있습니다. 
# 행의 개수는, 지점별 주문건수 입니다. 
SPLIT.DATA<-split(ALLDATA,ALLDATA$SoldToParty)


# 대리점별 연매출액, MarketPriceAmt 열의 값을 모두 더합니다. 
SALE.MONEY.YEAR<-c()
for(i in 1:10418){
  SALE.MONEY.YEAR[i]<-c(sum(SPLIT.DATA[[i]]$MarketPriceAmt))
}

# 연매출액 입니다. 
SALE.MONEY.YEAR
# 연매출액/12 == 대리점별 월 평균매출액입니다. 
SALE.MONEY.MONTH<-SALE.MONEY.YEAR/12
#--------------------------------------------------------

# 대리점별 각 행의 개수가 주문건수로 하였을때,
# table() 함수로 빈도수를 표현/12 로 하면 월평균주문건수

MONTH.ORDER.NUMBER<-table(ALLDATA$SoldToParty)/12
#-------------------------------------------------------



SORT.SOLDTOPARTY<-sort(ALLDATA$SoldToParty)

# 대리점코드 오름차순으로 정렬 (유니크하게)
DARI.CODE<-unique(SORT.SOLDTOPARTY)

ALLFDATA %>% split(ALLFDATA$FzCode)
VUCHONG<-unique(ALLFDATA[order(sort(ALLFDATA$FdealerCode)),])


str(VUCHONG)
str(WORKYEAR)

signiture<-data.frame(SALE.MONEY.MONTH,SALE.MONEY.YEAR)
cbind(signiture,VUCHONG)



as.data.frame(VUCHONG)
as.data.frame(WORKYEAR)
as.data.frame(SALE.MONEY.YEAR)
as.data.frame(SALE.MONEY.MONTH)
as.data.frame(MONTH.ORDER.NUMBER)


VUCHONG
WORKYEAR


SALE.MONEY.MONTH
SALE.MONEY.YEAR
MONTH.ORDER.NUMBER


signiture<-cbind(SALE.MONEY.MONTH,SALE.MONEY.YEAR,MONTH.ORDER.NUMBER)
signiture

#merge(signiture,VUCHONG)

View(signiture)
View(VUCHONG)
View(WORKYEAR)















# VUCHONG #check용

# VUCHONG<-c()
# TEST<-c()
# for (i in 1:10418){
#   if (sum(DARI.CODE[i]==ALLFDATA$FdealerCode)){
#     TEST[i]<-c(which(DARI.CODE[i]==ALLFDATA$FdealerCode))
#     VUCHONG[i]<-unique(ALLFDATA$FzCode[TEST])
#   }
# }

# TEST<-which(DARI.CODE[1]==ALLFDATA$FdealerCode)
# unique(ALLFDATA$FzCode[TEST])



# ORDER.NUMBER<-table(ALLDATA$SoldToParty,ALLDATA$SapID)
# ORDER.NUMBER
# ORDER.NUMBER<-data.frame(ORDER.NUMBER)
# ORDER.NUMBER$Freq<-ORDER.NUMBER$Freq/12
# ORDER.NUMBER
# 
# 
# SALE.MONEY<-split(ALLDATA,ALLDATA$SoldToParty)
# sale_money<-c()
# for (i in 1:10418){
#   sale_money[i]<-c(sum(SALE.MONEY[[i]]$MarketPriceAmt))
# }
# 
# signiture<-data.frame(ORDER.NUMBER,sale_money)
# signiture$sale_money_year<-signiture$sale_money/12
# signiture



# #----------------------------------------------------------------------------------
# 
# DATA1<-DATA1 %>% filter(ItemType=="TAN" & is.na(CancelCode))
# # 1월의 대리점 종류는 총 4985개
# DATA1<-split(DATA1,DATA1$SoldToParty)
# str(DATA1)
# DATA1.number<-c()
# for (i in 1:4985){
#   DATA1.number[i]<-c(nrow(DATA1[[i]]))
# }
# 
# 
# # 1월달 대리점별 주문건수가 들어있다. 대리점코드는 오름차순 되어있습니다.
# DATA1.number
# 
# #-----------------------------------------------------------------------------------
# 
# DATA2<-DATA2 %>% filter(ItemType=="TAN" & is.na(CancelCode))
# # 1월의 대리점 종류는 총 4985개
# DATA2<-split(DATA2,DATA2$SoldToParty)
# str(DATA2)
# head(DATA2)
# 
# 
# DATA2.number<-c()
# 
# for (i in 1:4568){
#   DATA1.number[i]<-c(nrow(DATA1[[i]]))
# }
# 
# # 1월달 대리점별 주문건수가 들어있다. 
# DATA1.number












# # 2015년 Order.csv 를 열 제거 후 합침
# ALLDATA=bind_rows(DATA1,DATA2,DATA3,DATA4,DATA5,DATA6,DATA7,DATA8,DATA9,DATA10,DATA11,DATA12)
# 
# 
# 
# 
# 
# 
# # 조건문으로 축소
# # ItempType=="TAN"과 결측치
# ALLDATA=ALLDATA %>% filter(ItemType=="TAN" & is.na(CancelCode))
# 
# #str(ALLDATA)  #check용 131만건, 8개 변수
# 
# 
# # 지점별로 분리합니다.
# SPLIT_NUMBER<-split(ALLDATA,ALLDATA$SoldToParty)
# 
# # 지점별 주문 건수 #return: sale_number에 넣습니다.
# #sale_number #9122개입니다. 지점이 총 9122개 입니다. #지점별 오름차순으로 정렬되었어요
# sale_number<-c()
# for (i in 1:10122){
#  sale_number[i]<-c(nrow(SPLIT_NUMBER[[i]]))
# }
# 
# 
# 
# #class(sale_number) # numeric
# # sale_numer #check용
# # 시그니쳐 테이블에 사용할 월평균건수 sale_numer
# 
# #-------------------------------------------------------------------------------
# 
# # Organize<-read.csv("Fdealer1501.csv",sep=",",header = T,stringsAsFactors = F)
# # Organize<-Organize[,-c(2,4,6,8,10)]
# # Organize$FdealerCode
# # str(Organize)
# 
# 
# # 월매출액
# 
# ALLDATA
# SPLIT_MONEY<-split(ALLDATA,ALLDATA$SoldToParty)
# SPLIT_MONEY[[1]]
# sum(SPLIT_MONEY[[1]]$MarketPriceAmt * SPLIT_MONEY[[1]]$OrderQty)/nrow(SPLIT_MONEY[[1]])
# 
# #SPLIT_MONEY[[1]]$MarketPriceAmt*SPLIT_MONEY[[1]]$OrderQty
# 
# sale_money<-c()
# for (i in 1:10122){
#   sale_money[i]<-c(sum(SPLIT_MONEY[[1]]$MarketPriceAmt * SPLIT_MONEY[[1]]$OrderQty)/nrow(SPLIT_MONEY[[1]]))
# }
# sale_money
# 
# # 시그니쳐 테이블에 사용할 월매출액 MarketPriceAmt 와 OrderQty를 곱합니다.
# # sale_money # numeric
# 
# 
# #data.frame(sale_money,sale_number) # test용
# 
# #----------------------------------------------------------------------------------
# 
# sort(ALLDATA$SoldToParty)
# signature_soldtoparty=sort(unique(ALLDATA$SoldToParty))
# class(signature_soldtoparty)
# 
# data.frame(signature_soldtoparty,sale_money,sale_number)








