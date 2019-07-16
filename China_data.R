# 중국 화창품 기업 데이터 Handling

# Goal: 19개 부총의 1월~12월까지의 매출액 분석

# Data Handling Process 
# 1. dealerpriceamt, c53amt,code는 고려하지 않는다. 
# 2. itemtype에서 TAN만 고려한다. 
# 3. Cancel Code가 존재하면, 해당 레코드는 제외한다. 

library(dplyr)

getwd()
setwd("C:/Users/fkaus/OneDrive/문서/China")

DATA1<-read.csv("Order1501.csv",sep=",",header=T,stringsAsFactors = F)
DATA2<-read.csv("Order1502.csv",sep=",",header=T,stringsAsFactors = F)
DATA3<-read.csv("Order1503.csv",sep=",",header=T,stringsAsFactors = F)
DATA4<-read.csv("Order1504.csv",sep=",",header=T,stringsAsFactors = F)
DATA5<-read.csv("Order1505.csv",sep=",",header=T,stringsAsFactors = F)
DATA6<-read.csv("Order1506.csv",sep=",",header=T,stringsAsFactors = F)
DATA7<-read.csv("Order1507.csv",sep=",",header=T,stringsAsFactors = F)
DATA8<-read.csv("Order1508.csv",sep=",",header=T,stringsAsFactors = F)
DATA9<-read.csv("Order1509.csv",sep=",",header=T,stringsAsFactors = F)
DATA10<-read.csv('Order1510.csv',sep=",",header=T,stringsAsFactors = F)
DATA11<-read.csv("Order1511.csv",sep=",",header=T,stringsAsFactors = F)
DATA12<-read.csv("Order1512.csv",sep=",",header=T,stringsAsFactors = F)

DATA1=DATA1[,-c(1,3,5,8,10,12,13,14,16)]
DATA2=DATA2[,-c(1,3,5,8,10,12,13,14,16)]
DATA3=DATA3[,-c(1,3,5,8,10,12,13,14,16)]
DATA4=DATA4[,-c(1,3,5,8,10,12,13,14,16)]
DATA5=DATA5[,-c(1,3,5,8,10,12,13,14,16)]
DATA6=DATA6[,-c(1,3,5,8,10,12,13,14,16)]
DATA7=DATA7[,-c(1,3,5,8,10,12,13,14,16)]
DATA8=DATA8[,-c(1,3,5,8,10,12,13,14,16)]
DATA9=DATA9[,-c(1,3,5,8,10,12,13,14,16)]
DATA10=DATA10[,-c(1,3,5,8,10,12,13,14,16)]
DATA11=DATA11[,-c(1,3,5,8,10,12,13,14,16)]
DATA12=DATA12[,-c(1,3,5,8,10,12,13,14,16)]

# 2015년 Order.csv 를 열 제거 후 합침
ALLDATA=bind_rows(DATA1,DATA2,DATA3,DATA4,DATA5,DATA6,DATA7,DATA8,DATA9,DATA10,DATA11,DATA12)

# 조건문으로 축소
ALLDATA=ALLDATA %>% filter(ItemType=="TAN" & is.na(CancelCode))

# 월매출건수를 위한 TOTAL
TOTAL=nrow(ALLDATA)


#str(ALLDATA)  #check용 131만건, 8개 변수 


# 지점별로 분리합니다.
SPLIT_NUMBER<-split(ALLDATA,ALLDATA$SoldToParty)
SPLIT_NUMBER[[1]]



for (i in 1:10122){
  sale_number[i]<-c(nrow(SPLIT_NUMBER[[i]])/TOTAL)
}
#class(sale_number) # numeric
# sale_numer #check용 
# 시그니쳐 테이블에 사용할 월평균건수 sale_numer

#-------------------------------------------------------------------------------

# Organize<-read.csv("Fdealer1501.csv",sep=",",header = T,stringsAsFactors = F)
# Organize<-Organize[,-c(2,4,6,8,10)]
# Organize$FdealerCode
# str(Organize)


# 월매출액 

ALLDATA
SPLIT_MONEY<-split(ALLDATA,ALLDATA$SoldToParty)
SPLIT_MONEY[[1]]
sum(SPLIT_MONEY[[1]]$MarketPriceAmt * SPLIT_MONEY[[1]]$OrderQty)/nrow(SPLIT_MONEY[[1]])

#SPLIT_MONEY[[1]]$MarketPriceAmt*SPLIT_MONEY[[1]]$OrderQty

sale_money<-c()
for (i in 1:10122){
  sale_money[i]<-c(sum(SPLIT_MONEY[[1]]$MarketPriceAmt * SPLIT_MONEY[[1]]$OrderQty)/nrow(SPLIT_MONEY[[1]]))
}
sale_money

# 시그니쳐 테이블에 사용할 월매출액 MarketPriceAmt 와 OrderQty를 곱합니다. 
# sale_money # numeric


#data.frame(sale_money,sale_number) # test용 

#----------------------------------------------------------------------------------

sort(ALLDATA$SoldToParty)
signature_soldtoparty=sort(unique(ALLDATA$SoldToParty))
class(signature_soldtoparty)

data.frame(signature_soldtoparty,sale_money,sale_number)








