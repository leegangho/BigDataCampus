library(dplyr)

setwd("C:/Users/fkaus/OneDrive/문서/China")
Ord01=read.csv("Order1501.csv",header = T)
Ord02=read.csv("Order1502.csv",header = T)
Ord03=read.csv("Order1503.csv",header = T)
Ord04=read.csv("Order1504.csv",header = T)
Ord05=read.csv("Order1505.csv",header = T)
Ord06=read.csv("Order1506.csv",header = T)
Ord07=read.csv("Order1507.csv",header = T)
Ord08=read.csv("Order1508.csv",header = T)
Ord09=read.csv("Order1509.csv",header = T)
Ord10=read.csv("Order1510.csv",header = T)
Ord11=read.csv("Order1511.csv",header = T)
Ord12=read.csv("Order1512.csv",header = T)

Ord01=Ord01[!complete.cases(Ord01$CancelCode)&Ord01$ItemType=="TAN",]
Ord02=Ord02[!complete.cases(Ord02$CancelCode)&Ord02$ItemType=="TAN",]
Ord03=Ord03[!complete.cases(Ord03$CancelCode)&Ord03$ItemType=="TAN",]
Ord04=Ord04[!complete.cases(Ord04$CancelCode)&Ord04$ItemType=="TAN",]
Ord05=Ord05[!complete.cases(Ord05$CancelCode)&Ord05$ItemType=="TAN",]
Ord06=Ord06[!complete.cases(Ord06$CancelCode)&Ord06$ItemType=="TAN",]
Ord07=Ord07[!complete.cases(Ord07$CancelCode)&Ord07$ItemType=="TAN",]
Ord08=Ord08[!complete.cases(Ord08$CancelCode)&Ord08$ItemType=="TAN",]
Ord09=Ord09[!complete.cases(Ord09$CancelCode)&Ord09$ItemType=="TAN",]
Ord10=Ord10[!complete.cases(Ord10$CancelCode)&Ord10$ItemType=="TAN",]
Ord11=Ord11[!complete.cases(Ord11$CancelCode)&Ord11$ItemType=="TAN",]
Ord12=Ord12[!complete.cases(Ord12$CancelCode)&Ord12$ItemType=="TAN",]

#8월 자료변환
Ord08$MarketPriceAmt=as.numeric(Ord08$MarketPriceAmt)
Ord08$OrderQty=as.integer(Ord08$OrderQty)
#head(Ord08$MarketPriceAmt)
#order1508

Ord08$OrderDate<-as.character(Ord08$OrderDate)
Ord08$ItemType<-as.character(Ord08$ItemType)
Ord08$ItemNo<-as.character(Ord08$ItemNo)
Ord08$MarketPriceAmt<-as.numeric(Ord08$MarketPriceAmt)
Ord08$OrderStatus<-as.character(Ord08$OrderStatus)
Ord08$OrderQty<-as.integer(Ord08$OrderQty)
Ord08$DealerPriceAmt<-as.numeric(Ord08$DealerPriceAmt)
Ord08$C53Amt<-as.numeric(Ord08$C53Amt)

names(Ord03)[13] <- c("C53Amt")
names(Ord03)[5] <- c("FdealerName")

Del01=read.csv("Delivery1501.csv",header = T)
Del02=read.csv("Delivery1502.csv",header = T)
Del03=read.csv("Delivery1503.csv",header = T)
Del04=read.csv("Delivery1504.csv",header = T)
Del05=read.csv("Delivery1505.csv",header = T)
Del06=read.csv("Delivery1506.csv",header = T)
Del07=read.csv("Delivery1507.csv",header = T)
Del08=read.csv("Delivery1508.csv",header = T)
Del09=read.csv("Delivery1509.csv",header = T)
Del10=read.csv("Delivery1510.csv",header = T)
Del11=read.csv("Delivery1511.csv",header = T)
Del12=read.csv("Delivery1512.csv",header = T)






########################################################################
#Cancelcode와 TAN필터링

Ord01<-Ord01%>%filter(ItemType=="TAN")
Ord01<-Ord01%>%filter(is.na(CancelCode)==T)

Ord02<-Ord02%>%filter(ItemType=="TAN")
Ord02<-Ord02%>%filter(is.na(CancelCode)==T)

Ord03<-Ord03%>%filter(ItemType=="TAN")
Ord03<-Ord03%>%filter(is.na(CancelCode)==T)

Ord04<-Ord04%>%filter(ItemType=="TAN")
Ord04<-Ord04%>%filter(is.na(CancelCode)==T)

Ord05<-Ord05%>%filter(ItemType=="TAN")
Ord05<-Ord05%>%filter(is.na(CancelCode)==T)

Ord06<-Ord06%>%filter(ItemType=="TAN")
Ord06<-Ord06%>%filter(is.na(CancelCode)==T)

Ord07<-Ord07%>%filter(ItemType=="TAN")
Ord07<-Ord07%>%filter(is.na(CancelCode)==T)

Ord08<-Ord08%>%filter(ItemType=="TAN")
Ord08<-Ord08%>%filter(is.na(CancelCode)==T)

Ord09<-Ord09%>%filter(ItemType=="TAN")
Ord09<-Ord09%>%filter(is.na(CancelCode)==T)

Ord10<-Ord10%>%filter(ItemType=="TAN")
Ord10<-Ord10%>%filter(is.na(CancelCode)==T)

Ord11<-Ord11%>%filter(ItemType=="TAN")
Ord11<-Ord11%>%filter(is.na(CancelCode)==T)

Ord12<-Ord12%>%filter(ItemType=="TAN")
Ord12<-Ord12%>%filter(is.na(CancelCode)==T)


DE<-rbind(Del01,Del02,Del03,Del04,Del05,Del06,Del07,Del08,Del09,
          Del10,Del11,Del12)

DE<-DE%>%filter(ItemType=="TAN")

DE$SapID<-as.numeric(DE$SapID)

Ord01$SapID<-as.numeric(Ord01$SapID)
###########################################################################3

#########################################################################
#상품개별가격구하기
item1<-Ord01%>%select(ItemNo,MarketPriceAmt,OrderQty)
item1$price<-item1$MarketPriceAmt/item1$OrderQty
item1<-item1[,-c(2,3)]

item2<-Ord02%>%select(ItemNo,MarketPriceAmt,OrderQty)
item2$price<-item2$MarketPriceAmt/item2$OrderQty
item2<-item2[,-c(2,3)]

item3<-Ord03%>%select(ItemNo,MarketPriceAmt,OrderQty)
item3$price<-item3$MarketPriceAmt/item3$OrderQty
item3<-item3[,-c(2,3)]

item4<-Ord04%>%select(ItemNo,MarketPriceAmt,OrderQty)
item4$price<-item4$MarketPriceAmt/item4$OrderQty
item4<-item4[,-c(2,3)]

item5<-Ord05%>%select(ItemNo,MarketPriceAmt,OrderQty)
item5$price<-item5$MarketPriceAmt/item5$OrderQty
item5<-item5[,-c(2,3)]

item6<-Ord06%>%select(ItemNo,MarketPriceAmt,OrderQty)
item6$price<-item6$MarketPriceAmt/item6$OrderQty
item6<-item6[,-c(2,3)]

item7<-Ord07%>%select(ItemNo,MarketPriceAmt,OrderQty)
item7$price<-item7$MarketPriceAmt/item7$OrderQty
item7<-item7[,-c(2,3)]

item8<-Ord08%>%select(ItemNo,MarketPriceAmt,OrderQty)
item8$price<-item8$MarketPriceAmt/item8$OrderQty
item8<-item8[,-c(2,3)]

item9<-Ord09%>%select(ItemNo,MarketPriceAmt,OrderQty)
item9$price<-item9$MarketPriceAmt/item9$OrderQty
item9<-item9[,-c(2,3)]

item10<-Ord10%>%select(ItemNo,MarketPriceAmt,OrderQty)
item10$price<-item10$MarketPriceAmt/item10$OrderQty
item10<-item10[,-c(2,3)]

item11<-Ord11%>%select(ItemNo,MarketPriceAmt,OrderQty)
item11$price<-item11$MarketPriceAmt/item11$OrderQty
item11<-item11[,-c(2,3)]

item12<-Ord12%>%select(ItemNo,MarketPriceAmt,OrderQty)
item12$price<-item12$MarketPriceAmt/item12$OrderQty
item12<-item12[,-c(2,3)]


#상품목록합치기
item<-rbind(item1,item2,item3,item4,item5,item6,item7
            ,item8,item9,item10,item11,item12)

#중복되는 상품제거
item<-distinct(item,ItemNo,price)


rm(item1,item2,item3,item4,item5,item6,item7
   ,item8,item9,item10,item11,item12)

Ord<-rbind(Ord01,Ord02,Ord03,Ord04,Ord05,Ord06,Ord07,Ord08,Ord09,Ord10,Ord11,Ord12)

#각 주문당 총액구하기
ORD<-Ord%>%group_by(SapID)%>%mutate(sumP=sum(MarketPriceAmt))
ORD<-ORD%>%select(SapID,sumP)

#중복되는 주문 지우기
ORD<-distinct(ORD,SapID,sumP)

#주문일자와 주문 구하기
Ord_code<-Ord%>%select(OrderDate,SapID)
#중복되는 주문 지우기
Ord_code<-distinct(Ord_code,OrderDate,SapID)

#주문코드를 기준으로 주문일자와 주문 총액 합치기
Ord_ord<-left_join(ORD,Ord_code,by="SapID")
#----------------------------------------------------------------------------------------







#주문 코드를 기준으로 주문일자,주문총액이 들어간 데이터를 전체 배송데이터와 합치기
Ord_de15<-left_join(DE,Ord_ord,by="SapID")

#상품항목을 상품번호를 기준으로 배송데이터와 합치기
Ord_de15<-left_join(Ord_de15,item,by="ItemNo")

Ord_de15<-Ord_de15[,-c(3,4,9,10,11,13,14)]

#한 행당 주문 총 금액 구하기
Ord_de15$oneprice<-Ord_de15$DeliveryQty*Ord_de15$price

#한 행당 누적 금액 구하기
Ord_de15<-Ord_de15%>%group_by(SapID)%>%mutate(누적=cumsum(oneprice))
#배송완료여부 구하기(누적액과 총주문액이 같아지면 배송완료)
Ord_de15<-Ord_de15%>%group_by(SapID)%>%mutate(배송완료여부=ifelse(누적==sumP,1,0))