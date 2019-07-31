library(dplyr)

setwd("C:/Users/fkaus/OneDrive/문서/주문파일")
order1501=read.csv("Order1501.csv",header = T)
order1502=read.csv("Order1502.csv",header = T)
order1503=read.csv("Order1503.csv",header = T)
order1504=read.csv("Order1504.csv",header = T)
order1505=read.csv("Order1505.csv",header = T)
order1506=read.csv("Order1506.csv",header = T)
order1507=read.csv("Order1507.csv",header = T)
order1508=read.csv("Order1508.csv",header = T)
order1509=read.csv("Order1509.csv",header = T)
order1510=read.csv("Order1510.csv",header = T)
order1511=read.csv("Order1511.csv",header = T)
order1512=read.csv("Order1512.csv",header = T)

order1501=order1501[!complete.cases(order1501$CancelCode)&order1501$ItemType=="TAN",]
order1502=order1502[!complete.cases(order1502$CancelCode)&order1502$ItemType=="TAN",]
order1503=order1503[!complete.cases(order1503$CancelCode)&order1503$ItemType=="TAN",]
order1504=order1504[!complete.cases(order1504$CancelCode)&order1504$ItemType=="TAN",]
order1505=order1505[!complete.cases(order1505$CancelCode)&order1505$ItemType=="TAN",]
order1506=order1506[!complete.cases(order1506$CancelCode)&order1506$ItemType=="TAN",]
order1507=order1507[!complete.cases(order1507$CancelCode)&order1507$ItemType=="TAN",]
order1508=order1508[!complete.cases(order1508$CancelCode)&order1508$ItemType=="TAN",]
order1509=order1509[!complete.cases(order1509$CancelCode)&order1509$ItemType=="TAN",]
order1510=order1510[!complete.cases(order1510$CancelCode)&order1510$ItemType=="TAN",]
order1511=order1511[!complete.cases(order1511$CancelCode)&order1511$ItemType=="TAN",]
order1512=order1512[!complete.cases(order1512$CancelCode)&order1512$ItemType=="TAN",]

head(order1503)
names(order1503)[13] <- c("C53Amt")


#8월 자료변환
order1508$MarketPriceAmt=as.numeric(order1508$MarketPriceAmt)
order1508$OrderQty=as.integer(order1508$OrderQty)
head(order1508$MarketPriceAmt)
order1508

order1508$OrderDate<-as.character(order1508$OrderDate)
order1508$ItemType<-as.character(order1508$ItemType)
order1508$ItemNo<-as.character(order1508$ItemNo)
order1508$MarketPriceAmt<-as.numeric(order1508$MarketPriceAmt)
order1508$OrderStatus<-as.character(order1508$OrderStatus)
order1508$OrderQty<-as.integer(order1508$OrderQty)
order1508$DealerPriceAmt<-as.numeric(order1508$DealerPriceAmt)
order1508$C53Amt<-as.numeric(order1508$C53Amt)

delivery1501=read.csv("Delivery1501.csv",header = T)
delivery1502=read.csv("Delivery1502.csv",header = T)
delivery1503=read.csv("Delivery1503.csv",header = T)
delivery1504=read.csv("Delivery1504.csv",header = T)
delivery1505=read.csv("Delivery1505.csv",header = T)
delivery1506=read.csv("Delivery1506.csv",header = T)
delivery1507=read.csv("Delivery1507.csv",header = T)
delivery1508=read.csv("Delivery1508.csv",header = T)
delivery1509=read.csv("Delivery1509.csv",header = T)
delivery1510=read.csv("Delivery1510.csv",header = T)
delivery1511=read.csv("Delivery1511.csv",header = T)
delivery1512=read.csv("Delivery1512.csv",header = T)


#주문경소상수(명)------------------------------------------------------------------------------------------------------

주문경소상수.1월=length(unique(order1501$SoldToParty))
주문경소상수.2월=length(unique(order1502$SoldToParty))
주문경소상수.3월=length(unique(order1503$SoldToParty))
주문경소상수.4월=length(unique(order1504$SoldToParty))
주문경소상수.5월=length(unique(order1505$SoldToParty))
주문경소상수.6월=length(unique(order1506$SoldToParty))
주문경소상수.7월=length(unique(order1507$SoldToParty))
주문경소상수.8월=length(unique(order1508$SoldToParty))
주문경소상수.9월=length(unique(order1509$SoldToParty))
주문경소상수.10월=length(unique(order1510$SoldToParty))
주문경소상수.11월=length(unique(order1511$SoldToParty))
주문경소상수.12월=length(unique(order1512$SoldToParty))

m1=matrix(c(주문경소상수.1월,주문경소상수.2월,주문경소상수.3월,주문경소상수.4월,주문경소상수.5월,주문경소상수.6월,주문경소상수.7월,주문경소상수.8월,주문경소상수.9월,주문경소상수.10월,주문경소상수.11월,주문경소상수.12월))
colnames(m1)=c("주문경소상수(명)")
rownames(m1)=c("1월","2월","3월","4월","5월","6월","7월","8월","9월","10월","11월","12월")
View(m1)

#주문건수-----------------------------------------------------------------------------------------------------------------

주문건수.1월<-as.data.frame(order1501 %>% group_by(SoldToParty) %>% summarise(주문건수=length(table(SapID))))
주문건수.1월<-sum(주문건수.1월$주문건수)

주문건수.2월<-as.data.frame(order1502 %>% group_by(SoldToParty) %>% summarise(주문건수=length(table(SapID))))
주문건수.2월<-sum(주문건수.2월$주문건수)

주문건수.3월<-as.data.frame(order1503 %>% group_by(SoldToParty) %>% summarise(주문건수=length(table(SapID))))
주문건수.3월<-sum(주문건수.3월$주문건수)

주문건수.4월<-as.data.frame(order1504 %>% group_by(SoldToParty) %>% summarise(주문건수=length(table(SapID))))
주문건수.4월<-sum(주문건수.4월$주문건수)

주문건수.5월<-as.data.frame(order1505 %>% group_by(SoldToParty) %>% summarise(주문건수=length(table(SapID))))
주문건수.5월<-sum(주문건수.5월$주문건수)

주문건수.6월<-as.data.frame(order1506 %>% group_by(SoldToParty) %>% summarise(주문건수=length(table(SapID))))
주문건수.6월<-sum(주문건수.6월$주문건수)

주문건수.7월<-as.data.frame(order1507 %>% group_by(SoldToParty) %>% summarise(주문건수=length(table(SapID))))
주문건수.7월<-sum(주문건수.7월$주문건수)

주문건수.8월<-as.data.frame(order1508 %>% group_by(SoldToParty) %>% summarise(주문건수=length(table(SapID))))
주문건수.8월<-sum(주문건수.8월$주문건수)

주문건수.9월<-as.data.frame(order1509 %>% group_by(SoldToParty) %>% summarise(주문건수=length(table(SapID))))
주문건수.9월<-sum(주문건수.9월$주문건수)

주문건수.10월<-as.data.frame(order1510 %>% group_by(SoldToParty) %>% summarise(주문건수=length(table(SapID))))
주문건수.10월<-sum(주문건수.10월$주문건수)

주문건수.11월<-as.data.frame(order1511 %>% group_by(SoldToParty) %>% summarise(주문건수=length(table(SapID))))
주문건수.11월<-sum(주문건수.11월$주문건수)

주문건수.12월<-as.data.frame(order1512 %>% group_by(SoldToParty) %>% summarise(주문건수=length(table(SapID))))
주문건수.12월<-sum(주문건수.12월$주문건수)

m2=matrix(c(주문건수.1월,주문건수.2월,주문건수.3월,주문건수.4월,주문건수.5월,주문건수.6월,주문건수.7월,주문건수.8월,주문건수.9월,주문건수.10월,주문건수.11월,주문건수.12월))
colnames(m2)=c("주문건수(건)")
rownames(m2)=c("1월","2월","3월","4월","5월","6월","7월","8월","9월","10월","11월","12월")
View(m2)

#주문 금액 --------------------------------------------------------------------------------------------------------------------------------

주문금액.1월<-sum(order1501$MarketPriceAmt)/1000000
주문금액.2월<-sum(order1502$MarketPriceAmt)/1000000
주문금액.3월<-sum(order1503$MarketPriceAmt)/1000000
주문금액.4월<-sum(order1504$MarketPriceAmt)/1000000
주문금액.5월<-sum(order1505$MarketPriceAmt)/1000000
주문금액.6월<-sum(order1506$MarketPriceAmt)/1000000
주문금액.7월<-sum(order1507$MarketPriceAmt)/1000000
주문금액.8월<-sum(order1508$MarketPriceAmt)/1000000
주문금액.9월<-sum(order1509$MarketPriceAmt)/1000000
주문금액.10월<-sum(order1510$MarketPriceAmt)/1000000
주문금액.11월<-sum(order1511$MarketPriceAmt)/1000000
주문금액.12월<-sum(order1512$MarketPriceAmt)/1000000

m3=matrix(c(주문금액.1월,주문금액.2월,주문금액.3월,주문금액.4월,주문금액.5월,주문금액.6월,주문금액.7월,주문금액.8월,주문금액.9월,주문금액.10월,주문금액.11월,주문금액.12월))
colnames(m3)=c("주문금액")
rownames(m3)=c("1월","2월","3월","4월","5월","6월","7월","8월","9월","10월","11월","12월")

View(m3)



#배송완료금액------------------------------------------------------------------------------------------------



배송완료금액.1월<-order1501%>%filter(DeliveryStatus=="C")%>%summarise(sum(MarketPriceAmt))
배송완료금액.2월<-order1502%>%filter(DeliveryStatus=="C")%>%summarise(sum(MarketPriceAmt))
배송완료금액.3월<-order1503%>%filter(DeliveryStatus=="C")%>%summarise(sum(MarketPriceAmt))
배송완료금액.4월<-order1504%>%filter(DeliveryStatus=="C")%>%summarise(sum(MarketPriceAmt))
배송완료금액.5월<-order1505%>%filter(DeliveryStatus=="C")%>%summarise(sum(MarketPriceAmt))
배송완료금액.6월<-order1506%>%filter(DeliveryStatus=="C")%>%summarise(sum(MarketPriceAmt))
배송완료금액.7월<-order1507%>%filter(DeliveryStatus=="C")%>%summarise(sum(MarketPriceAmt))
배송완료금액.8월<-order1508%>%filter(DeliveryStatus=="C")%>%summarise(sum(MarketPriceAmt))
배송완료금액.9월<-order1509%>%filter(DeliveryStatus=="C")%>%summarise(sum(MarketPriceAmt))
배송완료금액.10월<-order1510%>%filter(DeliveryStatus=="C")%>%summarise(sum(MarketPriceAmt))
배송완료금액.11월<-order1511%>%filter(DeliveryStatus=="C")%>%summarise(sum(MarketPriceAmt))
배송완료금액.12월<-order1512%>%filter(DeliveryStatus=="C")%>%summarise(sum(MarketPriceAmt))

# 배송완료금액.1월<-order1501%>%filter(DeliveryStatus=="C")%>%summarise(sum(MarketPriceAmt))

# 배송완료금액.2월<-order1502%>%filter(DeliveryStatus=="C" | DeliveryStatus=="B")%>%summarise(sum(MarketPriceAmt))
# 배송완료금액.3월<-order1503%>%filter(DeliveryStatus=="C" | DeliveryStatus=="B")%>%summarise(sum(MarketPriceAmt))
# 배송완료금액.4월<-order1504%>%filter(DeliveryStatus=="C" | DeliveryStatus=="B")%>%summarise(sum(MarketPriceAmt))
# 배송완료금액.5월<-order1505%>%filter(DeliveryStatus=="C" | DeliveryStatus=="B")%>%summarise(sum(MarketPriceAmt))
# 배송완료금액.6월<-order1506%>%filter(DeliveryStatus=="C" | DeliveryStatus=="B")%>%summarise(sum(MarketPriceAmt))
# 배송완료금액.7월<-order1507%>%filter(DeliveryStatus=="C" | DeliveryStatus=="B")%>%summarise(sum(MarketPriceAmt))
# 배송완료금액.8월<-order1508%>%filter(DeliveryStatus=="C" | DeliveryStatus=="B")%>%summarise(sum(MarketPriceAmt))
# 배송완료금액.9월<-order1509%>%filter(DeliveryStatus=="C" | DeliveryStatus=="B")%>%summarise(sum(MarketPriceAmt))
# 배송완료금액.10월<-order1510%>%filter(DeliveryStatus=="C" | DeliveryStatus=="B")%>%summarise(sum(MarketPriceAmt))
# 배송완료금액.11월<-order1511%>%filter(DeliveryStatus=="C" | DeliveryStatus=="B")%>%summarise(sum(MarketPriceAmt))
# 배송완료금액.12월<-order1512%>%filter(DeliveryStatus=="C" | DeliveryStatus=="B")%>%summarise(sum(MarketPriceAmt))

# sum(order1501[order(order1501$OrderStatus=="C"),9])
# sum(order1502[order(order1502$OrderStatus=="C"),9])
# sum(order1503[order(order1503$OrderStatus=="C"),9])
# sum(order1504[order(order1504$OrderStatus=="C"),9])
# sum(order1505[order(order1505$OrderStatus=="C"),9])
# sum(order1506[order(order1506$OrderStatus=="C"),9])
# sum(order1507[order(order1507$OrderStatus=="C"),9])
# sum(order1508[order(order1507$OrderStatus=="C"),9])
# sum(order1509[order(order1507$OrderStatus=="C"),9])
# sum(order1510[order(order1507$OrderStatus=="C"),9])
# sum(order1511[order(order1507$OrderStatus=="C"),9])
# sum(order1512[order(order1507$OrderStatus=="C"),9])

# 배송완료금액.1월<-order1501%>%filter(DeliveryStatus=="C")%>%summarise(sum(MarketPriceAmt))
# 배송완료금액.2월<-order1502%>%filter(DeliveryStatus=="C")%>%summarise(sum(MarketPriceAmt))
# 배송완료금액.3월<-order1503%>%filter(DeliveryStatus=="C")%>%summarise(sum(MarketPriceAmt))
# 배송완료금액.4월<-order1504%>%filter(DeliveryStatus=="C")%>%summarise(sum(MarketPriceAmt))
# 배송완료금액.5월<-order1505%>%filter(DeliveryStatus=="C")%>%summarise(sum(MarketPriceAmt))
# 배송완료금액.6월<-order1506%>%filter(DeliveryStatus=="C")%>%summarise(sum(MarketPriceAmt))
# 배송완료금액.7월<-order1507%>%filter(DeliveryStatus=="C")%>%summarise(sum(MarketPriceAmt))
# 배송완료금액.8월<-order1508%>%filter(DeliveryStatus=="C")%>%summarise(sum(MarketPriceAmt))
# 배송완료금액.9월<-order1509%>%filter(DeliveryStatus=="C")%>%summarise(sum(MarketPriceAmt))
# 배송완료금액.10월<-order1510%>%filter(DeliveryStatus=="C")%>%summarise(sum(MarketPriceAmt))
# 배송완료금액.11월<-order1511%>%filter(DeliveryStatus=="C")%>%summarise(sum(MarketPriceAmt))
# 배송완료금액.12월<-order1512%>%filter(DeliveryStatus=="C")%>%summarise(sum(MarketPriceAmt))

# table(order1501$DeliveryStatus)
# 
# sum(order1508$MarketPriceAmt)/1000
# 
# order1508=order1508[!complete.cases(order1508$CancelCode)&order1508$ItemType=="TAN",]
# View(order1508)
# str(order1508)
# str(order1501)
# #---------------------------------------------------------------------------------------------------------
# 
# delivery1501
# order1501
# test<-merge(delivery1501,order1501,by="OrderDate",all.x = FALSE)
# str(test)
# names(delivery1501)[names(delivery1501)=="RegisterDate"]<-c("OrderDate")
# order.test<-order1501[order1501$DeliveryStatus=="C",]
# sum(order.test$MarketPriceAmt)

#---------------------------------------------------------------------------------------------------------
# all.delivery<-all.delivery[-c(order(all.delivery$NewCode>900000)),]
# head(all.delivery)
# all.delivery %>% filter(NewCode<900000)
# head(all.delivery)


all.delivery=rbind(delivery1501,delivery1502,delivery1503,delivery1504,delivery1505,delivery1506,delivery1507,delivery1508,delivery1509,delivery1510,delivery1511,delivery1512)
all.delivery<-all.delivery[all.delivery$ItemType=="TAN",-c(4,11,13)]
all.delivery<-all.delivery %>% filter(SoldToParty>999999 & SoldToParty<10000000)
all.delivery<-all.delivery[,c(7,10)]
head(all.delivery)

all.order<-bind_rows(order1501,order1502,order1503,order1504,order1505,order1506,order1507,order1508,order1509,order1510,order1511,order1512)
all.order$단위가격<-all.order$MarketPriceAmt/all.order$OrderQty
head(all.order)
head(all.delivery)

all.delivery$ItemNo<-as.character(all.delivery$ItemNo)
str(all.delivery)
#--------------------

all.order.price<-all.order[,c(7,20)]
all.order.price<-distinct(all.order.price,ItemNo,단위가격)
head(all.order.price)

all.delivery.need<-all.delivery[,c(5,7,10,12)]
head(all.delivery.need)

nrow(all.delivery.need)
nrow(all.order.price)


result<-merge(all.delivery.need,all.order.price,by="ItemNo")



#----------------------------------------------------------------------------------

delivery.1월<-delivery1501[delivery1501$ItemType=="TAN",-c(4,11,13)]
delivery.1월<-delivery.1월 %>% filter(SoldToParty>999999 & SoldToParty<10000000)
head(delivery.1월)

delivery.1월<-delivery.1월[,-c(1,2,3,4,6,8,11,14)]
head(delivery.1월)

order1501$단위가격<-order1501$MarketPriceAmt/order1501$OrderQty
head(order1501)

order1501.price<-order1501[,c(7,18)]
head(order1501.price)


#merge(all.order,all.delivery,by="ItemNo")



all.order.price<-distinct(all.order.price,ItemNo,단위가격)




# View(all.order.price)
# 
# merge(all.order.price,all.delivery,by="ItemNo",all=TRUE)
# 
# 
# 
# 
# 
# 
# all.delivery$단위가격<-all.order$단위가격
# 
# 
# 
# 
# all.order[all.order$DeliveryStatus==15710,]
# 
# all.delivery[all.delivery$SapID==15710,]
# 
# unique(all.order$ItemNo)


#---------------------------------------------------


item1<-order1501%>%select(ItemNo,MarketPriceAmt,OrderQty)
item1$price<-item1$MarketPriceAmt/item1$OrderQty
item1<-item1[,-c(2,3)]

item2<-order1502%>%select(ItemNo,MarketPriceAmt,OrderQty)
item2$price<-item2$MarketPriceAmt/item2$OrderQty
item2<-item2[,-c(2,3)]

item3<-order1503%>%select(ItemNo,MarketPriceAmt,OrderQty)
item3$price<-item3$MarketPriceAmt/item3$OrderQty
item3<-item3[,-c(2,3)]

item4<-order1504%>%select(ItemNo,MarketPriceAmt,OrderQty)
item4$price<-item4$MarketPriceAmt/item4$OrderQty
item4<-item4[,-c(2,3)]

item5<-order1505%>%select(ItemNo,MarketPriceAmt,OrderQty)
item5$price<-item5$MarketPriceAmt/item5$OrderQty
item5<-item5[,-c(2,3)]

item6<-order1506%>%select(ItemNo,MarketPriceAmt,OrderQty)
item6$price<-item6$MarketPriceAmt/item6$OrderQty
item6<-item6[,-c(2,3)]

item7<-order1507%>%select(ItemNo,MarketPriceAmt,OrderQty)
item7$price<-item7$MarketPriceAmt/item7$OrderQty
item7<-item7[,-c(2,3)]

item8<-order1508%>%select(ItemNo,MarketPriceAmt,OrderQty)
item8$price<-item8$MarketPriceAmt/item8$OrderQty
item8<-item8[,-c(2,3)]

item9<-order1509%>%select(ItemNo,MarketPriceAmt,OrderQty)
item9$price<-item9$MarketPriceAmt/item9$OrderQty
item9<-item9[,-c(2,3)]

item10<-order1510%>%select(ItemNo,MarketPriceAmt,OrderQty)
item10$price<-item10$MarketPriceAmt/item10$OrderQty
item10<-item10[,-c(2,3)]

item11<-order1511%>%select(ItemNo,MarketPriceAmt,OrderQty)
item11$price<-item11$MarketPriceAmt/item11$OrderQty
item11<-item11[,-c(2,3)]

item12<-order1512%>%select(ItemNo,MarketPriceAmt,OrderQty)
item12$price<-item12$MarketPriceAmt/item12$OrderQty
item12<-item12[,-c(2,3)]

item<-rbind(item1,item2,item3,item4,item5,item6,item7
            ,item8,item9,item10,item11,item12)



item<-distinct(item,ItemNo,price)
head(item)

DE<-merge(all.delivery,item,by="ItemNo",all=F)

