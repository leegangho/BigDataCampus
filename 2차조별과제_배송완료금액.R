library(dplyr)

setwd("C:/Users/fkaus/OneDrive/문서/주문파일")

order1501=read.csv("Order1501.csv",header = T,stringsAsFactors = F)
order1502=read.csv("Order1502.csv",header = T,stringsAsFactors = F)
order1503=read.csv("Order1503.csv",header = T,stringsAsFactors = F)
order1504=read.csv("Order1504.csv",header = T,stringsAsFactors = F)
order1505=read.csv("Order1505.csv",header = T,stringsAsFactors = F)
order1506=read.csv("Order1506.csv",header = T,stringsAsFactors = F)
order1507=read.csv("Order1507.csv",header = T,stringsAsFactors = F)
order1508=read.csv("Order1508.csv",header = T,stringsAsFactors = F)
order1509=read.csv("Order1509.csv",header = T,stringsAsFactors = F)
order1510=read.csv("Order1510.csv",header = T,stringsAsFactors = F)
order1511=read.csv("Order1511.csv",header = T,stringsAsFactors = F)
order1512=read.csv("Order1512.csv",header = T,stringsAsFactors = F)

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


# #8월 자료변환
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

delivery1501=read.csv("Delivery1501.csv",header = T,stringsAsFactors = F)
delivery1502=read.csv("Delivery1502.csv",header = T,stringsAsFactors = F)
delivery1503=read.csv("Delivery1503.csv",header = T,stringsAsFactors = F)
delivery1504=read.csv("Delivery1504.csv",header = T,stringsAsFactors = F)
delivery1505=read.csv("Delivery1505.csv",header = T,stringsAsFactors = F)
delivery1506=read.csv("Delivery1506.csv",header = T,stringsAsFactors = F)
delivery1507=read.csv("Delivery1507.csv",header = T,stringsAsFactors = F)
delivery1508=read.csv("Delivery1508.csv",header = T,stringsAsFactors = F)
delivery1509=read.csv("Delivery1509.csv",header = T,stringsAsFactors = F)
delivery1510=read.csv("Delivery1510.csv",header = T,stringsAsFactors = F)
delivery1511=read.csv("Delivery1511.csv",header = T,stringsAsFactors = F)
delivery1512=read.csv("Delivery1512.csv",header = T,stringsAsFactors = F)


# 각 월별로 SapID로 분리합니다. 
# 전체 Delivery에서 월별 SapID를 검색합니다. 
# Delivery의 Qty와 Order의 단위가격을 곱하고, 전체를 더합니다. 



# 1월 : 177055429

order1501$단위가격<-order1501$MarketPriceAmt/order1501$OrderQty
order1501<-order1501[order1501$DeliveryStatus=="C",]
order1501<-order1501[,-c(1,3,4,5,6,8,11,12,13,14,15,16,17)]

test<-order1501 %>% group_by(order1501$SapID)
test<-as.data.frame(test)

all.delivery=rbind(delivery1501,delivery1502,delivery1503,delivery1504,delivery1505,delivery1506,delivery1507,delivery1508,delivery1509,delivery1510,delivery1511,delivery1512)
all.delivery<-all.delivery[c(all.delivery$SoldToParty>999999),]
all.delivery<-all.delivery[c(all.delivery$SoldToParty<10000000),]
all.delivery<-all.delivery[all.delivery$ItemType=="TAN",-c(4,7,9,10,11,13,14,17)]


Complete.1<-all.delivery[c(which(all.delivery$SapID>15699 & all.delivery$SapID<23597)),]

result.1<-merge(Complete.1,test,by="ItemNo")
result.1$DeliveryQty<-as.numeric(result.1$DeliveryQty)
price.1<-sum(result.1$단위가격*result.1$DeliveryQty)
price.1

# 2월 : 127665158

order1502$단위가격<-order1502$MarketPriceAmt/order1502$OrderQty
order1502<-order1502[order1502$DeliveryStatus=="C",]
order1502<-order1502[,-c(1,3,4,5,6,8,11,12,13,14,15,16,17)]

test<-order1502 %>% group_by(order1502$SapID)
test<-as.data.frame(test)

all.delivery=rbind(delivery1501,delivery1502,delivery1503,delivery1504,delivery1505,delivery1506,delivery1507,delivery1508,delivery1509,delivery1510,delivery1511,delivery1512)
all.delivery<-all.delivery[c(all.delivery$SoldToParty>999999),]
all.delivery<-all.delivery[c(all.delivery$SoldToParty<10000000),]
all.delivery<-all.delivery[all.delivery$ItemType=="TAN",-c(4,7,9,10,11,13,14,17)]

head(order1502)
tail(order1502)

which(all.delivery$SapID>23600 & all.delivery$SapID<30465)
Complete.2<-all.delivery[c(which(all.delivery$SapID>23600 & all.delivery$SapID<30465)),]
result.2<-merge(Complete.2,test)
result.2$DeliveryQty<-as.numeric(result.2$DeliveryQty)
price.2<-sum(result.2$단위가격*result.2$DeliveryQty)
price.2

# 3월 : 181394130

order1503

order1503$단위가격<-order1503$MarketPriceAmt/order1503$OrderQty
order1503<-order1503[order1503$DeliveryStatus=="C",]
order1503<-order1503[,-c(1,3,4,5,6,8,11,12,13,14,15,16,17)]

test3<-order1503 %>% group_by(order1503$SapID)
test3<-as.data.frame(test3)

all.delivery=rbind(delivery1501,delivery1502,delivery1503,delivery1504,delivery1505,delivery1506,delivery1507,delivery1508,delivery1509,delivery1510,delivery1511,delivery1512)
all.delivery<-all.delivery[c(all.delivery$SoldToParty>999999),]
all.delivery<-all.delivery[c(all.delivery$SoldToParty<10000000),]
all.delivery<-all.delivery[all.delivery$ItemType=="TAN",-c(4,7,9,10,11,13,14,17)]

head(order1503)
tail(order1503)

Complete.3<-all.delivery[c(which(all.delivery$SapID>30465 & all.delivery$SapID<38386)),]
result.3<-merge(Complete.3,test3)
result.3$DeliveryQty<-as.numeric(result.3$DeliveryQty)
price.3<-sum(result.3$단위가격*result.3$DeliveryQty)
price.3

# 4월 :
str(order1504)

# order1504$단위가격<-order1504$MarketPriceAmt/order1504$OrderQty
# order1504<-order1504[order1503$DeliveryStatus=="C",]
# order1504<-order1504[,-c(1,3,4,5,6,8,11,12,13,14,15,16,17)]
# 
# test4<-order1504 %>% group_by(order1504$SapID)
# test4<-as.data.frame(test4)
# 
# all.delivery=rbind(delivery1501,delivery1502,delivery1503,delivery1504,delivery1505,delivery1506,delivery1507,delivery1508,delivery1509,delivery1510,delivery1511,delivery1512)
# all.delivery<-all.delivery[c(all.delivery$SoldToParty>999999),]
# all.delivery<-all.delivery[c(all.delivery$SoldToParty<10000000),]
# all.delivery<-all.delivery[all.delivery$ItemType=="TAN",-c(4,7,9,10,11,13,14,17)]
# 
# head(order1504)
# tail(order1504)
# 
# Complete.4<-all.delivery[c(which(all.delivery$SapID>38385 & all.delivery$SapID<47545)),]
# result.4<-merge(Complete.4,test4)
# result.4$DeliveryQty<-as.numeric(result.4$DeliveryQty)
# price.4<-sum(result.4$단위가격*result.4$DeliveryQty)

price.4<-order1504%>%filter(DeliveryStatus=="C")%>%summarise(sum(MarketPriceAmt))

price.4

# 5월 : 209975312

order1505$단위가격<-order1505$MarketPriceAmt/order1505$OrderQty
order1505<-order1505[order1505$DeliveryStatus=="C",]
order1505<-order1505[,-c(1,3,4,5,6,8,11,12,13,14,15,16,17)]

test5<-order1505 %>% group_by(order1505$SapID)
test5<-as.data.frame(test5)

all.delivery=rbind(delivery1501,delivery1502,delivery1503,delivery1504,delivery1505,delivery1506,delivery1507,delivery1508,delivery1509,delivery1510,delivery1511,delivery1512)
all.delivery<-all.delivery[c(all.delivery$SoldToParty>999999),]
all.delivery<-all.delivery[c(all.delivery$SoldToParty<10000000),]
all.delivery<-all.delivery[all.delivery$ItemType=="TAN",-c(4,7,9,10,11,13,14,17)]

head(order1505)
tail(order1505)

Complete.5<-all.delivery[c(which(all.delivery$SapID>47545 & all.delivery$SapID<57041)),]
result.5<-merge(Complete.5,test5)
result.5$DeliveryQty<-as.numeric(result.5$DeliveryQty)
price.5<-sum(result.5$단위가격*result.5$DeliveryQty)
price.5

# 6월 : 205147460

order1506$단위가격<-order1506$MarketPriceAmt/order1506$OrderQty
order1506<-order1506[order1506$DeliveryStatus=="C",]
order1506<-order1506[,-c(1,3,4,5,6,8,11,12,13,14,15,16,17)]

test6<-order1506 %>% group_by(order1506$SapID)
test6<-as.data.frame(test6)

all.delivery=rbind(delivery1501,delivery1502,delivery1503,delivery1504,delivery1505,delivery1506,delivery1507,delivery1508,delivery1509,delivery1510,delivery1511,delivery1512)
all.delivery<-all.delivery[c(all.delivery$SoldToParty>999999),]
all.delivery<-all.delivery[c(all.delivery$SoldToParty<10000000),]
all.delivery<-all.delivery[all.delivery$ItemType=="TAN",-c(4,7,9,10,11,13,14,17)]

head(order1506)
tail(order1506)

Complete.6<-all.delivery[c(which(all.delivery$SapID>56990 & all.delivery$SapID<66283)),]
result.6<-merge(Complete.6,test6)
result.6$DeliveryQty<-as.numeric(result.6$DeliveryQty)
price.6<-sum(result.6$단위가격*result.6$DeliveryQty)
price.6

# 7월 : 171764520

order1507$단위가격<-order1507$MarketPriceAmt/order1507$OrderQty
order1507<-order1507[order1507$DeliveryStatus=="C",]
order1507<-order1507[,-c(1,3,4,5,6,8,11,12,13,14,15,16,17)]

test7<-order1507 %>% group_by(order1507$SapID)
test7<-as.data.frame(test7)

all.delivery=rbind(delivery1501,delivery1502,delivery1503,delivery1504,delivery1505,delivery1506,delivery1507,delivery1508,delivery1509,delivery1510,delivery1511,delivery1512)
all.delivery<-all.delivery[c(all.delivery$SoldToParty>999999),]
all.delivery<-all.delivery[c(all.delivery$SoldToParty<10000000),]
all.delivery<-all.delivery[all.delivery$ItemType=="TAN",-c(4,7,9,10,11,13,14,17)]

head(order1507)
tail(order1507)

Complete.7<-all.delivery[c(which(all.delivery$SapID>66269 & all.delivery$SapID<74982)),]
result.7<-merge(Complete.7,test7)
result.7$DeliveryQty<-as.numeric(result.7$DeliveryQty)
price.7<-sum(result.7$단위가격*result.7$DeliveryQty)
price.7

# 8월 : 226497198
# sum 값에 na 발생.. 

order1508$단위가격<-order1508$MarketPriceAmt/order1508$OrderQty
order1508<-order1508[order1508$DeliveryStatus=="C",]
order1508<-order1508[,-c(1,3,4,5,6,8,11,12,13,14,15,16,17)]

test8<-order1508 %>% group_by(order1508$SapID)
test8<-as.data.frame(test8)

all.delivery=rbind(delivery1501,delivery1502,delivery1503,delivery1504,delivery1505,delivery1506,delivery1507,delivery1508,delivery1509,delivery1510,delivery1511,delivery1512)
all.delivery<-all.delivery[c(all.delivery$SoldToParty>999999),]
all.delivery<-all.delivery[c(all.delivery$SoldToParty<10000000),]
all.delivery<-all.delivery[all.delivery$ItemType=="TAN",-c(4,7,9,10,11,13,14,17)]

head(order1508)
tail(order1508)

Complete.8<-all.delivery[c(which(all.delivery$SapID>74808 & all.delivery$SapID<84462)),]
result.8<-merge(Complete.8,test8)
result.8$DeliveryQty<-as.numeric(result.8$DeliveryQty)
price.8<-sum(result.8$단위가격*result.8$DeliveryQty,na.rm=T)
price.8

# 9월 : 188996180

order1509$단위가격<-order1509$MarketPriceAmt/order1509$OrderQty
order1509<-order1509[order1509$DeliveryStatus=="C",]
order1509<-order1509[,-c(1,3,4,5,6,8,11,12,13,14,15,16,17)]

test9<-order1509 %>% group_by(order1509$SapID)
test9<-as.data.frame(test9)

all.delivery=rbind(delivery1501,delivery1502,delivery1503,delivery1504,delivery1505,delivery1506,delivery1507,delivery1508,delivery1509,delivery1510,delivery1511,delivery1512)
all.delivery<-all.delivery[c(all.delivery$SoldToParty>999999),]
all.delivery<-all.delivery[c(all.delivery$SoldToParty<10000000),]
all.delivery<-all.delivery[all.delivery$ItemType=="TAN",-c(4,7,9,10,11,13,14,17)]

head(order1509)
tail(order1509)

Complete.9<-all.delivery[c(which(all.delivery$SapID>84462 & all.delivery$SapID<93748)),]
result.9<-merge(Complete.9,test9)
result.9$DeliveryQty<-as.numeric(result.9$DeliveryQty)
price.9<-sum(result.9$단위가격*result.9$DeliveryQty,na.rm=T)
price.9

# 10월 : 192011645

order1510$단위가격<-order1510$MarketPriceAmt/order1510$OrderQty
order1510<-order1510[order1510$DeliveryStatus=="C",]
order1510<-order1510[,-c(1,3,4,5,6,8,11,12,13,14,15,16,17)]

test10<-order1510 %>% group_by(order1510$SapID)
test10<-as.data.frame(test10)

all.delivery=rbind(delivery1501,delivery1502,delivery1503,delivery1504,delivery1505,delivery1506,delivery1507,delivery1508,delivery1509,delivery1510,delivery1511,delivery1512)
all.delivery<-all.delivery[c(all.delivery$SoldToParty>999999),]
all.delivery<-all.delivery[c(all.delivery$SoldToParty<10000000),]
all.delivery<-all.delivery[all.delivery$ItemType=="TAN",-c(4,7,9,10,11,13,14,17)]

head(order1510)
tail(order1510)

Complete.10<-all.delivery[c(which(all.delivery$SapID>93654 & all.delivery$SapID<103428)),]
result.10<-merge(Complete.10,test10)
result.10$DeliveryQty<-as.numeric(result.10$DeliveryQty)
price.10<-sum(result.10$단위가격*result.10$DeliveryQty,na.rm=T)
price.10

# 11월 : 223302323

order1511$단위가격<-order1511$MarketPriceAmt/order1511$OrderQty
order1511<-order1511[order1511$DeliveryStatus=="C",]
order1511<-order1511[,-c(1,3,4,5,6,8,11,12,13,14,15,16,17)]

test11<-order1511 %>% group_by(order1511$SapID)
test11<-as.data.frame(test11)

all.delivery=rbind(delivery1501,delivery1502,delivery1503,delivery1504,delivery1505,delivery1506,delivery1507,delivery1508,delivery1509,delivery1510,delivery1511,delivery1512)
all.delivery<-all.delivery[c(all.delivery$SoldToParty>999999),]
all.delivery<-all.delivery[c(all.delivery$SoldToParty<10000000),]
all.delivery<-all.delivery[all.delivery$ItemType=="TAN",-c(4,7,9,10,11,13,14,17)]

head(order1511)
tail(order1511)

Complete.11<-all.delivery[c(which(all.delivery$SapID>103430 & all.delivery$SapID<112719)),]
result.11<-merge(Complete.11,test11)
result.11$DeliveryQty<-as.numeric(result.11$DeliveryQty)
price.11<-sum(result.11$단위가격*result.11$DeliveryQty,na.rm=T)
price.11

# 12월 : 138956

order1512$OrderQty<-as.numeric(order1512$OrderQty)

order1512$단위가격<-order1512$MarketPriceAmt/order1512$OrderQty
order1512<-order1512[order1512$DeliveryStatus=="C",]
order1512<-order1512[,-c(1,3,4,5,6,8,11,12,13,14,15,16,17)]

test12<-order1512 %>% group_by(order1512$SapID)
test12<-as.data.frame(test12)

all.delivery=rbind(delivery1501,delivery1502,delivery1503,delivery1504,delivery1505,delivery1506,delivery1507,delivery1508,delivery1509,delivery1510,delivery1511,delivery1512)
all.delivery<-all.delivery[c(all.delivery$SoldToParty>999999),]
all.delivery<-all.delivery[c(all.delivery$SoldToParty<10000000),]
all.delivery<-all.delivery[all.delivery$ItemType=="TAN",-c(4,7,9,10,11,13,14,17)]

head(order1512)
tail(order1512)
# 
# Complete.12<-all.delivery[c(which(all.delivery$SapID>103430 & all.delivery$SapID<112719)),]
# result.12<-merge(Complete.12,test12)
# result.12$DeliveryQty<-as.numeric(result.12$DeliveryQty)
# price.12<-sum(result.12$단위가격*result.12$DeliveryQty,na.rm=T)
# price.12

price.12<-order1512%>%filter(DeliveryStatus=="C")%>%summarise(sum(MarketPriceAmt))
price.12


m4=matrix(c(price.1,price.2,price.3,price.4,price.5,price.6,price.7,price.8,price.9,price.10,price.11,price.12))
colnames(m4)=c("배송완료금액")
rownames(m4)=c("1월","2월","3월","4월","5월","6월","7월","8월","9월","10월","11월","12월")

data.frame(price.1,price.2,price.3,price.4,price.5,price.6,price.7,price.8,price.9,price.10,price.11,price.12)
as.data.frame(m4)
View(m4)



