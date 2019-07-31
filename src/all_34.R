
library(dplyr)
setwd("C:/Users/fkaus/OneDrive/문서/주문파일")

order1503=read.csv("Order1503.csv",header = T, stringsAsFactors = F)
order1504=read.csv("Order1504.csv",header = T, stringsAsFactors = F)

order1503=order1503[!complete.cases(order1503$CancelCode)&order1503$ItemType=="TAN"&order1503$SoldToParty>999999&order1503$SoldToParty<10000000,]
order1504=order1504[!complete.cases(order1504$CancelCode)&order1504$ItemType=="TAN"&order1504$SoldToParty>999999&order1504$SoldToParty<10000000,]

delivery1503=read.csv("Delivery1503.csv",header = T, stringsAsFactors = F)
delivery1504=read.csv("Delivery1504.csv",header = T, stringsAsFactors = F)

delivery1503=delivery1503[delivery1503$ItemType=="TAN"&delivery1503$SoldToParty>999999&delivery1503$SoldToParty<10000000,]
delivery1504=delivery1504[delivery1504$ItemType=="TAN"&delivery1504$SoldToParty>999999&delivery1504$SoldToParty<10000000,]

# delivery1503<-delivery1503[delivery1503$RegisterDate<=delivery1503$DeliveryDate,]
# delivery1504<-delivery1504[delivery1504$RegisterDate<=delivery1504$DeliveryDate,]



unitprice1503=read.csv("UnitPrice1503.csv",header=T,stringsAsFactors = F)
unitprice1504=read.csv("UnitPrice1504.csv",header=T,stringsAsFactors = F)

unitprice1503$UnitPrice=as.numeric(unitprice1503$UnitPrice)
unitprice1504$UnitPrice=as.numeric(unitprice1504$UnitPrice)

unitprice1503=unitprice1503[,c(1,3)]
unitprice1504=unitprice1504[,c(1,3)]

m1=merge(delivery1503,unitprice1503,by="ItemNo")
m2=merge(delivery1504,unitprice1504,by="ItemNo")






order.34<-rbind(order1503,order1504)
delivery.34<-rbind(m1,m2)

order.34<-order.34[,c(1,2,4,9,10)]
delivery.34<-delivery.34[,-c(1,4,5,8,9,10,11,13,14,17)]

m3=merge(delivery.34,order.34,by="SapID")

m3=m3[,-c(2,4)]
m3=m3[m3$DeliveryDate>=m3$RegisterDate&m3$RegisterDate>=m3$OrderDate,]


names(m3)[10] <- c("RDC")
names(m3)[8] <- c("SoldToParty")
#View(m3)

write.csv(m3,file="m3.csv",row.names=F)
# 
# test<-read.csv("m3.csv",header = T)
# View(test)






















