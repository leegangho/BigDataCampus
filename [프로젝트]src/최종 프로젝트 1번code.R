# 2019년 1월, 2018년 12월 버스정보와 노선정보

library(dplyr)
library(ggplot2)
library(ggmap)

register_google(key="AIzaSyDb6CtknFf0WsNEHDErgOZZM_pTPWMfPbs")

setwd("C:/Users/fkaus/OneDrive/바탕 화면/데이터캠퍼스 프로젝트/test")
data1 = read.csv("201901월.csv",header=T)
data1<-data1[,-c(11,12,13,14,15,16,17,18,19,20,21,22)]


data2=read.csv("201812월.csv",header=T)
data2<-data2[,-c(11)]

busline.info=read.csv("노선정보.csv",header=T)

#---------------------------------------------------------------------------------------------------

# top 30 개의 노선을 찾았습니다.!!!!! 
data1.총승차객수=data1 %>% group_by(data1$노선번호) %>% summarise(sum(승차총승객수))
data1.총승차객수<-as.data.frame(data1.총승차객수)

TEST=sort(data1.총승차객수[,2],decreasing=T)

box=order(data1.총승차객수[,2],decreasing=T)
hd=head(data1.총승차객수[box,],30)



data2.총승차객수=data2 %>% group_by(data2$노선번호) %>% summarise(sum(승차총승객수))
data2.총승차객수<-as.data.frame(data2.총승차객수)

TEST2=sort(data2.총승차객수[,2],decreasing=T)

box2=order(data2.총승차객수[,2],decreasing=T)
hd2=head(data2.총승차객수[box2,],30)
hd2
#--------------------------------------------------------------------------------
# 2019년 1월 순번까지 정렬 (143 노선)

data1 = read.csv("201901월.csv",header=T)
data.2019=data1[,c(3,5,7,8,9,10)]
data.2019.143<-data.2019[data.2019$노선번호=="143",]
data.2019.143<-data.2019.143 %>% rename("정류소ID"="표준버스정류장ID")



busline143.info<-busline.info[busline.info$노선명=="143",]
busline143.info


test<-merge(data.2019.143,busline143.info,by="정류소ID",all.x=T)
test<- test[order(test$순번),]

#------------------------------------------------------------------------------------

# 143 노선에 대해서 각 정류장의 총 승하차 객수를 구했습니다. 
test
df1<-test%>%group_by(순번)%>%mutate(Boardsum=sum(승차총승객수))
df2<-test%>%group_by(순번)%>%mutate(Landsum=sum(하차총승객수))
df1$Landsum<-df2$Landsum


df2<-df1[,-c(5,6,11,12)]
df2<-unique(df2)
View(df2)
#------------------------------------------------------------------------------------

setwd("C:/Users/fkaus/OneDrive/바탕 화면/월별")

#head(busline.info)

test1<-read.csv("1월 Top1.csv",header=T)
df1<-test1 %>%group_by(표준버스정류장ID) %>% mutate(Boardsum=sum(승차총승객수))
df2<-test1 %>%group_by(표준버스정류장ID) %>% mutate(Landsum=sum(하차총승객수))
df1$Landsum<-df2$Landsum
df1<-unique(df1)

df1<-df1[,-c(5,6)]
df1<-unique(df1)
View(df1)


df1<-df1 %>% rename("정류소ID"="표준버스정류장ID")
df1<-df1 %>% rename("정류소명"="역명")
df1<-df1 %>% rename("노선명"="노선번호")

busline143.info<-busline.info[busline.info$노선명=="143",]
busline143.info

test1<-merge(df1,busline143.info,by="정류소ID",all.x=T)
test1<-test1[,-c(7,8,10,11,12)]
View(test)
# 1



test2<-read.csv("1월 Top2.csv",header=T)
df1<-test2 %>%group_by(표준버스정류장ID) %>% mutate(Boardsum=sum(승차총승객수))
df2<-test2 %>%group_by(표준버스정류장ID) %>% mutate(Landsum=sum(하차총승객수))
df1$Landsum<-df2$Landsum
df1<-unique(df1)

df1<-df1[,-c(5,6)]
df1<-unique(df1)
View(df1)

df1<-df1 %>% rename("정류소ID"="표준버스정류장ID")
df1<-df1 %>% rename("정류소명"="역명")
df1<-df1 %>% rename("노선명"="노선번호")

busline152.info<-busline.info[busline.info$노선명=="152",]
#busline152.info

test2<-merge(df1,busline152.info,by="정류소ID",all.x=T)
test2<-test2[,-c(7,8,10,11,12)]
View(test2)
# 2



test3<-read.csv("1월 Top3.csv",header=T)
df1<-test3 %>%group_by(표준버스정류장ID) %>% mutate(Boardsum=sum(승차총승객수))
df2<-test3 %>%group_by(표준버스정류장ID) %>% mutate(Landsum=sum(하차총승객수))
df1$Landsum<-df2$Landsum
df1<-unique(df1)

df1<-df1[,-c(5,6)]
df1<-unique(df1)
View(df1)

df1<-df1 %>% rename("정류소ID"="표준버스정류장ID")
df1<-df1 %>% rename("정류소명"="역명")
df1<-df1 %>% rename("노선명"="노선번호")

busline160.info<-busline.info[busline.info$노선명=="160",]
#busline152.info

test3<-merge(df1,busline160.info,by="정류소ID",all.x=T)
test3<-test3[,-c(7,8,10,11,12)]
View(test3)
# 3



test4<-read.csv("1월 Top4.csv",header=T)
df1<-test4 %>%group_by(표준버스정류장ID) %>% mutate(Boardsum=sum(승차총승객수))
df2<-test4 %>%group_by(표준버스정류장ID) %>% mutate(Landsum=sum(하차총승객수))
df1$Landsum<-df2$Landsum
df1<-unique(df1)

df1<-df1[,-c(5,6)]
df1<-unique(df1)
head(df1)
df1<-df1 %>% rename("정류소ID"="표준버스정류장ID")
df1<-df1 %>% rename("정류소명"="역명")
df1<-df1 %>% rename("노선명"="노선번호")

busline272.info<-busline.info[busline.info$노선명=="272",]
#busline152.info

test4<-merge(df1,busline272.info,by="정류소ID",all.x=T)
test4<-test4[,-c(7,8,10,11,12)]
head(test4)
#View(test3)
# 4



test5<-read.csv("1월 Top5.csv",header=T)
df1<-test5 %>%group_by(표준버스정류장ID) %>% mutate(Boardsum=sum(승차총승객수))
df2<-test5 %>%group_by(표준버스정류장ID) %>% mutate(Landsum=sum(하차총승객수))
df1$Landsum<-df2$Landsum
df1<-unique(df1)

df1<-df1[,-c(5,6)]
df1<-unique(df1)
head(df1)
df1<-df1 %>% rename("정류소ID"="표준버스정류장ID")
df1<-df1 %>% rename("정류소명"="역명")
df1<-df1 %>% rename("노선명"="노선번호")

busline271.info<-busline.info[busline.info$노선명=="271",]
#busline152.info

test5<-merge(df1,busline271.info,by="정류소ID",all.x=T)
test5<-test5[,-c(7,8,10,11,12)]
head(test5)
#View(test3)
# 5


test6<-read.csv("1월 Top6.csv",header=T)
df1<-test6 %>%group_by(표준버스정류장ID) %>% mutate(Boardsum=sum(승차총승객수))
df2<-test6 %>%group_by(표준버스정류장ID) %>% mutate(Landsum=sum(하차총승객수))
df1$Landsum<-df2$Landsum
df1<-unique(df1)

df1<-df1[,-c(5,6)]
df1<-unique(df1)
head(df1)
df1<-df1 %>% rename("정류소ID"="표준버스정류장ID")
df1<-df1 %>% rename("정류소명"="역명")
df1<-df1 %>% rename("노선명"="노선번호")

busline130.info<-busline.info[busline.info$노선명=="130",]
#busline152.info

test6<-merge(df1,busline130.info,by="정류소ID",all.x=T)
test6<-test6[,-c(7,8,10,11,12)]
head(test6)
#View(test3)
# 6 


test7<-read.csv("1월 Top7.csv",header=T)
df1<-test7 %>%group_by(표준버스정류장ID) %>% mutate(Boardsum=sum(승차총승객수))
df2<-test7 %>%group_by(표준버스정류장ID) %>% mutate(Landsum=sum(하차총승객수))
df1$Landsum<-df2$Landsum
df1<-unique(df1)

df1<-df1[,-c(5,6)]
df1<-unique(df1)
head(df1)
df1<-df1 %>% rename("정류소ID"="표준버스정류장ID")
df1<-df1 %>% rename("정류소명"="역명")
df1<-df1 %>% rename("노선명"="노선번호")

busline153.info<-busline.info[busline.info$노선명=="153",]
#busline152.info

test7<-merge(df1,busline153.info,by="정류소ID",all.x=T)
test7<-test7[,-c(7,8,10,11,12)]
head(test7)
#7

test8<-read.csv("1월 Top8.csv",header=T)
df1<-test8 %>%group_by(표준버스정류장ID) %>% mutate(Boardsum=sum(승차총승객수))
df2<-test8 %>%group_by(표준버스정류장ID) %>% mutate(Landsum=sum(하차총승객수))
df1$Landsum<-df2$Landsum
df1<-unique(df1)

df1<-df1[,-c(5,6)]
df1<-unique(df1)
head(df1)
df1<-df1 %>% rename("정류소ID"="표준버스정류장ID")
df1<-df1 %>% rename("정류소명"="역명")
df1<-df1 %>% rename("노선명"="노선번호")

busline360.info<-busline.info[busline.info$노선명=="360",]
#busline152.info

test8<-merge(df1,busline360.info,by="정류소ID",all.x=T)
test8<-test8[,-c(7,8,10,11,12)]
head(test8)
#8


test9<-read.csv("1월 Top9.csv",header=T)
df1<-test9 %>%group_by(표준버스정류장ID) %>% mutate(Boardsum=sum(승차총승객수))
df2<-test9 %>%group_by(표준버스정류장ID) %>% mutate(Landsum=sum(하차총승객수))
df1$Landsum<-df2$Landsum
df1<-unique(df1)

df1<-df1[,-c(5,6)]
df1<-unique(df1)
head(df1)
df1<-df1 %>% rename("정류소ID"="표준버스정류장ID")
df1<-df1 %>% rename("정류소명"="역명")
df1<-df1 %>% rename("노선명"="노선번호")

busline146.info<-busline.info[busline.info$노선명=="146",]
#busline152.info

test9<-merge(df1,busline146.info,by="정류소ID",all.x=T)
test9<-test9[,-c(7,8,10,11,12)]
head(test9)
#9



test10<-read.csv("1월 Top10.csv",header=T)
df1<-test10 %>%group_by(표준버스정류장ID) %>% mutate(Boardsum=sum(승차총승객수))
df2<-test10 %>%group_by(표준버스정류장ID) %>% mutate(Landsum=sum(하차총승객수))
df1$Landsum<-df2$Landsum
df1<-unique(df1)

df1<-df1[,-c(5,6)]
df1<-unique(df1)
head(df1)
df1<-df1 %>% rename("정류소ID"="표준버스정류장ID")
df1<-df1 %>% rename("정류소명"="역명")
df1<-df1 %>% rename("노선명"="노선번호")

busline340.info<-busline.info[busline.info$노선명=="340",]
#busline152.info

test10<-merge(df1,busline340.info,by="정류소ID",all.x=T)
test10<-test10[,-c(7,8,10,11,12)]
head(test10)
#10



test11<-read.csv("1월 Top11.csv",header=T)
df1<-test11 %>%group_by(표준버스정류장ID) %>% mutate(Boardsum=sum(승차총승객수))
df2<-test11 %>%group_by(표준버스정류장ID) %>% mutate(Landsum=sum(하차총승객수))
df1$Landsum<-df2$Landsum
df1<-unique(df1)

df1<-df1[,-c(5,6)]
df1<-unique(df1)
head(df1)
df1<-df1 %>% rename("정류소ID"="표준버스정류장ID")
df1<-df1 %>% rename("정류소명"="역명")
df1<-df1 %>% rename("노선명"="노선번호")

busline150.info<-busline.info[busline.info$노선명=="150",]
#busline152.info

test11<-merge(df1,busline150.info,by="정류소ID",all.x=T)
test11<-test11[,-c(7,8,10,11,12)]
head(test11)
#11


test12<-read.csv("1월 Top12.csv",header=T)
df1<-test12 %>%group_by(표준버스정류장ID) %>% mutate(Boardsum=sum(승차총승객수))
df2<-test12 %>%group_by(표준버스정류장ID) %>% mutate(Landsum=sum(하차총승객수))
df1$Landsum<-df2$Landsum
df1<-unique(df1)

df1<-df1[,-c(5,6)]
df1<-unique(df1)
head(df1)
df1<-df1 %>% rename("정류소ID"="표준버스정류장ID")
df1<-df1 %>% rename("정류소명"="역명")
df1<-df1 %>% rename("노선명"="노선번호")

busline147.info<-busline.info[busline.info$노선명=="147",]
#busline152.info

test12<-merge(df1,busline147.info,by="정류소ID",all.x=T)
test12<-test12[,-c(7,8,10,11,12)]
head(test12)
#12


test13<-read.csv("1월 Top13.csv",header=T)
df1<-test13 %>%group_by(표준버스정류장ID) %>% mutate(Boardsum=sum(승차총승객수))
df2<-test13 %>%group_by(표준버스정류장ID) %>% mutate(Landsum=sum(하차총승객수))
df1$Landsum<-df2$Landsum
df1<-unique(df1)

df1<-df1[,-c(5,6)]
df1<-unique(df1)
head(df1)
df1<-df1 %>% rename("정류소ID"="표준버스정류장ID")
df1<-df1 %>% rename("정류소명"="역명")
df1<-df1 %>% rename("노선명"="노선번호")

busline370.info<-busline.info[busline.info$노선명=="370",]
#busline152.info

test13<-merge(df1,busline370.info,by="정류소ID",all.x=T)
test13<-test13[,-c(7,8,10,11,12)]
head(test13)
#13


test14<-read.csv("1월 Top14.csv",header=T)
df1<-test14 %>%group_by(표준버스정류장ID) %>% mutate(Boardsum=sum(승차총승객수))
df2<-test14 %>%group_by(표준버스정류장ID) %>% mutate(Landsum=sum(하차총승객수))
df1$Landsum<-df2$Landsum
df1<-unique(df1)

df1<-df1[,-c(5,6)]
df1<-unique(df1)
head(df1)
df1<-df1 %>% rename("정류소ID"="표준버스정류장ID")
df1<-df1 %>% rename("정류소명"="역명")
df1<-df1 %>% rename("노선명"="노선번호")

busline4318.info<-busline.info[busline.info$노선명=="4318",]
#busline152.info

test14<-merge(df1,busline4318.info,by="정류소ID",all.x=T)
test14<-test14[,-c(7,8,10,11,12)]
head(test14)
#14


test15<-read.csv("1월 Top15.csv",header=T)
df1<-test15 %>%group_by(표준버스정류장ID) %>% mutate(Boardsum=sum(승차총승객수))
df2<-test15 %>%group_by(표준버스정류장ID) %>% mutate(Landsum=sum(하차총승객수))
df1$Landsum<-df2$Landsum
df1<-unique(df1)

df1<-df1[,-c(5,6)]
df1<-unique(df1)
head(df1)
df1<-df1 %>% rename("정류소ID"="표준버스정류장ID")
df1<-df1 %>% rename("정류소명"="역명")
df1<-df1 %>% rename("노선명"="노선번호")

busline421.info<-busline.info[busline.info$노선명=="421",]
#busline152.info

test15<-merge(df1,busline421.info,by="정류소ID",all.x=T)
test15<-test15[,-c(7,8,10,11,12)]
head(test15)
#15
#------# ----- # --------

test16<-read.csv("1월 Top16.csv",header=T)
df1<-test16 %>%group_by(표준버스정류장ID) %>% mutate(Boardsum=sum(승차총승객수))
df2<-test16 %>%group_by(표준버스정류장ID) %>% mutate(Landsum=sum(하차총승객수))
df1$Landsum<-df2$Landsum
df1<-unique(df1)

df1<-df1[,-c(5,6)]
df1<-unique(df1)
head(df1)
df1<-df1 %>% rename("정류소ID"="표준버스정류장ID")
df1<-df1 %>% rename("정류소명"="역명")
df1<-df1 %>% rename("노선명"="노선번호")

busline720.info<-busline.info[busline.info$노선명=="720",]
#busline152.info

test16<-merge(df1,busline720.info,by="정류소ID",all.x=T)
test16<-test16[,-c(7,8,10,11,12)]
head(test16)
#16


test17<-read.csv("1월 Top17.csv",header=T)
df1<-test17 %>%group_by(표준버스정류장ID) %>% mutate(Boardsum=sum(승차총승객수))
df2<-test17 %>%group_by(표준버스정류장ID) %>% mutate(Landsum=sum(하차총승객수))
df1$Landsum<-df2$Landsum
df1<-unique(df1)

df1<-df1[,-c(5,6)]
df1<-unique(df1)
head(df1)
df1<-df1 %>% rename("정류소ID"="표준버스정류장ID")
df1<-df1 %>% rename("정류소명"="역명")
df1<-df1 %>% rename("노선명"="노선번호")

busline420.info<-busline.info[busline.info$노선명=="420",]
#busline152.info

test17<-merge(df1,busline420.info,by="정류소ID",all.x=T)
test17<-test17[,-c(7,8,10,11,12)]
head(test17)
#17


test18<-read.csv("1월 Top18.csv",header=T)
df1<-test18 %>%group_by(표준버스정류장ID) %>% mutate(Boardsum=sum(승차총승객수))
df2<-test18 %>%group_by(표준버스정류장ID) %>% mutate(Landsum=sum(하차총승객수))
df1$Landsum<-df2$Landsum
df1<-unique(df1)

df1<-df1[,-c(5,6)]
df1<-unique(df1)
head(df1)
df1<-df1 %>% rename("정류소ID"="표준버스정류장ID")
df1<-df1 %>% rename("정류소명"="역명")
df1<-df1 %>% rename("노선명"="노선번호")

busline144.info<-busline.info[busline.info$노선명=="144",]
#busline152.info

test18<-merge(df1,busline144.info,by="정류소ID",all.x=T)
test18<-test18[,-c(7,8,10,11,12)]
head(test18)
#18


test19<-read.csv("1월 Top19.csv",header=T)
df1<-test19 %>%group_by(표준버스정류장ID) %>% mutate(Boardsum=sum(승차총승객수))
df2<-test19 %>%group_by(표준버스정류장ID) %>% mutate(Landsum=sum(하차총승객수))
df1$Landsum<-df2$Landsum
df1<-unique(df1)

df1<-df1[,-c(5,6)]
df1<-unique(df1)
head(df1)
df1<-df1 %>% rename("정류소ID"="표준버스정류장ID")
df1<-df1 %>% rename("정류소명"="역명")
df1<-df1 %>% rename("노선명"="노선번호")

busline140.info<-busline.info[busline.info$노선명=="140",]
#busline152.info

test19<-merge(df1,busline140.info,by="정류소ID",all.x=T)
test19<-test19[,-c(7,8,10,11,12)]
head(test19)
#19


test20<-read.csv("1월 Top20.csv",header=T)
df1<-test20 %>%group_by(표준버스정류장ID) %>% mutate(Boardsum=sum(승차총승객수))
df2<-test20 %>%group_by(표준버스정류장ID) %>% mutate(Landsum=sum(하차총승객수))
df1$Landsum<-df2$Landsum
df1<-unique(df1)

df1<-df1[,-c(5,6)]
df1<-unique(df1)
head(df1)
df1<-df1 %>% rename("정류소ID"="표준버스정류장ID")
df1<-df1 %>% rename("정류소명"="역명")
df1<-df1 %>% rename("노선명"="노선번호")

busline5531.info<-busline.info[busline.info$노선명=="5531",]
#busline152.info

test20<-merge(df1,busline5531.info,by="정류소ID",all.x=T)
test20<-test20[,-c(7,8,10,11,12)]
head(test20)
#20


test21<-read.csv("1월 Top21.csv",header=T)
df1<-test21 %>%group_by(표준버스정류장ID) %>% mutate(Boardsum=sum(승차총승객수))
df2<-test21 %>%group_by(표준버스정류장ID) %>% mutate(Landsum=sum(하차총승객수))
df1$Landsum<-df2$Landsum
df1<-unique(df1)

df1<-df1[,-c(5,6)]
df1<-unique(df1)
head(df1)
df1<-df1 %>% rename("정류소ID"="표준버스정류장ID")
df1<-df1 %>% rename("정류소명"="역명")
df1<-df1 %>% rename("노선명"="노선번호")

busline303.info<-busline.info[busline.info$노선명=="303",]
#busline152.info

test21<-merge(df1,busline303.info,by="정류소ID",all.x=T)
test21<-test21[,-c(7,8,10,11,12)]
head(test21)
#21


test22<-read.csv("1월 Top22.csv",header=T)
df1<-test22 %>%group_by(표준버스정류장ID) %>% mutate(Boardsum=sum(승차총승객수))
df2<-test22 %>%group_by(표준버스정류장ID) %>% mutate(Landsum=sum(하차총승객수))
df1$Landsum<-df2$Landsum
df1<-unique(df1)

df1<-df1[,-c(5,6)]
df1<-unique(df1)
head(df1)
df1<-df1 %>% rename("정류소ID"="표준버스정류장ID")
df1<-df1 %>% rename("정류소명"="역명")
df1<-df1 %>% rename("노선명"="노선번호")

busline301.info<-busline.info[busline.info$노선명=="301",]
#busline152.info

test22<-merge(df1,busline301.info,by="정류소ID",all.x=T)
test22<-test22[,-c(7,8,10,11,12)]
head(test22)
#22


test23<-read.csv("1월 Top23.csv",header=T)
df1<-test23 %>%group_by(표준버스정류장ID) %>% mutate(Boardsum=sum(승차총승객수))
df2<-test23 %>%group_by(표준버스정류장ID) %>% mutate(Landsum=sum(하차총승객수))
df1$Landsum<-df2$Landsum
df1<-unique(df1)

df1<-df1[,-c(5,6)]
df1<-unique(df1)
head(df1)
df1<-df1 %>% rename("정류소ID"="표준버스정류장ID")
df1<-df1 %>% rename("정류소명"="역명")
df1<-df1 %>% rename("노선명"="노선번호")

busline4212.info<-busline.info[busline.info$노선명=="4212",]
#busline152.info

test23<-merge(df1,busline4212.info,by="정류소ID",all.x=T)
test23<-test23[,-c(7,8,10,11,12)]
head(test23)
#23


test24<-read.csv("1월 Top24.csv",header=T)
df1<-test24 %>%group_by(표준버스정류장ID) %>% mutate(Boardsum=sum(승차총승객수))
df2<-test24 %>%group_by(표준버스정류장ID) %>% mutate(Landsum=sum(하차총승객수))
df1$Landsum<-df2$Landsum
df1<-unique(df1)

df1<-df1[,-c(5,6)]
df1<-unique(df1)
head(df1)
df1<-df1 %>% rename("정류소ID"="표준버스정류장ID")
df1<-df1 %>% rename("정류소명"="역명")
df1<-df1 %>% rename("노선명"="노선번호")

busline262.info<-busline.info[busline.info$노선명=="262",]
#busline152.info

test24<-merge(df1,busline262.info,by="정류소ID",all.x=T)
test24<-test24[,-c(7,8,10,11,12)]
head(test24)
#24

test25<-read.csv("1월 Top25.csv",header=T)
df1<-test25 %>%group_by(표준버스정류장ID) %>% mutate(Boardsum=sum(승차총승객수))
df2<-test25 %>%group_by(표준버스정류장ID) %>% mutate(Landsum=sum(하차총승객수))
df1$Landsum<-df2$Landsum
df1<-unique(df1)

df1<-df1[,-c(5,6)]
df1<-unique(df1)
head(df1)
df1<-df1 %>% rename("정류소ID"="표준버스정류장ID")
df1<-df1 %>% rename("정류소명"="역명")
df1<-df1 %>% rename("노선명"="노선번호")

busline273.info<-busline.info[busline.info$노선명=="273",]
#busline152.info

test25<-merge(df1,busline273.info,by="정류소ID",all.x=T)
test25<-test25[,-c(7,8,10,11,12)]
head(test25)
#25



test26<-read.csv("1월 Top26.csv",header=T)
df1<-test26 %>%group_by(표준버스정류장ID) %>% mutate(Boardsum=sum(승차총승객수))
df2<-test26 %>%group_by(표준버스정류장ID) %>% mutate(Landsum=sum(하차총승객수))
df1$Landsum<-df2$Landsum
df1<-unique(df1)

df1<-df1[,-c(5,6)]
df1<-unique(df1)
head(df1)
df1<-df1 %>% rename("정류소ID"="표준버스정류장ID")
df1<-df1 %>% rename("정류소명"="역명")
df1<-df1 %>% rename("노선명"="노선번호")

busline641.info<-busline.info[busline.info$노선명=="641",]
#busline152.info

test26<-merge(df1,busline641.info,by="정류소ID",all.x=T)
test26<-test26[,-c(7,8,10,11,12)]
head(test26)
#26


test27<-read.csv("1월 Top27.csv",header=T)
df1<-test27 %>%group_by(표준버스정류장ID) %>% mutate(Boardsum=sum(승차총승객수))
df2<-test27 %>%group_by(표준버스정류장ID) %>% mutate(Landsum=sum(하차총승객수))
df1$Landsum<-df2$Landsum
df1<-unique(df1)

df1<-df1[,-c(5,6)]
df1<-unique(df1)
head(df1)
df1<-df1 %>% rename("정류소ID"="표준버스정류장ID")
df1<-df1 %>% rename("정류소명"="역명")
df1<-df1 %>% rename("노선명"="노선번호")

busline5524.info<-busline.info[busline.info$노선명=="5524",]
#busline152.info

test27<-merge(df1,busline5524.info,by="정류소ID",all.x=T)
test27<-test27[,-c(7,8,10,11,12)]
head(test27)
#27


test28<-read.csv("1월 Top28.csv",header=T)
df1<-test28 %>%group_by(표준버스정류장ID) %>% mutate(Boardsum=sum(승차총승객수))
df2<-test28 %>%group_by(표준버스정류장ID) %>% mutate(Landsum=sum(하차총승객수))
df1$Landsum<-df2$Landsum
df1<-unique(df1)

df1<-df1[,-c(5,6)]
df1<-unique(df1)
head(df1)
df1<-df1 %>% rename("정류소ID"="표준버스정류장ID")
df1<-df1 %>% rename("정류소명"="역명")
df1<-df1 %>% rename("노선명"="노선번호")

busline142.info<-busline.info[busline.info$노선명=="142",]
#busline152.info

test28<-merge(df1,busline142.info,by="정류소ID",all.x=T)
test28<-test28[,-c(7,8,10,11,12)]
head(test28)
#28



test29<-read.csv("1월 Top29.csv",header=T)
df1<-test29 %>%group_by(표준버스정류장ID) %>% mutate(Boardsum=sum(승차총승객수))
df2<-test29 %>%group_by(표준버스정류장ID) %>% mutate(Landsum=sum(하차총승객수))
df1$Landsum<-df2$Landsum
df1<-unique(df1)

df1<-df1[,-c(5,6)]
df1<-unique(df1)
head(df1)
df1<-df1 %>% rename("정류소ID"="표준버스정류장ID")
df1<-df1 %>% rename("정류소명"="역명")
df1<-df1 %>% rename("노선명"="노선번호")

busline120.info<-busline.info[busline.info$노선명=="120",]
#busline152.info

test29<-merge(df1,busline120.info,by="정류소ID",all.x=T)
test29<-test29[,-c(7,8,10,11,12)]
head(test29)
#29



test30<-read.csv("1월 Top30.csv",header=T)
df1<-test30 %>%group_by(표준버스정류장ID) %>% mutate(Boardsum=sum(승차총승객수))
df2<-test30 %>%group_by(표준버스정류장ID) %>% mutate(Landsum=sum(하차총승객수))
df1$Landsum<-df2$Landsum
df1<-unique(df1)

df1<-df1[,-c(5,6)]
df1<-unique(df1)
head(df1)
df1<-df1 %>% rename("정류소ID"="표준버스정류장ID")
df1<-df1 %>% rename("정류소명"="역명")
df1<-df1 %>% rename("노선명"="노선번호")

busline202.info<-busline.info[busline.info$노선명=="202",]
#busline152.info

test30<-merge(df1,busline202.info,by="정류소ID",all.x=T)
test30<-test30[,-c(7,8,10,11,12)]
head(test30)
#30

test.1월<-rbind(test1,test2,test3,test4,test5,test6,test7,test8,test9,test10,test11,test12,test13,test14,
               test15,test16,test17,test18,test19,test20,test21,test22,test23,test24,test25,test26,test27,
               test28,test29,test30)
View(test.1월)
#write.csv(test.1월,file="1월top30.csv",row.names=F)

test.1월<-as.data.frame(test.1월)
test.1월$노선명.x<-as.factor(test.1월$노선명.x)
str(test.1월)
head(test.1월)
#_--------------------------------------------------------------------------------------------------------
head(test1)
ge<-geocode(enc2utf8("서울특별시"))
cen<-as.numeric(ge)
Map<-get_googlemap(center=cen,zoom=11,maptype = 'roadmap',color='bw')
p<-ggmap(Map)+geom_point(data=test.1월,aes(x=X좌표,y=Y좌표,color=노선명.x))
?geom_point
p
ggmap(Map)
