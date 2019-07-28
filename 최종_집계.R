library(dplyr)

# 시그니쳐테이블 집계 
# 주문건수, 주문금액, 배송완료금액, 배송완료율

# 배송 미완료는 signature.Table.by.Group1.csv에서 찾을 수 없습니다. 
# OrderDate-RegisterDate-DeliveryDate 순서이기 때문에 
# signature테이블 만들때 OrderDate-RegisterDate-DeliveryDate 순서를 벗어나면 제외했으므로 
# 미완료는? 




setwd("C:/Users/fkaus/OneDrive/문서/주문파일")

signature<-read.csv("Signature.Table.by.Group1.csv",header=T,stringsAsFactors = F)

signature.1월주문건수<-signature[signature$SapID>15698 & signature$SapID<23597,]
합계.1월주문건수<-length(unique(signature.1월주문건수$SapID))
합계.1월주문건수

signature.2월주문건수<-signature[signature$SapID>23600 & signature$SapID<30465,]
합계.2월주문건수<-length(unique(signature.2월주문건수$SapID))
합계.2월주문건수

signature.3월주문건수<-signature[signature$SapID>30465 & signature$SapID<38386,]
합계.3월주문건수<-length(unique(signature.3월주문건수$SapID))
합계.3월주문건수

signature.4월주문건수<-signature[signature$SapID>38385 & signature$SapID<47545,]
합계.4월주문건수<-length(unique(signature.4월주문건수$SapID))
합계.4월주문건수

signature.5월주문건수<-signature[signature$SapID>47545 & signature$SapID<57041,]
합계.5월주문건수<-length(unique(signature.5월주문건수$SapID))
합계.5월주문건수

signature.6월주문건수<-signature[signature$SapID>56990 & signature$SapID<66283,]
합계.6월주문건수<-length(unique(signature.6월주문건수$SapID))
합계.6월주문건수

signature.7월주문건수<-signature[signature$SapID>66269 & signature$SapID<74982,]
합계.7월주문건수<-length(unique(signature.7월주문건수$SapID))
합계.7월주문건수

signature.8월주문건수<-signature[signature$SapID>74868 & signature$SapID<84462,]
합계.8월주문건수<-length(unique(signature.8월주문건수$SapID))
합계.8월주문건수

signature.9월주문건수<-signature[signature$SapID>84462 & signature$SapID<97348,]
합계.9월주문건수<-length(unique(signature.9월주문건수$SapID))
합계.9월주문건수

signature.10월주문건수<-signature[signature$SapID>93654 & signature$SapID<103428,]
합계.10월주문건수<-length(unique(signature.10월주문건수$SapID))
합계.10월주문건수

signature.11월주문건수<-signature[signature$SapID>103430 & signature$SapID<112719,]
합계.11월주문건수<-length(unique(signature.11월주문건수$SapID))
합계.11월주문건수


signature.12월주문건수<-signature[signature$SapID>112383 & signature$SapID<124211,]
합계.12월주문건수<-length(unique(signature.12월주문건수$SapID))
합계.12월주문건수

#----------------------주문금액 & 배송완료 금액---------------------------------------------------
# 주문금액과 배송완료 금액 차이가 .... 


signature.1월주문금액<-signature[signature$SapID>15698 & signature$SapID<23597,]
합계.1월주문금액<-sum(unique(signature.1월주문금액$주문총액))/1000000
합계.1월주문금액

signature.1월배송완료금액<-signature[signature$SapID>15698 & signature$SapID<23597,]
합계.1월배송완료금액<-sum(signature.1월배송완료금액$배송총액.위안.)/1000000
합계.1월배송완료금액

합계.배송완료율.1월<-(합계.1월배송완료금액/합계.1월주문금액)*100
합계.배송완료율.1월
#--------------

signature.2월주문금액<-signature[signature$SapID>23600 & signature$SapID<30465,]
합계.2월주문금액<-sum(signature.2월주문금액$주문총액)/1000000
합계.2월주문금액

signature.2월배송완료금액<-signature[signature$SapID>23600 & signature$SapID<30465,]
합계.2월배송완료금액<-sum(signature.2월주문금액$배송총액.위안.)/1000000
합계.2월배송완료금액

합계.배송완료율.2월<-(합계.2월배송완료금액/합계.2월주문금액)*100
합계.배송완료율.2월


#--------------

signature.3월주문금액<-signature[signature$SapID>30465 & signature$SapID<38386,]
합계.3월주문금액<-sum(signature.3월주문금액$주문총액)/1000000
합계.3월주문금액

signature.3월배송완료금액<-signature[signature$SapID>30465 & signature$SapID<38386,]
합계.3월배송완료금액<-sum(signature.3월주문금액$배송총액.위안.)/1000000
합계.3월배송완료금액

합계.배송완료율.3월<-(합계.3월배송완료금액/합계.3월주문금액)*100
합계.배송완료율.3월


#--------------

signature.4월주문금액<-signature[signature$SapID>38385 & signature$SapID<47545,]
합계.4월주문금액<-sum(signature.4월주문금액$주문총액)/1000000
합계.4월주문금액

signature.4월배송완료금액<-signature[signature$SapID>38385 & signature$SapID<47545,]
합계.4월배송완료금액<-sum(signature.4월주문금액$배송총액.위안.)/1000000
합계.4월배송완료금액

합계.배송완료율.4월<-합계.4월주문금액/합계.4월배송완료금액
합계.배송완료율.4월

합계.배송완료율.4월<-(합계.4월배송완료금액/합계.4월주문금액)*100
합계.배송완료율.4월


#--------------

signature.5월주문금액<-signature[signature$SapID>47545 & signature$SapID<57041,]
합계.5월주문금액<-sum(signature.5월주문금액$주문총액)/1000000
합계.5월주문금액

signature.5월배송완료금액<-signature[signature$SapID>47545 & signature$SapID<57041,]
합계.5월배송완료금액<-sum(signature.5월주문금액$배송총액.위안.)/1000000
합계.5월배송완료금액

합계.배송완료율.5월<-(합계.5월배송완료금액/합계.5월주문금액)*100
합계.배송완료율.5월


#--------------

signature.6월주문금액<-signature[signature$SapID>56990 & signature$SapID<66283,]
합계.6월주문금액<-sum(signature.6월주문금액$주문총액)/1000000
합계.6월주문금액

signature.6월배송완료금액<-signature[signature$SapID>56990 & signature$SapID<66283,]
합계.6월배송완료금액<-sum(signature.6월주문금액$배송총액.위안.)/1000000
합계.6월배송완료금액


합계.배송완료율.6월<-(합계.6월배송완료금액/합계.6월주문금액)*100
합계.배송완료율.6월

#--------------

signature.7월주문금액<-signature[signature$SapID>66269 & signature$SapID<74982,]
합계.7월주문금액<-sum(signature.7월주문금액$주문총액)/1000000
합계.7월주문금액

signature.7월배송완료금액<-signature[signature$SapID>66269 & signature$SapID<74982,]
합계.7월배송완료금액<-sum(signature.7월주문금액$배송총액.위안.)/1000000
합계.7월배송완료금액

합계.배송완료율.7월<-(합계.7월배송완료금액/합계.7월주문금액)*100
합계.배송완료율.7월



#--------------

signature.8월주문금액<-signature[signature$SapID>74868 & signature$SapID<84462,]
합계.8월주문금액<-sum(signature.8월주문금액$주문총액)/1000000
합계.8월주문금액

signature.8월배송완료금액<-signature[signature$SapID>74868 & signature$SapID<84462,]
합계.8월배송완료금액<-sum(signature.8월주문금액$배송총액.위안.)/1000000
합계.8월배송완료금액

합계.배송완료율.8월<-(합계.8월배송완료금액/합계.8월주문금액)*100
합계.배송완료율.8월


#--------------#9월달 배송완료금액 NA발생

signature.9월주문금액<-signature[signature$SapID>84462 & signature$SapID<97348,]
합계.9월주문금액<-sum(signature.9월주문금액$주문총액)/1000000
합계.9월주문금액

signature.9월배송완료금액<-signature[signature$SapID>84462 & signature$SapID<97348,]
합계.9월배송완료금액<-sum(signature.9월주문금액$배송총액.위안.,na.rm=T)/1000000
합계.9월배송완료금액


합계.배송완료율.9월<-(합계.9월배송완료금액/합계.9월주문금액)*100
합계.배송완료율.9월

#--------------

signature.10월주문금액<-signature[signature$SapID>93654 & signature$SapID<103428,]
합계.10월주문금액<-sum(signature.10월주문금액$주문총액)/1000000
합계.10월주문금액

signature.10월배송완료금액<-signature[signature$SapID>93654 & signature$SapID<103428,]
합계.10월배송완료금액<-sum(signature.10월주문금액$배송총액.위안.,na.rm=T)/1000000
합계.10월배송완료금액

합계.배송완료율.10월<-(합계.10월배송완료금액/합계.10월주문금액)*100
합계.배송완료율.10월


#--------------

signature.11월주문금액<-signature[signature$SapID>103430 & signature$SapID<112719,]
합계.11월주문금액<-sum(signature.11월주문금액$주문총액)/1000000
합계.11월주문금액

signature.11월배송완료금액<-signature[signature$SapID>103430 & signature$SapID<112719,]
합계.11월배송완료금액<-sum(signature.11월주문금액$배송총액.위안.,na.rm=T)/1000000
합계.11월배송완료금액

합계.배송완료율.11월<-(합계.11월배송완료금액/합계.11월주문금액)*100
합계.배송완료율.11월


#--------------

signature.12월주문금액<-signature[signature$SapID>112383 & signature$SapID<124211,]
합계.12월주문금액<-sum(signature.12월주문금액$주문총액)/1000000
합계.12월주문금액

signature.12월배송완료금액<-signature[signature$SapID>112383 & signature$SapID<124211,]
합계.12월배송완료금액<-sum(signature.12월주문금액$배송총액.위안.,na.rm=T)/1000000
합계.12월배송완료금액

합계.배송완료율.12월<-(합계.12월배송완료금액/합계.12월주문금액)*100
합계.배송완료율.12월


#------------------------------총 배송회수-------------------------------------------

signature.1월총배송회수<-signature[signature$SapID>15698 & signature$SapID<23597,]
합계.1월총배송회수<-sum(table(signature.1월총배송회수$SapID))
합계.1월총배송회수

signature.2월총배송회수<-signature[signature$SapID>23600 & signature$SapID<30465,]
합계.2월총배송회수<-sum(table(signature.2월총배송회수$SapID))
합계.2월총배송회수

signature.3월총배송회수<-signature[signature$SapID>30465 & signature$SapID<38386,]
합계.3월총배송회수<-sum(table(signature.3월총배송회수$SapID))
합계.3월총배송회수

signature.4월총배송회수<-signature[signature$SapID>38385 & signature$SapID<47545,]
합계.4월총배송회수<-sum(table(signature.4월총배송회수$SapID))
합계.4월총배송회수

signature.5월총배송회수<-signature[signature$SapID>47545 & signature$SapID<57041,]
합계.5월총배송회수<-sum(table(signature.5월총배송회수$SapID))
합계.5월총배송회수

signature.6월총배송회수<-signature[signature$SapID>56990 & signature$SapID<66283,]
합계.6월총배송회수<-sum(table(signature.6월총배송회수$SapID))
합계.6월총배송회수


signature.7월총배송회수<-signature[signature$SapID>66269 & signature$SapID<74982,]
합계.7월총배송회수<-sum(table(signature.7월총배송회수$SapID))
합계.7월총배송회수

signature.8월총배송회수<-signature[signature$SapID>74868 & signature$SapID<84462,]
합계.8월총배송회수<-sum(table(signature.8월총배송회수$SapID))
합계.8월총배송회수

signature.9월총배송회수<-signature[signature$SapID>84462 & signature$SapID<97348,]
합계.9월총배송회수<-sum(table(signature.9월총배송회수$SapID))
합계.9월총배송회수


signature.10월총배송회수<-signature[signature$SapID>93654 & signature$SapID<103428,]
합계.10월총배송회수<-sum(table(signature.10월총배송회수$SapID))
합계.10월총배송회수

signature.11월총배송회수<-signature[signature$SapID>103430 & signature$SapID<112719,]
합계.11월총배송회수<-sum(table(signature.11월총배송회수$SapID))
합계.11월총배송회수


signature.12월총배송회수<-signature[signature$SapID>112383 & signature$SapID<124211,]
합계.12월총배송회수<-sum(table(signature.12월총배송회수$SapID))
합계.12월총배송회수

#--------------------주문당배송회수-------------------------------------------------------------------------------------------

합계.1월주문당배송회수<-합계.1월총배송회수/합계.1월주문건수
합계.1월주문당배송회수

합계.2월주문당배송회수<-합계.2월총배송회수/합계.2월주문건수
합계.2월주문당배송회수

합계.3월주문당배송회수<-합계.3월총배송회수/합계.3월주문건수
합계.3월주문당배송회수

합계.4월주문당배송회수<-합계.4월총배송회수/합계.4월주문건수
합계.4월주문당배송회수

합계.5월주문당배송회수<-합계.5월총배송회수/합계.5월주문건수
합계.5월주문당배송회수

합계.6월주문당배송회수<-합계.6월총배송회수/합계.6월주문건수
합계.6월주문당배송회수

합계.7월주문당배송회수<-합계.7월총배송회수/합계.7월주문건수
합계.7월주문당배송회수

합계.8월주문당배송회수<-합계.8월총배송회수/합계.8월주문건수
합계.8월주문당배송회수

합계.9월주문당배송회수<-합계.9월총배송회수/합계.9월주문건수
합계.9월주문당배송회수

합계.10월주문당배송회수<-합계.10월총배송회수/합계.10월주문건수
합계.10월주문당배송회수

합계.11월주문당배송회수<-합계.11월총배송회수/합계.11월주문건수
합계.11월주문당배송회수

합계.12월주문당배송회수<-합계.12월총배송회수/합계.12월주문건수
합계.12월주문당배송회수

#-------------금액기준.배송완료.소요기간.일.---------------------------------------------------------------------------------------

signature.1월금배소<-signature[signature$SapID>15698 & signature$SapID<23597,]
합계.1월금배소<-mean(signature.1월금배소$금액기준.배송완료.소요기간.일.)
합계.1월금배소

signature.2월금배소<-signature[signature$SapID>23600 & signature$SapID<30465,]
합계.2월금배소<-mean(signature.2월금배소$금액기준.배송완료.소요기간.일.)
합계.2월금배소

signature.3월금배소<-signature[signature$SapID>30465 & signature$SapID<38386,]
합계.3월금배소<-mean(signature.3월금배소$금액기준.배송완료.소요기간.일.)
합계.3월금배소

signature.4월금배소<-signature[signature$SapID>38385 & signature$SapID<47545,]
합계.4월금배소<-mean(signature.4월금배소$금액기준.배송완료.소요기간.일.,na.rm=T)
합계.4월금배소

signature.5월금배소<-signature[signature$SapID>47545 & signature$SapID<57041,]
합계.5월금배소<-mean(signature.5월금배소$금액기준.배송완료.소요기간.일.)
합계.5월금배소

signature.6월금배소<-signature[signature$SapID>56990 & signature$SapID<66283,]
합계.6월금배소<-mean(signature.6월금배소$금액기준.배송완료.소요기간.일.)
합계.6월금배소


signature.7월금배소<-signature[signature$SapID>66269 & signature$SapID<74982,]
합계.7월금배소<-mean(signature.7월금배소$금액기준.배송완료.소요기간.일.,na.rm=T)
합계.7월금배소

signature.8월금배소<-signature[signature$SapID>74868 & signature$SapID<84462,]
합계.8월금배소<-mean(signature.8월금배소$금액기준.배송완료.소요기간.일.,na.rm=T)
합계.8월금배소

signature.9월금배소<-signature[signature$SapID>84462 & signature$SapID<97348,]
합계.9월금배소<-mean(signature.9월금배소$금액기준.배송완료.소요기간.일.,na.rm=T)
합계.9월금배소


signature.10월금배소<-signature[signature$SapID>93654 & signature$SapID<103428,]
합계.10월금배소<-mean(signature.10월금배소$금액기준.배송완료.소요기간.일.,na.rm=T)
합계.10월금배소

signature.11월금배소<-signature[signature$SapID>103430 & signature$SapID<112719,]
합계.11월금배소<-mean(signature.11월금배소$금액기준.배송완료.소요기간.일.)
합계.11월금배소


signature.12월금배소<-signature[signature$SapID>112383 & signature$SapID<124211,]
합계.12월금배소<-mean(signature.12월금배소$금액기준.배송완료.소요기간.일.)
합계.12월금배소

#---------------------


#-------------MAX배송차수 구하기---------------------------------------------------------------------------------------

signature.1월MAX<-signature[signature$SapID>15698 & signature$SapID<23597,]
합계.1월MAX<-max(signature.1월MAX$차수)
합계.1월MAX

signature.2월MAX<-signature[signature$SapID>23600 & signature$SapID<30465,]
합계.2월MAX<-max(signature.2월MAX$차수)
합계.2월MAX

signature.3월MAX<-signature[signature$SapID>30465 & signature$SapID<38386,]
합계.3월MAX<-max(signature.3월MAX$차수)
합계.3월MAX

signature.4월MAX<-signature[signature$SapID>38385 & signature$SapID<47545,]
합계.4월MAX<-max(signature.4월MAX$차수)
합계.4월MAX

signature.5월MAX<-signature[signature$SapID>47545 & signature$SapID<57041,]
합계.5월MAX<-max(signature.5월MAX$차수)
합계.5월MAX

signature.6월MAX<-signature[signature$SapID>56990 & signature$SapID<66283,]
합계.6월MAX<-max(signature.6월MAX$차수)
합계.6월MAX


signature.7월MAX<-signature[signature$SapID>66269 & signature$SapID<74982,]
합계.7월MAX<-max(signature.7월MAX$차수)
합계.7월MAX

signature.8월MAX<-signature[signature$SapID>74868 & signature$SapID<84462,]
합계.8월MAX<-max(signature.8월MAX$차수)
합계.8월MAX

signature.9월MAX<-signature[signature$SapID>84462 & signature$SapID<97348,]
합계.9월MAX<-max(signature.9월MAX$차수)
합계.9월MAX


signature.10월MAX<-signature[signature$SapID>93654 & signature$SapID<103428,]
합계.10월MAX<-max(signature.10월MAX$차수)
합계.10월MAX

signature.11월MAX<-signature[signature$SapID>103430 & signature$SapID<112719,]
합계.11월MAX<-max(signature.11월MAX$차수)
합계.11월MAX


signature.12월MAX<-signature[signature$SapID>112383 & signature$SapID<124211,]
합계.12월MAX<-max(signature.12월MAX$차수)
합계.12월MAX













m1=matrix(c(합계.1월주문건수,합계.2월주문건수,합계.3월주문건수,합계.4월주문건수,합계.5월주문건수,합계.6월주문건수,합계.7월주문건수,합계.8월주문건수,합계.9월주문건수,합계.10월주문건수,합계.11월주문건수,합계.12월주문건수))
colnames(m1)=c("주문건수")
rownames(m1)=c("1월","2월","3월","4월","5월","6월","7월","8월","9월","10월","11월","12월")
View(m1)


m2=matrix(c(합계.1월주문금액,합계.2월주문금액,합계.3월주문금액,합계.4월주문금액,합계.5월주문금액,합계.6월주문금액,합계.7월주문금액,합계.8월주문금액,합계.9월주문금액,합계.10월주문금액,합계.11월주문금액,합계.12월주문금액))
colnames(m2)=c("주문금액")
rownames(m2)=c("1월","2월","3월","4월","5월","6월","7월","8월","9월","10월","11월","12월")
View(m2)

m3=matrix(c(합계.1월배송완료금액,합계.2월배송완료금액,합계.3월배송완료금액,합계.4월배송완료금액,합계.5월배송완료금액,합계.6월배송완료금액,합계.7월배송완료금액,합계.8월배송완료금액,합계.9월배송완료금액,합계.10월배송완료금액,합계.11월배송완료금액,합계.12월배송완료금액))
colnames(m3)=c("배송완료금액")
rownames(m3)=c("1월","2월","3월","4월","5월","6월","7월","8월","9월","10월","11월","12월")
View(m3)


m4=matrix(c(합계.배송완료율.1월,합계.배송완료율.2월,합계.배송완료율.3월,합계.배송완료율.4월,합계.배송완료율.5월,합계.배송완료율.6월,합계.배송완료율.7월,합계.배송완료율.8월,합계.배송완료율.9월,합계.배송완료율.10월,합계.배송완료율.11월,합계.배송완료율.12월))
colnames(m4)=c("배송완료율(금액)")
rownames(m4)=c("1월","2월","3월","4월","5월","6월","7월","8월","9월","10월","11월","12월")
View(m4)

m5=matrix(c(합계.1월총배송회수,합계.2월총배송회수,합계.3월총배송회수,합계.4월총배송회수,합계.5월총배송회수,합계.6월총배송회수,합계.7월총배송회수,합계.8월총배송회수,합계.9월총배송회수,합계.10월총배송회수,합계.11월총배송회수,합계.12월총배송회수))
colnames(m5)=c("총배송회수")
rownames(m5)=c("1월","2월","3월","4월","5월","6월","7월","8월","9월","10월","11월","12월")
View(m5)

m6=matrix(c(합계.1월주문당배송회수,합계.2월주문당배송회수,합계.3월주문당배송회수,합계.4월주문당배송회수,합계.5월주문당배송회수,합계.6월주문당배송회수,합계.7월주문당배송회수,합계.8월주문당배송회수,합계.9월주문당배송회수,합계.10월주문당배송회수,합계.11월주문당배송회수,합계.12월주문당배송회수))
colnames(m6)=c("주문당배송회수")
rownames(m6)=c("1월","2월","3월","4월","5월","6월","7월","8월","9월","10월","11월","12월")
View(m6)

m7=matrix(c(합계.1월금배소,합계.2월금배소,합계.3월금배소,합계.4월금배소,합계.5월금배소,합계.6월금배소,합계.7월금배소,합계.8월금배소,합계.9월금배소,합계.10월금배소,합계.11월금배소,합계.12월금배소))
colnames(m7)=c("금액기준배송완료소요기간")
rownames(m7)=c("1월","2월","3월","4월","5월","6월","7월","8월","9월","10월","11월","12월")
View(m7)

m8=matrix(c(합계.1월MAX,합계.2월MAX,합계.3월MAX,합계.4월MAX,합계.5월MAX,합계.6월MAX,합계.7월MAX,합계.8월MAX,합계.9월MAX,합계.10월MAX,합계.11월MAX,합계.12월MAX))
colnames(m8)=c("MAX배송회수")
rownames(m8)=c("1월","2월","3월","4월","5월","6월","7월","8월","9월","10월","11월","12월")
View(m8)


View(cbind(m1,m2,m3,m4,m5,m6,m7,m8))

all<-cbind(m1,m2,m3,m4,m5,m6,m7,m8)

write.csv(all,file = "all.csv",row.names=F)

