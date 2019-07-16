# 중국 화창품 기업 데이터 Handling

# Goal: 19개 부총의 1월~12월까지의 매출액 분석

# Data Handling Process 
# 1. dealerpriceamt, c53amt,code는 고려하지 않는다. 
# 2. itemtype에서 TAN만 고려한다. 
# 3. Cancel Code가 존재하면, 해당 레코드는 제외한다. 

library(dplyr)
library(xlsx)


getwd()
setwd("C:/Users/fkaus/OneDrive/문서/China")

DATA1<-read.csv("Order1501.csv",sep=",",header=T,stringsAsFactors = T)
str(DATA1)

# DATA2<-read.csv("Order1502.csv",sep=",",header=T,stringsAsFactors = T)
# DATA3<-read.csv("Order1503.csv",sep=",",header=T,stringsAsFactors = T)
# DATA4<-read.csv("Order1504.csv",sep=",",header=T,stringsAsFactors = T)
# DATA5<-read.csv("Order1505.csv",sep=",",header=T,stringsAsFactors = T)
# DATA6<-read.csv("Order1506.csv",sep=",",header=T,stringsAsFactors = T)
# DATA7<-read.csv("Order1507.csv",sep=",",header=T,stringsAsFactors = T)
# DATA8<-read.csv("Order1508.csv",sep=",",header=T,stringsAsFactors = T)
# DATA9<-read.csv("Order1509.csv",sep=",",header=T,stringsAsFactors = T)
# DATA10<-read.csv('Order1510.csv',sep=",",header=T,stringsAsFactors = T)
# DATA11<-read.csv("Order1511.csv",sep=",",header=T,stringsAsFactors = T)
# DATA12<-read.csv("Order1512.csv",sep=",",header=T,stringsAsFactors = T)


# data.frame의 형태이고, 총 266298개의 행을 가지며 17개의 독립변수가 존재 

# 1. dealerpriceamt, c53amt,code는 고려하지 않는다. 즉, 제거한다. 
# 해당 column을 전체 삭제합니다. 
DATA1=DATA1[,-c(1,3,5,8,10,12,13,14,16)]
str(DATA1)


# 2. itemtype에서 TAN만 고려한다.
# 3. Cancel Code에서 결측치가 있는 레코드가 주요한 데이터이다. 

# ItemType은 filter조건을 사용하여 TAN인 경우의 데이터만 모은다. 
# 2단계 과정을 거치면 26만의 데이터에서 16만 데이터 개수로 축소

DATA1=DATA1 %>% filter(ItemType=="TAN" & is.na(CancelCode))

# 쉬운거 부터!! 
# 월 평균 주문 건수: 대리점코드 & SapID의 개수를 센다.
SPLIT1<-split(DATA1,DATA$SapID)
SPLIT1[1]
SPLIT1[2]

length(SPLIT1)


SapID_count<- DATA1 %>% filter(SPLIT1) %>% length()











