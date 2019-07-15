# 중국 화창품 기업 데이터 Handling

# Goal: 19개 부총의 1월~12월까지의 매출액 분석

# Data Handling Process 
# 1. dealerpriceamt, c53amt,code는 고려하지 않는다. 
# 2. itemtype에서 TAN만 고려한다. 
# 3. Cancel Code가 존재하면, 해당 레코드는 제외한다. 


library(dplyr)




getwd()
DATA=read.csv("Order1501.csv",sep=",",header=T,stringsAsFactors = F)
str(DATA)
# data.frame의 형태이고, 총 266298개의 행을 가지며 17개의 독립변수가 존재 

# 1. dealerpriceamt, c53amt,code는 고려하지 않는다. 즉, 제거한다. 
# 해당 column을 전체 삭제합니다. 
DATA=DATA[,-c(12,13,16)]
str(DATA)


# 2. itemtype에서 TAN만 고려한다.
# 3. Cancel Code에서 결측치가 있는 레코드가 주요한 데이터이다. 

# ItemType은 filter조건을 사용하여 TAN인 경우의 데이터만 모은다. 
# 2단계 과정을 거치면 26만의 데이터에서 16만 데이터 개수로 축소

DATA=DATA %>% filter(ItemType=="TAN" & is.na(CancelCode))
str(DATA)
