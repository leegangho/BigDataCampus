setwd("C:/Users/fkaus/OneDrive/바탕 화면/데이터캠퍼스 프로젝트/데이터")
cor.buy.car<- read.csv("소득수준상관관계.csv",header=T)
head(cor.buy.car)

# -0.0748
# 약한 음의 관계 거의 의미가 없다. 
cor(cor.buy.car$소득수준,cor.buy.car$수소차)


# p-value:0.7829
# alternative hypothesis: true correlation is not equal to 0

cor.test(cor.buy.car$소득수준,cor.buy.car$수소차)


cor.house.car <- read.csv("집값상관관계.csv",header=T)

cor(cor.house.car$집값,cor.house.car$수소차)

