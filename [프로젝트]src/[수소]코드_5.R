library(dplyr)
library(corrgram)


setwd("C:/Users/fkaus/OneDrive/바탕 화면/데이터캠퍼스 프로젝트/데이터")
test1 <- read.csv("지방도.csv",header=T)

str(test1)
test1 <- test1[test1$조사.위치=="충북",]
test1 <- test1[,c(2,3,4,17)]
test1 <- test1[,-1]

names(test1)[1] <- "지역"
names(test1)[2] <- "시군"
names(test1)[3] <- "교통량"
test1

write.csv(tset1,file="충북교통량_지방도.csv")

test1 <- read.csv("충북교통량_지방도.csv",header=T)
test1
result <- test1 %>% group_by(시군) %>% summarise(result=sum(교통량))
View(result)
write.csv(result,file="충북교탕량_지방도(2).csv")


signature <- read.csv("signature_table.csv",header = T)
boxplot(test2)



#-------------------------------------------------------------------------------


signature<-read.csv("signature table (5).csv")

sig<-signature[,-1]
sig$총승용차수.대.<-as.numeric(sig$총승용차수.대.)
sig$총승용차수.대.<-c(84512,339626,35840,14931,50354,41390,18067,15919,11086,10351,13644)
x<-scale(sig)
x<-dist(sig)
plot(h <- hclust(dist(x),method="single")) 
plot(h <- hclust(dist(x),method="complete")) 
plot(h <- hclust(dist(x),method="average")) 
plot(h <- hclust(dist(x),method="centroid")) 
plot(h <- hclust(dist(x),method="ward.D2")) 

corrgram(signature,upper.panel=panel.conf)
#-------------------------------------------------------------------------------


View(test1)

test1 <- test1[test1$조사.위치=="충북",]
test1 <- test1[,c(3,4,5,6,17)]
#test1 <- test1[,-1]
test1 <- test1[test1$X=="청주",]

names(test1)


names(test1)[1] <- "도"
names(test1)[2] <- "시군"
names(test1)[3] <- "구/면"
names(test1)[4] <- "동/리"
View(test1)

write.csv(test1,file="충북_지방도.csv")




