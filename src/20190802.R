#20190802 비정형 데이터 분석 

library(RHINO)
library(makeDTM)
library(tm)
library(arules) # 연관규칙
library(arulesViz) # 연관규칙
library(igraph)
library(corrplot)
library(topicmodels)
library(e1071)



setwd("D:/BigDataCampus/practㅅceData/2차 실습파일")
#setwd("D:/BigDataCampus/practiceData")


# 통상적으로 +-0.3 이상이면 상관관계가 있음으로 판단한다. 
cor(iris$Sepal.Width,iris$Sepal.Length)   # -0.1175698

cor(iris[,1:4])
iris.cor<-cor(iris[,1:4])

# 전체적인 상관관계를 기호로 표시해서 보여준다. symnum() 
symnum(iris.cor)

initRhino()

# 텍스트를 읽어옵니다.
txt<-readLines("sample_news.txt")
 
# 텍스트에서 명사만 뽑아옵니다. 
noun<-lapply(txt,getMorph,"noun")

# 리스트에서 벡터형태로 형변환을 합니다. 
nounVec<-unlist(noun)
nounFreq<-table(nounVec)

# 빈도수를 내림차순 정렬하고, 20개 명사를 keywords에 저장합니다. 
keywords<-names(head(sort(nounFreq,decreasing = T),20))
docs<-as.data.frame(txt)

# makeDTM() 함수를 이용해서 docs를 DTM형태로 구성합니다. 
dtm<-makeDTM(docs,key=keywords,TEXT.name="txt",RHINO=T)

# DTM으로 만든 후, 상관관계를 알고싶은 열만 추출합니다. 
word1<-as.vector(dtm[,"정부"])
word2<-as.vector(dtm[,"가계"])

cor(word1,word2) #-0.375 
cor.test(word1,word2)

# DTM, 분석자가 지정한 단어, 분석자가 설정한 피어슨 상관계수 크기 기준 
findAssocTwo(dtm,"계획","투자")
findAssocsAll(dtm)


# 하나의 트랜잭션(사건) 에서 중복 항목을 삭제 합니다. 
# transactions 9835 rows, 169items(columns)
data<-read.transactions("groceries.csv",rm.duplicates = T,sep=",")

summary(data)
# sizes에서 1에 2159는 item이 하나만 있는것이 2159건 이라는 의미이다. 


# 지지도(support): 사건이 전체 데이터에서 나타나는 비율 
itemFrequencyPlot(data,support=0.01,main="Item Frequency Plot support 0.01")
itemFrequencyPlot(data,support=0.1,main="Item Frequency Plot support 0.1")

# 기본 support값은 0.1 입니다. 
# support=0.01로 하여서, 연관규칙이 없다는 결과가 나옵니다. 
rules<-apriori(data,parameter = list(support=0.01,confidence=0.8))

# support=0.001로 하면 410개의 연관규칙이 있다는 것을 말해줍니다. 
rules<-apriori(data,parameter = list(support=0.001,confidence=0.8))

# 연관규칙의 대략적인 정보를 보여줍니다. 
summary(rules)


inspect(head(sort(rules,by="confidence"),5))

# "whole milk","butter" 만 골라냅니다. 
interest<-subset(rules,items %in% c("whole milk","butter"))
inspect(interest[1:5])

txt<-readLines("sample_news.txt")
noun<-lapply(txt,getMorph,"noun")
nounVec<-unlist(noun)
nounFreq<-table(nounVec)

keywords<-names(head(sort(nounFreq,decreasing = T),20))
docs<-as.data.frame(txt)
dtm<-makeDTM(docs,key=keywords,TEXT.name="txt",RHINO=T)
dtm.df<-as.data.frame(dtm)


# 출현여부만을 data.frame형식으로 만들어 줍니다. 
dtm.abovemean<-ifelse(dtm>mean(apply(dtm,2,mean)),1,0)
dtm.abovemean


# 데이터가 작으므로 너무 많은 규칙이 만들어지지 않게 기준을 다소 높게 잡습니다. 
# support: 단어가 데이터에서 나타나는 비율
# confidence: 규칙이 맞을 확률 
rules<-apriori(dtm.abovemean,parameter=list(support=0.3,conf=0.8))

# 46개의 규칙이 만들어 졌습니다. 
arules::inspect(sort(rules))

# %pin% 은 %in% 과 비슷하나, partial matching을 수행합니다. 
rules2<-subset(rules,subset=lhs %pin% "경재" & confidence>0.7)
inspect(sort(rules2))

# 중요한 규칙을 중심으로 네트워크 그래프를 그립니다. 
plot(rules,method="graph",nodeCol="blue")

# 연관규칙 레이블을 " "으로 분리합니다. 
# 이후, 연관단어를 행렬구조로 변경하고, 행단위로 묶어 matrix로 변환합니다. 
rules.2<-labels(rules,ruleSep=" ")
rules.3<-sapply(rules.2,strsplit," ",USE.NAMES=F)

# do.call 함수는 리스트rule.3을 함수 rbind를 이용해 다른타입으로 변환합니다. 
rule.mat<-do.call("rbind",rules.3)
rule.mat


rule.g<-graph.edgelist(rule.mat,directed=T)
rule.g

plot.igraph(rule.g)


corrplot(cor(dtm.abovemean))


txt<-readLines("sample_news.txt")
dtm
#install.packages("topicmodels")

# 토픽의 개수를 5개로 합니다. 
lda<-LDA(dtm,k=5)
terms(lda,10)

data.cluster<-kmeans(dtm,centers=3)
data.cluster











