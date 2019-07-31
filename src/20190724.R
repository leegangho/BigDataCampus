#20190724 

install.packages("arulesViz")
library("arulesViz")
data("Groceries")
head(Groceries)
summary(Groceries)

rules<-apriori(Groceries,parameter=list(support=0.001,confidence=0.5))
rules
inspect(head(sort(rules,by="lift"),3))

plot(rules)
subrules2<-head(sort(rules,by="lift"),10)
plot(subrules2,method="graph",control=list(type="items"))

#install.packages("arules")
library("arules")
data(Adult)
Adult.df<-as(Adult,'data.frame')
Adult.df
rules.adult<-apriori(Adult)

rules.adult1<-apriori(Adult,parameter=list(support=0.1,confidence=0.6),appearance=list(rhs=c('income=small','income=large'),default='lhs'),control = list(verbose=F))
rules.adult1.sorted<-sort(rules.adult1,by="lift")

inspect(head(rules.adult1.sorted))
plot(rules.adult1.sorted,metod="scatterplot")
plot(rules.adult1.sorted,method="graph",control=list(type='items',alpha=0.5))

data(Titanic)
titan.df<-as.data.frame(Titanic)
head(titan.df)

summary(titan.df)
titanic<-NULL
for(i in 1:4){
  titanic<-cbind(titanic,rep(as.character(titan.df[,i]),titan.df$Freq))
}
titanic<-as.data.frame(titanic)
names(titanic)<-names(titan.df)[1:4]
summary(titanic)

rules.all<-apriori(titanic)
options(digits=3)
inspect(rules.all)

rules<-apriori(titanic,control=list(verbose=F),parameter=list(minlen=2,supp=0.005,conf=0.8),appearance = list(rhs=c("Survived=No","Survived=Yes"),default="lhs"))
rules.sorted<-sort(rules,by="lift")
inspect(rules.sorted)

subset.matrix<-is.subset(rules.sorted,rules.sorted)
subset.matrix[lower.tri(subset.matrix , diag=T)]<-NA
redundant<-colSums(subset.matrix,na.rm=T)>=1
which(redundant)




subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix
subset.matrix[lower.tri(subset.matrix, diag=T)] <- FALSE
redundant <- colSums(subset.matrix, na.rm=T) >=1
which(redundant)

plot(rules.all,measure="support",shading="order")
plot(rules.all,method="grouped")

plot(rules.sorted,method="graph",control=list(type="items"))
plot(rules.sorted,method="paracoord",control=list(reorder=TRUE))


#plot(rules.sorted,measure=c("support","lift"),shading="confidence",interactive = TRUE)

plot(rules.sorted,method="matrix",measure="lift")
plot(rules.sorted,method="matrix",measure=c("lift","confidence"),control = list(reorder=TRUE))
plot(rules.sorted,method="matirx3D",measure = "lift",control=list(reorder(TRUE)))





library("arules")
data(Adult)
Adult.df<-as(Adult,'data.frame')
Adult.df
rules.adult<-apriori(Adult)

rules.adult1<-apriori(Adult,parameter=list(support=0.7,confidence=0.8))
rules.adult1.sorted<-sort(rules.adult1,by="lift")

inspect(head(rules.adult1.sorted))
plot(rules.adult1.sorted,metod="scatterplot")
plot(rules.adult1.sorted,method="graph",control=list(type='items',alpha=0.5))

setwd("D:/BigDataCampus")
# data<-read.csv("mybasket.csv",sep=",",header=F)
# rules.data<-as(data,'data.frame')
# 
# summary(rules.data1)
# rules.data1<-apriori(data,parameter = list(support=0.1,confidence=0.0))
# # 42개 유형의 아이템과 956트랜잭션을 갖습니다. 
#  
# nrow(rules.data)
# # 13가지 가지 수를 갖습니다.
# inspect(rules.data1)
# itemFrequency(data)



mybasket.trans<-read.transactions("mybasket.csv",format="basket",sep=",")
summary(mybasket.trans)
itemFrequency(mybasket.trans)
itemFrequencyPlot(mybasket.trans)

mybasket.rules<-apriori(mybasket.trans,parameter=list(support=0.1,confidence=0.0))
summary(mybasket.rules)

sort(itemFrequency(mybasket.trans),decreasing = TRUE)
inspect(subset(mybasket.rules,subset=lhs %in% "clothes"& rhs %in% "snack"))

inspect(subset(mybasket.rules,subst=lhs%in% "clothes" & lhs %in% "snack"))










