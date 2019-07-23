#20190723 비지도 학습 알고리즘 

setwd("D:/BigDataCampus")

iris
cor(iris$Sepal.Width,iris$Sepal.Length)

autoparts<-read.csv("autoparts.csv",header=TRUE)
autoparts1<-autoparts[autoparts$prod_no=="90784-76001",c(2:11)]
autoparts2<-autoparts1[autoparts1$c_thickness<1000,]

# autoparts2의 두 변수는 -0.995의 값을 보인다. 두 변수는 역관계이다.
cor(autoparts2$separation,autoparts2$s_separation)

# 각 변수마다의 상관관계를 본다. 
x<-cor(autoparts2)
symnum(x)
# 상관관계 0.3이상이면 상관관계 존재, 0.8이상이면 강한 상관관계라고 본다. 

head(iris)
iris2<-iris[,3:4]
head(iris2)

# 정규화 합니다. 
# +-3 이내의 99.7% 안의 값을 갖도록 합니다.
data.scaled<-scale(iris2)

# 군집을 3개로 지정합니다. 
data.cluster<-kmeans(data.scaled,centers=3)
data.cluster

plot(data.scaled,col=data.cluster$cluster)

# 중심점을 추가합니다. 
points(data.cluster$centers,col=1:3,pch=8,cex=2)


autoparts<-read.csv("autoparts.csv",header=T)
autoparts3<-autoparts[autoparts$prod_no=="45231-3B610",c(2:11)]

# 1이 정상이고, 2와 3은 불량이다

autoparts3$flag<-1
autoparts3$flag[autoparts3$c_thickness>32]<-2
autoparts3$flag[autoparts3$c_thickness<20]<-3
table(autoparts3$flag)

data.scaled<-scale(autoparts3[,1:9])
head(data.scaled)

# 군집을 3개로 설정 
data.cluster<-kmeans(data.scaled,centers=3)
data.cluster

plot(data.scaled,col=data.cluster$cluster)
points(data.cluster$centers,col=1:3,pch=8,cex=2)

table(real=autoparts3$flag,pred=data.cluster$cluster)


data.cluster<-kmeans(data.scaled,centers=3)
plot(data.scaled,col=data.cluster$cluster)
points(data.cluster$centers,col=1:3,pch=8,cex=2)
table<-table(real=autoparts3$flag,pred=data.cluster$cluster)

(table[1,1]+table[2,2]+table[3,3])/sum(table)

autoparts<-read.csv("autoparts.csv",header=TRUE)
autoparts4<-autoparts[autoparts$prod_no=="45231-P3B750",-1]
autoparts$flag<-1
autoparts4$flag[autoparts4$c_thickness>32]<-2
autoparts4$flag[autoparts4$c_thickness<20]<-3
table(autoparts4$flag)

data.scaled<-scale(autoparts4[,1:9])
head(data.scaled)

d<-dist(data.scaled)
data.cluster<-hclust(d)
plot(data.cluster)

#-----------------------------------------------------------------------------

autoparts<-read.csv("autoparts.csv",header=TRUE)
autoparts4<-autoparts[autoparts$prod_no=="45231-P3B750",-1]
autoparts$flag<-1
autoparts4$flag[autoparts4$c_thickness>32]<-2
autoparts4$flag[autoparts4$c_thickness<20]<-3

d<-dist(autoparts4[,1:9])
d

fit<-cmdscale(d)
head(fit)
x<-fit[,1]
y<-fit[,2]

plot(x,y,xlab="Coordinate 1",ylab="Coordinate 2",main="MDS-autoparts4",type="n")
text(x,y,labels=autoparts4$flag,cex=0.7,col=autoparts4$flag)

mydata<-read.csv("occupancy_all.csv",header=T,stringsAsFactors = F)
nrow(mydata)
str(mydata)
head(mydata)
summary(mydata)
# Light와 CO2 column은 이상치일 수 있습니다. (예상)

mydata.2<-mydata[,-1]
head(mydata.2)

# boxplot을 통해서 이상치가 Light와 CO2임을 확인하였습니다. 
boxplot(mydata.2)

cor(mydata.2)
# Humidity와 HumidityRatio : 0.9381 
# Occupancy와 Light : 0.9153

mydata2<-mydata.2
mydata2$Occupancy[mydata2$Occupancy==0]<-2
data.scaled<-scale(mydata2[,1:5])
data.cluster<-kmeans(data.scaled,centers=2)
plot(data.scaled)

























