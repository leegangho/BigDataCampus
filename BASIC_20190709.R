
setwd("D:/BigDataCampus")
setwd("D:/BigDataCampus")

#txt파일 읽어오기

student_list=read.table("studentlist.txt",header=T,sep=",",stringsAsFactors = F)
student_list

#excel의 csv 파일 읽어오기 

student_list2=read.csv("example_studentlist.csv",header=T,sep=",",stringsAsFactors=F)
student_list2

#write.table(student_list,file="student_list_n.txt",row.names=F,col.names = T)
#write.csv(student_list,file="student_list_n.csv",row.names=F)

myNum=sample(x=1:100,size=100,replace=TRUE)
myNum
mean(myNum)

#myNum 인덱스를 무작위로 탐색하고, 해당 위치에 NA 값 대입
myNum[sample(x=1:100,size=20,replace=FALSE)]=NA
myNum
mean(myNum)

mean(myNum,na.rm=TRUE)
sum(myNum,na.rm=TRUE)
var(myNum,na.rm=TRUE)
sd(myNum,na.rm=TRUE)

sale=c(95,72,87,65)
weights=c(0.5,0.25,0.125,0.125)
mean(sale)
weighted.mean(sale,weights)

A_score=c(4.0,3.0)
B_score=c(3.0,4.0)
count=c(3,2)
weight=count/sum(count)
weighted.mean(A_score,weight)
weighted.mean(B_score,weight)


time=c(7,2,3,7,6,9,10,8,9,9,10)
mean(time)
median(time)

num_v=c(1,2,2,3,4,3,5,5,7,9,2,2,0)
char_v=c("o","it","the","it","it","가","가","가","가","가")
freq=table(num_v)
freq
which.max(freq)
names(freq)[3]

freq=table(char_v)
freq
which.max(freq)
names(freq)[4]


#which는 index를 반환한다. 
x=c("a","b","c","c","c","d","d")
table(x)
x=c(1,2,3,NA,4)
which.max(x)
which.min(x)

cub1=c(1,2,3,4,5,6)
cub_average1=mean(cub1)
cub_average1
sd(cub1)

cub2=c(sample(x=1:6,size=10,replace=TRUE))
cub2
#cub_average2=sum(cub2)/10
cub_average2=mean(cub2)
sd(cub2)

cub3=c(sample(x=1:6,size=1000000,replace=TRUE))
cub_average3=mean(cub3)
cub_average3
sd(cub3)

titanic_practice=read.csv("titanic.csv",header=TRUE,sep="\t",stringsAsFactors = T)
titanic_practice
x=titanic_practice[,"Age"]
#나이평균
mean(x,na.rm=TRUE)
table(titanic_practice[,"Age"])









x=c(6.5,4.0,7.1,8.3,5.4,7.6,9.0,15.7,16.7,6.4,5.0,8.5,5.7,7.7,7.2,12.4,7.1,5.5,9.7,4.4,7.0,6.3,8.3,6.9,5.7,7.6,7.9,7.9,6.0,8.2,10.4,9.9,3.9,9.8,8.2,5.6,7.9,6.4,7.4,7.0,13.0,8.7,6.4,6.7,7.4)
x
x_num=length(x)
x_num
x_num^(1/3)
round(x_num^(1/3))

(max(x)-min(x))/4
round((max(x)-min(x))/4)

min(x)
max(x)
x_cut=cut(x,breaks=seq(3.0,18.0,by=3.0),right=FALSE)
table(x_cut)
