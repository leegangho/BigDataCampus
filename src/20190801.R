#20190801 비정형 데이터 

setwd("D:/BigDataCampus/practiceData/2차 실습파일")
setwd("D:/BigDataCampus/practiceData")

library(rJava)
library(RCurl)
library(rvest)
library(dplyr)
library(xml2)
library(lubridate)
library(KoNLP)
library(RHINO)
library(devtools)
library(stringr)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(wordcloud2)
library(SnowballC)



#install.packages("wordcloud")
#install.packages("Rcpp")


# txt파일의 내용에서 명사만 추출하여 noun 변수에 저장합니다. 
# extractNoun은 KoNLP에 들어있는 함수입니다. 
txt<-readLines("sample_news.txt") 

# readLines으로 읽어왔으므로 한줄씩 명사를 검색합니다. 
noun<-lapply(txt,extractNoun)

# csv 파일을 열고, CONTENTS 열의 명사들만 추출합니다. 
txt<-read.csv("sample_voc.csv",stringsAsFactors = F)
noun<-lapply(txt$CONTENTS,extractNoun)
unlist(noun)


# RHINO는 명사와 동사 추출에 좀 더 성능이 좋아요
initRhino()   # RHINO 사용할때 항상 먼저 initRhino()을 먼저 실행시켜야 합니다. 

txt<-readLines("sample_news.txt")    # 파일을 읽어옵니다.
noun<-lapply(txt,getMorph,"noun")    # 명사만 추출합니다. 
nounVec<-unlist(noun)                # 리스트형태를 벡터형태로 변환합니다.
nounFreq<-table(nounVec)             # 단어의 빈도수를 확인합니다.
head(sort(nounFreq,decreasing=T),20) # 단어의 빈도수를 내림차순으로 정렬하여 출력합니다. 

initRhino()
txt<-readLines("sample_news.txt")
noun<-lapply(txt,getMorph,"noun")
nounVec<-unlist(noun)
nounFreq<-table(nounVec)
word<-names(head(sort(nounFreq,decreasing = T),20))     # 단어만 추출합니다. 
freq<-as.vector(head(sort(nounFreq,decreasing = T),20)) # 빈도만 추출합니다. as.vector()

sum<-sum(nounFreq)
percent<-round(freq/sum*100,digits=2)
mainTxt<-"고빈도 단어"

# las는 글씨를 가로 세로로 지정할 수 있습니다. 
# names.arg는 word로 이름들을 넣겠다는 뜻 입니다. 
bp<-barplot(percent,main=mainTxt,las=2,ylim=c(0,5),ylab="%",names.arg = word,col="black")
text(x=bp,y=percent+0.1,labels=paste(freq),col="black",cex=0.8)

colors<-c()
for ( i in 1:length(percent)){
  if(percent[i] >=4){
    colors<-c(colors,"red")
  }
  else if(percent[i]>=2){
    colors<-c(colors,"yellow")
  }
  else{
    colors<-c(colors,"blue")
  }
}

bp1<-barplot(percent,main=mainTxt,las=2,ylim=c(0,5),ylab="%",names.arg=word,col=colors)




# csv 파일을 열고, CONTENTS 열의 명사들만 추출합니다. 
txt<-read.csv("sample_voc.csv",stringsAsFactors = F)
noun<-lapply(txt$CONTENTS,extractNoun)
nounVec<-unlist(noun)
nounFreq<-table(nounVec)
word<-names(head(sort(nounFreq,decreasing=T),20))
freq<-as.vector(head(sort(nounFreq,decreasing = T),20))
sum<-sum(nounFreq)
percent<-round(freq/sum*100,digits=2)
mainTxt<-"고빈도 단어_voc"
bp<-barplot(percent,main=mainTxt,las=1,ylim=c(0,10),ylab="%",names.arg = word,col=colors)
text(x=bp,y=percent+0.1,labels = paste(freq),col="black",cex=0.8)



#useSejongDic()

# txt파일의 글자가 깨지므로, Encoding(data1) 
data1<-readLines("경주여행_지식인_2016.txt")
Encoding(data1)<-"UTF-8" 

length(data1) # 1321 입니다. 

data1<-unique(data1)
# 한글만 추출하기 위해 정규식 표현을 사용합니다. 
# "[^[:alpha:][:digit:]]"," " 는 특수문자를 제거하는 뜻입니다.  
data2<-str_replace_all(data1,"[^[:alpha:][:digit:]]"," ")

# 추출된 data2에서 명사만 뽑아냅니다. 
data3<-extractNoun(data2)

# data3의 중복된 표현은 없애고, 유니크만 뽑습니다. 
data4<-lapply(data3,unique)

# 벡터형태로 만들고, "\\d+" 는 Digit,0~9->""로 바꿉니다. 즉 삭제합니다.
data5<-gsub("\\d+","",unlist(data4))

# 스프링->스프링돔
data5<-gsub("스프링","스프링돔",data5)
# 파크->워터파크
data5<-gsub("파크","워터파크",data5)

data5<-gsub("\\^","",data5)

# 교촌, 마을, 한옥 -> 교촌한옥마을   로 대체합니다. 
data5<-gsub(paste(c("교촌","마을","한옥"),collapse="|"),"교촌한옥마을",data5)
data5<-gsub(paste(c("주상","절리"),collapse="|"),"주상절리",data5)
data5<-gsub(paste(c("보문단지","보문"),collapse="|"),"보문관광단지",data5)
data5<-gsub(paste(c("달동네","추억","추억의달동네"),collapse="|"),"추억의달동네",data5)
data5<-gsub(paste(c("한우","떡갈비"),collapse='|'),"한우수제떡갈비",data5)
data5<-gsub(paste(c("게스트","하우스"),collapse="|"),"게스트하우스",data5)
data5<-gsub(paste(c("월성","반월성"),collapse='|'),"반월성",data5)
data5<-gsub(paste(c("맛집이","맛집"),collapse='|'),"맛집",data5)
data5<-gsub(paste(c("교리","김밥","계란지단"),collapse="|"),"교리김밥",data5)
data5<-gsub(paste(c("천마","천마총"),collapse="|"),"천마총",data5)
data5<-gsub(paste(c("박물관","테디베어","테디베어박물관"),collapse="|"),"테디베어박물관",data5)
data5<-gsub("월드","월드엑스포",data5)
data5<-gsub("순두부","멧돌순두부",data5)
data5<-gsub(paste(c("현대","밀면"),collapse="|"),"현대밀면",data5)
data5<-gsub("한정","이조한정식",data5)
data5<-gsub("블루","블루원워터파크",data5)

# 다시한번 유니크한 값들만 뽑아냅니다.
data5<-lapply(data5,unique)

# 글자의 개수가 6자리 이하 부터 1자리 이상만 data6에 저장합니다. 
# Filter(function(y){nchar(y) <= 6 & nchar(y)>1},x)}
# 조건에 해당하는 값들을 True, False로 구분하고 True 만 data6에 저장합니다. 
data6<-sapply(data5,function(x){Filter(function(y){nchar(y) <= 6 & nchar(y)>1},x)})

# data6의 빈도수를 만듭니다. 
wordcount<-table(unlist(data6))
# 문자개수 10개 이하만 셉니다.

# 이미 글자는 6자리만 있으므로 wordcount<-Filter(function(x){nchar(x)<=10},wordcount) 코드는 필요없습니다. 
wordcount<-Filter(function(x){nchar(x)<=10},wordcount)
head(sort(wordcount,decreasing=T),100)

txt<-readLines("경주여행_지식인_2016.txt")
cnt_txt<-length(txt)
cnt_txt
for ( i in 1:cnt_txt){
  data5<-gsub("(txt[i])",'',data5)
}
head(data5,5)
data6<-sapply(data5,function(x){Filter(function(y){nchar(y)>=2},x)})
head(data6,5)

wordcount<-table(unlist(data6))
head(sort(wordcount,decreasing=T),100)

#install.packages("RColorBrewer")
#install.packages("tm")

palete<-brewer.pal(7,"Set2")
wordcloud(names(wordcount),freq=wordcount,scale=c(5,1),rot.per=0.25,min.freq=28,random.order = F,random.color=T,colors=palete)

wordcount<-table(unlist(data6))
data54<-head(sort(wordcount,decreasing = T),100)

#write.table(data54,"data54.txt")
data64<-read.table("data54.txt")
col4<-ifelse(data64$Freq>=100,"red","gray")
wordcloud(data64$Var1,freq = data64$Freq,scale=c(4,1),rot.per=0.25,min.freq=1,random.order=F,ordered.color=T,colors=col4)

#install.packages("wordcloud2")

wordcount2<-head(sort(wordcount,decreasing = T),100)

wordcloud2(wordcount2,gridSize = 1,size=0.5,shape="star")


#-------------------------------------------------------------------------------------------------


data('crude')
summary(crude)
inspect(crude[1])

length(crude)
crude[[1]]$content

crude[[1]]$meta

meta(crude[[1]],tag="author")<-'홍길동'
meta(crude[[1]],tag="2nd author")<-'홍길자'
crude[[1]]$meta

crude_lower<-tm_map(crude,tolower)
inspect(crude_lower[1])
crude_lower[[1]]

#install.packages("SnowballC")
crude_stemDocumnet<-tm_map(crude,stemDocument)
crude_stemDocumnet[[1]]$content


#------------------------------------------------------------------------------------------------------

# 연습문제
hiphop <- read.table("hiphop.txt", sep="\n")
hiphop <- readLines("hiphop.txt")


noun <- lapply(hiphop, extractNoun)
unlist(noun)


initRhino()
noun <- lapply(hiphop, getMorph, 'noun')
nounVec <- unlist(noun)

nounFreq <- table(nounVec)
head(sort(nounFreq, decreasing = T), 20)

#단어만 추출
word <- names(head(sort(nounFreq, decreasing = T), 20))

#빈도만 추출
freq <- as.vector(head(sort(nounFreq, decreasing = T), 20))

sum <- sum(nounFreq)
percent <- round(freq/sum*100, digits = 2)
mainTxt <- "고빈도 단어"

bp <- barplot(percent, main = mainTxt, las = 2 , ylim = c(0 , 20), ylab = "%",
              names.arg = word, col="black")
text(x=bp, y=percent+0.1, labels = paste(freq), col="black", cex=0.8)


wordcloud(names(nounFreq),freq=nounFreq,scale=c(5,1),rot.per=0.25,min.freq=28,
          random.order = F,random.color=T,colors=palete)



data1<-unique(noun)
data2<-sapply(data1,function(x){Filter(function(y){nchar(y)<=6 & nchar(y)>1},x)})
data2<-unlist(data2)
data2
nounFreq<-table(data2)
word<-names(head(sort(nounFreq,decreasing = T),20))
freq<-as.vector(head(sort(nounFreq,decreasing = T),20))

sum<-sum(nounFreq)
percent<-round(freq/sum*100,digits=2)
mainTxt<-"고빈도 단어"

wordcloud(names(nounFreq),freq=nounFreq,scale=c(5,1),rot.per=1,min.freq=1,random.order = F,random.color = T,colors=palete)

freq
# 연습문제 끝 
#--------------------------------------------------------------------------------------------------

data('crude')

# crude데이터의 요약(말뭉치를 요약해서 보여줍니다.)
summary(crude)

# 문서의 정보를 보여줍니다. 
inspect(crude[[1]])

# 문서의 내용을 보여줍니다. 
crude[[1]]$content

meta(crude[[1]],tag="author")<-"홍길동"
meta(crude[[1]],tag="2nd author")<-"홍길자"
crude[[1]]$meta

# tm_map을 사용해서 전체 content를 소문자로 변환합니다. 
crude_lower<-tm_map(crude,tolower)
inspect(crude_lower[1])
# 새롭게 crude_lower을 만들었기 때문에 meta 데이터가 없습니다. 
crude_lower[[1]]

# stemDocumnet함수는 단어에서 어근만 남긴다는 뜻 입니다. 
# 어근: 단어에서 중심 의미를 가진 단어만 해당됩니다. ex) 햇과일-> 과일만 추출합니다. 
crude_stemDocumnet<-tm_map(crude,stemDocument)
crude_stemDocumnet[[1]]$content


# 문서에서 줄넘김과 불필요한 공백을 제거합니다. 
crude_stripWhitespace<-tm_map(crude,stripWhitespace)
crude_stripWhitespace[[1]]$content

# 문서에서 문장부호를 제거합니다. 
crude_removePunctuation<-tm_map(crude,removePunctuation)
crude_removePunctuation[[1]]$content


# 숫자를 제거합니다. 
crude_removeNumbers<-tm_map(crude,removeNumbers)
crude_removeNumbers[[1]]$content


# 문서에서 특정 단어를 제거합니다. 
crude_removeWord<-tm_map(crude,removeWords,"Diamond")
crude_removeWord[[1]]$content


# 문서에서 불용어를 제거합니다. 
# stopwords("english")로 하면 기본적으로 it, our, your, my, myself 등등의 단어들은 제거됩니다. 
crude_removeWord<-tm_map(crude,removeWords,stopwords("english"))
crude_removeWord[[1]]$content


# 최대한 문서 안에서 텍스트 전처리를 실시해야 합니다. !!! 
crude_lower<-tm_map(crude,tolower)
crude_clean<-tm_map(crude_lower,PlainTextDocument)
crude_clean[[1]]$content
crude_clean[[1]]$meta

data("crude")

# 문서를 행, 단어를 열로 하는 행렬을 만듭니다. 
dtm<-DocumentTermMatrix(crude)
inspect(dtm)

# 1~10번 문서에 대하여 1~5번 단어만 봅니다. 
inspect(dtm[1:10,1:5])

# weighting=weightTfIdf 로 하면 가중치를 할당 시킬 수 있습니다. 
dtm2<-DocumentTermMatrix(crude,control=list(weighting=weightTfIdf))
inspect(dtm2[1:10,1:5])

dtm<-DocumentTermMatrix(crude)
# 최소 빈도 회수가 10회
# 최대 출현횟수는 highfreq
findFreqTerms(dtm,lowfreq=10)

# 지정된 단어와 상관관계를 갖는 단어를 보여줍니다. 
# 함께 자주 사용되거나, 함께 덜 사용되면 상관관계가 높아집니다. 
f<-findAssocs(dtm,"oil",0.7)
f$oil

class(dtm)
dtm.mx<-as.matrix(dtm)
class(dtm.mx)

dtm.df<-as.data.frame(dtm.mx)
dtm.label.df<-cbind(dtm.df,LAVEL=c(rep("crude",length(crude))))
head(dtm.label.df[,c(1260:1267)])





# 두개의 문서를 각각 함수에 넣은 뒤 합칩니다. 
data("crude"); data("acq")

to_dtm<-function(corpus){
  x<-tm_map(corpus,removePunctuation)
  x<-tm_map(x,removeWords,stopwords())
  return(DocumentTermMatrix(x))
}


crude_acq<-c(to_dtm(crude),to_dtm(acq))
crude_acq_df<-cbind(as.data.frame(as.matrix(crude_acq)),
                    LABEL=c(rep("crude",length(crude)),rep("acq",length(acq))))
head(crude_acq_df[,c(1:5,length(crude_acq_df))])













































