#20190729 기본함수와 정규표현식 

library(dplyr)
library(stringr)

setwd("D:/BigDataCampus")

# 영어를 찾습니다. 
LETTERS[3]
letters[10]

# 대문자, 소문자로 바꿔줍니다. 
tolower("KOREA")
toupper("korea")

# 문자의 수와 바이트의 수를 세어줍니다. 

nchar("Korea")   # 결과값 5
length("Korea")  # 결과값 1 

nchar('한국',type='bytes')

# start와 stop으로 필요 구간을 가져옵니다. 
substr("BigDataAnalysis",start=4,stop=7)
substr("BigDataAnalysis",4,7)

country<-c("Korea","Japan","China","Singapore","Russia")
substr(country,start=1,stop=3)

myword<-"This is the Big Data Analysis"
x<-strsplit(myword,split=" ")

mywords<-strsplit(myword,split=' ')
strsplit(mywords[[1]][6],split='')

myword1<-"This is the Big Data Analysis 1"
myword2<-"This is the Big Data Analysis 2"
myword3<-"This is the Big Data Analysis 3"
myword<-c(myword1,myword2,myword3)
for (i in myword){
  result<-strsplit(myword,split=" ")
  #result
}

#result  # 결과 출력 

myword<-"우리의 소원은 통일입니다. 꿈에도 소원은 통일입니다."
strsplit(myword,split="\\.")

number<-1:10
alphabet<-c("a","b","c")
paste(number,alphabet)
paste(number,alphabet,sep="")
paste(number,alphabet,sep="-")

paste(number,collapse="")
paste(alphabet,collapse="")
for(i in 1:length(result)){
  print(paste(result[[i]],collapse = " "))
}

myword<-"우리의 소원은 통일입니다.꿈에도 소원은 통일입니다."
x<-regexpr("입니다",myword)

myword<-"우리의 소언은 통일입니다.꿈에도 소원은 통일입니다."
x<-regexpr('입니다',myword)

# myword에서 시작위치: x[1] , 끝위치: 시작위치+글자수-1
substr(myword,x[1],x[1]+attr(x,'match.length')-1)


myword<-"우리의 소원은 통일입니다. 꿈에도 소원은 통일입니다."
x<-gregexpr('입니다',myword)


myword<-c("소원은 통일입니다. 사랑입니다.","꿈엔 통일입니다. 그러한 것입니다.")
x<-gregexpr('입니다',myword)

# 리스트의 원소의 INDEX를 알려줍니다. grep()
# grep('우리',myword,value=T) 라고 하면 검색 단어의 전체 문장을 출력합니다.
myword<-c("우리의 소원은 통일입니다.","꿈에도 소원은 통일입니다.")
grep('입니다',myword)
grep('우리',myword)
grep('우리',myword,value=TRUE)


#grepl()은 표현이 나타나는지를 논리값으로 알려줍니다. 
myword<-c("우리의 소원은 통일입니다.","꿈에도 소원은 통일입니다.")
grepl('입니다',myword) # TRUE, TRUE 

myword<-c("소원은 통일입니다. 사랑입니다.","꿈에도 통일입니다. 그것입니다.")
# 입니다->일까요 로 변환하지만 한 문장의 첫번째 등장하는 표현만을 바꿉니다. 
sub('입니다','일까요',myword)

# 입니다->일까요 로 변환하는데 전부 바꿉니다. 
gsub('입니다','일까요',myword)
gsub('입니다',' ',myword)


# 검색된 표현을 출력해줍니다. rematches()
myword<-c("소원은 통일입니다.사랑입니다.","꿈에도 통일입니다.그것입니다.")
mypattern<-gregexpr('입니다',myword)
regmatches(myword,mypattern)


myword<-c('1번째는 사랑입니다.','2번째는 우정입니다. 3번째는 같이')
mypattern<-gregexpr("[[:digit:]]",mypattern)
regmatches(myword,mypattern)

myword<-c("제1번째는 통일입니다.","제2번째는 가족입니다.")
mypattern<-gregexpr("(제)[[:digit:]](번째)",myword)
regmatches(myword,mypattern)

# (니다) 라는 앞의 문자가 1회 이상 나타나는 것을 추출합니다. 
myword<-c("1번째는 통일입니다.","2번째는 가족입니다.")
mypattern<-gregexpr("[[:alpha:]]+(니다)",myword)
regmatches(myword,mypattern)
#mypattern<-gregexpr("[[:alpha:]]{1,}(니다)",myword) 와 동일한 결과값입니다. 

myword<-c("1번째는 통일입니다.","2번째는 가족입니다.","3번째는 옵니다만")
mypattern<-gregexpr("[[:alpha:]]{1,}(니다)",myword)
regmatches(myword,mypattern)

# (니다) 이후에 문자가 붙어있으면 제외합니다. \\b
mypattern<-gregexpr("[[:alpha:]]{1,}(니다)\\b",myword)
regmatches(myword,mypattern)

myword<-c("1번째는 통일입니다.","2번째는 가족입니다.","3번째도 통일입니다.")
mypattern<-gregexpr("[[:alpha:]]{1,}(니다)",myword)
x<-regmatches(myword,mypattern)
table(unlist(x))



grep_ex<-c("a.txt","A.txt","ab.txt","123.txt","ba123.txt")

# a로 시작하는 문장을 찾아 index를 반환합니다. 
# value=TRUE 를 사용하면 결과값을 출력합니다.
grep("^a",grep_ex)
grep("^a",grep_ex,value=TRUE)


# 숫자로 시작하는 문장들은 제외하고, 모든 문자를 출력한다. 
grep("^[^0-9]",grep_ex,value=TRUE)

#------------------------------------------------------------------


reg_ex<-read.table("reg_ex.txt",sep = "\t")
head(reg_ex$V1)
reg_1<-grep("^ORA",reg_ex$V1,value=T)
head(reg_1,n=3)
reg_1

reg_2<-substr(reg_1,start=5,stop=9)
reg_2<-grep("^[0-9]",reg_2,value=TRUE)
reg_2

reg_2<-gregexpr("(^ORA-)[[:digit:]]{5}",reg_1)
reg_2<-regmatches(reg_1,reg_2)
reg_2<-unlist(reg_2)
str(reg_2)


sum(table(grep("ORA",reg_ex$V1)))

#------------------------------------------------------------------

#install.packages("stringr")

myword<-c("1번째는 통일입니다.","2번쨰는 가족입니다.")

# str_extract(text,key) 
# simplify=TRUE 옵션을 사용하면 결과를 리스트가 아닌 행렬로 출력합니다.
str_extract_all(myword,"입니다")

myword<-c("1번째는 통일입니다.1-2번째는 사랑입니다.","2번쨰는 가족입니다.")
# simplify=TRUE 옵션을 사용하면 결과를 리스트가 아닌 행렬로 출력합니다.
str_extract_all(myword,"[[:alpha:]]{1,}(니다)",simplify=TRUE)

myword<-c("1번째는 통일입니다.1-2번째는요 사랑입니다.","2번째는 가족입니다.")
str_locate(myword,"입니다")
str_locate_all(myword,'입니다')


myword<-c("1번째는 통일입니다.1-2번째는요 사랑입니다.","2번쨰는 가족입니다.")
str_detect(myword,'입니다')

str_replace(myword,"입니다","일까요")
str_replace_all(myword,"입니다","일까요")


# 마침표(구두점) 으로 텍스트를 자릅니다. 
myword<-c("1번째는 통일입니다.1-2번째는요 사랑입니다.","2번째는 가족입니다.")
str_split(myword,"\\.")

myword<-c("1번째는 통일입니다. 1-2번째는요 사랑입니다.","2번쨰는 가족입니다.")

# str_count는 해당 문장에서 key값이 몇번 나타났는지 빈도수를 알려줍니다. 
str_count(myword,"입니다")

# str_sub는 start:1 과 stop:4 지점의 문자열을 추출합니다. 
str_sub(myword,1,4)

str_length(myword)

number<-1:3
alphabet<-c("a","b","c")


# str_c함수는 벡터를 연결합니다. 
# 각각의 문장을 합칩니다. sep
str_c(number,alphabet,sep="")

# 분리된 분장을 하나의 문장으로 합칩니다. collapse
str_c(number,alphabet,collapse=".")

#---------------------------------------------------------------------------

#20190729 과제 
R.wiki<-readLines("R_wiki.txt")

upper.word<-str_extract_all(R.wiki,"([A-Z])[[:alpha:]]*")
upper.word<-unlist(upper.word)
upper.word


word2<-str_split(R.wiki," ")
sum(table(word2[[1]]))
sum(table(word2[[2]]))

str_locate_all(R.wiki,"([A-Z])[[:alpha:]]*")


# #1번문제  혁진이 코드
# 
# x<-str_extract_all(R_wiki,"[A-Z]{1,}[:alpha:]{0,}")
# 
# #2번문제
# 
# table(unlist(x))
# 
# x<-str_extract_all(R_wiki,"[A-Z]{1,}[:alpha:]{0,}")
# 
# #3번문제
# 
# y<-str_locate_all(R_wiki,"[A-Z]{1,}[:alpha:]{0,}")
# 
# y
# 
# 
# 
# y<-do.call("rbind",y)
# 
# x1<-data.frame(x[[1]])              
# 
# x2<-data.frame(x[[2]])
# 
# colnames(x1)<-c("x")
# 
# colnames(x2)<-c("x")
# 
# x<-rbind(x1,x2)
# 
# 
# 
# x<-cbind(x,y)
# 
# 
# 
# colnames(x)<-c("단어명","시작위치","종료위치")
# 
# x$문자수<-x$종료위치-x$시작위치+1

