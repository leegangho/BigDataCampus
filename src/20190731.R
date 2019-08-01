#20190731 텍스트 데이터 전처리 

#install.packages("rJava")

library(rJava)
library(RCurl)
library(rvest)
library(dplyr)
library(xml2)
library(lubridate)
library(KoNLP)
library(RHINO)
library(rJava)




# subtitle을 추출하는 코드 입니다. 
url<-"http://news.chosun.com/site/data/html_dir/2019/07/30/2019073002747.html"
news<-read_html(url,encoding="UTF-8")
news1<-html_nodes(news,"h3.news_subtitle")
news2<-html_text(news1,"news_subtitle")

# news1과 news2를 합친 코드 입니다. 
news3<-news %>% html_node("h3.news_subtitle") %>% html_text()


page<-read_html(url,encoding='UTF-8')
# html_nodes 로 하면 .par의 모든 내용을 가져옵니다. 
partMain<-page %>% html_nodes(".par") %>% html_text()


# Tag 안에 Tag로 하여 내용이 있는 경우 html_nodes(".author a")로 표현한다.
writers<-page %>% html_nodes(".author a") %>% html_text()


# id=harmonyContainer 로 본문이 지정되어있을경우에  html_node("#harmonyContainer")로 사용한다. 
url<-"http://v.media.daum.net/v/20180523174204909?rcmd=rn"
page<-read_html(url,encoding = "UTF-8")
main<-page %>% html_node("#harmonyContainer") %>% html_text()

#---------------------------------------------------------------------------


# RCurl로 웹페이지 수집 
url<-"http://www.inu.ac.kr/mbshome/mbs/inu/index.do"
page<-getURL(url,encoding="euc-kr")
download.file(url,"D:/BigDataCampus/incheon.txt")

# rvet로 기사 본문만 추출 및 저장 

url<-"https://news.naver.com/main/read.nhn?mode=LSD&mid=shm&sid1=105&oid=277&aid=0004510528"
page<-read_html(url,encoding="euc-kr")
title<-page %>% html_node("title") %>% html_text()
main<-page %>% html_nodes("#articleBodyContents") %>% html_text()
write(main,file="Apple_news.txt")

#----------------------------------------------------------------------------------------------------------



url="https://news.naver.com/main/ranking/popularDay.nhn?rankingType=popular_day&date=20190728"
news=read_html(url)

# 카테고리 명칭 작성합니다.
secion.names=c("Politics","Economy","Society","Life","World","IT")

#헤더라인 5개를 각 카테로기 별로 입력하는 데이터 프레임을 만듭니다. 
news.head=data.frame(head_1=c(1:6),head_2=NA,head_3=NA,head_4=NA,head_5=NA)

# row name을 카테고리 명칭으로 변경(추출하는 csv에서 첫 번째 열에 나타나도록 합니다.)
rownames(news.head)=secion.names


for(i in 1:ncol(news.head)){ # 현재 6행이므로 6번 반복
  findNum=paste0(".num",i)  # 헤드라인마다 번호가 다르므로 번호를 바꿀 수 있도록 합니다. 
  nodeValue=html_nodes(news,findNum) #class가 6개 이므로 node 6개여야 합니다. 
  
  for(j in 1:nrow(news.head)){  # 정치~it 까지의 카테고리에 대한 for 문
    # gsub()로 데이터표현을 바꿉니다. "\t" -> "" 로 바꿉니다.  
    extracted.head=gsub("\t","",nodeValue[j]%>%html_text())%>%strsplit("\n")
    trim.head=trimws(extracted.head[[1]])   # 글자 앞뒤의 빈공간을 엾애줍니다. 
    selected.head=trim.head[-which(trim.head=="")]   # 공란으로 된 character를 제외한 나머지를 찾습니다.
    if(selected.head[1]=="동영상기사"){  # 가끔 동영상 기사의 경우, 헤드보다 앞에 표시가 되도록 합니다. 
      news.head[j,i]=selected.head[2]
    }
    else{
      news.head[j,i]=selected.head[1]
    }
  }
}

write.csv(news.head,"popularNewsHeadlines.csv",row.names=T)
news.head


setDate<-ymd("2019-07-30")-days(0:365)
setDate<-format(setDate,"%Y%m%d")

for(d in 1:length(setDate)){
  urlNews<-paste0("https://news.naver.com/main/ranking/popularDay.nhn?rankingType=popular_day&date=",setDate[d])
  newsData<-read_html(urlNews)
  
  sectionName<-c("Policy","Economy","Society","Life","World","It")
  newsHead=data.frame(head_1=c(1:6),head_2=NA,head_3=NA,head_4=NA,head_5=NA)
  rownames(newsHead)<-sectionName
  
  for(i in 1:(ncol(newsHead))){
    findNum<-paste0(".num",i)
    nodeValue<-html_nodes(newsData,findNum)
    
    for(j in 1:nrow(newsHead)){
      extractHead<-gsub("\t","",nodeValue[j] %>% html_text()) %>% strsplit("\n")
      trimHead<-trimws(extractHead[[1]])
      selectHead<-trimHead[-which(trimHead=="")]
      if(selectHead[1]=="동영상기사"){
        newsHead[j,i]<-selectHead[2]
      }
      else{
        newsHead[j,i]<-selectHead[1]
      }
    }
  }
  write.csv(newsHead,paste0("poopularNewsHeadlines(",setDate[d],").csv"))
}

#---------------------------------------------------------------------------------------------

# 네이버 금융

url<-"https://finance.naver.com/item/board.nhn?code=005930&page=1"
page<-read_html(url,encoding='euc-kr')
page2<-html_nodes(page,'td.title a')
page3<-html_attr(page2,'title')
page4<-as.data.frame(page3)
page4

page5<-list()
for(i in 1:10){
  url<-paste("https://finance.naver.com/item/board.nhn?code=005930&page=",i,sep='')
  page5[[i]]<-read_html(url,encoding='euc-kr') %>% html_nodes("td.title a") %>% html_attr('title')
}

write.csv(unlist(page5),'ddt.csv')
page5


#-----------------------------------------------------------------------------------------------

#install.packages("KoNLP")
#install.packages("rJava")


setwd("D:/BigDataCampus/practiceData")
txt<-readLines("sample.txt")


# txt<-readLines()이므로 한줄씩 읽는다. 
# lapply에서 한 문장씩 형태소를 분석해서 noun에 저장합니다. 
noun<-lapply(txt,extractNoun)
noun<-unlist(noun)
noun

install.packages("devtools")
initRhino()
noun2<-lapply(txt,getMorph,"noun")
noun2<-unlist(noun2)
noun2


