#20190731 텍스트 데이터 전처리 

#install.packages("rJava")

library(RCurl)
library(rvest)
library(dplyr)


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


title
main
