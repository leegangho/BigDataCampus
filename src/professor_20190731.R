### 뉴스 기사 크롤링 일반 ###

##R에서 rvest 함수를 통해 html로 생성된 웹 페이지를 스크래핑하는 방법

getwd()
# https://news.naver.com/main/ranking/popularDay.nhn?rankingType=popular_day&date=20190728
# https://news.naver.com/main/list.nhn?mode=LPOD&mid=sec&oid=020&date=20190729

install.packages("rvest")
library(rvest)
library(xml2)

urlNews <- "https://news.naver.com/main/ranking/popularDay.nhn?rankingType=popular_day&date=20190728"   # 웹 스크래핑을 할 url을 입력합니다.
newsData <- read_html(urlNews)   # 입력된 url에서 html을 읽어옵니다.

# 카테고리 명칭 작성
sectionName <- c("Policy","Economy","Society","Life","World","IT")  

# 헤드라인 5개를 각 카테고리 별로 입력하는 데이터 프레임을 만듭니다.
newsHead <- data.frame(head_1=c(1:6), head_2=NA, head_3=NA, head_4=NA, head_5=NA)

# row name을 카테고리 명칭으로 변경 (추출하는 csv에서 첫 번째 열에 나타납니다.)
rownames(newsHead) <- sectionName   

for(i in 1:(ncol(newsHead))){   # 1~5면의 헤드에 대한 for문입니다.
  findNum <- paste0('.num',i)   # 헤드라인 마다 번호가 다르므로 번호를 바꿀 수 있도록 합니다. 
  nodeValue <- html_nodes(newsData, findNum)   # i가 1일 경우, num1에 해당하는 노드를 찾아냅니다. 
  
  for(j in 1:nrow(newsHead)){   # 정치 ~ IT/과학까지의 카테고리에 대한 for문입니다.
    extractHead <- gsub("\t","",nodeValue[j] %>% html_text()) %>% strsplit("\n")  # j 번째 노드에서 text를 추출한 후에 탭(\t)을 제거한 후 \n으로 구분합니다.
    trimHead <- trimws(extractHead[[1]]) # 글자 앞뒤의 빈 공간을 없애줍니다.
    selectHead <- trimHead[-which(trimHead=="")]   # 공란으로 된 character를 제외한 나머지를 추출합니다.
    if(selectHead[1]=="동영상기사"){ # 가끔 동영상 기사의 경우, 헤드 보다 앞에 표시가 되어 구분하여 헤드를 추출합니다.
      newsHead[j,i] <- selectHead[2]
    }else{
      newsHead[j,i] <- selectHead[1]  
    }
  }
}

write.csv(newsHead,"popularNewsHeadlines.csv")



### 1년치 랭킹 뉴스 

install.packages("lubridate") # 날짜를 다루는 패키지
library(lubridate)


setDate <- ymd("2019-07-30") - days(0:365)   # 2019-07-30로부터 1년 동안의 날짜를 모두 나타냅니다.
setDate <- format(setDate,"%Y%m%d")   #yyyy-mm-dd의 구조를 url에 적용할 수 있도록 yyyymmdd의 형태로 바꿔줍니다.



for (d in 1:length(setDate)) {
  urlNews <- paste0("https://news.naver.com/main/ranking/popularDay.nhn?rankingType=popular_day&date=",setDate[d])   # 웹 스크래핑을 할 url을 입력합니다.
  newsData <- read_html(urlNews)   # 입력된 url에서 html을 읽어옵니다.
  
  sectionName <- c("Policy","Economy","Society","Life","World","IT")   # 카테고리 명칭 작성
  newsHead <- data.frame(head_1=c(1:6), head_2=NA, head_3=NA, head_4=NA, head_5=NA)   # 헤드라인 5개를 각 카테고리 별로 입력하는 데이터 프레임을 만듭니다.
  rownames(newsHead) <- sectionName   # row name을 카테고리 명칭으로 변경 (추출하는 csv에서 첫 번째 열에 나타납니다.)
  
  for(i in 1:(ncol(newsHead))){   # 1~5면의 헤드에 대한 for문입니다.
    findNum <- paste0('.num',i)   # 헤드라인 마다 번호가 다르므로 번호를 바꿀 수 있도록 합니다. 
    nodeValue <- html_nodes(newsData, findNum)   # i가 1일 경우, num1에 해당하는 노드를 찾아냅니다. 
    
    for(j in 1:nrow(newsHead)){   # 정치 ~ IT/과학까지의 카테고리에 대한 for문입니다.
      extractHead <- gsub("\t","",nodeValue[j] %>% html_text()) %>% strsplit("\n")  # j 번째 노드에서 text를 추출한 후에 탭(\t)을 제거한 후 \n으로 구분합니다.
      trimHead <- trimws(extractHead[[1]]) # 글자 앞뒤의 빈 공간을 없애줍니다.
      selectHead <- trimHead[-which(trimHead=="")]   # 공란으로 된 character를 제외한 나머지를 추출합니다.
      if(selectHead[1]=="동영상기사"){ # 가끔 동영상 기사의 경우, 헤드 보다 앞에 표시가 되어 구분하여 헤드를 추출합니다.
        newsHead[j,i] <- selectHead[2]
      }else{
        newsHead[j,i] <- selectHead[1]  
      }
    }
  }
  write.csv(newsHead,paste0("popularNewsHeadlines(", setDate[d], ").csv"))   # 날짜별로 csv 파일로 저장합니다. 디렉토리 설정을 안하면 문서 폴더 내에 저장됩니다.
}



### 네이버 금융 '삼성전자' 종목토론실 글 가져오기 

# https://finance.naver.com/item/board.nhn?code=005930&page=2


library(rvest)

url <- 'https://finance.naver.com/item/board.nhn?code=005930&page=1'

page <- read_html(url, encoding = 'euc-kr')

page2 <- html_nodes(page, 'td.title a')
page3 <- html_attr(page2, "title")

page4 <- as.data.frame(page3)
page4

# for문 사용하여 다음 페이지까지 긁어오기

page5 <- list()
for(i in 1:10){
  url <- paste('https://finance.naver.com/item/board.nhn?code=005930&page=', i, sep='')
  page5[[i]] <- read_html(url, encoding = 'euc-kr')%>% html_nodes('td.title a')%>% html_attr('title')
  
}

write.csv(unlist(page5),"ddt.csv")



### rvest html제거

url <- 'http://news.chosun.com/site/data/html_dir/2019/07/30/2019073000052.html'

news <- read_html(url, encoding = "UTF-8")
news1 <- html_nodes(news, "h3.news_subtitle")  
news2 <- html_text(news1, "news_subtitle")
news2

news3 <-news %>% html_node("h3.news_subtitle") %>% html_text()
news3

# 본문 추출 

page <- read_html(url, encoding = "UTF-8")
partMain <- page %>% html_node(".par")%>% html_text()
Encoding(partMain) <- "UTF-8"
partMain

# 태그의 태그 추출

url <- 'http://news.chosun.com/site/data/html_dir/2019/07/30/2019073000052.html'

page <- read_html(url, encoding = "UTF-8")
writers <- page %>% html_nodes(".author a") %>% html_text()
writers 


# 본문 추출 - 아이디 검색

url <- "https://news.v.daum.net/v/20180523174204909?rcmd=rn"

page <- read_html(url, encoding = "UTF-8")
main <- page %>% html_node("#harmonyContainer") %>% html_text()
main

setwd("C:/rtest")
getwd()



