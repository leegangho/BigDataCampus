# 20190730 비정형 데이터 크롤러를 이용합니다. 트위터

if(!require(twitteR)){install.packages("twitteR"); library(twitteR)}

api_key<-"YsjuH3M5coC3wDsVoGSwjTUgN"
api_secret<-"Gz0KSF9SbE8R1eo1Sd7lLOZYBrmqxPXohEFWlNRtUMtQ1qOwnI"
access_token<-"1133607038833967104-SXWxQ4Zez439jRmyTp0Llu0QGymZ0v"
access_secret<-"tsqwgB4sMPIQTZnMLLEzhE6eJ7dm09a2Ml4yuUnVZocxi"

setup_twitter_oauth(api_key,api_secret,access_token,access_secret)

keyword<-enc2utf8("JYP")
tweets<-searchTwitter(keyword,n=150,lang = "ko")

# 수집한 트위터 개수 확인 
length(tweets)

temp<-tweets[[1]]
temp$getScreenName()
temp$getText()

if(!require(plyr)){install.packages("plyr"); library(plyr)}

# laply함수는 저장을  리스트->데이터프레임
tweets.text<-laply(tweets,function(t) t$getText())
length(tweets.text)
head(tweets.text)
tweets.text

setwd("D:/BigDataCampus")
write(tweets.text,file="tweets.txt")


#install.packages("rvest")

library(rvest)
all.reviews<-c()
#url_base<-"https://movie.daum.net/moviedb/grade?movieId=119835&type=netizen&page=2"
url_base<-"https://movie.naver.com/movie/point/af/list.nhn?&page=1"

for(page in 1:2){
  url<-paste(url_base,page,sep='')
  htxt<-read_html(url)
  table<-html_nodes(htxt,'.list_netizen')
  content<-html_nodes(table,'.title')
  reviews<-html_text(content)
  
  if(length(reviews)==0){break}

  reviews<-gsub("\r","",reviews)
  reviews<-gsub("\t","",reviews)
  all.reviews<-c(all.reviews,reviews)
  print(page)
  Sys.sleep(0.7)
}

write.table(all.reviews,'naver_movie.txt')
View(all.reviews)


#install.packages("RCurl")
library(RCurl)

url<-"http://news.chosun.com/site/data/html_dir/2018/05/23/2018052300694.html"
page<-getURL(url,encoding="euc-kr")
download.file(url,"D:/BigDataCampus/Rcrawl/page.txt")



