##         ##
##연습 문제##
##         ##


#
install.packages("nycflights13")
library(nycflights13)
data(package = "nycflights13")

airlines
airports
flights
planes
weather


#행의 개수는?
dim(flights)
str(flights)
nrow(flights)
flights

library(dplyr)
glimpse(flights)

#변수 명
names(flights)
colnames(flights)

#flights에서 1월 1일 데이터 개수
flights %>% 
  filter(month==1, day==1) %>% 
  summarise(n())

flights %>% 
  filter(month==1 & day==1) #답


# 도착 지연시간 120분 미만
flights %>% 
  filter(arr_delay<120) %>% 
  summarise(n())


#출발 시간과 도착 시간이 모두 1시간 이상 지연된 항공편
flights %>% 
  filter(arr_delay>=60&dep_delay>=60) %>% 
  summarise(n())


flights_new <- flights %>% 
  filter(arr_delay>=60&dep_delay>=60) %>% 
  select(flight, origin,dest,
         arr_delay,dep_delay,
         distance,air_time)
glimpse(flights_new)

#flights_new에서 도착지연 오름차순, 출발지연 내림차순
flights_new %>% arrange(arr_delay,desc(dep_delay))

#gain 추가(도착지연시간-출발지연시간),SPEED(distance/air_time) 
attach(flights_new)
flights_new$gain <- arr_delay - dep_delay
flights_new$speed <- distance/air_time
flights_new


flights_new$gain <- flights_new$arr_delay - flights_new$dep_delay
flights_new$speed <- flights_new$distance/flights_new$air_time
flights_new

##답
flights_new %>% 
  mutate(gain = arr_delay-dep_delay,
         speed = distance/air_time)
flights_new


# 출발지연시간 평균과 표준편차
flights <- na.omit(flights) #(NA값 제거)

flights

flights %>% 
  summarise(delay_mean = mean(dep_delay, na.rm = T), 
            delay_sd = sd(dep_delay, na.rm = T))

#월별 평균 출발 시간
flights %>% 
  group_by(month) %>% 
  summarise(평균출발시간=mean(dep_time, na.rm = T))




## R교육 3일차 : 크롤링(Crawling)


install.packages("rvest")
#install.packages("stringr")
library(rvest)
library(stringr)

##'국회' 검색해서 
## 뉴스 10페이지까지 뉴스제목과 요약내용 긁어오기

title = c()
body = c() #그릇

url_base = "https://search.daum.net/search?w=news&q=%EA%B5%AD%ED%9A%8C&DA=PGD&spacing=0&p="

# for문 반복 i 자리는 변수 자리 
# paste 문자열 두 개 합치는 함수
# paste0: sep 없이 붙여줌 뒤에 프린트는 확인용
# read_html(사이트 긁어와)
# html_nodes(분리해서 긁어옴)
# html_text(txt 긁어옴)

for(i in 1:10){
  url_crawl = paste0(url_base, i); print(url_crawl)
  hdoc = read_html(url_crawl) #사이트 긁어와
  
  t_css = ".f_link_b" # '.'(class) 소제목(daum 기준)
  b_css = ".desc" #'.'(class) 본문(daum 기준)
  
  t_node = html_nodes(hdoc, ".f_link_b") #분리해서 긁어옴
  b_node = html_nodes(hdoc, ".desc")
  
  title_part = html_text(t_node)
  body_part = html_text(b_node)
  
  title = c(title, title_part) #벡터 생성
  body = c(body, body_part) #벡터 생성
}

news <- cbind(title, body) #벡터 묶기

#news

write.csv(news, file = "crawltest.csv")


## R교육 3일차 : 텍스트 마이닝

#install.packages("rJava") #한글 쓰려면 자바 필요,없어도 됨
#KoNLP 설치될 때 같이 설치됨
#Java 이미 깔려 있으면 설치하면 안됨
#install.packages("memoise")
#install.packages("KoNLP")
#install.packages("wordcloud2")

library(rJava)
library(KoNLP)
library(dplyr)
library(memoise)
library(wordcloud2)

useSejongDic()

text = readLines("ahn.txt")


nouns <- extractNoun(text)
#f2 누르면 소스코드가 나옴
nouns

nouns <- unlist(nouns) #dataframe으로 바로 안 바뀌므로
nouns #벡터 형태로 저장됨

nouns <- nouns[(nchar(nouns)>=2)] 
#nchar() : 캐릭터 개수, 글자 개수
nouns

wordFreq <- table(nouns)

wordFreq <- table(nouns) %>% 
  sort(decreasing = T) #%>% head(20)

wordFreq <- sort(wordFreq, deceasing = T)
wordFreq
wordFreq <- head(wordFreq, 20)
#
wordcloud2(wordFreq, fontFamily = '맑은 고딕')



#문장 줄이기

#1
nouns <- text %>% 
  extractNoun() %>% 
  unlist()
  
nouns[nchar(nouns)>=2] %>% 
  table() %>% 
  sort(decreasing=T) %>% #head(20) %>% 
  wordcloud2(fontFamily = '맑은 고딕')

#2
nouns[nchar(text %>% 
  extractNoun() %>% 
  unlist())>=2] %>% 
  table() %>% 
  sort(decreasing=T) %>% #head(20) %>% 
  wordcloud2(fontFamily = '맑은 고딕')

#//w: 정규표현식

##감성분석하기 (트위터)
#install.packages("ROAuth")
#install.packages("twitteR")
#install.packages(c("devtools","rjson", "bit64","hitter","ROAuth"))
#install.packages("hitter")


library(devtools)
library(ROAuth)
library(twitteR)
library(rjson)
library(bit64)

# save(".rda") <- Rdata 파일 저장하기
# load(".rda") <- Rdata 파일 불러오기

API_key <- "NFUUd6DOrO7LP2cKYHeuBM2ig"
API_secret <- "LRL4wKC6hwUpBdrKolEE1nQU3iLtzzsUWPU3f58efdG55fko0E"


## 2018년 전 세계에서 승객이 가장 많았던 공항 10군데 시각화

library(rvest) #read_html() 써야 함
library(ggplot2)
#install.packages("ggmap")
library(ggmap)

html.airports <- read_html("https://en.wikipedia.org/wiki/List_of_busiest_airports_by_passenger_traffic")
html.airports
df <- html_table(html_nodes(html.airports, "table")[[1]], 
                 fill = TRUE)
#[[]] : list
#[[1]] : list 중 첫번째

head(df) #상위 10개보기!
head(df$Airport)
colnames(df)[6]
colnames(df)[6] <- "total"
df$total
df$total <- gsub(',','',df$total) #콤마 없애기(콤마-공백전환)
#gsub : 전부 대체 global subtitute(R 이외에도 쓰이는 함수)

df$total
mode(df$total)

df$total <- as.numeric(df$total) #숫자 타입으로 변환

mode(df$total)

getwd()
write.csv(df$Airport, file = "geoname.csv")


