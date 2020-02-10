


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

#df <- head(df,10)

head(df$Airport)
df$Airport   

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

#library(readr)
#read_csv: string을 chr로 읽음

geo <- read.csv("geoname - geoname.csv")

#geo <- read.csv("geoname - geoname.csv", stringsAsFactors = FALSE)

str(geo)
glimpse(geo)

#geo$Latitude <- as.character(geo$Latitude)
#geo$Longitude <- as.character(geo$Longitude)
#Factor여서 바로 numeric으로 안 바뀜
#캐릭터로 바꿨다가 numeric으로 바뀌어야 바뀜
#안 바뀐 채로 바로 하면 소수점 날아갈 수도


df <- cbind(df, lat = geo$Latitude, lot = geo$Longitude)

str(geo)
glimpse(geo)

df$lot <- as.numeric(df$lot) 
df$lat <- as.numeric(df$lat)


#install.packages("maps")
library(maps)

world <- map_data("world")

ggplot(df, aes(x = lot, y = lat)) +
  geom_polygon(data = world, 
               aes(x = long, y = lat, 
                   group = group), 
               fill = "grey75", color = "grey70") +
  geom_point(data = df, color = rainbow(30), 
             alpha =.25,
             aes(size = total), na.rm = T) +
  geom_point(data = df, color = rainbow(30), 
             alpha=.75,
             shape = 1,
             aes(size = total), na.rm = T) +
  geom_segment(data = df, 
               aes(x = lot, y = lat, 
                   xend = lot, yend = lat,
                   color = Airport))

#colnames(df) <- make.unique(names(df))

df

rlang::last_error()


###감성분석하기

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

#API_key <- "NFUUd6DOrO7LP2cKYHeuBM2ig"
#API_secret <- "LRL4wKC6hwUpBdrKolEE1nQU3iLtzzsUWPU3f58efdG55fko0E"
#acess_token = "919782743856185344-3GbTaoMlalFamrnLNpvxl1Ng2EnsNX"
#acess_secret = "ZwZEim7LP1Dvff3oQ574r75IJmfLgxJJwbdOMHvA8Ius3"

# save(".rda") <- Rdata 파일 저장하기
# load(".rda") <- Rdata 파일 불러오기





#rm(access_token)

#Obama

#tweet_Obama <- searchTwitter("Obama", lang = "en", n = 1000)

load("Obama.rda")
load("Trump.rda")

tweet_Obama <- twListToDF(tweets_Obama)

Obama_text <- tweet_Obama$text

head(Obama_text)



Obama_text <- gsub("\\W", " ", Obama_text) 
#정규표현식 "\\W(대문자)": 특수문자
#알파벳 아닌 것들은 빈칸으로 바뀜
Obama_df <- as.data.frame(Obama_text)
length(Obama_df); length(Obama_text)

#Trump

#tweet_Trump <- searchTwitter("Trump", lang = "en", n = 1000 )

tweet_Trump <- twListToDF(tweets_Trump)

Trump_text <- tweet_Trump$text

head(Trump_text)


Trump_text <- gsub("\\W", " ", Trump_text) 
#정규표현식 "\\W(대문자)": 특수문자
#알파벳 아닌 것들은 빈칸으로 바뀜
Trump_df <- as.data.frame(Trump_text)
length(Trump_df); length(Trump_text)


#감성 사전 

pos.word <- scan("positive-words.txt", 
                 what = "character", 
                 comment.char = ";" )
neg.word <- scan("negative-words.txt",
                 what = "character",
                 comment.char = ";")
#긍정어 단어만 나열된 개수만큼 +점수


#score.sentiment를 정의함
score.sentiment = function(sentences, 
                           pos.words, neg.words, 
                           .progress='none')
{
  require(plyr);
  require(stringr);
  scores = laply(sentences, function(sentence, 
                                     pos.words, neg.words) {
    sentence = gsub('[^A-z ]','', sentence)
    sentence = tolower(sentence);
    word.list = str_split(sentence, '\\s+');
    words = unlist(word.list);
    pos.matches = match(words, pos.words);
    neg.matches = match(words, neg.words);
    pos.matches = !is.na(pos.matches);
    neg.matches = !is.na(neg.matches);
    score = sum(pos.matches) - sum(neg.matches);
    return(score);
  }, pos.words, neg.words, .progress=.progress );
  scores.df = data.frame(score=scores, text=sentences);
  return(scores.df);
}

Obama_scores <- score.sentiment(Obama_text,
                                pos.word, neg.word, 
                                .progress = 'text')
hist(Obama_scores$score)



#install.packages("sentimentr")
#library(sentimentr)

Trump_scores <- score.sentiment(Trump_text,
                                pos.word, neg.word, 
                                .progress = 'text')
hist(Trump_scores$score)

a <- dim(Obama_scores)[1] #1000
b <- dim(Trump_scores)[1] #1000


nrow(Obama_scores)

alls <- rbind(as.data.frame(cbind(type=rep("Obama",a), #Obama 1000번 복제
                                  score = Obama_scores[,1])), #1열:score
              as.data.frame(cbind(type=rep("Trump",b), #Trump 1000번 복제
                                  score = Trump_scores[,1])))
#rbind: 밑으로
library(dplyr)
glimpse(Obama_scores)

str(alls)

glimpse(alls)
alls$score <- strtoi(alls$score)
#글자를 숫자로 바꾸는 것 string to int.
glimpse(alls)

library(ggplot2)

#type= Obama, Trump
ggplot(alls, aes(x = score, color = type)) + 
  geom_density(size=1)

#
#Error in .Call.graphics(C_palette2, .Call(C_palette2, NULL)) : invalid graphics state 

#그래픽 안 먹히면 dev.off()

#동적 시각화

library(ggplot2)

ggplot(data = mpg, aes(displ, cty,
                       color = drv)) +
  geom_point() + xlim(3,5) + ylim(10,25)


p <- ggplot(data = mpg, aes(displ, cty,
                       color = drv)) +
  geom_point()

#install.packages("plotly")
library(plotly)

ggplotly(p) #interactive


#rChart는 에러 자주 남
#googleVis도 있으

###

p1 <- ggplot(mpg, aes(class, fill = drv))+
  geom_bar()

p2 <- ggplot(mpg, aes(class, fill = drv))+
  geom_bar(position = "dodge")

ggplotly(p1)

ggplotly(p2) #바 분리해서 찍어내기


#rCharts는 비공식적 패키지
#비공식적 패키지 설치하기
library(devtools)
install_github("ramnathv/rCharts")
install_github("saurfang/rCharts", 
               ref = "utf8-writelines")
library(rCharts)

hair_eye_male <- subset(as.data.frame(HairEyeColor),
                        Sex=="Male")

nPlot(Freq~Hair, group = "Eye", 
      data = hair_eye_male,
      type = "multiBarChart")

rPlot(mpg~wt | am + vs, 
      data = mtcars, 
      type = "point",
      color = "gear")

rPlot(mpg ~ wt | am + vs, 
      data = mtcars, 
      type = "point",
      color = "gear")


map3 <- Leaflet$new()

map3$setView(c(37.533, 127.025), zoom = 13)

map3$marker(c(37.533, 127.025), 
           bindPopup = "<p> I am a popup </p>")
map3$marker(c(37.533, 127.025),
            bindPopup = "<p> Hi~ </p>")
map3


m1 <- mPlot(x= "date", 
            y=c("psavert", "uempmed"),
            type = "Line", data = economics)


m1$set(pointSize = 0, lineWidth=1)
m1

#              #
##            ##
### 오후강의 ### 지도 시각화
##            ##
#              #

## 오늘은 단계 구분도!

#반응형이어야 알아 보기 쉽고 예쁘지

#Hexbin
#가독성 높이기 위해 지형모형을 통일시키고


#Cartogram 왜곡된 것 
#실제 지도모형이 아니라 변수별로 퍼지거나 왜곡되는

#devtools::install_github('dkahale/ggmap')
#library(ggmap)


#Windows 사용자들은 ‘지역명’이나 ‘주소’를 한글로 지정하려면
#반드시 enc2utf8() 함수를 이용하여 한글의 인코딩 방식을
#UTF-8으로 변경


install.packages("leaflet")
library(leaflet)
#install.packages("cartography")
#library(Cartography)

library(dplyr)
library(readxl)
library(stringr)

eq <- read_xlsx('국내지진.xlsx')
View(eq)
#다운받은 파일을 열어보고 자료의 속성과 구조를 파악
#특히 csv, txt
#객체 생성시 가급적 간단하고 식별 용이하게
# 기존의 성공한 데이터를 가공하도록

glimpse(eq)

#정규표현식 (북한 들어간 행은 빼고 뽑아내겠다)
eq01 <- eq[grep("북한", eq$위치, inver =T), ]
View(eq01)

#위도 경도(chr) 뒤의 
#불필요한 문자 삭제 후(str_replace) 숫자형 바꾸기

eq01$위도 <- str_replace(eq01$위도, ' N', "")
eq01$위도 <- as.numeric(eq01$위도)
eq01$경도 <- str_replace(eq01$경도, ' E', "")
eq01$경도 <- as.numeric(eq01$경도)

glimpse(eq01)
View(eq01)


#API 없이 오픈 소스를 쓴다, addMarkers(파라미터 값 설정)
leaflet() %>% addTiles() %>% 
  addMarkers(lng=eq01$경도, lat=eq01$위도,
             popup=paste0("발생시각:", eq01$발생시각,
                          "<br/>위치:", eq01$위치,
                          "<br/>규보:", eq01$규모 ))

#paste0: 공란 없이 붙이기

library(devtools)
install_github("cardiomoon/kormaps2014")
install_github("cardiomoon/moonBook2")
install.packages("rlang")


library(kormaps2014)
library(moonBook2)

library(rlang)
library(readxl)
library(dplyr)

#한국행정지도(2014) 패키지 kormaps2014 안내 << 참고

areacode
str(kormap1)
str(kormap2)
str(kormap3)

korpop1
str(korpop1)

library(ggplot2)
install.packages("mapproj")
library(mapproj)


theme_set(theme_gray(base_family = "NanumGothic"))

ggplot(korpop1, aes(map_id = code, fill = 총인구_명)) +
  geom_map(map = kormap1, color = "black", 
           size = 0.1)+
  expand_limits(x = kormap1$long, y = kormap1$lat)+
  scale_fill_gradientn(colors = c('white', 'orange', 'red'))+
  ggtitle("2015년도 시도별 인구분포도")+
  coord_map()


#시군구별, 읍면동별 단계구분도의 예제

ggChoropleth(korpop2, kormap2, fillvar = "남자_명")

ggChoropleth(korpop3, kormap3, fillvar = "주택_계_호")

areacode <- changeCode(areacode)

ggChoropleth(korpop3, kormap3, 
             fillvar = "총인구_명",
             subarea = c("전라", "광주"))

ggChoropleth(korpop2,kormap2,
             fillvar = "남자_명",
             interactive = TRUE)

korpop4 <- changeCode(korpop3)
View(korpop4)
korpop5 <- korpop4 %>% select(행정구역별_읍면동)
korpop6 <- korpop3 %>% select(남자_명)
korpop7 <- cbind(korpop5, korpop6)



ggChoropleth(korpop3, kormap3,
             fillvar = "남자_명",
             interactive = TRUE,
             subarea = c("전라", "광주"),
             tooltip = "행정구역별_읍면동")


#install.packages("ggiraphExtra")
#library(ggiraphExtra)


###결핵 신환 발생 데이터 tbc


summary(tbc)

data(tbc)

#install.packages("extrafont")
#library(extrafont)
#font_import()
#y

tbc1=tbc[tbc$year %in% c(2001,2005,2010,2015),]


ggChoropleth(tbc1, kormap1,
             fillvar = "NewPts",
             facetvar = "year",
             tooltip = "name",
             interactive = TRUE)

tbc1 <- changeCode

###
###
###
###치킨집
###
###
###


seoulres <- read_xlsx("6110000_서울특별시_07_24_04_P_일반음식점.xlsx")

View(seoulres)

#시트 2장임

seoulres01 <- read_xlsx("6110000_서울특별시_07_24_04_P_일반음식점.xlsx",
                        sheet = '일반음식점_1')


seoulres02 <- read_xlsx("6110000_서울특별시_07_24_04_P_일반음식점.xlsx",
                        sheet = '일반음식점_2')



glimpse(seoulres01)
glimpse(seoulres02)

seoulres03 <-  rbind(seoulres01, seoulres02)

View(seoulres03)

glimpse(seoulres03)

dim(seoulres01); dim(seoulres02); dim(seoulres03)
#병합은 행으로 병합

seoulch <- seoulres03 %>% 
  filter(상세영업상태명 == '영업'&위생업태명%in%c('호프/통닭', '통닭(치킨)'))

glimpse(seoulch)

sum(is.na(seoulch$소재지우편번호))
sum(is.na(seoulch$소재지전체주소))


##소재지 전체주소
seoulch01 <- na.omit(seoulch$소재지전체주소)

glimpse(seoulch01)
View(seoulch01)

library(stringr)


seoulch02 <- substr(seoulch01,7,10) #서울특별시 마포구
#마:7번째 #구:10번째
View(seoulch02)

seoulch03 <- gsub(" ","", seoulch02) #공백 제거
View(seoulch03)


seoulch04 <- str_replace_all(seoulch03,
                             "을충신필명중오인남다수북흥회쌍입광정저순만황초서장태주무산]","")
View(seoulch04)

seoulchch <- seoulch03[276:616]
View(seoulchch)

seoulchch01 <- substr(seoulchch, 1, 2)
View(seoulchch01)

seoulch05 <- seoulch03[-(276:616)]
View(seoulch05)
View(seoulch04)

Seoulch06 <- c(seoulchch01, seoulch05)
View(Seoulch06)

seoulch06 <- table(Seoulch06)
seoulch06

# 소재지우편번호(구우편번호) #쓸 건 많아도 이게 더 편함

View(seoulch)


seoulch07 <- na.omit(seoulch$소재지우편번호)
View(seoulch07)

seoulch08 <- substr(seoulch07, 1, 3)
View(seoulch08)

seoulch09 <- table(seoulch08)
View(seoulch09)

gu <- c("중구", "종로구", "서대문구", "마포구", "은평구",
        "동대문구", "중랑구", "도봉구","성동구", "강동구",
        "강남구", "성북구", "서초구", "송파구", "노원구",
        "용산구", "강북구", "광진구", "영등포구", "관악구",
        "구로구", "금천구", "동작구", "강서구", "양천구")

seoulch010 <- data.frame(gu, seoulch09)
View(seoulch010)
glimpse(seoulch010)


install.packages("treemap")

library(treemap)

treemap(seoulch010, index= "gu", vSize = "Freq",
        title = "서울시 구별 치킨집수")


treemap(Seoulch06, title = "서울시 구별 치킨집수수")


## interactive graph 만들기

korpop2[1:25,]


ch <- korpop2[1:25,]
str(ch)

ch01 <- select(ch, c('행정구역별_읍면동', 'code'))
str(ch01)


ch02 <- changeCode(ch01)
View(ch02)

ch02$'행정구역별_읍면동' <- as.character(ch02$'행정구역별_읍면동')
ch02$code <- as.character(ch02$code)


ch03 <- arrange(ch02, ch02$'행정구역별_읍면동')
ch03

seoulch010
seoulch11 <- rename(seoulch010, '행정구역별_읍면동' ='gu', '치킨집수' = Freq)
seoulch11

seoulch11$'행정구역별_읍면동' <- as.character(seoulch11$'행정구역별_읍면동')

seoulch12 <- select(seoulch11, -seoulch08)
seoulch12
str(seoulch12)

seoulch13 <- arrange(seoulch12, seoulch12$'행정구역별_읍면동')
seoulch13 #사전순

seoulch14 <- cbind(ch03, seoulch13)
seoulch14

seoulch15 <- seoulch14[ ,-1] #1열 제거
seoulch15

ggChoropleth(seoulch15, kormap2, fillvar="치킨집수",interactive=TRUE,
             subarea="서울", tooltip="행정구역별_읍면동",
             title = '서울시 구별 치킨집수')


###
###
### 분식

seoulres <- read_xlsx("6110000_서울특별시_07_24_04_P_일반음식점.xlsx")

View(seoulres)

#시트 2장임

seoulres01 <- read_xlsx("6110000_서울특별시_07_24_04_P_일반음식점.xlsx",
                        sheet = '일반음식점_1')


seoulres02 <- read_xlsx("6110000_서울특별시_07_24_04_P_일반음식점.xlsx",
                        sheet = '일반음식점_2')

seoulres03 <-  rbind(seoulres01_1, seoulres02_1)

seoulbs <- seoulres03 %>% 
  filter(상세영업상태명 == '영업'&위생업태명%in%c('분식'))

sum(is.na(seoulbs$소재지우편번호))
sum(is.na(seoulbs$소재지전체주소))


##소재지 전체주소
seoulbs01 <- na.omit(seoulbs$소재지전체주소)

glimpse(seoulbs01)
View(seoulbs01)

library(stringr)


seoulbs02 <- substr(seoulbs01,7,10) #서울특별시 마포구
#마:7번째 #구:10번째
View(seoulbs02)

seoulbs03 <- gsub(" ","", seoulbs02) #공백 제거
View(seoulbs03)


seoulbs04 <- str_replace_all(seoulbs03,
                             "을충신필명중오인남다수북흥회쌍입광정저순만황초서장태주무산]","")
View(seoulbs04)

seoulbsbs <- seoulbs03[804:1444]
View(seoulbsbs)

seoulbsbs01 <- substr(seoulbsbs, 1, 2)
View(seoulbsbs01)

seoulbs05 <- seoulbs03[-(804:1444)]
View(seoulbs05)
View(seoulbs04)

Seoulbs06 <- c(seoulbsbs01, seoulbs05)
View(Seoulbs06)

seoulbs06 <- table(Seoulbs06)
seoulbs06


# 소재지우편번호(구우편번호) #쓸 건 많아도 이게 더 편함

View(seoulbs)


seoulbs07 <- na.omit(seoulbs$소재지우편번호)
View(seoulbs07)

seoulbs08 <- substr(seoulbs07, 1, 3)
View(seoulbs08)

seoulbs09 <- table(seoulbs08)
View(seoulch09)

gu <- c("중구", "종로구", "서대문구", "마포구", "은평구",
        "동대문구", "중랑구", "도봉구","성동구", "강동구",
        "강남구", "성북구", "서초구", "송파구", "노원구",
        "용산구", "강북구", "광진구", "영등포구", "관악구",
        "구로구", "금천구", "동작구", "강서구", "양천구")

seoulbs010 <- data.frame(gu, seoulbs09)
View(seoulbs010)
glimpse(seoulbs010)


#install.packages("treemap")

library(treemap)

treemap(seoulbs010, index= "gu", vSize = "Freq",
        title = "서울시 구별 분식식집수")

## interactive graph 만들기

korpop2[1:25,]


bs <- korpop2[1:25,]
str(bs)

bs01 <- select(bs, c('행정구역별_읍면동', 'code'))
str(bs01)


bs02 <- changeCode(bs01)
View(bs02)

bs02$'행정구역별_읍면동' <- as.character(bs02$'행정구역별_읍면동')
bs02$code <- as.character(bs02$code)


bs03 <- arrange(bs02, bs02$'행정구역별_읍면동')
bs03

seoulbs010
seoulbs11 <- rename(seoulbs010, '행정구역별_읍면동' ='gu', '분식집수' = Freq)
seoulbs11

seoulbs11$'행정구역별_읍면동' <- as.character(seoulbs11$'행정구역별_읍면동')

seoulbs12 <- select(seoulbs11, -seoulbs08)
seoulbsh12
str(seoulbs12)

seoulbs13 <- arrange(seoulbs12, seoulbs12$'행정구역별_읍면동')
seoulbs13 #사전순

seoulbs14 <- cbind(bs03, seoulbs13)
seoulbs14

seoulbs15 <- seoulbs14[ ,-1] #1열 제거
seoulbs15

ggChoropleth(seoulbs15, kormap2, fillvar="분식집수",interactive=TRUE,
             subarea="서울", tooltip="행정구역별_읍면동",
             title = '서울시 구별 분식집수')

