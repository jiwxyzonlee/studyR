##
##
#단순 선형회귀분석 실습

cars

library(dplyr)

glimpse(cars)
#Observations: 50
#Variables: 2
#$ speed(x) <dbl> 4, 4, 7, 7, 8, 9, 10, 10, 10...
#$ dist(y)  <dbl> 2, 10, 4, 22, 16, 10, 18, 26...


plot(cars)

#직선을 찾는다

lm(y~x)
#lm(formula, data, subset, weights, na.action,
#   method = "qr", model = TRUE, x = FALSE, y = FALSE, qr = TRUE,
#   singular.ok = TRUE, contrasts = NULL, offset, ...)

lm(dist~speed, data = cars)
#'data='가 붙어있으면 자리 위치 상관 없음
#Call:
#  lm(formula = dist ~ speed, data = cars)

#Coefficients:
#     (Intercept)            speed  
#(절편)-17.579        (기울기)3.932 



lm_car <- lm(dist~speed, cars)

abline(lm_car)

abline(lm_car, col = "red", lwd = 2)

library(ggplot2)

ggplot(cars, aes(speed, dist)) +
  geom_point() +
  geom_smooth(method = "lm", se = F)


# 다이아몬드

library(ggplot2)

glimpse(diamonds)

lm_dia <- lm(price~carat, diamonds)
lm_dia

plot(diamonds$carat, diamonds$price)

par(mfrow=c(2,2))
plot(lm_dia)
abline(lm_dia)

dev.off()

par(mfrow=c(1,3)) #아래는 같은 그림으로 나타남

plot(diamonds$carat, diamonds$price)
plot(diamonds$price ~ diamonds$carat)
plot(price ~ carat, data = diamonds)

ggplot(diamonds, aes(carat, price)) +
  geom_point() +
  geom_smooth(method = "lm", se = F)


par(mfrow=c(1,1))

lm_dia$coefficients
#(Intercept)          carat 
#-2256.361 (y절편)    7756.426 (기울기)


cars

plot(cars)
abline(lm_car)

ggplot(cars, aes(speed, dist)) +
  geom_point() +
  geom_smooth()

ggplot(cars, aes(speed, dist)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(cars, aes(speed, dist)) +
  geom_point() +
  geom_smooth(method = "lm", se = F)


predict(lm_car)
#과거의 어떠한 정보가 들었었는지 보여줌

#speed가 30일 때가 궁금함

#predict(lm_car, x) <- x자리에는 dataframe 들어가야
#data.frame(..., row.names = NULL, check.rows = FALSE,
#           check.names = TRUE, fix.empty.names = TRUE,
#           stringsAsFactors = default.stringsAsFactors())

lm_car
#Coefficients:
#  (Intercept)        speed  
#-17.579        3.932  



predict(lm_car, data.frame(speed = 30))
#speed가 30일 때 예측값

y = -17.579 + 3.932*30
y

predict(lm_car, data.frame(speed = 40))

y1 = -17.579 + 3.932*40
y1

predict(lm_car, data.frame(speed = c(30, 35, 40, 45)))

summary(lm_car)
#Call:
#  lm(formula = dist ~ speed, data = cars)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-29.069  -9.525  -2.272   9.215  43.201 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -17.5791     6.7584  -2.601   0.0123 *  
#  speed         3.9324     0.4155   9.464 1.49e-12 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 15.38 on 48 degrees of freedom
#Multiple R-squared:  0.6511,	Adjusted R-squared:  0.6438 
#F-statistic: 89.57 on 1 and 48 DF,  p-value: 1.49e-12

names(lm_car)
length(lm_car)

a <- lm_car

a$coefficients
names(a)
length(a)

a$coefficients
a$residuals
a$effects

a$rank
a$fitted.values
a$assign
a$df.residual 
a$qr
a$xlevels
a$call
a$terms
a$model
a$residuals #잔차

predict()
#아래것 다 알아서 예측해서 해줌
#predict.glm, 
#predict.lm, 
#predict.loess, 
#predict.nls, 
#predict.poly, 
#predict.princomp, 
#predict.smooth.spline.

predict.lm()
#predict(object, newdata, se.fit = FALSE, scale = NULL, df = Inf,
#        interval = c("none", "confidence", "prediction"),
#        level = 0.95, type = c("response", "terms"),
#        terms = NULL, na.action = na.pass,
#        pred.var = res.var/weights, weights = 1, ...)

new <- data.frame(speed = c(122))
new
predict(lm_car, 
        newdata = new, 
        interval = "confidence") #신뢰도


new <- data.frame(speed = c(122, 125, 130, 133))
predict(lm_car, 
        newdata = new, 
        interval = "confidence") #신뢰도


#fit      lwr      upr
#1 462.1748 373.0091 551.3405
#2 473.9720 382.3029 565.6411
#3 493.6340 397.7923 589.4758
#4 505.4313 407.0857 603.7768


predict(lm_car, 
        newdata = new, 
        interval = "confidence", level = 0.9)
#level : Tolerance/confidence level.

#fit      lwr      upr
#1 462.1748 387.7949 536.5547
#2 473.9720 397.5038 550.4402
#3 493.6340 413.6851 573.5830
#4 505.4313 423.3937 587.4688

fitted.values(lm_car) #예측값 
residuals(lm_car) #잔차, 실측값에서 예측값을 뺀 것
predict(lm_car)


#Regression

#회귀분석의 기본가정
#두 변수 관계는 선형
#오차항의 확률 분포는 정규분포
#오차항은 모든 독립변수 값에 대하여 동일한 분산 갖는다
#오차항의 평균(기대값)은 0

#오차항들끼리는 독립, 어떤 패턴 나타나면 안됨(독립성)
#독립변수 상호간에는 상관관계가 없어야 한다

#상관은 인과관계와 무관
#회귀분석은 인과관계 가정

#최소제곱법은 오차^2 
#(원으로 하는 점과 직선의 거리가 아니라 
#y값 차이의 최솟값의 제곱) (y1-y2)^2
# = 잔차제곱합
#정사각형의 넓이의 최솟값
#오차가 커질수록 비용이 엄청나게 커진다
# probability가 0에 가깝다<-귀무가설기각/대립가설채택(대안채택)
# 귀무가설 = 선형이 아닙니다, 기각=선형입니다

#행정학 1종 오류, 2종 오류 보기

#1종오류란 옳은 "귀무가설(효과가 없다는 가설)을 기각"
#하는 오류인데
#이는 "틀린 대안을 선택하는 오류" 또는 
#"효과가 없는데 있다고 판단하는 오류"에 해당합니다.

#"옳은 귀무가설(효과x) 기각"
#    = "틀린 대안 선택", "틀린 대립가설 채택"
#    = 효과가 없는데 있다고 판단



#2종오류란 틀린 귀무가설(효과가 없다는 가설)을 인용"
#하는 오류인데
#이는 "옳은 대안을 선택하지 않는 오류" 또는 
#"효과가 있는데 없다고 판단하는 오류"에 해당합니다.

#"틀린 귀무가설 인용" 
#   = "옳은 대안 불채택", "옳은 대립가설 불채택"
#   = 효과가 있는데 없다고 판단



#신뢰 구간 내 : 귀무가설 인용(대립가설 불채택) - 2종오류 조심
#기각 구간 내 : 귀무가설 기각(대립가설 채택) -1종오류 조심

#설명못하는 값이 적을 수록 좋다 : (y1-y2) <- 잔차, 에러 SSE
#평균에서 직선까지는 설명가능한 값 SSR(regression)
#ssT = SSR + SSE
#SSR은 그대로, SSE가 직선에 가까워지는 값(직선상일때 R^2= 1)
#SSR/SST = 1- SSE/SST = R^2

#우상향 1
#우상향 0<R^2<1
#관계 없음 = 0
#우하향 -1 or -1>R^2>0
#cor() :상관행렬
#cor.test() x와y의 상관관계 검정

#p-value: 1.49e-12 (귀무가설 기각)
#true correlation is not equal to 0 :상관관계 있다
# -> 대립가설 채택
#R-squared :0.6511,  R^2 = 0.6511, R=0.8069




  #   #   #
#   #   #   #
#           #
# 오후 수업 #
#           #
#   #   #   #
  #   #   #

# 

# Frequentist probability
# Bayesian probability

# 머신러닝과의 상관성은?
# 나이브 베이지안 알고리즘

# 새로운 증거, 소거, 확률은?

# 맹목적으로 나이브하게





sms_raw <- read.csv("sms_spam.csv", 
                    stringsAsFactors = F)

glimpse(sms_raw)
table(sms_raw)


#convert type(spam/ham) to factor(chr 때문에 범주형으로 해준다)
sms_raw$type <- factor(sms_raw$type)


#examine the type variable more carefully

glimpse(sms_raw$type)
table(sms_raw$type)

write.csv(sms_raw, "sms_raw.csv")

#text mining 처리 package
#build a corpus using the text mining (tm) package
install.packages("tm")
library(tm)




maintainer("tm") #누가 만들었나
citation("tm") #인용할 때


vignette("tm") #예제, 샘플 보여주기



#VCorpus (volatile Corpus)
#Pcorpus (permanent corpus)
sms_corpus <- VCorpus(VectorSource(sms_raw$text))
#말뭉치 생성

#examine the sms corpus

print(sms_corpus)
#<<VCorpus>>
#Metadata:  corpus specific: 0, document level (indexed): 0
#Content:  documents: 5559

inspect(sms_corpus[1:2])


#check corpus message
as.character(sms_corpus[[1]])
lapply(sms_corpus[1:2], as.character) #모든 열 적용


#clean up the corpus using tm_map()
sms_corpus_clean <- tm_map(sms_corpus,
                           content_transformer(tolower)) #대문자 소문자로


#show the difference between sms_corpus and corpus_clean
as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]]) #차이 비교


sms_corpus_clean <- tm_map(sms_corpus_clean, 
                           removeNumbers)
sms_corpus_clean <- tm_map(sms_corpus_clean, 
                           removeWords, 
                           stopwords())

sms_corpus_clean <- tm_map(sms_corpus_clean, 
                           removePunctuation)


#tip: create a custom function to replace (rather than remove)
#removePunctuation("hello...world")



#illustration of word stemming

install.packages("SnowballC")
library(SnowballC)
wordStem(c("learn", "learned","learning","learns")) #줄기만 남기고 정리

sms_corpus_clean <- tm_map(sms_corpus_clean, 
                           stemDocument)

sms_corpus_clean <- tm_map(sms_corpus_clean, 
                           stripWhitespace)
#eliminate unneeded whitespace

#examine the final clean corpus
lapply(sms_corpus[1:3], as.character)
lapply(sms_corpus_clean[1:3], as.character)

#create a document-term sparse matrix
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)

#alternative solution: create a document-term sparse matrix directly from
sms_dtm2 <- DocumentTermMatrix(sms_corpus, 
                               control = list(tolower = TRUE,
                                              removeNumbers = T,
                                              stopwords = T,
                                              removePunctuation = T,
                                              stemming = T))

#alternative solution: using custom stop words function ensures identical r
sms_dtm3 <- DocumentTermMatrix(sms_corpus, 
                               control = list(tolower = T,
                                              removeNumbers = T,
                                              stopwords = function(x){
                                                removeWords(x, stopwords())},
                                              removePunctuation = T,
                                              stemming = T))

#compare the result

sms_dtm
sms_dtm2
sms_dtm3

#creating training and test datasets
sms_dtm_train <- sms_dtm[1:4169,]
sms_dtm_test <- sms_dtm[4170:5559,] 

#also save the labels
sms_train_labels <- sms_raw[1:4169,]$type
sms_test_labels <- sms_raw[4170:5559,]$type

#check that the proportion of spam is similar
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

#word cloud visualization
install.packages("wordcloud")
library(wordcloud)

wordcloud(sms_corpus_clean, min.freq = 50, random.order = F)
#원본


wordcloud(sms_corpus_clean, min.freq = 50, random.order = T)


# install.packages("tidytext")
# library(tidytext)
# DF <- tidy(sms_dtm)
# DF
# DF %>% arrange(desc(count)) %>% head(20)


spam <- subset(sms_raw,type == "spam") 

ham <- subset(sms_raw, type == "ham")

wordcloud(spam$text, min.freq = 40, scale = c(3, 0.5))
wordcloud(ham$text, min.freq = 40, scale = c(3, 0.5))

sms_dtm_freq_train <- removeSparseTerms(sms_dtm_train, 0.999)
sms_dtm_freq_train

#indicator features for frequent words
findFreqTerms(sms_dtm_train, 5) #5번 이상 나온 단어

#save frequently-appearing
sms_freq_words <- findFreqTerms(sms_dtm_train, 5)
str(sms_freq_words)

#creat DTMs with only the frequent terms
sms_dtm_freq_train <- sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]


#convert counts to a factor
convert_counts <- function(x){
  x <- ifelse(x > 0, "Yes", "No")
}


#apply() convert_counts() to columns of train/test data
sms_train <- apply(sms_dtm_freq_train, 
                   MARGIN = 2, 
                   convert_counts)
sms_test <- apply(sms_dtm_freq_test, 
                  MARGIN = 2, 
                  convert_counts)
View(sms_test)


##Step 3:Training a model on the data ----
library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_train_labels)

##Step 4: Evaluating model performance ----
sms_test_pred <- predict(sms_classifier,  sms_test)

library(gmodels)
CrossTable(sms_test_pred, sms_test_labels,
           prop.chisq = F, prop.t = F, prop.r = F,
           dnn = c('predicted', 'actual'))

#             | actual 
#   predicted |       ham |      spam | Row Total | 
#-------------|-----------|-----------|-----------|
#         ham |      1201 |        30 |      1231 | 
#             |     0.995 |     0.164 |           | 
#-------------|-----------|-----------|-----------|
#        spam |         6 |       153 |       159 | 
#             |     0.005 |     0.836 |           | 
#-------------|-----------|-----------|-----------|
#Column Total |      1207 |       183 |      1390 | 
#             |     0.868 |     0.132 |           | 
#-------------|-----------|-----------|-----------|

#햄이라고 예측했는데 진짜 햄은 1201 그 중 스팸은 30
#스팸이라고 예측했는데 그 중 햄은 6이고 진짜 스팸은 153


##Step 5: Improving model performance ----
sms_classifier2 <- naiveBayes(sms_train, sms_train_labels, laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_test_labels,
           prop.chisq = F, prop.t = F, prop.r = F,
           dnn = c('predicted', 'actual'))

#             | actual 
#   predicted |       ham |      spam | Row Total | 
#-------------|-----------|-----------|-----------|
#         ham |      1202 |        28 |      1230 | 
#             |     0.996 |     0.153 |           | 
#-------------|-----------|-----------|-----------|
#        spam |         5 |       155 |       160 | 
#             |     0.004 |     0.847 |           | 
#-------------|-----------|-----------|-----------|
#Column Total |      1207 |       183 |      1390 | 
#             |     0.868 |     0.132 |           | 
#-------------|-----------|-----------|-----------|


