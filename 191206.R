#install.packages("ggrepel")
#install.packages("chron")
#install.packages("xts")
#install.packages("highcharter")

library(ggplot2); library(dplyr); library(RColorBrewer)

library(ggthemes); library(ggrepel); library(chron)

library(lubridate); library(xts); library(highcharter)


citation("lubridate")
citation("chron")
citation("highcharter")




data <- read.csv("ctrucks_clean.csv", stringsAsFactors = F)

#stringAsFactor: 안 넣어도 상관없으나 가끔 truckid가 범주형 취급돼서

head(data)

glimpse(data)



data$origin = as.factor(data$origin)

data$supplier = as.factor(data$supplier)

data$Date = ymd(data$Date) #date로 바뀜
#$ Date       <date> 2015-03-05, 2015-03-09,

data$month = month(data$month, label = T)
#$ month      <ord> 3, 3, 3, 3, 3, 3, 3 ord로 바뀜

#열 새로 만들기
data$week_day = weekdays(data$Date, abbreviate = T)
#mutate로 파생변수 만드는 방법도 있는데

glimpse(data)

# 트럭만 빈도수 별로 20까지 
data %>% arrange(desc(truckid)) %>% head(20)

truck <- data %>% 
  group_by(truckid) %>%
  summarize(freq = n()) %>% 
  top_n(20) %>% 
  as.data.frame()  #ppt용 #20등이 3개라 총 22개 나옴
#이게 맞는 거


truck <- data %>% 
  group_by(truckid) %>%
  summarize(freq = n()) %>% 
  arrange(desc(freq)) %>% 
  head(20) %>% 
  as.data.frame()

truck
#View(truck)



ggplot(truck, aes(reorder(truckid, freq), y = freq, fill = freq)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Truck") +
  ylab("Frequency") +
  coord_flip()

#reorder 재정렬

ggplot(truck, aes(reorder(truckid, freq), y = freq, fill = freq)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Truck") +
  ylab("Frequency") +
  coord_flip() +
  scale_fill_gradientn(name = '',
                      colors = rev(brewer.pal(10, 'Spectral'))) + #palette
  geom_text(aes(label=freq), hjust = 0.5, size = 5) +  #이게 바끝에 숫자 크기
  ggtitle("The top 20 Trucks with the most deliveries") +
  theme(legend.position = 'none', #legend 없애줌
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(size = 25, hjust = 0.4)) +
  coord_flip()


#Creates nice looking color palettes especially for thematic maps

# brewer.pal(n, name)
# display.brewer.pal(n, name)
# display.brewer.all(n=NULL, type="all", select=NULL, exact.n=TRUE, 
# colorblindFriendly=FALSE)
# brewer.pal.info

display.brewer.all()
#R에서 쓰는 색깔 다 보기

#There are 3 types of palettes, sequential, diverging, and qualitative.

#1. Sequential palettes are suited to ordered data that progress from low to high. 
#   Lightness steps dominate the look of these schemes, with light colors 
#   for low data values to dark colors for high data values. 

#2. Diverging palettes put equal emphasis on mid-range critical values and extremes 
#   at both ends of the data range. The critical class or break 
#   in the middle of the legend is emphasized with light colors 
#   and low and high extremes are emphasized with dark colors that have contrasting hues. 

#3. Qualitative palettes do not imply magnitude differences between legend classes, 
#   and hues are used to create the primary visual differences between classes. 
#   Qualitative schemes are best suited to representing nominal or categorical data.




data %>% 
  group_by(week_day, month) %>% #주중별로 묶고 같은 주의 월별로 묶고r
  count() %>% 
  ggplot(aes(reorder(week_day, n), y = n, fill = n)) +
  geom_bar(stat = "identity", position = "dodge")+
  xlab("Day")+
  ylab("Delivery Count")+
  scale_fill_gradientn(name= '', colors = rev(brewer.pal(10, 'Spectral')))+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  ggtitle("Delivery Counts through the Days by Month")+
  facet_wrap( ~ month)+
  theme(legend.position = 'none', #legend 없애줌
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15, face = "bold"),
        strip.background = element_rect(color = "red", fill = "#CCCCFF"))



#여기서 3,4,5월만 나타내도록 만들기


#filter(month %in% 3,4,5) %>% 
#filter(month == 3| month == 4| month == 5) %>%

data %>% filter(month %in% c(3,4,5)) %>%
  group_by(week_day, month) %>% #주중별로 묶고 같은 주의 월별로 묶고r
  count() %>% 
  ggplot(aes(reorder(week_day, n), y = n, fill = n)) +
  geom_bar(stat = "identity", position = "dodge")+
  xlab("Day")+
  ylab("Delivery Count")+
  scale_fill_gradientn(name= '', colors = rev(brewer.pal(10, 'Spectral')))+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  ggtitle("Delivery Counts through the Days by Month")+
  facet_wrap( ~ month)+
  theme(legend.position = 'none', #legend 없애줌
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15, face = "bold"),
        strip.background = element_rect(color = "red", fill = "#CCCCFF"))


data %>% filter(month == 3| month == 4| month == 5) %>%
  group_by(week_day, month) %>% #주중별로 묶고 같은 주의 월별로 묶고r
  count() %>% 
  ggplot(aes(reorder(week_day, n), y = n, fill = n)) +
  geom_bar(stat = "identity", position = "dodge")+
  xlab("Day")+
  ylab("Delivery Count")+
  scale_fill_gradientn(name= '', colors = rev(brewer.pal(10, 'Spectral')))+
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  ggtitle("Delivery Counts through the Days by Month")+
  facet_wrap( ~ month)+
  theme(legend.position = 'none', #legend 없애줌
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15, face = "bold"),
        strip.background = element_rect(color = "red", fill = "#CCCCFF")) #월 써 있는사각형

glimpse(data)


by_date_2015 = data %>% 
  filter(Date <= ("2016-01-01")) %>% 
  group_by(Date) %>% 
  summarise(Total = n())

time_series = xts(by_date_2015$Total, order.by = by_date_2015$Date)

by_date_2016 = na.omit(data) %>% 
  filter(Date >= ("2016-01-01")&Date < ("2017-01-01")) %>% 
  group_by(Date) %>% 
  summarise(Total = n())
time_series = xts(by_date_2016$Total, order.by = by_date_2016$Date)

hchart(time_series, name = "Number of Deliveries") %>% 
  hc_add_theme(hc_theme_chalk()) %>% 
  hc_title(text = "Total Deliveries for 2016") %>% 
  hc_legend(enabled = T)


#default #fivethirtyeight #Economist
#Financial Times(ft) #Dotabuff #Flat
#Simple #Elementary #Google #Firefox(ffx)
#Monokai #Tufte #Sparkline #Grid Light
#Sand Signika #Dark Unica #Chalk #Hand Drawn
#Null #Create themes







#
#상관관계 먼저 확인
#회귀분석은 기본적으로 상관관계에 대한 통계적 분석

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

##1종 옳은 귀무가설 기각, 틀린대안 선택 효과 없는데 있다고 착각해서 (틀린)대립가설 채택
##2종 틀린 귀무가설 인용, 옳은대안 미선택 효과 있는데 없다고 착각해서 (옳은)대립가설 미채택

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

#상관관계가 없지 않아요 != 상관관계가 있어요

install.packages("corrplot")
library(corrplot)

cor(cars)
#speed      dist
#speed 1.0000000 0.8068949
#dist  0.8068949 1.0000000
#위 입력값으로 corrplot
corrplot.mixed(cor(cars))
#mixed:왼쪽은 숫자 오른쪽은 동그라미(크기, 색깔, 짙은 정도)

cor(airquality)
cor(mpg)
cor(mtcars)

fit.cars <- lm(dist ~ speed, data = cars)
summary(fit.cars)
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#낮을수록 좋다(귀무가설이 기각되어야 하니까)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -17.5791     6.7584  -2.601   0.0123 *  
#  speed         3.9324     0.4155   9.464 1.49e-12 ***


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
predict(fit.cars, 
        newdata = new, 
        interval = "confidence") #신뢰도


new <- data.frame(speed = c(122, 125, 130, 133))
predict(fit.cars, 
        newdata = new, 
        interval = "confidence") #신뢰도


#fit      lwr      upr
#1 462.1748 373.0091 551.3405
#2 473.9720 382.3029 565.6411
#3 493.6340 397.7923 589.4758
#4 505.4313 407.0857 603.7768

#"prediction"은 틀린 값까지 고려


predict(fit.cars, 
        newdata = new, 
        interval = "confidence", level = 0.9)
#level : Tolerance/confidence level.

#fit      lwr      upr
#1 462.1748 387.7949 536.5547
#2 473.9720 397.5038 550.4402
#3 493.6340 413.6851 573.5830
#4 505.4313 423.3937 587.4688

fitted.values(fit.cars) #예측값 
residuals(fit.cars) #잔차, 실측값에서 예측값을 뺀 것
predict(fit.cars)


#잔차의 분산이 작다 = 데이터의 분산이 작다
#정규성 검정 => qplot


fit <- lm(dist~speed, cars)
fit

par(mfrow=c(2,2))
plot(fit)
#1 residual fitted
#residual 선이 0값에 그어진 건 잔차와 선 사이의 거리가 가깝게 있다는 것
#대칭 형태는 값이 좋지 않다는 것을 의미
#23,35,49 이상치, 잔차가 크다(별로 안 좋음)

#2 Q-Qplot
#직선의 기울기에 맞게 점이 찍혀 있다 = 정규성 가정을 만족한다, 따라간다

#3 Scale-Location

#4 Residuals Leverage
#cook distance라고 불리는 두 빨간선 사이에 점들이 모여있는 게 좋음
#(그래프 상에는 cook distance 하나만 나타남. 원래는 +,-값 두 개 나타남)


par(mfrow=c(1,1))

#등분산성 검정

#install.packages("lmtest")
library(lmtest)

bptest(fit)

#studentized Breusch-Pagan test

#data:  fit
#BP = 3.2149, df = 1, p-value = 0.07297

# p-value = 0.07297  <- 귀무가설 인용/채택
# 이 검정에선 숫자가 클수록 좋음(등분산 되어있습니다)

#install.packages("car")
library(car)

ncvTest(fit)

#Non-constant Variance Score Test 
#Variance formula: ~ fitted.values 
#Chisquare = 4.650233, Df = 1, p = 0.031049

# p = 0.031049  <- 귀무가설 인용/채택
# 이 검정에선 숫자가 클수록 좋음(등분산 되어있습니다)


plot(fit, 2)


#정규성 검정
shapiro.test(fit$residuals)
shapiro.test(fit$res)

#Shapiro-Wilk normality test

#data:  fit$residuals
#W = 0.94509, p-value = 0.02152

# p-value = 0.02152
#유의확륙(p-value)dl 작으면 정규성 가정에 문제가 있을 수 있다
#정규성 가정에 의심을 해봐야 한다
#p값이 클수록 좋다


# --> 그럴 경우 회귀분석 다시 해야
# a값 변환
#cars에 그래프를 그리면 1차 선형이 아니라 2차 함수 그래프가 그려져야 함
#plot(fit,1)에서 음, 양이 섞여 나오는 이유


#2차를 1차로 그릴 수 있도록 가정해야 함
# 루트 씌우세요(dist에 루트를 씌워야 함)

data2 <- cars
data2$sqrt.dist <- sqrt(data2$dist) #루트 씌우기
#colnames(data2)[2] <- "sqrt.dist" 이거 하지 말고 그냥 위에서 열 만들어 버림
head(data2)

fit2 <- lm(sqrt.dist~speed, data = data2)
fit2
#y~x에서 y 대입값 변경

par(mfrow=c(2,2))
plot(fit)
plot(fit2)

#아까보다 더 평평해짐

bptest(fit2)
#studentized Breusch-Pagan test

#data:  fit2
#BP = 0.011192, df = 1, p-value = 0.9157
#아까보다 p값 커져 있음

shapiro.test(fit2$res)
#Shapiro-Wilk normality test

#data:  fit2$res
#W = 0.97332, p-value = 0.3143
#아까보다 p값 커져 있음

coefficients(fit)
coefficients(fit2)

#> coefficients(fit)
#(Intercept)       speed 
#-17.579095    3.932409 
#> coefficients(fit2)
#(Intercept)       speed 
#1.2770502   0.3224125 


names(fit)

residuals(fit)
residuals(fit2)

summary(fit)
summary(fit2)

#Call:
#  lm(formula = sqrt.dist ~ speed, data = data2)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-2.0684 -0.6983 -0.1799  0.5909  3.1534 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  1.27705    0.48444   2.636   0.0113 *  
#  speed        0.32241    0.02978  10.825 1.77e-14 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 1.102 on 48 degrees of freedom
#Multiple R-squared:  0.7094,	Adjusted R-squared:  0.7034 
##F-statistic: 117.2 on 1 and 48 DF,  p-value: 1.773e-14 <<커져 있음




##다중선형회귀분석

#blackbox
#whitebox (열어보기 쉬운 것)



#Boston Housing Data
#부동산 가격 예측하기

library(readxl)
BostonHousing <- read_excel("BostonHousing.xls")

library(dplyr)
data <- tbl_df(BostonHousing)
data
#tbl_df() : tibble 인 것 같은데 ㅇㅇ
#function (data) 
#{
#  as_tibble(data, .name_repair = "check_unique")
#}

glimpse(data)


#install.packages("psych")
library(psych)

pairs.panels(data[names(data)])
#그림과 숫자 모두 보여줌
head(data[names(data)])

names(data)


#data <- data[ , -15]


names(data) <- tolower(names(data))
#소문자로 바꿔주기(R말고 다른 곳도 공통적으로 쓰임)
data %>% 
  names() %>% 
  tolower()

#반대로 대문자로 바꾸기
#names(data) <- toupper(names(data))
#names(data)

data_lm_full <- lm(medv~ ., data)
#medv: Boston 주택 가격
#lm(y~ ., data) 점은 전부다 할 때 

data_lm_full
#Call:
#  lm(formula = medv ~ ., data = data)
#
#Coefficients:
#(Intercept)     crim           zn        indus  
#3.646e+01   -1.080e-01    4.642e-02    2.056e-02  
#chas          nox           rm          age  
#2.687e+00   -1.777e+01    3.810e+00    6.922e-04  
#dis           rad          tax          ptratio  
#-1.476e+00    3.060e-01   -1.233e-02   -9.527e-01  
#b           lstat  
#9.312e-03   -5.248e-01  

#lstat 과 medv와의 상관관계가 -0.74 임
#rm과 medv와 상관관계 0.70

summary(data_lm_full)
#Pr(>|t|)  
#(Intercept) ***
#  crim        ** 
#  zn          ***
#  indus          
#chas        ** 
#  nox         ***
#  rm          ***
#  age            
#dis         ***
#  rad         ***
#  tax         ** 
#  ptratio     ***
#  b           ***
#  lstat       ***
#Residual standard error: 4.745 on 492 degrees of freedom
#Multiple R-squared:  0.7406,	Adjusted R-squared:  0.7338 
#F-statistic: 108.1 on 13 and 492 DF,  p-value: < 2.2e-16
#별 없는 거는 무시해도 됨, 별 세개는 유의미
#adjusted R-squared: 수정된 알제곱
  #열 증가할수록 알제곱값 달라짐. 
  #변수와 반비례로 패널티를 줘서 나온 값
  #기본 알제곱보다 항상 값이 작다다
#p-value: < 2.2e-16
#p값 매우 작으므로 상당히 타당하다




pkg_name_vec <- c("mlbench", 
                  "dplyr",
                  "psych",
                  "MASS",
                  "glmnet",
                  "reshape2",
                  "Rfast")



#  #  #  #
#        #
##      ##
## 오후 ##
##      ##
#        #
#  #  #  #

#순도 높이도록 한다
#엔트로피를 낮추는 방향으로 한다
#정보를 더 얻는 방향으로 한다. 지니지수를 가지고 나눈다

#random forest = decision tree *n

#boosting = 체로 거르고 거르고 거르다
            #(가중치를 준다, 안된 것들만 집중적으로 다룸)

#잘 정리되는 방향으로 질문을 던져버려야

#빅데이터 흐름을 타면서 의사결정나무 변형 형태가 주로 쓰인다

#반응변수가 범주형
#카이제곱 통계량의 p
#지니지수
#엔트로피 지수


#재귀적 분기 recursive partitioning
#가지치기 pruning = overfitting 방지
#drop out(솎아주기)

apple_df <- read.csv("apple.csv", stringsAsFactors = F)

summary(apple_df)

glimpse(apple_df)
#각 열마다 단위가 다르므로 표준화/정규화 필요

boxplot(weight ~ model, data = apple_df, ylab = "무게")
#미시마가 제일 무거움, 홍로에 이상치 있음

boxplot(sugar ~ model, data = apple_df, ylab = "당도")

boxplot(acid ~ model, data = apple_df, ylab = "산도")


library(ggplot2)

k <- ggplot(apple_df, 
            aes(factor(color), 
                fill = factor(model)))

k + geom_bar()


k + geom_bar(position = "dodge")


##


str(iris)

glimpse(iris)

levels(iris$Species)

table(iris$Species)

##


library(caret)

#트레인데이터와 파티션 데이터 분리

#추출할 위치 정보를 벡터로 반환 받음 (list = F)
#각 종류별로 80% 도출
iris_row_idx <- createDataPartition(iris$Species,
                                    p = 0.8, list = F)

#데이터 프레임에서 추출할 행번호 얻음
str(iris_row_idx)
View(iris_row_idx)
#ctrl f11

#추출한 위치 정보 활용해 iris 데이터셋에서 훈련 데이터 추출
iris_train_data <- iris[iris_row_idx, ]

#추출한 데이터 확인
str(iris_train_data)

#꽃 종류별 데이터 수 확인

table(iris_train_data$Species)


###

#테스트 데이터 추출(iris_row_idx 제외한 행 데이터 추출
#벡터 내 존재하는 인덱스를 제외하라는 의미는 '-' 기호 이용)
iris_test_data <-  iris[-iris_row_idx, ]

#데이터 확인
str(iris_test_data)

#테스트 데이터 확인(꽃 종류별로 균일하게 10개씩 총 30건 추출)
table(iris_test_data$Species)

#출력 데이터 확인
summary(iris_train_data)

summary(iris_test_data)

###


library(rpart)

#분류분석 rpart 함수 실행
#iris_train_data의 모든 항목을 넣기 위해 '.' 사용
#성분이 두 개로 나뉠 때까지
iris_rpart_result <- rpart(Species~.,
                           data = iris_train_data,
                           control = rpart.control(minsplit = 2))


#분류분석 결괏값 출력
iris_rpart_result

#install.packages("rpart.plot")
library(rpart.plot)

#의사결정 트리 그래프 출력
rpart.plot(iris_rpart_result)

#CP값 조회

iris_rpart_result$cptable

iris_rpart_result
#1) root 120 80 setosa (0.3333333 0.3333333 0.3333333)  

#2) Petal.Length< 2.45 40  0 setosa (1.0000000 0.0000000 0.0000000) * #빨강

#3) Petal.Length>=2.45 80 40 versicolor (0.0000000 0.5000000 0.5000000) #67% 

#6) Petal.Width< 1.75 45  5 versicolor (0.0000000 0.8888889 0.1111111) #38%

#12) Petal.Length< 4.95 40  1 versicolor (0.0000000 0.9750000 0.0250000) #33%

#24) Petal.Width< 1.65 39  0 versicolor (0.0000000 1.0000000 0.0000000) * #31-32%

#25) Petal.Width>=1.65 1  0 virginica (0.0000000 0.0000000 1.0000000) * #초록1-2%

#13) Petal.Length>=4.95 5  1 virginica (0.0000000 0.2000000 0.8000000)  #초록 4%

#26) Petal.Width>=1.65 1  0 versicolor (0.0000000 1.0000000 0.0000000) * #회색1%

#27) Petal.Width< 1.65 4  0 virginica (0.0000000 0.0000000 1.0000000) * #초록 3%

#7) Petal.Width>=1.75 35  0 virginica (0.0000000 0.0000000 1.0000000) * # 초록 29%


iris_rpart_result$cptable

#가지치기
iris_prune_tree <- prune(iris_rpart_result, cp = 0.0125)

#Decision Tree 그리기
rpart.plot(iris_prune_tree)

#> iris_rpart_result$cptable
#CP nsplit rel error xerror       xstd
#1 0.5000      0    1.0000 1.1875 0.05560978
#2 0.4375      1    0.5000 0.6125 0.06730489
#3 0.0375      2    0.0625 0.1375 0.03951200
#4 0.0125      3    0.0250 0.0500 0.02457980
#5 0.0100      5    0.0000 0.0500 0.02457980 <<<이거 제거한 값값

iris_prune_tree

#할 때마다 퍼센티지 다름
#1) root 120 80 setosa (0.3333333 0.3333333 0.3333333)  

#2) Petal.Length< 2.45 40  0 setosa (1.0000000 0.0000000 0.0000000) * #빨강

#3) Petal.Length>=2.45 80 40 versicolor (0.0000000 0.5000000 0.5000000) #67% 

#6) Petal.Width< 1.75 45  5 versicolor (0.0000000 0.8888889 0.1111111) #38%

#12) Petal.Length< 4.95 40  1 versicolor (0.0000000 0.9750000 0.0250000) #33%

#13) Petal.Length>=4.95 5  1 virginica (0.0000000 0.2000000 0.8000000)  #초록 4%

#7) Petal.Width>=1.75 35  0 virginica (0.0000000 0.0000000 1.0000000) * # 초록 29%


#테스트 데이터 확인 - 훈련 데이터와 칼럼명이 같아야 함(단 종속변수 칼럼은 없어)
str(iris_test_data)

#predict

predict(iris_rpart_result, iris_test_data, type = "class")

#실제 값과 예상 값을 한눈에 볼 수 있게 데이터 프레임 만들기
#실제값 actual, expect 예상값
actual <- iris_test_data$Species
expect <- predict(iris_rpart_result, iris_test_data, type = "class")

#데이터 프레임 만들기
iris_predict_df <- data.frame(actual, expect)

#결괏값 확인
iris_predict_df

#혼동행렬 만들기기
table(iris_predict_df)

#위 퍼센티지 다를 때마다 7-9 왔다갔다 함
#expect
#actual       setosa versicolor virginica
#setosa         10          0         0
#versicolor      0          9         1
#virginica       0          0        10




      ###
    #     #
 ##        ##
##   apple  ## 
##          ##
 ##        ##
   #      #
     ####
  
library(caret)

apple <- read.csv("apple.csv", stringsAsFactors = F)

#트레인데이터와 파티션 데이터 분리

#추출할 위치 정보를 벡터로 반환 받음 (list = F)
#각 종류별로 80% 도출
apple_row_idx <- createDataPartition(apple$model,
                                    p = 0.8, list = F)

#데이터 프레임에서 추출할 행번호 얻음
str(apple_row_idx)
View(apple_row_idx)
#ctrl 11

#추출한 위치 정보 활용해 apple 데이터셋에서 훈련 데이터 추출
apple_train_data <- apple[apple_row_idx, ]

#추출한 데이터 확인
str(apple_train_data)

#사과 종류별 데이터 수 확인

table(apple_train_data$model)


###

#테스트 데이터 추출(apple_row_idx 제외한 행 데이터 추출
#벡터 내 존재하는 인덱스를 제외하라는 의미는 '-' 기호 이용)
apple_test_data <-  apple[-apple_row_idx, ]

#데이터 확인
str(apple_test_data)

#테스트 데이터 확인(꽃 종류별로 균일하게 10개씩 총 30건 추출)
table(apple_test_data$model)

#출력 데이터 확인
summary(apple_train_data)

summary(apple_test_data)

###


library(rpart)

#분류분석 rpart 함수 실행
#apple_train_data의 모든 항목을 넣기 위해 '.' 사용
#성분이 두 개로 나뉠 때까지
apple_rpart_result <- rpart(model~.,
                           data = apple_train_data,
                           control = rpart.control(minsplit = 2))


#분류분석 결괏값 출력
apple_rpart_result

#install.packages("rpart.plot")
library(rpart.plot)

#의사결정 트리 그래프 출력
rpart.plot(apple_rpart_result)

#CP값 조회

apple_rpart_result$cptable

apple_rpart_result
#1) root 20 16 로얄후지 (0.2 0.2 0.2 0.2 0.2)  
#2) acid< 0.525 16 12 로얄후지 (0.25 0.25 0.25 0.25 0)  
#4) weight>=361 8  4 로얄후지 (0.5 0.5 0 0 0)  
#8) color=적색 4  0 로얄후지 (1 0 0 0 0) *
#9) color=홍색 4  0 미시마 (0 1 0 0 0) *
#5) weight< 361 8  4 아오리 (0 0 0.5 0.5 0)  
#10) sugar< 13.35 3  0 아오리 (0 0 1 0 0) *
#11) sugar>=13.35 5  1 홍로 (0 0 0.2 0.8 0)  
#22) X< 8.5 2  1 아오리 (0 0 0.5 0.5 0)  
#44) X>=7 1  0 아오리 (0 0 1 0 0) *
#45) X< 7 1  0 홍로 (0 0 0 1 0) *
#23) X>=8.5 3  0 홍로 (0 0 0 1 0) *
#3) acid>=0.525 4  0 홍옥 (0 0 0 0 1) *



#가지치기
apple_prune_tree <- prune(apple_rpart_result, cp = 0.0625)

#Decision Tree 그리기
rpart.plot(apple_prune_tree)

#> apple_rpart_result$cptable
#CP nsplit rel error xerror      xstd
#1 0.2500      0    1.0000 1.2500 0.0000000
#2 0.1875      3    0.2500 1.2500 0.0000000
#3 0.0625      4    0.0625 0.3125 0.1210307
#4 0.0100      5    0.0000 0.3125 0.1210307 <<<이거 제거한 값값

apple_prune_tree



#테스트 데이터 확인 - 훈련 데이터와 칼럼명이 같아야 함(단 종속변수 칼럼은 없어)
str(apple_test_data)

#predict

predict(apple_rpart_result, apple_test_data, type = "class")

#실제 값과 예상 값을 한눈에 볼 수 있게 데이터 프레임 만들기
#실제값 actual, expect 예상값
actual <- apple_test_data$model
expect <- predict(apple_rpart_result, apple_test_data, type = "class")

#데이터 프레임 만들기
apple_predict_df <- data.frame(actual, expect)

#결괏값 확인
apple_predict_df

#혼동행렬 만들기기
table(apple_predict_df)

#expect
#actual     로얄후지 미시마 아오리 홍로 홍옥
#로얄후지        1      0      0    0    0
#미시마          0      1      0    0    0
#아오리          0      0      0    1    0
#홍로            0      0      0    1    0
#홍옥            0      0      0    0    1






      #
     ##
    ###
   ####
#랜덤 포레스트#


install.packages("randomForest")
require(randomForest)



ind <- sample(2, nrow(iris), replace = T, prob = c(0.7, 0.3))


trainData <- iris[ind == 1, ]
testData <- iris[ind == 2, ]


rf <- randomForest(as.factor(Species)~., 
                   data = trainData, 
                   ntree = 100,
                   procimity = T,
                   importance = T)

print(rf)

predict(rf)

table(predict(rf), trainData$Species)

irisPred <- predict(rf, newdata = testData)
irisPred

table(irisPred, testData$Species)


#번외
tf <- randomForest(as.factor(Species)~., 
                   data = testData, 
                   ntree = 100,
                   procimity = T,
                   importance = T)
predict(tf)
table(predict(tf), testData$Species)

irisPred1 <- predict(tf, newdata = trainData)
irisPred1
table(irisPred1, trainData$Species)

##값이 왜 다르게 나올까

#> table(predict(rf), trainData$Species)

#setosa versicolor virginica
#setosa         34          0         0
#versicolor      0         36         5
#virginica       0          3        30
#> table(irisPred1, trainData$Species)

#irisPred1    setosa versicolor virginica
#setosa         34          0         0
#versicolor      0         37         4
#virginica       0          2        31
#> table(irisPred, testData$Species)

#irisPred     setosa versicolor virginica
#setosa         16          0         0
#versicolor      0         11         0
#virginica       0          0        15
#> table(predict(tf), testData$Species)

#setosa versicolor virginica
#setosa         16          0         0
#versicolor      0         10         0
#virginica       0          1        15



#Bagging Bootstrap aggregating %=% 재추출(resampling)
#랜덤 포레스트는 배깅에서 발전한 것
#랜덤포레스트 다음으로 많이 쓰이는 게 부스팅이다!
#부스팅은 변형 가능한 모든 걸 합치는 것
#부스팅 => 체에 거르기 
#부스팅 알고리즘 중에 xgboost 가장 많이 씀

#스택킹은 열거한 알고리즘을 쌓는 것



#AdaBoost



#using AdaBoost
install.packages("C50")
library(C50)
library(caret)

credit <- read.csv("credit.csv")
#default는 파산할 거다



m_C50_bst <- C5.0(default ~., data = credit, trials = 100)

#Using AdaBoost.M1
install.packages("adabag")
library(adabag)

#Create a Adaboost.M1 model
set.seed(300)

m_adaboost <- boosting(default ~., data = credit)
p_adaboost <- predict(m_adaboost, credit)

head(p_adaboost$class)

p_adaboost$confusion

#create 
set.seed(300)

adaboost_cv <- boosting.cv(default~., data = credit)
adaboost_cv$confusion





##Bagging



#install.packages("ipred")
library(ipred)

set.seed(300)
mybag <- bagging(default~., data = credit, nbagging = 25)

credit_pred <- predict(mybag, credit)

table(credit_pred, credit$default)

#estimate performance of ipred bagged trees

#library(caret)

#set.seed(300)

#ctrl <- trainControl(method = "cv", number = 10)
#train(default~., data = credit, method = "treebag",)





##mushrooms




mushrooms <- read.csv("mushrooms.csv")
glimpse(mushrooms)
table(mushrooms$type)

#glimpse(credit)

#library(RWeka)

#type도 해보고 bruises도 해보고

mush_C50_bst <- C5.0(bruises ~., data = mushrooms, trials = 100)

#Using AdaBoost.M1
#install.packages("adabag")
library(adabag)

#Create a Adaboost.M1 model
set.seed(300)

mush_adaboost <- boosting(bruises ~., data = mushrooms)
predict_mush_adaboost <- predict(mush_adaboost, mushrooms)

head(predict_mush_adaboost$class)

predict_mush_adaboost$confusion

#create 
set.seed(300)

adaboost_cv <- boosting.cv(bruises~., data = mushrooms)
adaboost_cv$confusion


