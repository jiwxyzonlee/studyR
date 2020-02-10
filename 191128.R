
###복습차원 연습문제


#1 관측치의 개수와 변수의 개수

str(airquality)


#2 요약 통계량

summary(airquality)

#3 오존이 32보다 크고, 바람은 9보다 작은 날

airquality %>% 
  filter(Ozone>32&Wind<9) %>% 
  select(Month, Day)

airquality %>% 
  filter(Ozone>32&Wind<9) %>% 
  select(Day)

airquality %>% #답
  filter(c(Ozone>32&Wind<9)) %>% 
  summarise(n())# <- 이거 기억하기

#mean() : 평균
#sd() : 표준편차
#sum() : 합계
#median() : 중앙값
#min() : 최솟값
#max() : 최댓값
#n() : 빈도

#4 오존,바람,온도, 월 골라서 80도 이상,오존 내림차순, 6행

airquality %>% 
  filter(c(Ozone, Wind, Temp, Month)) %>% 
  select(Temp>=80) %>% 
  arrange(desc(Temp>=80)) %>% 
  head() #틀림

airquality %>% 
  select(Ozone, Wind, Temp, Month) %>% 
  filter(Temp>=80) %>% 
  arrange(desc(Ozone)) %>% #답
  head()

airquality %>% 
  filter(Temp>=80) %>% 
  select(Ozone, Wind, Temp, Month) %>% 
  arrange(desc(Ozone)) %>% #답
  head()

#5 ozone, wind, temp, month 골라서 월별로 평균 바람속도

airquality %>% 
  group_by(Month) %>% 
  select(Ozone,Wind,Temp,Month) %>% 
  summarise(평균바람속도=mean(Wind))

airquality %>% 
  select(Ozone,Wind,Temp,Month) %>%  #답
   group_by(Month)%>% 
  summarise(평균바람속도=mean(Wind))

#6 Wind이 10보다 큰 데이터 중 월별로 평균 온도

airquality %>% 
  group_by(Month) %>% 
  filter(Wind>10) %>% 
  summarise(평균온도=mean(Temp))

airquality %>% 
  filter(Wind>10) %>% #답
  group_by(Month) %>% 
  summarise(평균온도=mean(Temp))




#
##
#
##






install.packages("data.table")
library(data.table)

citation("data.table")

getwd()

data <- fread("gamedata.csv") #fread #빨리 읽기 fast read

dim(data)
head(data)
summary(data)
glimpse(data)


install.packages("readr")

library(readr)

data1 <- read_csv("gamedata.csv") #fread와 속도 차이 비교

#read.csv보다 read_csv가 더 빨리 읽는다

dim(data1)
head(data1)
summary(data1)
glimpse(data1)


data <- fread("conveniencestore.csv", encoding = "UTF-8")
#UTF-8은 대문자로!
# encoding = (중요!)

dim(data)
glimpse(data)
head(data)

data1 <- read_csv("conveniencestore.csv")
# read_csv는 알아서 인코딩

head(data1)

summary(data1)
summary(data)
#fread보다 read_csv가 summary 더 빠름

rm(list=ls())




data <- sample(4, 20, replace=T)
data

data[3]

table(data)

hist(data) #히스토그램 함수 hist()
hist(table(data))


barplot(data)       #scan() 함수 기억하기(console에 입력했던)
barplot(table(data))

table(data) %>% barplot()

pie(data)
pie(table(data))

table(data) %>% pie() #더 간단쓰

data %>% 
  table() %>% 
  barplot()            #더 논리적

data %>% 
  table() %>% 
  pie()


#abline: 기울기가 있어서 이렇게 부름 y= a + bx


#화면 분할
par(mfrow=c(2,2))

fit <- lm(dist~speed, data = cars)
fit
summary(fit)

#화면창 보기
plot(fit)


library(ggplot2)

par(mfrow=c(1,1))

mtcars
str(mtcars)

#cyl = cylinder cylinder num을 factor
#(카테고리,범주형, 요인형)으로
  
mtcars$cyl <- as.factor(mtcars$cyl)
str(mtcars)
#num이 factor로 바뀐 걸 볼 수 있다!

#번외
mtcars$cyl <- as.numeric(mtcars$cyl)

#mpg~hp = y~x (y값은 따른다, x값에 의해서)
plot(mpg~hp, data=mtcars, col=cyl, 
     pch=c(4,6,8)[mtcars$cyl],cex=1.2)

legend("topright", legend=levels(mtcars$cyl),
       pch = c(4,6,8),
       col=levels(mtcars$cyl))



library(ggplot2)

ggplot(mtcars, 
       aes(x=hp, y=mpg, 
           color=cyl, shape=cyl))+geom_point(size=3)

#'data=' 써주자 
#mapping x와 y축 값 입력, aes(x=,y=)  (aesthetic)


mpg
dim(mpg)
names(mpg)


# 1단계 (축) - 바닥을 만듦
ggplot(data=mpg, aes(x=displ, y= hwy))

ggplot(mpg, aes(displ, hwy))

# 배경에 산점도 추가 - 점(point)을 찍자(point) 
ggplot(data=mpg, aes(x = displ, y = hwy)) + geom_point()


# x축 범위 3~6으로 지정
ggplot(data=mpg, 
       aes(x = displ, y = hwy)) + geom_point() +  xlim(3,6)
##x축 제한해서 보기


#x축 범위 3~6으로 지정, y축 10~30으로 지정
ggplot(data=mpg, 
       aes(x = displ, y = hwy)) + geom_point() +  xlim(3,6)+
  ylim(10,30)


str(mpg)

glimpse(mpg)

dplyr::glimpse(mpg) 

#:: library 안 하고 바로 dplyr의 glimpse 쓰게끔
#library <-> detach

dplyr::

head(mpg,3)
names(mpg)

#drv의 f: front(전륜구동) r(후륜구동) 4(4륜구동)
#mpg의 class?

ggplot(data=mpg, 
       aes(x = displ, y = hwy, color = drv)) + 
  geom_point(size = 2)
#color = drv가 준 변화!
#연비 바로 전륜/후륜/4륜 나눠서 시각적으로 확인

#번외
ggplot(data=mpg, 
       aes(x = displ, y = hwy, color = drv, shape = drv)) + 
  geom_point(size = 2)
#shape도 마찬가지


ggplot(data=mpg, 
       aes(x = displ, y = hwy, color = cty)) + 
  geom_point(size = 2)
#cty

mpg$cty

#번외
ggplot(data=mpg, 
       aes(x = displ, y = hwy, color = cty, shape = drv)) + 
  geom_point(size = 2)

ggplot(data = mpg,
       aes( x = displ, y = hwy))+
  geom_point(aes(color = class))
#color가 들어가는 함수 위치 차이는? 내일
#근데 전자가 더 편하긴 함

#비교
ggplot(data = mpg,
       aes( x = displ, y = hwy, color = class))+
  geom_point(size=2)



#코드 재사용 용이하게 하기

p <- ggplot(data=mpg,
            aes(x = displ, y = hwy))

p + geom_point(aes(color = class))

q <- geom_point(aes(color = class))

p + q



#shape

ggplot(data = mpg,
       aes( x = displ, y = hwy))+
  geom_point(size = 2) +
  geom_smooth(method="lm")
#방법 추가해서 변화 잘 보이게 하기:+ geom_smooth(method="")

ggplot(data = mpg,
       aes( x = displ, y = hwy, color = drv))+
  geom_point(size = 2) +
  geom_smooth(method="lm")
#방법 추가해서 변화 잘 보이게 하기:+ geom_smooth(method="")

ggplot(data = mpg,
       aes( x = displ, y = hwy, color = drv, shape = drv))+
  geom_point(size = 2) +
  geom_smooth(method="lm")
#방법 추가해서 변화 잘 보이게 하기:+ geom_smooth(method="")




# 배경 추가하기: + theme_xxxx()

p2 <- ggplot(data = mpg,
       aes( x = displ, y = hwy, color = drv, shape = drv))+
  geom_point(size = 2)


p3 <- p2 + geom_smooth(method="lm")



p3 + theme_dark()

p3 + theme_bw()

p3+ theme_classic()

p3 + theme_gray()

p3 + theme_linedraw()

p3 + theme_light()

p3 + theme_minimal()

p3 + theme_void()

p3 + theme_test()


### 배경 테마 더!

install.packages("ggthemes")
library(ggthemes)

p2 +theme_wsj()

p2 +theme_economist()

p2 +theme_excel_new()

p2 +theme_fivethirtyeight()

p2 +theme_solarized_2()

p2 +theme_solarized()

p2 +theme_stata()

p2 +theme_wsj()

p2 +theme_tufte()

p2 +theme_map()

p2 +theme_pander()



##라벨 +labs(title="", x="", y="")

p2 <- ggplot(data = mpg,
             aes( x = displ, y = hwy, color = drv, 
                  shape = drv))+
  geom_point(size = 2)


p3 <- p2 + geom_smooth(method="lm")



p3 + 
  labs(title="<배기량에 따른 고속도로 연비 비교>",
       x = "배기량", y = "연비")



##패싯  +facet_grid(drv~cut)
#면 분할 방법
#변수끼리의 관계를 나타냄


d <- ggplot(data = mpg,
       aes( x = displ, y = hwy, color = drv))+
  geom_point()

d

d + facet_grid(drv~.) # drv by 행(3)

d + facet_grid(.~cyl) # cyl by 열(4)

d + facet_grid(drv~cyl) #drv 3,  cyl 4 = 12면


## 참고
#pairs
xx <- c(1,2,3,4,5)
yy <- c(20,13,40,50,60)
zz <- c(10,5,100,20,10)
c <- matrix(c(xx,yy,zz),5,3)
c
pairs(c)
#칼럼수가 많고 변수가 많고 수치형일 때 pair함수 사용


d + facet_grid( ~ class)

d + facet_wrap( ~ class)
#행 또는 열의 개수가 많아지면 wrap

d + facet_wrap( ~ class, nrow = 2) #행 바꾸기

d + facet_wrap( ~ class, ncol = 4) #열 바꾸기



## jitter

#slight irregular movement, variation, or unsteadiness,
#especially in an electrical signal or electronic device.
#뭉뚱그림

dplyr::glimpse(mpg)

#

ggplot(data = mpg,
       aes( x = displ, y = hwy, 
            color = drv))+
  geom_point(size = 2)

ggplot(data = mpg,
       aes( x = displ, y = hwy, 
            color = drv))+
  geom_point(size = 2, 
             position = "jitter")


#번외
ggplot(data = mpg,
       aes( x = displ, y = hwy, 
            color = drv,
            position = "jitter"))+
  geom_point(size = 2)  #별 차이 없음, 점에만 들어가나봐



###

p1 <- ggplot(data = mpg,
       aes( x = displ, y = hwy, 
            color = drv))

p1

p1 + geom_point(size=2)

p1 + geom_line()

p1 + geom_point(size = 2) + geom_line()




###y값이 없다면 빈도가 y가 됨 (bar graph)

ggplot(data = mpg, aes(x = displ)) + geom_bar()

ggplot(data = mpg, aes(x = displ, fill=factor(drv))) + 
  geom_bar()

ggplot(data = mpg, aes(x = displ, fill=factor(drv))) + 
  geom_bar(position = "dodge")


## filling bar graph


ggplot(data = mpg, aes(x = displ, fill=factor(drv))) + 
  geom_bar(position = "fill")

ggplot(data = mpg, aes(x = displ, fill=factor(drv))) + 
  geom_bar(position = "fill") +
  facet_wrap( ~ class) #여러 면 많을 때 한꺼번에

#번외
ggplot(data = mpg, aes(x = displ, fill=factor(drv))) + 
  geom_bar(position = "fill") +
  facet_grid( ~ class) #(열로만 면 나눔)



###히스토그램

ggplot(data=mpg, aes(x=displ)) +geom_histogram()

ggplot(data=mpg, aes(x=displ)) +geom_histogram(fill="blue")

ggplot(data=mpg, aes(x=displ)) +
  geom_histogram(fill="blue", binwidth = 0.1)



###scatterplot3d (원래 어제 배워야 할 것)

mtcars
?mtcars
str(mtcars)
names(mtcars)

plot(mtcars)
attach(mtcars)

glimpse(mtcars)

wt
plot(wt)
mpg
plot(wt,mpg)
plot(wt,mpg,main="wt와 mpg의 관계")
plot3d(wt,disp,mpg) #돌아감



install.packages("scatterplot3d")
install.packages("rgl")

library(scatterplot3d)
library(rgl)

scatterplot3d(wt, disp, mpg,
              main = "3D Scatter Plot")

scatterplot3d(wt, disp, mpg, pch=16, 
              highlight.3d = TRUE, type = "h",
              main = "3D Scatter Plot")

plot3d(wt, disp, mpg,
       main = "wt Vs mpg disp", col="red", size = 10)

plot3d(wt, disp, mpg,
       main = "wt Vs mpg disp", col="red", size = 5)
#돌아감



###
###
##

#boxplot


abc <- c(110,300,150,280,310) #시즌별 Baseball 판매현황
def <-  c(180,200,210,190,170) #시즌별 Soccerball 판매현황
ghi <-  c(210,150,260,210,70)  #시즌별 Beachball 판매현황
boxplot(abc,def,ghi)

#어디에 몰려있나 개괄적으로 볼 수 있음


# col : 상자 내부의 색 지정
# names : 각 막대의 이름 지정
# range : 막대의 끝에서 수염까지의 길이를 지정
# width : 박스의 폭을 지정
# notch : TRUE이면 상자의 허리 부분을 가늘게 표시
# horizontal : TRUE이면 상자를 수평으로 그림

boxplot(abc,def,ghi,col=c("yellow", "cyan", "green"), 
        names=c("BaseBall", "SoccerBall ","BeachBall"),
        horizontal = TRUE)

summary(abc) #노랑
summary(def)
summary(ghi)


##Scatterplot01

head(iris) #붓꽃

#basic scatterplot

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point()

#option넣어서 꾸며보기

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(color="red", fill = "blue", shape = 21,
             alpha=0.5, size=6, stroke=2) 
#alpha: 투명도 stroke: 테두리 굵기




#범주별로 색 다르게 넣기, 옵션 넣기
ggplot(iris, 
       aes(x = Sepal.Length, y = Sepal.Width,
                 color=Species, shape=Species)) +
  geom_point(size=3, alpha=0.6) 


ggplot(iris, 
       aes(x = Sepal.Length, y = Sepal.Width,
           color=Petal.Length, size=Petal.Length)) +
  geom_point(alpha=0.6)


##Scatterplot02

head(mtcars)

data = head(mtcars)

#이름 붙이기

ggplot(data, aes(x=wt, y=mpg)) + geom_point() +
  geom_text(label=rownames(data), nudge_x = 0.25,
            nudge_y = 0.25, heck_overlap=T) 
#heck_overlap 겹치기 허용

x11()

#labels 상자 치기
ggplot(data, aes(x=wt, y=mpg)) + geom_point() +
  geom_label(label=rownames(data),nudge_x = 0.25,
             nudge_y = 0.2)

ggplot(data, aes(x=wt, y=mpg, fill=cyl)) + 
  geom_label(label=rownames(data),color="white",size=5)

ggplot(data=iris, aes(x=Sepal.Length, y=Petal.Length)) + 
  geom_point()+
  geom_rug(col="steelblue", alpha=0.1,size=1.5)


###scaterplot04

install.packages("ggExtra")
library(ggExtra)

library(ggplot2)

head(mtcars)


#classic plot

ggplot(mtcars, aes(x = wt, y = mpg, 
                   color = cyl, size = cyl)) +
  geom_point()+
  theme(legend.position = "none")

p <- ggplot(mtcars, aes(x = wt, y = mpg, 
                        color = cyl, size = cyl)) +
  geom_point()+
  theme(legend.position = "none")

ggMarginal(p, type="histogram")


#marginal density
ggMarginal(p, type="density")

#marginal boxplot
ggMarginal(p, type = "boxplot")


#사이즈 변경
ggMarginal(p, type="histogram", size=10)

ggMarginal(p, type="histogram", 
           fill= "slateblue", 
           xparams = list(bins=10),
           yparams = list(bins=9))


#x만 나타내기

ggMarginal(p, margins = 'x', color= "purple", size= 4)


# scatterplot05
# Data

#Dendrogram (중요★)(자주 씀)
#정지는 plot
#반응성 있는 건 Viewer에서 나타남


library(ggplot2)

data=data.frame(
  cond=rep(c("condition_1", "condition_2"),each = 10),
  my_x=1:100 + rnorm(100, sd=9), 
  my_y = 1:100 + rnorm(100, sd = 16))

#표준편차 9로 뽑아내서

ggplot(data, 
       aes(x = my_x, y = my_y)) + 
  geom_point(shape=1)

ggplot(data, 
       aes(x = my_x, y = my_y)) + 
  geom_point(shape=1) + 
  geom_smooth(method = lm, color="red", se = FALSE)

#lm은 직선을 나타냄(선형)
#se=FALSE: 오차범위 허용하지 않도록


#오차범위 허용은 
ggplot(data, 
       aes(x = my_x, y = my_y)) + 
  geom_point(shape=1) + 
  geom_smooth(method = lm, color="red", se = TRUE)


position = "jitter" #갖고 놀아보기


#Create data

a=seq(1,29)+4*runif(29,0.4)

b=seq(1,29)^2+runif(29,0.28)

#무작위 변수
#runif: 난수

par(mfrow=c(2,2))

plot(a,b , pch=20)

plot(a-b, pch = 18)

hist(a, border=F, col=rgb(0.2,0.2,0.8,0.7), main="")
#0.7은 alpha

boxplot(a,col="grey", xlab="a")


# density plot

library(rattle)
install.packages("rattle")

#is.na 결칙치
# !is.na 결칙치 제거
#subset 일부 추출해서 하위 셋

head(weatherAUS)
names(weatherAUS)
View(weatherAUS)

cities <- c("Canberra", "Darwin", "Melbourne", "Sydney")
ds <- subset(weatherAUS, 
             Location %in% cities & ! is.na(Temp3pm))
                #열          #행              #열
#결칙치는 빠져있는 것. 빠져있는 게 있으면 에러,분석불가

View(ds) #생성된 행열 이해 안 되면 보기

p <- ggplot(ds, 
            aes(Temp3pm, colour=Location, fill=Location))
#x만 나온 건가 보다

p <- p + geom_density(alpha=0.55)

p


#참고
exam %>% filter(class==1|class==3|class==5) 
exam %>% filter(class%in%c(1,3,5)) #위와 같음


#유명한 다이아몬드! 깎아낸 자료
data("diamonds")
data(diamonds)
head(diamonds)

# Density of price for each type of cut of the diamond
#히스토그램의 변형

ggplot(data=diamonds, 
       aes(x = price, group = cut, fill= cut)) +
  geom_density(adjust=1.5)

#transparency
ggplot(data=diamonds, 
       aes(x = price, group = cut, fill= cut)) +
  geom_density(adjust = 1.5, alpha = 0.2)

#Stacked(누적)
ggplot(data=diamonds, 
       aes(x = price, group = cut, fill= cut)) +
  geom_density(adjust=1.5, position = "fill")


#create data
x1 = rnorm(100)
x2 = rnorm(100, mean = 2)
par(mfrow = c(2,1))

par(mar=c(0,5,3,3))
plot(density(x1),
     main = "", xlab = "", ylim = c(0,1),
     xaxt = "n", las = 1, col = "slateblue1", lwd = 4)

par(mar=c(5,5,0,3))

plot(density(x2), 
     main = "", xlab = "Value of my variable",
     ylim = c(1,0), las = 1, col = "tomato3", lwd = 4)

##참고
#여백(margin) 추가 함수
#   par(mar=c(0,0,0,0))
# c(bottom, left, top, right) 순서
# The default is c(5, 4, 4, 2) + 0.1

#ylim (y축 범위 설정)



###        ###
###  보충  ###
###        ###


data <- data.frame(
  name=c("north", "south", " south-east", 
         "north-west", "south-west","north-east", 
         "west", "east"),
  val = sample(seq(1,10), 8))

library(dplyr)
install.packages("forcats")
library(forcats)
#fct_reorder는 forcats 패키지에 있음

data %>% mutate(name = fct_reorder(name, val)) %>% 
  ggplot(aes(x = name, y = val)) +
  geom_bar(stat = "identity") +
  coord_flip()
#보통은 데이터값에 따라 내림차순으로 나타남


data %>% mutate(name = fct_reorder(name, desc(val))) %>% 
  ggplot(aes(x = name, y = val)) +
  geom_bar(stat = "identity") +
  coord_flip()



#errorbar

data <- data.frame(name = letters[1:5],
                   value = sample(seq(4,15),5),
                   sd = c(1,0.2,3,2,4))

#letters[1:5] a부터 e까지

ggplot(data) +
  geom_bar(aes(x = name, y = value), 
           stat = "identity",
           fill = "skyblue", alpha = 0.7) +
  geom_errorbar(aes(x = name, 
                    ymin = value-sd, ymax = value+sd),
                width = 0.4, colour = "orange",
                alpha = 0.9, size = 1.3)


ggplot(data) +
  geom_bar(aes(x = name, y = value),
           stat = "identity",
           fill = "skyblue", alpha = 0.5) +
  geom_crossbar(aes(x = name, y = value,
                    ymin = value -sd, ymax = value+sd),
                width = 0.4, colour = "orange", 
                alpha = 0.9, size = 1.3)


ggplot(data) +
  geom_bar(aes(x = name, y = value),
           stat = "identity",
           fill = "skyblue", alpha = 0.5) +
  geom_linerange(aes(x = name, y = value,
                    ymin = value -sd, ymax = value+sd),
                colour = "orange", 
                alpha = 0.9, size = 1.3)


ggplot(data) +
  geom_bar(aes(x = name, y = value),
           stat = "identity",
           fill = "skyblue", alpha = 0.5) +
  geom_pointrange(aes(x = name, y = value,
                     ymin = value -sd, ymax = value+sd),
                 colour = "orange", 
                 alpha = 0.9, size = 1.3)

ggplot(data) +
  geom_bar(aes(x = name, y = value),
           stat = "identity",
           fill = "skyblue", alpha = 0.5) +
  geom_errorbar(aes(x = name, 
                      ymin = value -sd, ymax = value+sd),
                  width = 0.4, colour = "orange", 
                  alpha = 0.9, size = 1.3) +coord_flip()


##dendrogram

dendrapply

##sankey diagram

###
###연습문제###
###

library(dplyr)


kbo <- read.csv("kbo.csv", header = T)

#1 kb 연도별 타율
kbo %>% group_by(연도) %>%   #틀림
  mutate(타율=안타/타수)
#이건 열 추가 느낌이고


kbo %>% group_by(연도) %>% 
  summarise(타율=sum(안타)/sum(타수))

#2 kb데이터를 연도별 타율 가운데 2001년 리그 평균 타율
kbo %>% group_by(연도) %>% 
  summarise(타율=sum(안타)/sum(타수)) %>% 
  filter(연도==2001)
#이건 새로운 행열 도출이고


#3 통산 병살타가 가장 많은 세 개 '팀'
kbo %>% select(병살) %>%    #틀림
  arrange(desc()) %>% head(3)

kbo %>% group_by(팀) %>% 
  summarise(병살=sum(병살)) %>%  arrange(desc(병살)) %>% 
  head(3)


