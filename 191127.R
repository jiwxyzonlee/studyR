
airquality
dim(airquality)
sum()

air <- airquality
air
str(air)

#obs = observations 관측치

a = 1
a # <- object

#dependency

install.packages("dplyr")
citation("dplyr")


library(dplyr)

glimpse()

#int 정수 숫자

#% numeric

str(air)

glimpse(air)
#열 구분 쉽게
#dbl= double(소수점)


#month, day만 뽑아서 air1으로 묶기
air1 <- air[ ,c(5,6)]
air1

glimpse(air1)


#ozone, wind 만 뽑아서 air1으로 묶기
air1 <-air[,c(1,3)]
air1

air1<-air[,c("Ozone", "Wind")]
air1


#20번째 행까지
air1<-air[1:20,]
air1

#6obs까지
head(air1)

#5,6열 제외하고 6obs 출력
air1<-air[,c(-5,-6)]
head(air1)


#행열 이름만 보기
colnames(air)
rownames(air)

names(air)
#열이 뜬다!

#xlsx보다 csv가 더 작아서 자주 사용



x1<-sample(45:50,10, replace=T)
x1


x2<-sample(50,10)
x2

set.seed(1234)
#?????
#The set.seed() function is generated
#through the process of pseudorandom number generator
#that makes every modern computers
#have the same sequence of numbers.
#If we don't use set.seed() function,
#we will all have different sequence of numbers.


sample(153,15)
#153개 중 15개 랜덤

air[sample(153,15),]

index<-sample(153,15)
index
#뽑아낸다
air[index,]
air(index,)

n #(개수)
nrow(air) #행의 개수

sample(nrow(air),15)

dim(air)

a<-sample(153,15)
a
a[3] 
#a 모두 말고 세번째 것만

sample(153,15)
sample(153,15)[3]

dim(air)
dim(air)[1]

#alt -  는 <-
# <- <- <- <- <- <- 

index <- sample(153,153*0.7)
index
index<-sample(nrow(air),nrow(air)*0.7)
index

train <- air[index,] 
test <- air[-index,] #앞 70퍼 빼고 테스트
test

ls()
rm(a)
ls()
rm(ata1,x1,x2,x3)
ls()
rm(list=ls()) #객체 모두 지우기

#모르면 R가서 help(sample) 혹은 ?sample 넣기
#아니면 원하는 단어에 커서 놓고 f1(이게 더 편함)

sample()
rm()


#plyr = apply R

#5 verbs


library(dplyr)
exam <- read.csv("csv_exam.csv")
exam #filter는 조건을 적어준다

exam %>% filter(class==1) #1반(열)만(조건맞는 행뽑기)
exam %>% filter(class!=1) #1반 아닌 경우만 (class!=1)
exam %>% filter(math>50) #수학50점 초과
exam %>% filter(english>=80)
exam %>% filter(class==1&math>=50) # &교집합

exam %>% filter(class!=1&math>=50) # 번외

exam %>% filter(math>=90|english>=90)

#ctrl + shift + m = %>% (expand to matching)


#The infix operator %>% is not part of base R,
#but is in fact defined by the package magrittr(CRAN)
#and is heavily used by dplyr (CRAN).

#It works like a pipe,
#hence the reference to Magritte's famous painting
#The Treachery of Images.

library(magrittr)

library(dplyr)

airquality %>% filter(Day>28) #Pipe 예제

airquality %>% filter(Day>28) %>% filter(Month==9)
#shift entr %>% 뒤에서 해야 먹힘

# &로 줄이는 것도 가능

airquality %>% filter(Day>28&Month==9)

exam %>% filter(class==1|class==3|class==5) 
exam %>% filter(class%in%c(1,3,5)) #위와 같음


exam %>% filter(class%in%c(1,3)&english>80)

class1 <- exam %>% filter(class==1)
mean(class1$math)



exam %>% select(math) #df로 나타남

exam$math # $는 벡터로 나열된 거임

exam %>% select(class,math,english)
exam %>% select(-math)

select(exam,class,math,english,science) #원래 방법
exam %>% select(class,math,english,science) #이상적

exam %>% filter(class==1) %>% select(english) #o
exam %>% select(english) %>% filter(class==1) #x

#Error in class == 1 : 
#atomic과 리스트 타입들에 대해서만
#비교(1)가 가능합니다 ??????

#가독성 shift enter %>% 뒤에서

exam %>% arrange(math) #math 오름차순

exam %>% arrange(desc(math)) #math 내림차순

exam %>% arrange(class, math)
#우선 class 오름차순, 그리고 math 내림차순


#파생변수 추가하기
#mutate 바꿔줌
exam %>% 
  mutate(total=math+english+science) %>% 
  head  #total이라는 열 추가

exam %>% 
  mutate(total=math+english+science,
         mean = (math+english+science/3)) %>% head

#pass  ifelse조건 참, 빠질 때
#fail ifelse조건 거짓, 안 빠질 때
exam %>% 
  mutate(test=ifelse(science>=60,"pass","fail")) %>% 
  head

exam %>% 
  mutate(total=math+english+science) %>% 
  arrange(total) %>% head

#n은 아무데나 넣을 수가 없음(mean은 가능하나)

mean(exam$math) #이렇게 해도 되는데 안되는 이유는

#summarise는 반드시 group by (class)로 해야 함
#클래스별로 그룹을 지었을 때 

exam %>% summarize(mean_math=mean(math))

exam %>% group_by(class) %>% 
  summarise(mean_math=mean(math))

exam %>% 
  group_by(class) %>% #class별로 분리
  summarise(mean_math=mean(math), #math 평균
            sum_math=sum(math), #math 합계와 중앙값
            median_math=median(math),n=n()) 
#n은 학생수

mpg %>% 
  group_by(manufacturer) %>% #회사별 분리
  filter(class=="suv") %>%  #suv추출
  mutate(tot=(cty+hwy)/2) %>%  #통합연비
  summarise(mean_tot=mean(tot)) %>% #통합연비 요약
  arrange(desc(mean_tot)) %>% head(5) #일부 출력



##
##
### R을 활용한 data visualization

citation("ggplot2")

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
plot3d(wt,disp,mpg)



install.packages("scatterplot3d")
install.packages("rgl")

library(scatterplot3d)
library(rgl)

#R graphic Device
x11()

#2x3 화면 분할
par(mfrow=c(2,3))
#mf: multi-figure
#row: 행부터 채워짐

#type옵션 다르게 하면서 plot실행
plot(0:6,0:6,main="default")
plot(0:6,0:6, type="b",main="type=\"b\"")
plot(0:6,0:6, type="c",main="type=\"c\"")
plot(0:6,0:6, type="o",main="type=\"o\"")
plot(0:6,0:6, type="s",main="type=\"s\"")
plot(0:6,0:6, type="S",main="type=\"S\"")

#번외
plot(0:6,0:6, type="p",main="type=\"p\"")
plot(0:6,0:6, type="l",main="type=\"l\"")
plot(0:6,0:6, type="h",main="type=\"h\"")
plot(0:6,0:6, type="n",main="type=\"n\"")


x11()


par(mfrow=c(1,1)) #1x1 분할
#막대그래프로 나타낼 객체 만들기
x <- c(38,52,24,8,3)
barplot(x)

#x변수 이름 설정하기
names(x) <- c("Excellent","Very Good",
              "Good","Fair","Poor")
barplot(x)
x
pie(x)

names(x) <- c("Excellent","Very Good",
                "Good","Fair","Poor")
barplot(x)
barplot(x, xlab="수준",ylab="점수")
barplot(x, xlab="수준",ylab="점수",col="blue")
barplot(x, xlab="수준",ylab="점수",col="blue", horiz=TRUE)
barplot(x, xlab="수준",ylab="점수",
        col=c("blue","light blue","red","yellow","grey"), 
        horiz=TRUE)
#색깔 넣기! 유후!


#객체 만들기2
y <- scan()
barplot(y)

table(y) #데이터 도수 표현



#table함수를 사용하여 범주화하기
barplot(table(y),xlab="Beverage", ylab="Frequency")

#여백(margin) 추가 함수
#   par(mar=c(0,0,0,0))
# c(bottom, left, top, right) 순서
# The default is c(5, 4, 4, 2) + 0.1


#비율로 나타내기
barplot(table(y)/length(y),xlab="Beverage",
        ylab="Proportion") #lengt(y) : 데이터 개수 길이


#객체만들기3
sales <- c(45,44,46) #sales 데이터 객체 
names(sales) <- c("Park", "Kim","Lee") #데이터이름설정
barplot(sales, main="Sales", ylab="Thousands")

#범위 조절하기
barplot(sales, main="Sales", ylab="Thousands",
        ylim=c(42,46),xpd=FALSE) #ylim (y축 범위 설정)
#xpd: 막대 벗어남 허용여부

#Error in plot.new() : figure margins too large
#                         -->여백 조정하기


#barplot(H, width = 1, beside = FALSE, 
#       main=‘title’, col=NULL, horiz= …)
# H (height): 백터나 행렧 입력 가능 (당연히 numeric)
# beside 인수 : 옆으로 나란히. FALSE 는 누적
# col : 그래프의 색상
# horiz= 막대를 평행하게





# 데이터 불러오기
#read.table(): txt 파일 불러오기
score <- read.table("score.txt", header= T,
                    fileEncoding="utf-8")

#파이차트 그리기
#paste():문자열 붙이는 함수
#clockwise=TRUE : 파이 차트 시작각도를 90도로 설정
#labels-파이에 이름붙이기
#pie(x, labels = names(x),col= null,clockwise = FALSE, …)
pie(score$국어, 
    labels=paste(score$성명,"-",score$국어),
    col=rainbow(10),clockwise=TRUE)

#두줄로 된 label 나타내기
#n: 줄바꿈
pie(score$국어,
    labels=paste(score$성명,"\n","(",score$국어,")"),
    col=rainbow(10),clockwise=TRUE)


##googlevis 패키지 사용하기
install.packages("googleVis")
library(googleVis)

#색상 설정
buildcolors <- function(color_count){
  colors <- rainbow(color_count)
  colors <- substring(colors,1,7)
  colors <- paste(colors,collapse="','")
  colors <- paste("'",colors,"'",sep="")
  colors <- paste("[",colors,"]",sep="")
  return(colors)
}

cols <- buildcolors(10)




#y=a+bx

#abline(a,b,lty,col,other options)

cars
cars[1:4,]

#회귀 모형 적합
z <- lm(dist~speed, data=cars)
summary(z)

#cars 데이터를 산점도로 나타내기
x11()
par(mfrow=c(1,1))
plot(cars,main="abline")

#horizontal
abline(h=20)
abline(h=30)

#vertical
abline(v=20, col='blue')

#y=a+bx
abline(a=40,b=4,col='red')

#reg 인수
abline(z,lty=2,lwd=2,col='green')

#coef 인수
abline(z$coef,lty=3,lwd=2,col='red')


#R graphic device 실행하기

x11()

plot(1:10,type="n", xlab="",ylab="",main="legend")

legend("bottomright","(x,y)",pch=1,title="bottomright")
legend("bottom","(x,y)",pch=1,title="bottom")
legend("bottomleft","(x,y)",pch=1,title="bottomleft")
legend("left","(x,y)",pch=1,title="left")
legend("topleft","(x,y)",pch=1,title="topleft")
legend("top","(x,y)",pch=1,title="top")
legend("topright","(x,y)",pch=1,title="topright")
legend("right","(x,y)",pch=1,title="right")
legend("center","(x,y)",pch=1,title="center")
legends <- c("legend1","legend2")

legend(3,8,legend=legends,pch=1:2, col=1:2)
legend(7,8,legend=legends,pch=1:2, col=1:2,lty=1:2)
legend(3,4,legend=legends,fill=1:2)
legend(7,4,legend=legends,fill=1:2,density=30)


#그래프 필요한 추가 기능

# create data

x <- c(1,3,6,8,9)
y <- c(12,56,78,32,9)

#draw the plot
plot(x,y)

#segments (점 잇기)
segments(6,78,8,32)

#연결하는 사각형 그리기
rect(4,20,6,30,density=3)

#텍스트 추가
text(4,40,"이것은 샘플입니다",srt=55) #srt는 각도

mtext("상단의 문자열입니다", side=3) #side는 옆 여백

mtext("우측의 문자열입니다",side=4,adj=0.3)

#그림의 테두리 그리기. 색은 빨간색
box(lty=2,col="red")

#x축 추가, y축 40 위치 표시하고 색은 빨간색
axis(1,pos=40,at=0:10,col=2)

#y축 추가. x축 5의 위치에 10:60까지 표시
axis(2,pos=5,at=10:60)



#sunflower plot
#create data
x <- c(1,1,1,2,2,2,2,2,2,3,3,4,5,6)
y <- c(2,1,4,2,3,2,2,2,2,2,1,1,1,1)
zz <- data.frame(x,y) #산포도를 위한 데이터를 생성한다
zz

#make the plot
sunflowerplot(zz)
#중복된 점 선 더 추가해서 표시

#mtcars를 사용하겠다는 명령어
data(mtcars)
stars(mtcars[,1:4]) 
# (mtcars 1~4 항목 대상으로 그래프를 그리는 명령어)
stars(mtcars[1:4],flip.labels=FALSE,key.loc=c(13,1.5))

par(mfrow=c(1,1))
par(mfrow=c(1,1)) #1x1 분할

#다른 옵션 설정
stars(mtcars[1:4],key.loc=c(13,1.5),draw.segments=TRUE)


#symbol()
#create data

xx <- c(1,2,3,4,5)
yy <- c(2,3,4,5,6)
zz <- c(10,5,100,20,10)
#make the plot
symbols(xx,yy,zz)


#pairs
xx <- c(1,2,3,4,5)
yy <- c(20,13,40,50,60)
zz <- c(10,5,100,20,10)
c <- matrix(c(xx,yy,zz),5,3)
c
pairs(c)
#칼럼수가 많고 변수가 많고 수치형일 때 pair함수 사용

#filled.contour
filled.contour(volcano,color.palette=terrain.colors,asp=1)
title(main="volcano data: filled contour map")

#lty=line type (blank하면 안 보이게 할 수 있음)



#조건문 예제
#0.0~1.0 사이의 난수 100개
x <- runif(100)
y <- runif(100)

#y값이 0.5보다 크면 1, 아나면 18(조건문)
plot(x,y,pch=ifelse(y>0.5,1,18))

# pch = point character


#조건문 속의 조건문
plot(x,y,pch = ifelse(y > 0.6, 15, 
                      ifelse(y > 0.4 , 5 , 14 )))
#0.6보다 크면 15
#0.4~0.6 5
#0.4이하 14

plot(x,y, pch=ifelse(y>=0.7, 8, 
                     ifelse(y >= 0.5 , 5 , 
                            ifelse ( y >= 0 , 12))))
#0.7과 같거나 크면 8 아니면
#0.5와 같거나 크면 5(0.7보단 작음) 0.5~0.7
#0이상 12 (0.5보단 작음)




#연습문제

read.csv("kbo.csv")
df_kbo <- read.csv("kbo.csv")
head(df_kbo)

#1
kbo <- read.csv("kbo.csv")
head(kbo)

#2
head(kbo %>% arrange(desc(팀)))

arrange(kbo,desc(팀)) %>% head()
head(arrange(kbo,desc(팀)))

kbo %>% arrange(desc(팀)) %>% head()

#3 2017년 추출 6행까지
kbo %>% c(6,)

filter(kbo,연도==2017) %>% head()

kbo %>% filter(연도==2017) %>% head()

#4 안타,2루타,3루타,홈런
#틀림 - filter(kbo,안타,2루타,3루타,홈런) %>% head()

select(kbo, 안타, X2루타, X3루타, 홈런) %>% head()

select(kbo,안타,홈런, 총루타) %>% head()

kbo %>% select(안타, X2루타, X3루타, 홈런) %>% head()

#5 2017년도 

#틀림 - filter(kbo,연도==2017) %>%  select(kbo,안타, x2루타, x3루타,홈런) %>% head(5)

filter(kbo,연도 == 2017) %>%
  select(안타, X2루타, X3루타,홈런) %>% head(5)

kbo %>% filter(연도==2017) %>% 
  select(안타, X2루타, X3루타,홈런) %>% 
  head(5)


#6
mutate(kbo,타율=안타/타수) %>% head()

kbo %>% mutate(타율=안타/타수) %>% head()


#연도별 
kbo %>% group_by(연도) 


