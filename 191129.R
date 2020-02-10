

# dplyr 패키지 연습문제

library(ggplot2)
library(dplyr)

ggplot(data = mpg, x= displ, y= hwy)


mpg %>% 
  filter(displ <= 4) %>% group_by(displ) %>% 
  summarise("평균 연비" = mean(hwy))
#group_by 안 해도 됨

mpg %>% 
  filter(displ>=5) %>% group_by(displ) %>% 
  summarise("평균연비"=mean(hwy))
#group_by 안 해도 됨


#mpg %>% 
#  mutate(displ1 = (displ<=4) + (displ >= 5)) %>% 
#  ifelse(displ<=4,displ1, ifelse(displ>=5, displ1)) %>%
#  group_by(displ1) %>% 
#  summarise("평균연비" = mean(hwy))

mpg %>% 
  mutate(ifelse(displ <= 4, mpg$displ, ifelse(displ >= 5, mpg$displ, NA ))) %>%
  group_by(displ) %>% 
  summarise("평균연비" = mean(hwy)) 


#참고
mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy)
welfare$income <- ifelse(
  welfare$income ==0, NA, welfare$income)


plot(x,y,pch = ifelse(y > 0.6, 15, 
                      ifelse(y > 0.4 , 5 , 14 )))

d4 <- mpg %>% filter(displ <= 4)
d5 <- mpg %>% filter(displ >= 5)




#tibble : 10행까지만 보여줌, data.frame의 상위버전 

#tibble로 바꾸는 방법
airquality <- as_tibble(airquality)
airquality


#audi, ford

mtcars$audi
names(mtcars)


#아우디와 포드의 평균 주행연비
mpg %>% 
  filter(cty) %>% group_by(manufacturer(audi)) %>%  #틀림
  summarise("주행연비"= mean(cty))

mpg %>% 
  filter(manufacturer == "audi" |manufacturer== "ford") %>% 
  group_by(manufacturer) %>%
             summarise("주행연비" = mean(cty))
           
#hwy, cty -> 하나로 통합하여 평균연비(total
#평균 연비가 높은 자동차 모델 상위 6개)


mpg %>% 
  mutate(total=(hwy+cty)/2) %>% 
  arrange(desc(total)) %>% 
  head()

d2 <- mpg %>% 
  mutate(total=(hwy+cty)/2) %>% 
  arrange(desc(total)) %>% 
  head()

d2 <- as.data.frame(d2)
arrange(d2,desc(total))


##

mpg_new <- mpg %>% select(class,cty,manufacturer)
mpg_new


mpg_new %>% 
  filter(class=="suv") %>% 
  summarise(mean(cty))

mpg_new %>% 
  filter(class=="compact") %>% 
  summarise(mean(cty))

mpg_new %>% 
  filter(class=="suv"|class=="compact") %>% 
  group_by(class) %>%  
  summarise(mean(cty))

#느낀 바: select는 열 가져와주고 filter는 (가져온 열 중) 행



mpg_new %>% 
  filter(class=="compact") %>%  #틀림
  group_by(manufacturer) %>%
  arrange(desc(class)) 
  
mpg_new %>% 
  filter(class=="compact") %>% 
  group_by(manufacturer) %>%
  summarise(n=n()) %>% 
  arrange(desc(n)) 
#summarise(n=n())은 여기서 compact 생산대수를 말함


names(mpg)[8]

#열 이름 바꾸기
names(mpg)[8] <-  "city"

names(mpg)[8] <- "cty"

names(mpg)

ggplot(mpg, aes(x=displ,y=hwy))+geom_point()

ggplot(mpg, aes(displ,hwy, colour=class))+geom_point()



ggplot(mpg, aes(x=displ,y=hwy))+geom_point(color="blue")

ggplot(mpg, aes(x=displ,y=hwy))+geom_point(aes(color="blue"))
#??? aes는 바닥색 이미 있음, overriding

ggplot(mpg, aes(x=displ,y=hwy))+
  geom_point(aes(color="blue"))+
  

ggplot(mpg, aes(displ,cty, color=class))+
  geom_point()
ggplot(mpg, aes(displ,cty, color=drv))+
  geom_point()
ggplot(mpg, aes(displ,cty, color = cty)) + 
  geom_point()
ggplot(mpg, aes(displ,cty, color=trans))+
  geom_point()

ggplot(mpg, aes(displ,cty, color=manufacturer))+
  geom_point()

ggplot(mpg, aes(displ,cty, shape = drv)) + 
  geom_point()
ggplot(mpg, aes(displ,cty, shape = class)) + 
  geom_point()
ggplot(mpg, aes(displ,cty, shape = trans)) + 
  geom_point()
ggplot(mpg, aes(displ,cty, shape = cty)) + 
  geom_point()

ggplot(mpg, aes(displ,cty, size = cty))+
  geom_point()
ggplot(mpg, aes(displ,cty, size = trans))+
  geom_point()
ggplot(mpg, aes(displ,cty, size = cty))+
  geom_point(color=cty)
ggplot(mpg, aes(displ,cty, size = cty))+
  geom_point(aes(color=cty)) #열의 이름을 적을 땐 aes(범주)넣어야야


ggplot(mpg, aes(displ,cty, size = cty, color= drv))+
  geom_point()

ggplot(economics, aes(date, unemploy))+
  geom_line() #시계열의 그래프

ggplot(mpg, aes(cty))+
  geom_histogram()

ggplot(mpg, aes(cty))+
  geom_histogram(bins=20) #bins는 기둥의 개수

ggplot(mpg, aes(cty))+
  geom_histogram(bins=)


ggplot(mpg, aes(cty))+
  geom_bar(bins=20)


dia <- diamonds

class(dia)
names(dia)
dia
#ord=ordered

class()
typeof(dia)
str(dia)
glimpse(dia)

ggplot(diamonds, 
       aes(carat,price))+
  geom_point()
#점은 우상향으로 53,940개가 찍힐 것

ggplot(diamonds, 
       aes(carat,price,
           color=clarity ))+
  geom_point()

ggplot(diamonds, 
       aes(carat,price,
           color=cut ))+
  geom_point()

ggplot(diamonds, 
       aes(carat,price,
           color=depth ))+
  geom_point()

#번외
ggplot(diamonds, 
       aes(carat,depth,
           color=clarity ))+
  geom_point()


##Facetting
#~:~에 따라서

ggplot(mpg, aes(displ,hwy))+
  geom_point()+geom_smooth()

ggplot(mpg, aes(displ,hwy))+
  geom_point()+geom_smooth(method="lm") #회색은 예측범위
#linear model

ggplot(mpg, aes(displ,hwy))+
  geom_point()+geom_smooth(method="loess")

ggplot(mpg, aes(displ,hwy))+
  geom_point(position = "jitter")+geom_smooth(metod="loess")
#lo.cal regression


ggplot(mpg, aes(drv,hwy))+
  geom_violin()

ggplot(mpg, aes(drv,hwy))+
  geom_violin(position="jitter")
#뚱뚱한 데가 빈도 많은 곳

ggplot(mpg, aes(drv,hwy))+
  geom_jitter()

ggplot(mpg, aes(drv,hwy))+
  geom_jitter(position = "jitter")

ggplot(mpg, aes(hwy),)+geom_freqpoly()
#히스토그램을 만든 뒤 가운뎃점 연결한 그래프 도수분포다각형



ggplot(mpg, aes(manufacturer)) +geom_bar()
#제조사 별로 바 (y는 빈도수, 제조사가 언급되는 횟수(count))

drugs <- data.frame(drug=c("a","b","c"),
                    effect=c(4,9,6))

ggplot(drugs, aes(drug,effect))+geom_bar(stat = "identity")
#정체성 -> 정체성 있다고 알려줘야 한다 꼭 identity
#default값은 stat= "count" 임
#빈도 아니고 y값 넣으려면 stat="identity" 넣어야 함


economics
ggplot(economics, aes(date, unemploy/pop))+ geom_line()

ggplot(economics, aes(date, uempmed))+ geom_line()


names(economics)


#boxplot 중간 라인이 median (1/4 구분) 이상치는 점/동그라미 표시


#데이터 정제하기(결측치)-2일차 자료

df <- data.frame(sex = c("M","F",NA,"M","F"),
                 score = c(5,4,3,4,NA))
df

is.na(df) #na이냐/아니냐?
table(is.na(df))

table(is.na(df$sex))
table(is.na(df$score))

mean(df$score) #na 제외 안 하고
sum(df$score)

df %>% 
  filter(is.na(score)) #na이면 T

df %>% 
  filter(!is.na(score)) #na가 아닌 것들

df_nomiss <- df %>% filter(!is.na(score))
mean(df_nomiss$score)
sum(df_nomiss$score)

df_nomiss <- df %>% filter(!is.na(score)&!is.na(sex))
df_nomiss   #결측치 제거

df_nomiss2 <- na.omit(df) #모든 변수 결측치 없애고 추출
df_nomiss2                #na가 보이는 행을 지워줌
#특별한 경우가 아니면 안 써야. 그 행데이터를 지워버리기 때문에



#가장 권장되는 결과
mean(df$score, na.rm=T)  #가장 많이 씀
                        #na가 있다고 옵션을 넣어줌
                        #na.rm = na를 지워라

sum(df$score, na.rm = T)

#패키지 자체가 많음(예: 최소한의 데이터 손실을 위한)



#이상치 (weird)(뭔가 읭스러운 수치) %= 오류값



outlier <- data.frame(sex=c(1,2,1,3,2,1),
                      score=c(5,4,3,4,2,26))
outlier

table(outlier$sex)
table(outlier$score)

outlier$sex <- ifelse(outlier$sex==3, NA, outlier$sex)
outlier

#함수 자체가 박스값, 박스플롯

#박스플롯(아웃라이어$스코어) #동그라미가 이상치
boxplot(outlier$score)


outlier$score <- ifelse(outlier$score>5,NA, outlier$score)
boxplot(outlier$score)

##-----------------------------------------------------

####################################깃헙 만드세요

##

minho2<-function(x,y){
  xx<-x
  yy<-y
  return(sum(xx,yy))
}

minho2(2,3)



##

minho3 <- function(x,y){
  x3 <- x+1
  y3 <- y+1
  x4 <- minho2(x3,y3)
  +return(x4)
}
minho3(2,4)

##


minho4 <- function(){
  x <- 10
  y <- 10
  return(invisible(x*y))
}
minho4()
result <- minho4()
result


##
x <- 70
ls()
minho5 <- function(){ #x<<-(바깥에 있는 x)
  x <- 10 #로컬변수
  y <- 20
  
  x <<-40 
  return(x+y)
}

minho5()
x


sum1 <- 0

for(i in seq(1,10, by=1)) sum1 <- sum1+i   #1씩 증가
sum1  #수열


sum1 <- 0
for(i in 1:5){
  for(j in 1:5)
    sum1 <- sum1+i*j
}
sum1


##


#while 조건이 맞을 때까지 돌린다

sum2 <- 0
i <- 1


while(i<=10){
      sum2 <- sum2+i;
      i <- i+1
}
sum2


###########리눅스 빠삭하게 하세요
#리눅스가 기본이 될 거다
##

sum2 <- 0
i <- 0
while(i<=5){
  j <- 0
  while(j<=5){
    sum2 <- sum2+i*j
    j <- j+1
  }
  i <- i+1
}
sum2

###repeat문의 사용

sum3 <- 0
i <- 1
repeat{
  sum3 <- sum3+i
  i <- i+1
  if(i>10)break
}

sum3
i


###
sum3 <- 0
i <- 0
repeat{
  if(i>5)break
  j <- 0
  repeat{
    if(j>5)break
    sum3 <- sum3+i*j
    j <- j+1
  }
  i <- i+1
}
sum3

#e1071

#klaR

#kernlab


