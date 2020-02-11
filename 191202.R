#r교육 2일치


boxplot(mpg$hwy)
#수염: 1.5배 내 제일 큰 것, 1.5배 내 제일 작은 것(이상치 말고)
#mpg내 실제 있는 데이터만 가지고 표현해낸 것


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

outlier %>% 
  filter(!is.na(sex)&!is.na(score)) %>% 
  group_by(sex) %>% 
  summarise(mean_score = mean(score))



#1.5 IQR
#1.5 IQR 바깥을 벗어나면 이상치다



boxplot(mpg$hwy)


ggplot(mpg,aes( ,hwy))+geom_boxplot()

#근데 왜 x축에 넣으면 안 보일까

boxplot(mpg$drv)

mpg$drv
summary(mpg)
head(mpg)


ggplot(mpg,aes( drv, ))+geom_boxplot()

ggplot(mpg,aes( , drv))+geom_boxplot()

ggplot(mpg,aes( drv , hwy))+geom_boxplot()


mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy)
table(is.na(mpg$hwy))

mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy=mean(hwy, na.rm = T))


ggplot(mpg, aes(manufacturer,displ, 
                color = manufacturer, 
                fill = manufacturer))+
  geom_boxplot()



###

#R교육 3일차


#install.packages("foreign")

library(foreign)
library(dplyr)
library(ggplot2)


raw_welfare <- read.spss(file = "Koweps_hpc10_2015_beta1.sav")



welfare <- as.data.frame(raw_welfare)


str(welfare)
glimpse(welfare)
head(welfare)
tail(welfare)
summary(welfare)

dim(welfare)

length(names(welfare))


welfare <- welfare %>% 
  select(gender = h10_g3, birth = h10_g4,
         marriage = h10_g10, religion = h10_g11,
         income = p1002_8aq1, job = h10_eco9,
         region = h10_reg7)

str(welfare)
plot(welfare)
summary(welfare)
#na값이 몇개인지 잘 보여주는 건 summary

pairs(job~ income+gender+region, data = welfare)
pairs(welfare)
facet_grid(welfare)

#gender는 범주형의 factor이어야 한다(막대기)


boxplot(welfare)
boxplot(welfare$income)

sum(is.na(welfare))

sum(!is.na(welfare))

#중요 : col으로 sum
colSums(is.na(welfare))
colSums(!is.na(welfare))

summary(welfare$income)
# (-) 왜안됨?

#psych

#install.packages("psych")

library(psych)

describe(welfare)
#summary와 쌍벽

describe(welfare$income)

mean(welfare$income)
mean(welfare$income, na.rm = T)

range(welfare$income)
range(welfare$income, na.rm = T)

welfare$income <- ifelse(
  welfare$income ==0, NA, welfare$income)
#0인 사람을 빼주자

summary(welfare$income)
boxplot(welfare$income)
plot(welfare$income)

ggplot(welfare, aes(income)) +geom_density()
ggplot(welfare, aes(income)) +geom_freqpoly()


#성별로 바꿔 나타내기
summary(welfare$gender)
welfare$gender <- ifelse(
  welfare$gender == 1, "male", "female")

summary(welfare$gender)
table(welfare$gender)

ggplot(data=welfare, aes(x=gender), fill = gender) +
  geom_bar()

ggplot(data=welfare, aes(x=gender)) +
  geom_bar(aes(fill=gender)) 
#범례 들어가려면 뒤에서 aes

barplot(table(welfare$gender), col=)

###### 성별별로 소득 평균
#(권장)
welfare %>% 
  group_by(gender) %>% 
  summarise(평균=mean(income, na.rm=T))

#(교재)
welfare %>% 
  filter( !is.na(income)) %>%
  group_by(gender) %>% 
  summarise(평균=mean(income)) 
#######

data_gender <- welfare %>% 
  group_by(gender) %>% 
  summarise(평균=mean(income, na.rm=T))

ggplot(data=data_gender, 
       aes(gender, 평균, fill = gender)) +
  geom_bar(stat='identity')

##권장
welfare %>% 
  group_by(gender) %>% 
  summarise(평균=mean(income, na.rm=T)) %>% 
  ggplot(aes(gender, 평균, fill = gender)) +
  geom_bar(stat='identity')


welfare %>% select(gender, income) %>% 
  ggplot(aes(x=income, color = gender)) +
  geom_density()


class(welfare$birth)
summary(welfare$birth)

qplot(welfare$birth)

#이상치 확인
boxplot(welfare$birth)

#결측치 확인
sum(is.na(welfare$birth))

#없던 열 만들기(mutate보다 더 쉬운 방법)
welfare$age <- 2015 - welfare$birth +1
summary(welfare$age)
qplot(welfare$age)
plot(welfare$age)

barplot(welfare$age)
table(welfare$age)
barplot(table(welfare$age))

age_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income=mean(income))

head(age_income)

plot(age_income)

ggplot(age_income, aes(x=age, y=mean_income)) +
  geom_line()


ggplot(age_income, aes(x=age, y=mean_income)) +
  geom_bar(stat='identity') +
  geom_smooth()

ggplot(age_income, aes(x=age, y=mean_income)) +
  geom_point()


ggplot(age_income, aes(x=age, y=mean_income)) +
  geom_point(size=2, color = "blue") +
  geom_line()

ggplot(age_income, aes(x=age, y=mean_income)) +
  geom_point(size=2, color = "blue") +
  geom_line() +geom_smooth()


#KoNLP

install.packages("KoNLP")
library(KoNLP)
#KoNLP 쓰려면 자바가 필요함


useSejongDic()

#library(wordcloud2)

welfare <- welfare %>% 
  mutate(age_gen = ifelse(age<30, "young", 
                          ifelse(age<=40, "middle",
                                 ifelse(age <=50, "old",
                                        ifelse(age<60, "very old","very very old")))))
head(welfare)

table(welfare$age_gen)
qplot(welfare$age_gen)

age_gen_income <- welfare %>% 
  group_by(age_gen) %>% 
  summarise(평균소득 = mean(income, na.rm=T))
age_gen_income

ggplot(age_gen_income, aes(age_gen, 평균소득))+
  geom_bar(stat='identity', aes(fill=age_gen))+
  scale_x_discrete(limits = 
                     c("young", "middle", "old",
                       "very old", "very very old"))
#x 순서 지정



gender_income <- welfare %>% 
  group_by(age_gen, gender) %>% 
  summarise(mean_income=mean(income, na.rm = T))
gender_income

ggplot(gender_income, aes(age_gen, mean_income, fill=gender))+
  geom_col(position = "dodge") +
  scale_x_discrete(limits = 
                   c("young", "middle", "old",
                     "very old", "very very old"))

ggplot(gender_income, aes(mean_income, age_gen, fill=gender))+
  geom_bar(stat='identity')
  









##
##오후 강의##
###





#3개 그룹간의 평균 비교

data(iris)
str(iris)
#누구나 다 알고 있는 데이터
#데이터 분석의 기본.학습용 데이터

temp <- c(sample( 1:50 , 30), 
          sample( 51:100 , 30), 
          sample( 101:150 , 30))
temp

iris.training <- iris[temp,]
iris.testing <- iris[-temp,]

library(nnet)
#require() : library와 비슷하나 더 좋음
#libraray()

neuralNetResult <- nnet(Species ~., 
                        data = iris.training, 
                        size = 3, decay = 0)


neuralNetResult

summary(neuralNetResult)

install.packages("NeuralNetTools")
library(NeuralNetTools)
library(nnet)
#install.packages("neuralnet")
#library(neuralnet)
library(reshape2)

install.packages("reshape") #이게 꼭 필요
library(reshape)

source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

plot.nnet(neuralNetResult)


pred <- predict(neuralNetResult,iris.testing, 
                type="class")
pred

real <- iris.testing$Species
table(real, pred)


summary(neuralNetResult)

pairs(iris)

ggplot2::diamonds
#ggplot2의 데이터 불러오기


datasets::iris
#데이터 소스 불러오기


#IMDB movie data
install.packages("ggplot2movies")
require(ggplot2movies)

summary(movies)
dim(movies)

library(dplyr)
glimpse(movies)




train <- read.csv("mnist_train.csv")

dim(train)

pairs(train)

m = matrix(unlist(train[10,-1]), nrow = 28, byrow = TRUE)

m

train[10,-1]
train[10,1]

image(m,col=grey.colors(255))

library(caret)

write.csv(m, "mnist3.csv")

# reverses (rotates the matrix)
rotate <- function(x) t(apply(x, 2, rev)) 

rotate



# reverses (rotates the matrix)
rotate <- function(x) t(apply(x, 2, rev)) 

# Plot and show some of images
par(mfrow=c(2,3))
lapply(1:6, 
       function(x) image(
         rotate(matrix(unlist(train[x,-1]),nrow = 28, byrow = TRUE)),
         col=grey.colors(255),
         xlab=train[x,1]
       )
)

par(mfrow=c(1,1)) # set plot options back to default

#load caret library
library (caret)

#split the dataset to 80% training and 20% testing
inTrain<- createDataPartition(train$label, p=0.8, list=FALSE)
training<-train[inTrain,]
testing<-train[-inTrain,]

#store the datasets into .csv files
write.csv (training , file = "train-data.csv", row.names = FALSE) 
write.csv (testing , file = "test-data.csv", row.names = FALSE)

#load h2o library
library(h2o)

#start a local h2o cluster
local.h2o <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, nthreads=-1)

#read the csv files
training <- read.csv ("train-data.csv") 
testing  <- read.csv ("test-data.csv")

# convert digit labels to factor for classification
training[,1]<-as.factor(training[,1])

# pass dataframe from inside of the R environment to the H2O instance
trData<-as.h2o(training)
trData[,1]<-as.factor(trData[,1])
tsData<-as.h2o(testing)
tsData[,1]<-as.factor(tsData[,1])

#measure start time
start<-proc.time()

#deep learning model
model.dl <- h2o.deeplearning(x = 2:785,
                             y = 1,
                             trData,
                             activation = "Tanh",
                             hidden=rep(160,5),
                             epochs = 20)
#measure end time
end <- proc.time()

#time difference
diff=end-start
print(diff)

#use model to predict testing dataset
pred.dl<-h2o.predict(object=model.dl, newdata=tsData[,-1])
pred.dl.df<-as.data.frame(pred.dl)

summary(pred.dl,exact_quantiles=TRUE)
test_labels<-testing[,1]

#calculate number of correct prediction
sum(diag(table(test_labels,pred.dl.df[,1])))

# shut down virtual H2O cluster
h2o.shutdown(prompt = FALSE)

