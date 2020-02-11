

#youtube 3blue1brown
#딥러닝 입문 cs230, cs231n



knitr::kable(anscombe)

anscombe.1 <- data.frame(x = anscombe[["x1"]], 
                         y = anscombe[["y1"]], 
                         Set = "Anscombe Set 1")
anscombe.1

anscombe.2 <- data.frame(x = anscombe[["x2"]], 
                         y = anscombe[["y2"]], 
                         Set = "Anscombe Set 2")

anscombe.3 <- data.frame(x = anscombe[["x3"]], 
                         y = anscombe[["y3"]], 
                         Set = "Anscombe Set 3")

anscombe.4 <- data.frame(x = anscombe[["x4"]], 
                         y = anscombe[["y4"]], 
                         Set = "Anscombe Set 4")

anscombe.data <- rbind(anscombe.1, anscombe.2, 
                       anscombe.3, anscombe.4)
anscombe.data

aggregate(cbind(x, y) ~ Set, anscombe.data, mean)

aggregate(cbind(x, y) ~ Set, anscombe.data, sd)

model1 <- lm(y ~ x, subset(anscombe.data, 
                           Set == "Anscombe Set 1"))
model1

model2 <- lm(y ~ x, subset(anscombe.data, 
                           Set == "Anscombe Set 2"))

model3 <- lm(y ~ x, subset(anscombe.data, 
                           Set == "Anscombe Set 3"))

model4 <- lm(y ~ x, subset(anscombe.data, 
                           Set == "Anscombe Set 4"))



library(plyr)



correlation <- function(data) {
  
  x <- data.frame(r = cor(data$x, data$y))
  
  return(x)
  
}



ddply(.data = anscombe.data, 
      .variables = "Set", 
      .fun = correlation)



summary(model1)

summary(model2)

summary(model3)

summary(model4)



library(ggplot2)



gg <- ggplot(anscombe.data, aes(x = x, y = y))

gg <- gg + geom_point(color = "black")

gg <- gg + facet_wrap(~Set, ncol = 2)

gg <- gg + geom_smooth(formula = y ~ x, 
                       method = "lm", se = FALSE, # 이 줄 지우고 한번 해보기
                       data = anscombe.data)

gg


#install.packages("datasauRus")
library(datasauRus)
if(requireNamespace("dplyr")){
  suppressPackageStartupMessages(library(dplyr))
  datasaurus_dozen %>% 
    group_by(dataset) %>% 
    summarize(
      mean_x    = mean(x),
      mean_y    = mean(y),
      std_dev_x = sd(x),
      std_dev_y = sd(y),
      corr_x_y  = cor(x, y)
    )
}

if(requireNamespace("ggplot2")){
  library(ggplot2)
  ggplot(datasaurus_dozen, aes(x=x, y=y, colour=dataset))+
    geom_point()+
    theme_void()+
    theme(legend.position = "none")+
    facet_wrap(~dataset, ncol=3)
}



#비지도 학습 - 군집 분석(Clustering)


#K-means K:숫자

#clustering != 분류(Classification)

##
#k-means Clustering
library(caret)

data("iris")
View(iris)


#데이터 준비
set.seed(123)

InTrain <- createDataPartition(y=iris$Species, 
                               p=0.7, list = FALSE) 
training <- iris[InTrain,]
testing <- iris[-InTrain]

#표준화
training.data <- scale(training[-5])
summary(training.data)

#모델 작성
iris.kmeans <- kmeans(training.data[,-5], 
                      centers = 3, iter.max = 10000)
iris.kmeans$centers

#군집 확인
training$cluster <- as.factor(iris.kmeans$cluster)
qplot(Petal.Width, Petal.Length, 
      color=cluster, data=training)
table(training$Species, training$cluster)


##clustering 실습
## elbow 기법
##

#install.packages("NbClust")
library(NbClust)

nc <- NbClust(training.data, 
              min.nc = 2, max.nc = 15, 
              method = "kmeans")

barplot(table(nc$Best.n[1,]),
        xlab = "Number of Clusters", 
        ylab = "Number of Criteria" ,
          main = "Number of Clusters Chosen")

#rpart: recurrent partition
#분류를 위한 재귀적 분할, 회귀 및 
#의사결정 나무로 분류하는 머신러닝 방법

training.data <- as.data.frame(training.data)
modFit <- train(x = training.data[,-5], 
                y = training$cluster,
                method = "rpart")


testing.data <- as.data.frame(scale(testing[-5]))
testClusterPred <- predict(modFit, testing.data) 
table(testClusterPred ,testing$Species)


#인터넷 도움받기

#데이터 준비
library(caret)
set.seed(1712)

inTrain <- createDataPartition(y = iris$Species, 
                               p = 0.7, list = F)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
training

testing

#표준화
training.data <- scale(training[-5])
summary(training.data)

#모델작성
iris.kmeans <- kmeans(training.data[,-5], 
                      centers = 3, iter.max = 10000)
iris.kmeans$centers

#군집확인
training$cluster <- as.factor(iris.kmeans$cluster)
qplot(Petal.Width, Petal.Length, 
      colour = cluster, data = training)
table(training$Species, training$cluster)

#군집 중심 개수 결정
library(NbClust)

nc <- NbClust(training.data, 
              min.nc = 2, max.nc = 15, 
              method = "kmeans")

par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen")

wssplot <- function(data, nc = 15, seed = 1234) {
  wss <- (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab = "Number of Clusters",
       ylab = "Within groups sum of squares")}

wssplot(training.data)

#새로운 데이터에 군집 할당
training.data <- as.data.frame(training.data)
modFit <- train(x = training.data[,-5], 
                y = training$cluster,
                method = "rpart")

testing.data <- as.data.frame(scale(testing[-5]))
testClusterPred <- predict(modFit, testing.data) 
table(testClusterPred ,testing$Species)



## 의사결정 tree
## 밀도기반 군집분석




##LakeHuron, ribbon (시각화)

huron <-data.frame(year=1875:1972, 
                   level = as.vector(LakeHuron))


dim(huron)

ggplot(huron, aes(x=year)) +geom_area(aes(y=level))

p <- ggplot(huron, aes(year))
p <- p + geom_area(aes(y = level))
p + coord_cartesian(ylim = c(570,590)) #y값 제한


p <- ggplot(data = huron, aes(x=year))

p <- p + geom_area(aes(y=level))

p + coord_cartesian(ylim=C(min(huron$level)-2, 
                           max(huron$level)+2))

p <- ggplot(huron, aes(x=year))

p + geom_ribbon(aes(ymin=min(level)-2, ymax = level+2),
                fill = "skyblue")


#

LakeHuron


huron <- data.frame(year = 1875:1972, 
                    level = as.vector(LakeHuron))

ggplot(data=huron, aes(x=year)) + geom_area(aes(y=level))

#----------------------------------------------------
  
  p <- ggplot(data=huron, aes(x=year))

p <- p + geom_area(aes(y=level))

p + coord_cartesian(ylim=c(570, 590))

#----------------------------------------------------
  
  p <- ggplot(data=huron, aes(x=year))

p <- p + geom_area(aes(y=level))

p + coord_cartesian(ylim = c(min(huron$level)-2, 
                             max(huron$level)+2))

#----------------------------------------------------
  
  p <- ggplot(huron, aes(x=year))

p + geom_ribbon(aes(ymin=min(level)-2, ymax=level+2))

#---------------------------------------------
  
  p <- ggplot(huron, aes(x=year))

p + geom_ribbon(aes(ymin=level-2, ymax=level+2), 
                colour="skyblue", fill = "skyblue")






##k=인접 이웃 실습


# Wisconsin Breast Cancer data

#install.packages(knn)

library(knn)
library(class)
#install.packages("gmodels")
library(gmodels)

wbcd <- read.csv("wisc_bc_data.csv",
               stringsAsFactors = F)

# 위스콘신 대학의 연구자들이 기부한 데이터

# 유방 암 조직 검사에 대한 569개의 데이터와 32개의 속성



str(wbcd)

# M(Malignant) : 악성 / B(Benign) : 양성

# radius : 반지름 / texture : 텍스처 
#/ perimeter : 둘레 / area : 면적 / smmothness : 평활도 

# compactness : 다짐도 / concavity : 요면 
#/ concave points : 요면점 / symmetry : 대칭 
#/ fractal dimension : 프렉탈 차원



wbcd <- wbcd[-1] # id 삭제



table(wbcd$diagnosis)



wbcd$diagnosis <- factor(wbcd$diagnosis,
                       levels = c("B", "M"),
                       labels=c("Benign", "Malignant"))



summary(wbcd) # 단위가 굉장히 다름 -> 정규화가 필요





#### 데이터 표준화 ####



# 최대최소 표준화 (0~1값으로 변환)

normalize <- function(x){
  
  return((x-min(x))/(max(x)-min(x)))
  
}



wbcd_n <- as.data.frame(lapply(wbcd[2:31],normalize)) 
# 리스트를 데이터프레임형식으로 변환





#### 데이터 분할 #####



# 이 데이터 경우 기록물은 
#임의의순서로 저장되어있기 때문에, 샘플링이 단순

wbcd_train <- wbcd_n[1:469,]

wbcd_test  <- wbcd_n[470:569,]



wbcd_train_labels<-wbcd[1:469,1]

wbcd_test_labels <- wbcd[470:569,1]



prop.table(table(wbcd_train_labels))

prop.table(table(wbcd_test_labels)) 
# 데이터 분할이 골고루 잘 되었는지 확인





#### 모델 훈련 (가중치X) ####



# install.packages("class")

library(class)



wbcd_test_pred <- knn(train = wbcd_train,
                    
                    test = wbcd_test,
                    
                    cl = wbcd_train_labels,
                    # class : train 데이터의 각 행에 대한 
                    #범주인 팩터 벡터
                    
                    k = 21)

table(wbcd_test_pred)





#### 모델 성능 평가 #####



# install.packages("gmodels")

library(gmodels)



CrossTable(x = wbcd_test_labels,y = wbcd_test_pred,
           prop.chisq = FALSE,prop.c = FALSE)



##### Chapter 3: Classification using Nearest Neighbors --------------------

## Example: Classifying Cancer Samples ----
## Step 2: Exploring and preparing the data ---- 

# import the CSV file
wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)

# examine the structure of the wbcd data frame
str(wbcd)

# drop the id feature
wbcd <- wbcd[-1]

# table of diagnosis
table(wbcd$diagnosis)

# recode diagnosis as a factor
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))

# table or proportions with more informative labels
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

# summarize three numeric features
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# test normalization function - result should be identical
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))

# normalize the wbcd data
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))

# confirm that normalization worked
summary(wbcd_n$area_mean)

# create training and test data
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

# create labels for training and test data

wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

## Step 3: Training a model on the data ----

# load the "class" library
library(class)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k = 21)

## Step 4: Evaluating model performance ----

# load the "gmodels" library
install.packages("gmodels")
library(gmodels)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq = FALSE)

## Step 5: Improving model performance ----

# use the scale() function to z-score standardize a data frame
wbcd_z <- as.data.frame(scale(wbcd[-1]))

# confirm that the transformation was applied correctly
summary(wbcd_z$area_mean)

# create training and test datasets
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]

# re-classify test cases
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k = 21)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq = FALSE)

# try several different values of k
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=1)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=5)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=11)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=15)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=27)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)


