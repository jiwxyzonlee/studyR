p <- c(3,5,6,8)

q <- c(3,3,3)

p+q


#recycling


1=2

geom_density()
geom_line()

economics
ggplot(economics, aes(date, unemploy/pop))+ geom_line()

ggplot(economics, aes(date, uempmed))+ geom_line()


#예측오차 함수 정의


yi <- c(sample(10, 20, replace = T))
yhat_i <- c(sample(10, 20, replace = T))

#measring the quality of fit
mse <- function(yi, yhat_i){
  (mean((yi-yhat_i)^2))
}

mse(yi, yhat_i)

rmse <- function(yi, yhat_i){
  sqrt(mean((yi-yhat_i)^2))
}
#루트 씌우기


#abs : 절대값
mae <- function(yi, yhat_i){
  mean(abs(yi-yhat_i))
}

mape <- function(yi, yhat_i){
  mean(abs((yi-yhat_i)/yi))*100
}

#4개 값 한꺼번에
myfcn_measures <- function(yi, yhat_i){
  c(mse(yi, yhat_i), rmse(yi, yhat_i), mae(yi, yhat_i), mape(yi,yhat_i))
}

myfcn_measures(yi, yhat_i)





####191206 이어서

# install and load multiple packages at once
pkg_name_vec <- c("mlbench",
                  "dplyr",
                  "psych",
                  "car",
                  "MASS",
                  "glmnet",
                  "reshape2",
                  "Rfast")
for (pkg_name in pkg_name_vec) {
  if (!requireNamespace(package = pkg_name, quietly = TRUE)) {
    install.packages(pkgs = pkg_name)
  }
  library(package = pkg_name, character.only = TRUE)
}
rm(pkg_name_vec, pkg_name)

#없으면 패키지 상태, 있으면 라이브러리, 트루면 실행, 없으면 install)



#패키지 설치 및 불러오기
#pkg_name_vec <- c("mlbench", 
#                  "dplyr",
#                  "psych",
#                  "MASS",
#                  "glmnet",
#                  "reshape2",
#                  "Rfast")

#install.packages("glmnet")
#library(glmnet)

#install.packages("Rfast")
#library(Rfast)


#install.packages("mlbench")
#library(mlbench)
#보스턴 하우징 데이터셋 기본으로 들어가 있음

data(package = "mlbench")
#패키지 안에 들어있는 데이타셋 뭐있는지 알 수 있음


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


data_lm <- lm(medv~ ., data)
#medv: Boston 주택 가격
#lm(y~ ., data) 점은 전부다 할 때 

data_lm
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

summary(data_lm)
#             Pr(>|t|)    
#(Intercept)  3.28e-12 ***
#  crim       0.001087 ** 
#  zn         0.000778 ***
#  indus      0.738288    
#chas         0.001925 ** 
#  nox        4.25e-06 ***
#  rm         < 2e-16 ***
#  age        0.958229    
#dis          6.01e-13 ***
#  rad        5.07e-06 ***
#  tax        0.001112 ** 
#  ptratio    1.31e-12 ***
#  b          0.000573 ***
#  lstat      < 2e-16 ***
#Residual standard error: 4.745 on 492 degrees of freedom
#Multiple R-squared:  0.7406,	Adjusted R-squared:  0.7338 
#이 값을 얼마만큼 신뢰할 수 있는가
#수정된 알제곱을 본다

#F-statistic: 108.1 on 13 and 492 DF,  p-value: < 2.2e-16
#별 없는 거는 무시해도 됨, 별 세개는 유의미
#adjusted R-squared: 수정된 알제곱
#열 증가할수록 알제곱값 달라짐. 
#변수와 반비례로 패널티를 줘서 나온 값
#기본 알제곱보다 항상 값이 작다다
#p-value: < 2.2e-16
#p값 매우 작으므로 상당히 타당하다
#상당히 높은 확률로 귀무가설을 기각한다



#별 세 개짜리만 추출해서 다시 결과 내보기


#[02] ZN 25,000 평방피트를 초과하는 거주지역의 비율
#[05] NOX 10ppm 당 농축 일산화질소
#[06] RM 주택 1가구당 평균 방의 개수
#[08] DIS 5개의 보스턴 직업센터까지의 접근성 지수
#[09] RAD 방사형 도로까지의 접근성 지수
#[11] PTRATIO 자치시(town)별 학생/교사 비율
#[12] B 1000(Bk-0.63)^2, 여기서 Bk는 자치시별 흑인의 비율을 말함.
#[13] LSTAT 모집단의 하위계층의 비율(%)  저소득 주민들의 비율

#R을 자동으로 골라주는 방식, 변수를 자동으로 골라주는 방식
#   =중요한 것만 추리기(overfitting 방지)


#전진 (선택)법(값이 제일 잘 나올 때까지 변수를 한 개씩 추가해 나가는 방법)
#후진 (소거)법(전체 변수에서 한 개씩을 빼면서 검사)

#Bidirectional Elimination = 양방향
#(전진 선택법+후진소거법, 반복하면서 중요 변수 찾아냄)


#전역 탐색법(가능한 모든 변수 조합에 대해 모형 구축, 최적의 조합 찾아냄)
#교호작용(두 개가 interaction 하면서 새로운 작용을 만듦) %=% 시너지 효과

#R에서는 이차상호작용 모형 (A+B+C)^2
#R에서는 formula 인터페이스를 이용해 쉽게 적합할 수 있다.
#(예) 최대 2개까지 상호작용하는 경우 (A+B+C)^2=A+B+C+A:B+A:C+B:C

data_lm_full_2 <- lm(medv ~. ^2, data = data)
data_lm_full_2

length(coef(data_lm_full_2))
#변수 13개에서 두개만 뽑아 교호작용한 것의 개수
#y절편은 92 안에서 왔다 갔다



library(caret)

#6:2:2
set.seed(1606)
n <- nrow(data)
idx <- 1:n
training_idx <- sample(idx, n*.60)
idx <- setdiff(idx, training_idx)
validate_idx <- sample(idx, n*.20)
test_idx <- setdiff(idx, validate_idx)
training <- data[training_idx,]
validation <- data[validate_idx,]
test <- data[test_idx, ]

#stepwise selection
#MASS::stepAIC( ) 함수는 중요한 변수를 자동으로 선택해준다.
data_lm_full <- lm(medv ~., data = data)
#보통 7트레이닝, 3은 테스트
data_forward <- stepAIC(data_lm_full,
                        direction = "forward",
                        scope = list(upper = ~.^2,
                                     lower = ~1),
                        trace = FALSE)
data_backward <- stepAIC(data_lm_full, direction = "backward",
                         scope = list(upper = ~.^2,
                                      lower = ~1),
                         trace = FALSE)
data_both <- stepAIC(data_lm_full, direction = "both",
                     scope = list(upper = ~.^2,
                                  lower = ~1),
                     trace = TRUE)

summary(data_forward)
#Residual standard error: 2.792 
#on 440 degrees of freedom
#Multiple R-squared:  0.9197,	
#Adjusted R-squared:  0.9078 
#F-statistic: 77.52 on 65 and 440 DF,  
#p-value: < 2.2e-16

summary(data_backward)
summay (data_bothward)


#validation set을 이용한 모형 평가

y_obs <- validation$medv
yhat_full <- predict(data_lm_full_2,
                     newdata = validation)
yhat_forward <- predict(data_forward, 
                        newdata = validation)
yhat_backward <- predict(data_backward, 
                         newdata = validation)
yhat_both <- predict(data_both,
                     newdata = validation)

#평균오차 : 오차의 평균
#부호로 인해 잘못된 값 나올 수 있어서 절대값
#=> 평균 절대 오차(Mean absoulute error; MAE)

#MAPE: 평균 절대 비율(percentage) 오차
#실제값 대비 얼마나 예측값 차이가 있는지 %표현

#MSE : Mean Squared error
#RMSE : Root

#분산 값이 작게 나올 값일수록 잘 나온 값
#MAE~ 숫자일 때 예측 모델일 때 성능평가 기준

#분류모델에서는 적용 안됨


#예측오차 함수 정의

#measring the quality of fit
mse <- function(yi, yhat_i){
  (mean((yi-yhat_i)^2))
}

mse(yi, yhat_i)

rmse <- function(yi, yhat_i){
  sqrt(mean((yi-yhat_i)^2))
}
#루트 씌우기


#abs : 절대값
mae <- function(yi, yhat_i){
  mean(abs(yi-yhat_i))
}

mape <- function(yi, yhat_i){
  mean(abs((yi-yhat_i)/yi))*100
}

myfcn_measures <- function(yi, yhat_i){
  c(mse(yi, yhat_i), rmse(yi, yhat_i), mae(yi, yhat_i), mape(yi,yhat_i))
}

myfcn_measures(yi, yhat_i)

#choose the best regression model
matrix_measures <- matrix(rep(0, 16), ncol = 4)
colnames(matrix_measures) <- c("MSE", "RMSE", "MAE", "MAPE")
rownames(matrix_measures) <- c("full", "forward", 
                               "backward", "both")
matrix_measures[1, ] <- myfcn_measures(y_obs, yhat_full)
matrix_measures[2, ] <- myfcn_measures(y_obs, yhat_forward)
matrix_measures[3, ] <- myfcn_measures(y_obs, yhat_backward)
matrix_measures[4, ] <- myfcn_measures(y_obs, yhat_both)

matrix_measures

colMins(matrix_measures)

myfcn_measures(test$medv, predict(data_both, newdata = test))

length(data_forward$coefficients)
length(coef(data_backward))
length(coef(data_both))

#다중공선성
#독립적이어야 하는데 독립적이지 않을 때
#그로인해 값이 달라짐



#변수 선택법
#상호작용, 교호작용
#다중 공증성


#fitting the full model
#full: 변수를 다 넣은 것

vif(data_lm_full)
#detecting multicollinearity 다중공선성
#1/1-R^2 
# vif 값이 10보다 크면 문제가 심각하다
#variance inflation factor 분산 팽창 요인/지수

#외적 타당성과 내적 타당성의 문제
#단순선형회귀분석 : 기존에 있던 데이터를 지나치게 
#맞추려는 결과(overfitting) 새로운 걸 못함
#변수를 12개만 해도 되는데 70개 전체 다 했을 때
#noise까지 학습했다

#반대는 너무 적게함(underfitting)

#지도학습: 주어진 데이터를 가지고 
#하나의 함수 유추하는 머신러닝과정
#회귀분석(regression)-숫자,예측//분류(classification)-범주
#잘 예측했나(오차값 존재)/잘 분류했나(오차값 나올 일 없음)


#과적합 방지방법
#교차검증(훈련, 검증, 테스트)/정규화


#        #
#        #
#        #
## 오후 ##
#        #
#        #
#        #




#알아놓으면 좋은 함수

#엑셀 읽을 때

x <- readClipboard()
#복사/자르기 하고 콘솔창에서 실행

#벡터 형식이 아니라 예쁘게 불러오기
#(복사해서 붙이는 것과 다름 없음)
x <- read.table(file = "clipboard", sep = "\t", header = T)


#이거는 뭘까
y <- read.clipboard.csv()

#의사결정나무의 주요 특징
#위부터 뿌리라 하고 마지막뿌리를 terminal node, 잎이라고도 함
#뒤집힌 뿌리 모양

#분할기준의 선택 splitting rule
#정지규칙 stopping rule

#끝마다 예측값 할당법

#lotsize: 잔디밭 크기. 잔디밭 넓이. 소득과 비례한가
#다른
#불순도가 0이기 때문에 더 분류를 하지 않는다
#순도 100%라는 말은 잘 안 쓴다
#순도 100%이면 분류 끝

#모든 node 불순도가 0이 될때까지 쪼개면 full tree
#나무가 다 자라났다
#construct


library(readxl)

churn <- read.csv("churn.csv", stringsAsFactors = F)
#가입자 이탈 데이터
#통신사 데이터로 고객의 특성과 고객 이탈 여부 정보
#수업에서 factor 그냥 냅둬서 안 바꿈



data <- read.csv("churn.csv")

str(churn)
str(data)
library(caret)


#data$churn <- data$Churn. #이거는 새로운 열생성해서 넣는 거임

names(data)

names(data)[21] <- "churn"

str(data$churn)


set.seed(1234)

index <- createDataPartition(y = data$churn, p = 0.7,
                             list = F)
train <- data[index, ]
test <- data[-index, ]

library(dplyr)

glimpse(train)
glimpse(test)


library(rpart)
library(rpart.plot)

rpart_tree <- rpart(churn~., train[ ,c(-1,-4)],
                    control = rpart.control(minsplit = 10,
                                            minbucket = 3,
                                            cp = 0.03,
                                            maxdepth = 10))
rpart_tree

#cp: complexity parameter

#rpart(formula, data, weights, subset, 
#      na.action = na.rpart, method,
#      model = FALSE, 
#      x = FALSE, y = TRUE, 
#      parms, control, cost, ...


#rpart.control(minsplit = 20, 
#              minbucket = round(minsplit/3), 
#              cp = 0.01, 
#              maxcompete = 4, 
#              maxsurrogate = 5, 
#              usesurrogate = 2, 
#              xval = 10,
#              surrogatestyle = 0, 
#              maxdepth = 30, ...)


par(mfrow = c(1,1))

plot(rpart_tree); text(rpart_tree, cex = 0.8)
rpart.plot(rpart_tree, cex = 0.7)
#rpart.plot은 의사결정의 과정 자체가 나와있어서 좋다


#text(rpart_tree, cex = 0.7)

t <- test[2, ]

predict(rpart_tree, newdata = t )

#> rpart_tree
#n= 2334 

#node), split(스플릿 조건), n(조건에 맞는 개수), loss(조건에 안 맞는 개수), yval, (yprob)
#      * denotes terminal node
library(rattle)

fancyRpartPlot(rpart_tree)

printcp(rpart_tree)
plotcp(rpart_tree)



#불순도, cp

#불순도 계산 기준은 사람마다 다름
#지니지수, 엔트로피지수

#지니지수
#1-(A/(A+B))^2-(B/(A+B)^2) = a1
#8/16 * (1-(7/8)^2 -(1/8)^2) + 8/16*(1-(3/8)^2-(5/8)^2) = b1
#a1 - b1 = gain

#불확실성 감소 순도 증가 정보획득

#cp가 적어야 이파리 영향력이 작아짐


rpart_pred <- predict(rpart_tree, 
                      test[ ,c(-1,-4)], 
                      type = "class" )
confusionMatrix(rpart_pred, test$churn, positive = "True.")
#Confusion Matrix and Statistics

#Reference
#Prediction False. True.
#False.    843(a)    54(b)
#True.      12(c)    90(d)

#a= TP(true positive)
#b= FP(false positive)(negative를 positive라고 말함)
#c= FN(false negative)(negative라고 말했는데 틀림)
#d= TN(true negative)
#accuracy는 TP와 TN의 비율
#accuracy가 높다고 해서 무조건 좋은 건 아니다
#precision = TP/TP+FP = a/a+b
#sensitivity= TP/TP+FN = a/a+c
#Specificity= TN/FP+TN = d/b+d

#보통은 가로가 actual, 세로가 predict(종종 바뀜)

#Accuracy : 0.9339          
#95% CI : (0.9167, 0.9485)
#No Information Rate : 0.8559          
#P-Value [Acc > NIR] : 8.414e-15       

#Kappa : 0.6953          

#Mcnemar's Test P-Value : 4.494e-07       
#                                          
#            Sensitivity : 0.62500         
#            Specificity : 0.98596         
#         Pos Pred Value : 0.88235         
#         Neg Pred Value : 0.93980         
#             Prevalence : 0.14414         
#         Detection Rate : 0.09009         
#   Detection Prevalence : 0.10210         
#      Balanced Accuracy : 0.80548         
                                          
#       'Positive' Class : True.       




rpart.prune <-prune(rpart_tree, cp = 0.01, "CP")

rpart.plot(rpart.prune,cex = 0.7)

rpart_pred2 <- predict(rpart.prune, 
                       test[ , c(-1,-4)], 
                       type = "class")

confusionMatrix(rpart_pred2, test$churn, positive = "True.")




#####package Party
install.packages("party")
library(party)

party_tree <- ctree(churn ~., train[, c(-1,-4)])

plot(party_tree)

party_pred <- predict(party_tree, test[,c(-1,-4)],
                      type = "response")
confusionMatrix(party_pred, test$churn, positive = "True.")


#Accuracy

confusionMatrix(rpart_pred2, test$churn, positive = "True.")$overall[1]
confusionMatrix(party_pred, test$churn, positive = "True.")$overall[1]


install.packages("arules")
install.packages("arulesViz")

library(arules); library(wordcloud); library(arulesViz)
library(wordcloud2)


data("Groceries")
summary(Groceries)

library(dplyr)
glimpse(Groceries)
str(Groceries)
#sparse matrix 희소행렬(아주 드물게 n이 나옴)


#inspect(Groceries)

class(Groceries)
#> class(Groceries)
#[1] "transactions 트랜잭션을 바로 다룰 수는 없음
#=>데이터 프레임으로 바꿔줘야 함
#attr(,"package")
#[1] "arules"

test <- as.data.frame(Groceries)
#에러 뜸
#> test <- as.data.frame(Groceries)
#Error in as.data.frame.default(Groceries) : 
#  cannot coerce class ‘structure("transactions", 
#                                 package = "arules")’ to a data.frame

#데이터프레임으로 변환 ##중요!
groceries_df <- as(Groceries, "data.frame")

#품목 검사
itemName <- itemLabels(Groceries) #아이템 이름 추출
itemCount <- itemFrequency(Groceries)*9835


#워드클라우드
col <- brewer.pal(8, "Dark2") #다크투라는 팔레트에서

#x11()


wordcloud(words = itemName, freq = itemCount, min.freq = 1,
          scale = c(3, 0.2), col = col, random.order = F)

#apriori 함수로 규칙 생성 (연관규칙을 만든다)

rules <- apriori(Groceries, parameter = list(support = 0.01,
                                             confidence =0.35))

#생성된 규칙 검사
inspect(rules)
#안을 봅시다
summary(rules)
#glimpse(rules)
#str(rules)
#plot(rules)

inspect(sort(rules, by = "lift"))
rules %>% sort(by = "lift") %>% inspect() 
#가장 높은 향상도 값을 갖는 규칙으로 나열

#규칙을 plot으로 표현하기

plot(rules, method = "scatterplot")
plotly_arules(rules, method = "scatterplot",
              measure = c("support", "confidence"), 
              shading = "lift")

plotly_arules(rules, method = "matrix", 
              measure = c("support", "confidence"),
              shading = "lift")


#지지도(support)

#x=빵 > 빵이 몇 번 등장하느냐
#전체5번 중에 3번 : support = 3/5


#x를 사면 y를 산다
#빵을 사면 우유를 산다
#(빵과 우유가 같이 등장하는 정도)
#전체5번 중 2번 : 2/5

#어떤 규칙이 관련성이 있더라도 잘 등장하지 않으면 의미없다

#신뢰도(confidence)
#빵을 사면 우유를 산다
#빵을 샀을 때 우유도 산 정도
#빵 3번 산 중에 2번: 2/3 (조건부 확률)

#향상도(lift)
#x를 사면 y를 산다
#빵을 사면 우유를 산다
#
#우유는 빵과 상관 없이 필수품(가정?) -그럼 우유 100퍼 사게 됨
#그럼 우유 5번 삼 : 2/5 (그 중 빵과 우유 같이 산 게 2번)