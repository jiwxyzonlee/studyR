install.packages("RCurl")
# load the library
library(RCurl)
# specify the URL for the Iris data CSV
urlfile <-'http://archive.ics.uci.edu/ml/machine-learning-databases/letter-recognition/letter-recognition.data'
# download the file
downloaded <- getURL(urlfile, ssl.verifypeer=FALSE)
# treat the text data as a steam so we can read from it
connection <- textConnection(downloaded)
# parse the downloaded data as CSV
letters <- read.csv(connection, header=FALSE)
# preview the first 5 rows
colnames(letters)<-c("letter","xbox", "ybox","width","height",
                     "onpix","xbar","ybar","x2bar","y2bar",
                     "xybar","x2ybar","xy2bar","xedge","xedgey",
                     "yedge","yedgex")

View(letters)

write.csv(letters, file="letters.csv")


#training set and test set
test_split = 0.2
train_size = round((dim(letters)[1]*(1 - test_split))) #결과 트레인 이만*0.8=16000, 테스트 4만
set.seed(20180621)
train_index = sample(1:(dim(letters)[1]), train_size) #1부터 2만까지 16000개 랜덤 추출

letters_train <- letters[train_index,]
letters_test <- letters[-(train_index),]


install.packages("e1071")
library(e1071)

## fitting svm with linear kernel

###아래가 핵심##

letters_linear <- svm(letter~. , data = letters_train,
                      kernel = "linear")
summary(letters_linear)

## 결과값 해설하기
## 

pred_linear <- predict(letters_linear, letters_test)
table(pred_linear, letters_test$letter)

agreement_linear <- pred_linear == letters_test$letter
table(agreement_linear)
prop.table(table(agreement_linear))

##fitting svm with rbf kernel
letters_rbf <- svm(letter~. , data=letters_train,
                   kernel = "radial")

summary(letters_rbf)


pred_rbf <- predict(letters_rbf, letters_test)
table(pred_rbf, letters_test$letter)

agreement_rbf <- pred_rbf == letters_test$letter
table(agreement_rbf)
prop.table(table(agreement_rbf))


## ## ## ## ## ## ##


letters_nn <- read.csv("letters.csv")

str(letters_nn)

install.packages("nnet")

library(nnet)

#divide into training and test data

letters_train <- letters_nn[1:16000, ]

letters_test <- letters_nn[16001:20000, ]


letters.nn = nnet(letter ~., data = letters_train,
                  rang = 0.1, decay = 5e-4,
                  size = 20, maxit = 5000)

letters.predict = predict(letters.nn, letters_test, type = "class")

u = union(letters_test$letter, letters.predict)

nn.table = table(factor(letters.predict, u), factor(letters_test$letter, u))

nn.table

install.packages("caret")
library(caret)

confusionMatrix(nn.table)









