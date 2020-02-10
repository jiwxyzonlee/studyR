#사칙연산

5+10/(2+3)
(1/2+1/3)^2/(1/3^2)
5*7

#mode는 상태를 알려줌

num1 <- 3.5
mode(num1)
num2 <- 3
mode(num2)

char1 <- "blue"
char2 <- '1'
char3 <- 1
mode(char1)
mode(char2)
mode(char3)

logic1 <- c(TRUE, FALSE, TRUE)
mode(logic1)

logic2 <- c(T,F,T)
mode(logic2)

logic3 <- c(TRUE, false)
mode(logic3)

#대문자 소문자 주의

obj <- c(3,5,7,9,11)
name_1 <- c(2,4,6,8,10)
name.2 <- c(1,3,5,7,9)
.name2 <- c(1,3,5,7,9)
#2name <- c(1,3,5,7,9)
#_name <- c(1,3,5,7,9)

#  .<- 이것만 괜찮음 첫글자에 숫자나 기호 사용불가


#if<-c(1,2,3)
#else<-c(1,2,3)
#for<-c(1,2,3)

#if, else,for도 단순히 혼자서 문두에 못옴

obj2<-c(1,2,"A","B")
obj2
obj3<-c(T,F,1,2)
obj3
obj4<-c("A","B", T,F)
obj4

# char->num->logic 순서

A<-T
B<-F
C<-c(T,T)
D<-c(F,T)

A&B
C&D

A|B
C|D

A&&B
C&&D

A||B
C||D

#함수 순서대로 비교


1<2
1=2
1==2
1!=2

# !는 not, =는 케이스

A<-c(3,4)
B<-c(5,4)
C<-c(6,7)

A<B
A<=B
A==B
A!=B

#이것도 순서대로 비교

x <-9 #는 주석 
# x <- 12
x
ls()

#ls는 사용한 변수 (모두!!!)  보여줌
#rm은 제거할 때

rm(x)


#행열 data
mat1 <- matrix(1:12)
mat1

#3행 4열
mat2<-matrix(1:12, nrow=3, ncol=4)
mat2

#byrow 순서대로
mat3<-matrix(1:12, nrow=3, ncol=4, byrow=T)
mat3

mat4<-matrix(1:12, 3,4)
mat4

####
rownames(mat3)<-c("국어","영어","수학")
mat3
####

####
colnames(mat3)<-c("a1","a2","a3","a4")
mat3
####

mat3[2,3]
mat3[2,] #2행만
mat3[,2]
mat3[,-2] #2열 제외
mat3["영어",]
mat3[,2:3] #2열부터 3열까지
mat3[c(1,2),]
mat3[c(1,2),c(2,4)]
t(mat3) #행과 열 바꾸기


x1<-c(100,80,60,40,30)
x2<-c("A","B","c","A","B")

#data.frmae은 함수끼리 묶음
df<-data.frame(x1,x2)
df

df<-data.frame(score=x1,grade=x2)
df
df$score
df$grade # $는 뽑아내기, index


df2<-data.frame(score=x1, grade=x2,stringsAsFactors=F)
df2 #범주형끼리 안 묶음
df2$score
df2$grade

mean(df2$score)

t(df2)
t(df)

#현재 작업폴더 확인
getwd()

#새로운 작업폴더 지정
setwd("c:/rlab")

getwd()

#폴더 내 파일 이름 보기
dir()

getwd()
setwd("c:\\rlab")





ex1<-read.table("c:\\rlab/data.txt")
ex1
View(ex1)
colnames(ex1)

ex2<-read.table("data.txt", header=TRUE)
ex2
#변수명이 있을 때는 header=T로 불러올 수 있다
#첫번째 라인은 header예요<표시

colnames(ex2)

#txt형의 자료는 공백(스페이스바) 또는 탭으로
#구분되어 있어야 한다


x1<-1:20
x2<-rep(c("a","b"),10) #복제 replicate
x3<-sample(1:100,20) #무작위 20개
x1;x2;x3
x1
View(x1)

data1<-cbind(x1,x2,x3) #묶기
data1

dataframe<-as.data.frame(data1)
dataframe

data2<-data.frame(x1,x2,x3)
data2


write.table(data1,file="matrix.txt")
read.table("matrix.txt")

write.table(data1,file="matrix.txt", sep=",")
read.table("matrix.txt", sep=",") #콤마으로 분리

write.table(data1,file="matrix.txt",sep="\t")
read.table("matrix", sep="\t") #탭으로 분리

write.table(data1,file="matrix.txt", sep="$")
#임의로 지정
read.table("matrix.txt", sep="$")

write.table(data2,file="dataframe.txt")
read.table("dataframe.txt")

read.table("dataframe.txt",header=T,sep="\t")


read.csv("data.csv", header=T, sep",")
read.csv("data.csv")

txt<-read.table("data.txt", header=T)
write.csv(txt,file="data.csv")

install.packages("readxl")
library(readxl) # require()도 가능

df_exam<-read_excel("excel_exam.xlsx")
df_exam

mean(df_exam$math)



df_exam_novar <-read_excel("excel_exam_novar.xlsx", col_names = F)
df_exam_novar




df_exam_sheet<-read_excel("excel_exam_sheet.xlsx",sheet=3)
df_exam_sheet

df_csv_exam<-read.csv("csv_exam.csv")
df_csv_exam




df_csv_exam<-read.csv("csv_exam.csv",stringAsFactors = F)
df_csv_exam

df_midterm<-data.frame(english=c(90,80,60,70),math=c(50,60,10,20),class=c(1,1,2,2))
df_midterm #p.95

## english math class
##1    90  50    1

write.csv(df_midterm,file="df_midterm.csv")
save(df_midterm,file="df_midterm.rda")


rm(df_midterm)
df_midterm

load("df_midterm.rda")
df_midterm

df_exam<-read_excel("excel_exam.xlsx")
df_csv_exam<-read.csv("csv_exam.csv")



head(df_exam,10) #앞에서 10줄 (디폴트 6줄)
tail(df_exam,3) #뒤에서 3줄(디폴트3)
View(df_exam)
dim(df_exam) #행, 열 값출력
str(df_exam) #데이터 속성 확인
summary(df_exam) #요약 통계량 출력



