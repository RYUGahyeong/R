#R로 배우는 데이터 과학
#2장 데이토 과학으로 풍덩
#2-6
#iris 데이터
str(iris)  
head(iris, 10)
plot(iris)
plot(iris$Petal.Width,iris$Petal.Length, col=iris$Species)  #Petal.Width, Petal.Length의 상관관계 시각화
#==========================
#tips 데이터
tips = read.csv('https://raw.githubusercontent.com/mwaskom/seaborn-data/master/tips.csv') 
str(tips)
head(tips,10)
summary(tips)   #요약 통계
library(dplyr)
library(ggplot2)
tips%>%ggplot(aes(size))+geom_histogram()    #size 변수의 히스토그램
tips%>%ggplot(aes(total_bill, tip))+geom_point()  #total_bill과 tip 변수 상관관계 산점도
tips%>%ggplot(aes(total_bill, tip))+geom_point(aes(col=day))  # 요일을 색으로 표시
tips%>%ggplot(aes(total_bill, tip))+geom_point(aes(col=day, pch=sex), size=3)  #성별을 기호로 표시
#==========================
#3장 데이터형과 연산
#3-2
#변수사용법
x = 1    #x에 1을 할당
y = 2    #y에 2를 할당
z = x + y
z  # z = 3
z <- x + y
z
x + y -> z
z
#x와 y에 저장된 값 교환
x = 1
y = 2
temp = x    #임시변수 temp에 x값 저장
x = y       #x와 y값 교환
y = temp    #temp에 저장된 값 y에 대입
x    #2
y    #1
#==========================
#3-3데이터형
x = 5
y = 2
x/y
xi = 1+2i
yi = 1-2i
xi+yi    # 2+0i
str = "Hello, World!"
str
blood.type = factor(c('A', 'B', 'O', 'AB'))
blood.type
T    #TRUE
F    #FALSE
xinf = Inf    #Inf = 양의 무한대
yinf = -Inf   #-Inf = 음의 무한대
xinf/yinf    #NaN = 연산 불가능한 값 표시
#==========================
#3-5 벡터
#1. 벡터 생성
1:7    #1부터 7까지 1씩 증가시켜 요소가 7개인 벡터 생성 1 2 3 4 5 6 7
7:1    #7부터 1까지 1씩 감소시켜 요소가 7개인 벡터 생성 7 6 5 4 3 2 1
vector(length = 5)    # vector(length = n)함수: 요소사 n개인 빈 벡터 생성 FALSE FALSE FALSE FALSE FALSE
#c함수: 일반 벡터 생성
c(1:5)    #1~5 요소로 구성된 벡터 생성 1:5와 동일
c(1, 2, 3, c(4:6))    # 1~3 요소와 4~6 요소를 결합한 1~6 요소로 구성된 벡터 생성
x=c(1, 2, 3)
x    # [1] 1 2 3
y=c()
y=c(y, c(1:3))
y    # [1] 1 2 3
#seq함수: 순열 벡터 생성
seq(from=1, to=10, by=2)    # 1부터 10까지 증가하는 벡터 생성  1 3 5 7 9
seq(1, 10, by=2)  #1부터 10까지 증가하는 벡터 생성  1 3 5 7 9
seq(0, 1, by =0.1)  #0부터 1까지 0.1씩 증가하는 요소가 11개인 벡터 생성 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0
seq(0, 1, length.out=11)  #0부터 1까지 요소가 11개인 벡터 생성 length.out=요소 개수
#rep 함수: 순열 벡터 생성
rep(c(1:3), times=2)  #(1, 2, 3) 벡터를 2번 반복한 벡터 생성 times=벡터의 반복 횟수 1 2 3 1 2 3
rep(c(1:3), each=2)  #(1, 2, 3) 벡터의 개별 요소를 2번 반복한 벡터 생성 each=요소의 반복 횟수 1 1 2 2 3 3
#2.벡터 연산
x=c(2, 4, 6, 8, 10)
length(x)  # 벡터의 길이(크기)를 구함
x[1]  # 벡터의 1번 요소값
x[c(1, 2, 3)]  #x 벡터의 1, 2, 3번 요소를 구할 때는 벡터로 묶어야함
x[-c(1, 2, 3)]  #x 벡터에서 1, 2, 3 요소를 제외한 값 출력
x[c(1:3)]  #x 벡터에서 1번부터 3번 요소를 출력
x+2  #x 벡터의 개별 요소에 2를 각각 더함 4  6  8 10 12
x= c(1, 2, 3, 4)
y= c(5, 6, 7, 8)
x+y  #x와 y벡터의 크기가 동일함로 각 요소별로 더함
z=c(3, 4)
x+z  #x벡터가 z벡터 크기의 정수배인 경우엔 작은 쪽 벡터 요소를 순환하여 더함
w=c(5, 6, 7)
x+w  #x와 w의 크기가 정수배가 아니므로 연산 오류
#3.벡터 연산의 유용한 함수
#all,any함수: 벡터 내 모든, 일부 요소의 조건 검토
x=1:10
x>5  #벡터의 요소 값이 5보다 큰지 확인  FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
all(x>5)  #x 벡터의 요소 값이 모두 5보다 큰지 확인  FALSE
any(x>5)  #x 벡터의 요소 값 중 일부가 5보다 큰지 확인  TRUE
#head,tail함수: 데이터의 앞, 뒤 일부 요소 추출
x=1:10
head(x)  #데이터의 앞 6개 요소 출력
tail(x)  #데이터의 뒤 6개 요소 출력
head(x, 3)  #데이터의 앞 3개 요소 추출
tail(x, 3)  #데이터의 뒤 3개 요소 추출
#union, intersect, setdiff, setequal 함수: 벡터 간 집합 연산
x=c(1, 2, 3)
y=c(3, 4, 5)
z=c(3, 1, 2)
union(x, y)  # 합집합
intersect(x, y)  #교집합
setdiff(x, y)  #차집합(x에서 y와 동일한 요소 제외)  1 2
setdiff(y, x)  #차집합(y에서 x와 동일한 요소 제외)  4 5
setequal(x, y)  #x와 y가 동일한지 비교 FALSE
setequal(x, z)  #x와 z가 동일한지 비교
#==========================
#3-6 배열(행렬)
#1.배열 생성
#N차원 배열 생성
x=array(1:5, c(2, 4))  #1:5 <- 벡터 데이터 c(2, 4) <- 차원을 정의하는 벡터
x
x[1, ]  #1행의 요소 값 출력
x[, 2]  #2행의 요소 값 출력
dimnamex = list(c("1st", "2nd"), c("1st", "2nd", "3rd", "4th"))  #행과 열 이름 설정
x = array(1:5, c(2, 4), dimnames = dimnamex)
x
x["1st", ]
x[, "4th"]
#2차원 배열 생성
x = 1:12
x
matrix(x, nrow=3)  # (행렬로 구성할 벡터, nrow =행 개수)
matrix(x, nrow=3, byrow = T)  #byrow - 데이터를 행 단위로 배치할지 여부(T/F)
#벡터를 묶어 배열 설정
v1 = c(1, 2, 3, 4)
v2 = c(5, 6, 7, 8)
v3 = c(9, 10, 11, 12)
cbind(v1, v2, v3)  #cbind - 열 단위로 묶어 배열 생성
rbind(v1, v2, v3)  #rbind - 행 단위로 묶어 배열 생성
#2.배열 연산
x = array(1:4, dim = c(2, 2))
y = array(5:8, dim = c(2, 2))
x
y
x + y
x - y
x * y  # 각 열별 곱셈
x %*% y  # 수학적인 행렬 곱셈
t(x)  # x의 전치 행렬
solve(x)  #x의 역행렬
det(x)  #x의 행렬식
#3.베열에 유용한 함수
#apply 함수: 배열의 행 또는 열별로 함수 적용
x = array(1:12, c(3, 4))
x
apply(x, 1, mean)  # 가운데 값이 1이면 함수를 행별로 적용
apply(x, 2, mean)  # 가운데 값이 2이면 함수를 열별로 적용
#dim 함수: 배열의 크기(차원의 수)
x = array(1:12, c(3, 4))
dim(x)  # 3 4
#sample 함수: 벡터나 배열에서 샘플 추출
x = array(1:12, c(3, 4))
sample(x)  #배열의 요소를 임의로 섞어 추출
sample(x, 10)  #배열 요소 중 10개를 골라 추출
sample(x, 10, prob = c(1:12)/24)  #각 요소별 추출 확률을 달리할 수 있음
sample(10)  #단순히 숫자만 사용하여 샘플을 만들 수 있음
#==========================
#3-7데이터 프레임 생성
name = c("철수", "춘향", "길동")
age = c(22, 20, 25)
gender = factor(c("M", "F", "M"))
blood.type = factor(c("A", "O", "B"))

patients = data.frame(name, age, gender, blood.type)
patients
#한 행으로 출력하기
patients = data.frame(name = c("철수", "춘향", "길동"), age = c(22, 20, 25), 
                      gender = factor(c("M", "F", "M")), blood.type = factor(c("A", "O", "B")))

patients
#데이터 프레임 요소에 접근
patients$name  #name 속성 값 출력
patients[1, ]  #1행 값 출력
patients[, 2]  #2행 값 출력
patients[3, 1]  #3랭 1열 값 출력
patients[patients$name=="철수",]  #환자 중 철수에 대한 정보 출력
patients[patients$name=="철수", c("name", "age")]  #철수 이름과 나이 정보만 추출
#데이터 프레임에 유용한 함수
#attach, detach 함수: 데이터 프레임의 속성명을 변수명으로 변경
head(cars)
attach(cars)  #attach 함수를 통해 cars의 각 속성을 변수로 이용하게 함
speed  #speed 라는 변수명을 직접 사용 가능
detach(cars)  #detach 함수를 통해 cars의 각 속성을 변수로 이용하는 것을 해제함
speed
#with함수: 데이터 프레임에 다양한 함수를 적용
#데이터 속성을 이용해 함수 적용
mean(cars$speed)
max(cars$speed)
#with 함수를 이용해 함수 적용
with(cars, mean(speed))
with(cars, max(speed))
#subset함수: 데이터 프레임에서 일부 데이터만 추출
subset(cars, speed>20)  #속도가 20 초과인 데이터만 추출
subset(cars, speed>20, select =c(dist))  #속도가 20 초과인 dist 데이터만 추출, 여러 열 선택은 c() 안을 ,로 구분
subset(cars, speed>20, select =-c(dist))  #속도가 20초과인 데이터 중 dist를 제외한 데이터만 추출
#na.omit함수: 데이터 프레임의 결측값 제거
head(airquality)  #airquality 데이터에는 NA가 포함되어있음
head(na.omit(airquality))  #NA가 포함된 값을 제외하여 추출함
#merge 함수: 여러 데이터 프레임 병합
name = c("철수", "춘향", "길동")
age = c(22, 20, 25)
gender = factor(c("M", "F", "M"))
blood.type = factor(c("A", "O", "B"))

patients1 = data.frame(name, age, gender)
patients1
patients2 = data.frame(name, blood.type)
patients2

patients = merge(patients1, patients2, by = "name")
patients

#이름이 같은 열 변수가 없으면 merge 함수의 by.x와 by.y에 합칠 때 사용할 열의 속성명을 각각 기입해주어야 함
name1 = c("철수", "춘향", "길동")
name2 = c("민수", "춘향", "길동")
age = c(22, 20, 25)
gender = factor(c("M", "F", "M"))
blood.type = factor(c("A", "O", "B"))

patients1 = data.frame(name1, age, gender)
patients1
patients2 = data.frame(name2, blood.type)
patients2
patients = merge(patients1, patients2, by.x = "name1", by.y = "name2", all = TRUE)
patients
#is.data.frame, as.data.frame함수: 데이터 프레임 형식 확인 및 변환
x = array(1:12, c(3, 4))
is.data.frame(x)  #현재 x는 데이터 프레임이 아님  FALSE
as.data.frame(x)
x = as.data.frame(x)  #as.data.frame 함수로 x를 데이터 프레임 형식으로 변환
is.data.frame(x)  # TRUE
names(x) = c("1st", "2nd", "3rd", "4th")  #데이터 프레임으로 변환 시 자동 지정되는 열 이름을 names 함수로 재지정함
x
#==========================
#3-8리스트
#리스트 생성
patients = data.frame(name = c("철수", "춘향", "길동"), age = c(22, 20, 25),
                      gender = factor(c("M", "F", "M")), blood.type = factor(c("A", "O", "B")))
no.patients = data.frame(day = c(1:6), no=c(50, 60, 55, 52, 65, 58)) 
listPatients = list(patients, no.patients)  #데이터를 단순 추가
listPatients

listPatients = list(patients = patients, no.patients = no.patients)  #각 데이터에 이름을 부여하면서 추가
listPatients
#리스트 요소에 접근
listPatients$patients  #요소명 입력
listPatients[[1]]  #인덱스 입력
listPatients[["patients"]]  #요소명을 ""에 입력
listPatients[["no.patients"]]  #요소명을 ""에 입력
#리스트에 유용한 함수
#lapply, sapply함수: 리스트 요소에 다양한 함수 적용
#no.patients 요소의 평균을 구해줌
lapply(listPatients$no.patients, mean)
#patients 요소의 평균을 구해줌, 숫자 형태가 아닌 것은 평균이 구해지지 않음
lapply(listPatients$patients, mean)
sapply(listPatients$no.patients, mean)
#sapply()의 simplify 옵션을 F로 하면 lapply() 결과와 동일한 결과를 반환함
sapply(listPatients$no.patients, mean, simplify=F)