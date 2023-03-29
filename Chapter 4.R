#4. 데이터 취득과 정제
#4-2 데이터 정제를 위한 조건문과 반복문
#1.조건문
test = c(15, 20, 30, NA, 45)  #벡터인 경우
test[test<40]  #값이 40 미만인 요소 추출
test[test%%3!=0]  #값이 3으로 나누어 떨어지지 않는 요소 추출
test[is.na(test)]  #NA인 요소 추출

characters = data.frame(name=c("길동", "춘향", "철수"), age=c(30, 16, 21),
                        gender=factor(c("M", "F", "M")))
characters
characters[characters$gender=="F",]  #성별이 여성인 행 추출
characters[characters$age<30&characters$gender=="M",]  # 30살 미만의 남성 행 추출
#if문
x = 5
if(x %% 2==0) {
  print('x는 짝수')  #조건식이 참일 때 수행
} else {
  print('x는 홀수')  #조건식이 거짓일 때 수행
}

x=-1
if(x>0) {
  print('x is a positive value.')
} else if(x<0) {
  print('x is a negative value.')
} else {
  print('x is zero')
}

#ifelse문
x=c(-5:5)
options(digits = 3)  #숫자 표현 시 유효자릿수를 3자리로 설정
sqrt(x)
sqrt(ifelse(x>=0, x, NA))  #NaN이 발생하지 않게 음수면 NA로 표시

students = read.csv("C:/Sources/students.csv")
students  #데이터에 100 초과값과 음수값이 포함되어있음
students[, 2]=ifelse(students[, 2]>=0 & students[, 2]<=100, students[, 2], NA)
students[, 3]=ifelse(students[, 3]>=0 & students[, 3]<=100, students[, 3], NA)
students[, 4]=ifelse(students[, 4]>=0 & students[, 4]<=100, students[, 4], NA)
students  #ifelse문으로 2~4열 값 중 0~100 외의 값은 NA로 처리함

#2.반복문
#repeat 문을 이용해 1부터 10까지 수 증가시키기
i = 1  #i의 시작값은 1
repeat {
    if(i>10) {  #i가 10을 넘으면 반복을 중단(break)함
      break
    } else {
          print(i)
          i = i+1  #i를 1증가시킴
    }
}
#while 문을 이용해 1부터 10까지 수 증가시키기
i = 1  #i의 시작값은 1
while (i<=10) {  #i가 10 이하인 동안 반복함
  print(i)
  i=i+1    #i를 1 증가시킴
}
#while 문을 이용해 구구단 2단 만들기
i = 1
while(i<10) {
  print(paste(2, "X", i, "=", 2*i))
  i=i+1
}
#for 문을 이용해 1부터 10까지 수 증가시키기
for(i in 1:10) {
  print(i)
}
#for문을 이용해 구구단 2단 만들기
for(i in 1:9) {
  print(paste(2, "X", i, "=", 2*i))
}
#for 문을 이용해 구구단 2~9단 만들기
for(i in 1:9) {
  for(j in 1:9) {
    print(paste(i, "X", j, "=", i*j))
  }
}
#1부터 10까지의 수 중 짝수면 출력하기
for(i in 1:10) {
  if(i%%2==0) {
    print(i)
  }
}
#1부터 10까지의 수 중 소수 출력하기
for(i in 1:10) {
    check = 0
    for(j in 1:i) {
      if(i%%j==0) {
        check = check+1
      }
    }
    if(check==2) {
      print(i)
    }
  
}

students = read.csv("C:/Sources/students.csv")
students  #데이터에 100 초과 값과 음수 값이 포함되어 있음
for(i in 2:4) {
  students[, i]=ifelse(students[, i]>=0&students[, i]<=100, students[, i], NA)
}
students  #ifelse 문으로 2~4열 값 중 0~100 외의 값은 NA로 처리함

#3.사용자 정의 함수: 원하는 기능 묶기
fact = function(x) {  #함수의 이름은 fact, 입력은 x
    fa = 1            #계승값을 저장할 변수
    while(x>1) {      #x가 1보다 큰 동안 반복
      fa = fa*x       #x 값을 fa에 곱한 후 fa에 다시 저장
      x = x-1         #x 값을 1 감소
    }
    return(fa)       #최종 계산된 fa 반환
}
fact(5) #5!을 계산한 결과 출력

my.is.na <- function(x) {  #table(is.na()) 함수를 하나로 묶은 my.is.na 함수를 만듦
  table(is.na(x))
}
my.is.na(airquality)  #이 결과는 table(is.na(airquality))와 같음
table(is.na(airquality))

#4.데이터 정제 예제 1: 결측값 처리
#is.na 함수를 이욯해 결측값 처리 하기
str(airquality)  #airquality 데이터의 구조를 살펴봄
#airquality 데이터에서 NA인 것은TRUE, 아니면 FALSE로 나타냄. 데이터가 많아 head 함수로 추려냄
head(is.na(airquality))
table(is.na(airquality))  #NA가 총 44개 있음
table(is.na(airquality$Temp))  #Temp에는 NA가 없음을 확인
table(is.na(airquality$Ozone))  #Ozone에는 NA가 37개 발견됨
mean(airquality$Temp)  #NA가 없는 Temp는 평균이 구해짐
mean(airquality$Ozone)  #NA가 있는 Ozone은 평균이 NA로 나옴
air_narm = airquality[!is.na(airquality$Ozone), ]    #Ozone 속성에서 NA가 없는 값만 추출함
air_narm
mean(air_narm$Ozone)  #결측값이 제거된 데이터에서는 mean 함수가 정상적으로 동작
air_narm1 = na.omit(airquality)
mean(air_narm1$Ozone)  #na.omit 함수를 이용해 결측값 처리하기
mean(airquality$Ozone, na.rm = T)  #함수 속성인 na.rm을 이용해 결측값 처리하기

#5.데이터 정제 예제2: 이상값 처리
#이상값이 포함된 환자 데이터
patients = data.frame(name = c("환자1", "환자2", "환자3", "환자4", "환자5"),
                      age = c(22, 20, 25, 30, 27),
                      gander=factor(c("M", "F", "M", "K", "F")),
                      blood.type = factor(c("A", "O", "B", "AB", "C")))
patients
#성별에서 이상값 제거
patients_outrm = patients[patients$gander=="M"|patients$gander=="F", ]
patients_outrm
#성별과 혈액형에서 이상값 제거
patients_outrm1 = patients[(patients$gander=="M"|patients$gander=="F") &
                            (patients$blood.type=="A"|patients$blood.type=="B"
                             |patients$blood.type=="O"|patients$blood.type=="AB"), ]
patients_outrm1

#이상값이 포함된 환자 데이터 성별을 남1 여2로 혈액형은 A, B, O, AB형을 각각 1, 2, 3, 4로 표시
patients = data.frame(name = c("환자1", "환자2", "환자3", "환자4", "환자5"),
                      age = c(22, 20, 25, 30, 27),
                      gander=c(1, 2, 1, 3, 2),
                      blood.type = c(1, 3, 2, 4, 5))
patients
#성별에 있는 이상값을 결측값으로 변경
patients$gander = ifelse((patients$gander<1|patients$gander>2), NA, patients$gander)
patients
#혈액형에 있는 이상값도 결측값으로 변경
patients$blood.type = ifelse((patients$blood.type<1|patients$blood.type>4), NA, patients$blood.type)
patients
#결측값 모두 제거
patients[!is.na(patients$gander)&!is.na(patients$blood.type), ]

#boxplot 활용 이상값 처리
boxplot(airquality[, c(1:4)])  #Ozone, Solar.R, Wind, Temp에 대한 boxplot
boxplot(airquality[, 1])$stats  #Ozone의 boxplot 통계값 계산
air = airquality  #임시저장 변수로 airquality 데이터 복사
table(is.na(air$Ozone))  #Ozone의 현재 NA 개수 확인
#이상값을 NA로 변경
air$Ozone = ifelse(air$Ozone>122, NA, air$Ozone)
table(is.na(air$Ozone))  #이상값 처리 후 NA 개수 확인(2개 증가)
#NA 제거
air_narm=air[!is.na(air$Ozone), ]
mean(air_narm$Ozone)
