#8. 일반화 선형 모델
#2.일반화 선형 모델
#1.단순 데이터에 적용
muffler = data.frame(discount = c(2.0, 4.0, 6.0, 8.0, 10.0), profile = c(0, 0, 0, 1, 1))
g = glm(profile ~ discount, data = muffler, family = binomial)
coef(g)
fitted(g)
residuals(g)
deviance(g)
#muffler 데이터에 glm을 적용한 결과 
plot(muffler, pch = 20, cex = 2)
abline(g, col = 'blue', lwd = 2)
newd = data.frame(discount = c(1, 5, 12, 20, 30))  #5개의 새로운 할인율
p = predict(g, newd, type = 'response')
print(p)
#muffler 데이터에 glm을 적용한 결과와 새로운 데이터에 대한 예측값
plot(muffler, pch = 20, cex = 2, xlim = c(0, 32))
abline(g, col = 'blue', lwd = 2)
res = data.frame(discount = newd, profit = p)
points(res, pch = 20, cex = 2, col = 'red')
legend("bottomright", legend = c("train data", "new data"), pch = c(20, 20), cex = 2, col = c("black", "red"), bg = "gray")
#2.실제 데이터에 적용: Haberman survival 데이터
Haberman = read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/haberman/haberman.data", header = FALSE)
names(Haberman) = c('age', 'op_year', 'no_nodes', 'survival')
str(Haberman)
Haberman$survival = factor(Haberman$survival)  #범주형으로 변환
str(Haberman)  #400개의 샘플, 4개의 변수
h = glm(survival ~ age + op_year + no_nodes,  data = Haberman, family = binomial)
deviance(h)        
#새로운 환자 두 명
new_patients = data.frame(age = c(37, 66), op_year = c(58, 60), no_nodes = c(5, 32))
predict(h, newdata = new_patients, type = 'response')  #5년 이내 사망할 확률 1번 환자 22.26% 2번 환자 84.49%
#3.특징 선택
h = glm(survival ~ age + no_nodes, data = Haberman, family = binomial)
coef(h)
deviance(h)
new_patients = data.frame(age = c(37, 66), no_nodes = c(5, 32))  #새로운 환자 2명
predict(h, newdata = new_patients, type = 'response')

#4. 로지스틱 회귀의 적용: UCLA admission 데이터
ucla = read.csv('https://stats.idre.ucla.edu/stat/data/binary.csv')
str(ucla)
#1. 시각화로 상관관계 분석하기
plot(ucla)  #상관관계 확인
library(dplyr)
library(ggplot2)
#geom_point와 geom_jitter를 사용하여 ucla 데이터 분포를 시각화
ucla %>% ggplot(aes(gre, admit)) + geom_point()  #1
ucla %>% ggplot(aes(gre, admit)) + geom_jitter()  #약간의 잡음 추가 2
ucla %>% ggplot(aes(gre, admit)) + geom_jitter(aes(col = admit)) #두 부류의 학생을 색으로 구분 3
ucla %>% ggplot(aes(gre, admit)) + geom_jitter(aes(col = factor(admit)))  #범주형으로 변경
ucla %>% ggplot(aes(gre, admit)) + geom_jitter(aes(col = factor(admit)), height = 0.1, width = 0.0)  #잡음 정도를 10%로 줄이로 gre변수에는 잡음을 전혀 추가하지 않음
#gpa와 rank 변수의 분석 그래프
library(gridExtra)
p1 = ucla %>%ggplot(aes(gpa, admit)) + geom_jitter(aes(col = factor(admit)), height = 0.1, width = 0.0)
p2 = ucla %>%ggplot(aes(rank, admit)) + geom_jitter(aes(col = factor(admit)), height = 0.1, width = 0.1)
grid.arrange(p1, p2, ncol = 2)
#glm 적용하기
m = glm(admit ~ ., data = ucla, family = binomial)
coef(m)
deviance(m, type = 'response')
summary(ucla)
#최적 모델로 얻은 식: admit = 0.00229396*gre + 0.77701357*gpa + -0.56003139*rank -3.44954840
#rank의 값이 작을수록 좋은 대학
#새로운 학생의 합격 여부 예측
s = data.frame(gre = c(376), gpa = c(3.6), rank = c(3))
predict(m, newdata = s, type = 'response')  #새로운 학생의 합격률: 18.7% 가량이다. 

#5.로지스틱 회귀의 적용: colon 데이터
library(survival)
str(colon)  #1858개의 샘플, 16개의 변수, 변수 rx는 세 가지 값을 가진 범주형 나머지 15개 변수는 숫자형
#1.시각화하기
plot(colon)  #colon데이터에 plot 함수를 적용한 시각화
#colon데이터에 jitter를 적용한 시각화
p1 = colon %>% ggplot(aes(extent, status)) + geom_jitter(aes(col = factor(status)), height = 0.1, width = 0.1)
p2 = colon %>% ggplot(aes(age, status)) + geom_jitter(aes(col = factor(status)), height = 0.1, width = 0.1)
p3 = colon %>% ggplot(aes(sex, status)) + geom_jitter(aes(col = factor(status)), height = 0.1, width = 0.1)
p4 = colon %>% ggplot(aes(nodes, status)) + geom_jitter(aes(col = factor(status)), height = 0.1, width = 0.1)
grid.arrange(p1, p2, p3, p4, ncol = 2, nrow =2)
#extent변수는 암 세포가 침습한 깊이를 나타내는데 값이 클수록 status 1(재발 또는 사망)인 샘플이 많다는 사실 확인
#age변수는 age가 클수록 status가 1인 샘플이 많지만, 생각보다는 젊은 환자도 1인 경우가 많고 나이가 많은 환자 중에도 0인 경우가 많음을 확인
#즉 결장암에 걸리면 나이가 적더라도 재발 또는 사망에 이르는 비율이 크다는 사실을 알 수 있다.
#2. glm 적용하기
#status 변수를 반응 변수로 하고 나머지 모든 변수를 설명 변수로 설정
m = glm(status ~ ., data = colon, family = binomial)
m
deviance(m) #잔차 제곱합으로 666.32를 얻었다.
table(is.na(colon))  #결측값 82개
clean_colon = na.omit(colon)  #결즉값 제거
m = glm(status ~ ., data = clean_colon, family = binomial)
m
deviance(m)
#분별력이 없는 변수 study, time, etype, id를 빼고 glm적용
clean_colon = clean_colon[c(TRUE, FALSE), ]
m = glm(status ~ rx + sex + age + obstruct + perfor + adhere + nodes + differ + extent + surg +
          node4, data = clean_colon, family = binomial)
m