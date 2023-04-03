#7. 모델링과 예측: 선형 회귀
#3.단순 선형 회귀
#1.모델 적합
x = c(3.0, 6.0, 9.0, 12.0)  #설명 번수x
y = c(3.0, 4.0, 5.5, 6.5)  #반응 변수y
m = lm(y ~ x)  #모델 적합(학습)
m
plot(x, y)
abline(m, col = 'red')

coef(m)  #매개변수(계수) 값을 알려줌
fitted(m)  #훈련 집합에 있는 샘플에 대한 예측값
residuals(m)  #잔차를 알려줌
deviance(m)/length(x)  #잔차 제곱합을 평균 제곱 오차로 변환하여 출력
summary(m)  #모델의 상세 분석

#2.예측
newx = data.frame(x=c(1.2, 2.0, 20.65))  #3개의 새로운 값값
predict(m, newdata = newx)  #예측 수행

#4.단순 선형 회귀의 적용: cars 데이터
str(cars)
head(cars)
plot(cars)
#1.모델 적합
car_model = lm(dist~speed, data = cars)
coef(car_model)
abline(car_model, col = 'red')  #cars데이터에 대한 최적 모델
#2.예측
fitted(car_model)
residuals(car_model)
nx1 = data.frame(speed = c(21.5))  #시속 21.5로 달리고 있을 때 제동 거리
predict(car_model, nx1)  #제동거리는 66.96769
nx2 = data.frame(speed = c(21.5, 25.0, 25.5, 26.0, 26.5, 27.0, 27.5, 28.0))  #25부터 0.5씩 증가시키며 달렸을 때 제동 거리
predict(car_model, nx2)  #66.96769, 80.73112, 82.69733, 84.66353, 86.62974, 88.59594, 90.56215, 92.52835
nx = data.frame(speed = c(21.5, 25.0, 25.5, 26.0, 26.5, 27.0, 27.5, 28.0))
plot(nx$speed, predict(car_model, nx), col = 'red', cex = 2, pch = 20)  #새로운 데이터에 대해 predict로 예측한 결과
abline(car_model)
#고차 다항식 적용과 분산 분석(ANOVA ANalysis Of VAriance)
plot(cars, xlab ='속도', ylab = '거리') #cars 데이터를 그림
x = seq(0, 25, length.out = 200)  #예측할 지점
for(i in 1:4) {
  m = lm(dist~poly(speed, i), data = cars)
  assign(paste('m', i, sep = '.'), m)                    #i차 모델 m을 m.i라 부름
  lines(x, predict(m, data.frame(speed = x)), col = i)   #m으로 예측한 결과를 겹쳐 그림
}  #1, 2, 3, 4차 방정식을 이용한 모델

#5.모델의 통계량 해석
str(women)
women
women_model = lm(weight ~ height, data = women)
coef(women_model)
plot(women)
abline(women_model, col = 'red')  #women 데이터의 선형 회귀 모델
summary(women_model)  #heightm 계수의 p-값이 0.05 보다 작은 1.90e-14값을 가지므로 통계적으로 매우 유의미한 모델링이 되었음을 확인
#cars 데이터를 사용하여 모델링한 결과와 비교하기
car_model = lm(dist ~ speed, data = cars)
summary(car_model)  #speed 계수의 p-겂이 1.49e-12fh 0.05보다 매우 작은 값이기 때문에 통계적으로 유의미한 모델이다.

#6.다중 선형 회귀
library(scatterplot3d)
x = c(3.0, 6.0, 3.0, 6.0)
u = c(10.0, 10.0, 20.0, 20.0)
y = c(4.65, 5.9, 6.7, 8.02)
scatterplot3d(x, u, y, xlim = 2:7, ylim = 7:23, zlim = 0:10, pch = 20, type = 'h') #scatterplot3d함수를 이용한 데이터 시각화
m = lm(y ~ x + u)  #다중 선형 회귀 적용
coef(m)
s = scatterplot3d(x, u, y, xlim = 2:7, ylim = 7:23, zlim = 0:10, pch = 20, type = 'h')
s$plane3d(m)  #데이터에 모델을 중첩시켜 시각화
#오차 분석
fitted(m)
residuals(m)
deviance(m)  #잔차 제곱합
deviance(m) /length(x)  #평균 제곱 오차차
#새로운 데이터에 대한 예측 수행
nx = c(7.5, 5.0)
nu = c(15.0, 12.0)
new_data = data.frame(x = nx, u = nu)
ny = predict(m, new_data)
ny
s = scatterplot3d(nx, nu, ny, xlim = 0:10, ylim = 7:23, zlim = 0:10, pch = 20, type = 'h', color = 'red', angle = 60)
s$plane3d(m)  #새로운 데이터에 대한 예측 결과

#7.다중 선형 회귀의 적용: trees 데이터
str(trees)
summary(trees)
scatterplot3d(trees$Girth, trees$Height, trees$Volume)  #trees 데이터의 분포(Girth, Height는 설명변수 Volume은 반응변수)
#lm 함수 사용 다중 선형 회귀 적용
m = lm(Volume ~ Girth + Height, data = trees)
m
s = scatterplot3d(trees$Girth, trees$Height, trees$Volume, pch = 20, type = 'h', angle = 55)
s$plane3d(m)  #trees 데이터와 모델을 중첩하여 시각화
#자르기로 마음먹은 벚나무 세 그루위 지름과 키를 측정하여 부피 예상
ndata = data.frame(Girth = c(8.5, 13.0, 19.0), Height = c(72, 86, 85))
predict(m, newdata = ndata)   #6.457794 32.394034 60.303746 총 합99.185574

