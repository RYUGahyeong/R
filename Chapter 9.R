#9. 분류를 위한 모델
#3.결정 트리 함수의 사용
#1.iris 데이터에 rpart 적용하기
iris
library(rpart)
r = rpart(Species ~ ., data = iris)  #iris의 반응변수는 Species
print(r)  #학습된 결정 트리에 대한 자세한 정보
#rpart로 구한 결정 트리의 시각화``
par(mfrow = c(1, 1), xpd = NA)
plot(r)
text(r, use.n = TRUE)
p = predict(r, iris, type = 'class')
table(p, iris$Species)
#2.rpart의 옵션 이용하기
r_prior = rpart(Species ~ ., data = iris, parms = list(prior = c(0.1, 0.1, 0.8)))
plot(r_prior)
text(r_prior, use.n = TRUE)  #사전 확률울 옵션으로 주고 구한 결정 트리
#3.결정 트리로 예측하기
#예측
newd = data.frame(Sepal.Length = c(5.11, 7.01, 6.32), Sepal.Width = c(3.51, 3.2, 3.31),
                  Petal.Length = c(1.4, 4.71, 6.02), Petal.Width = c(0.19, 1.4, 2.49))
#출력
print(newd)
predict(r, newdata = newd) 
#3개의 샘플에 대해 setosa versicolor virginica에 속할 확률이 첫 번째 샘플은1.0, 0.0, 0.0
#두 번째 샘플은 0.0, 0.907, 0.978이다. 따라서 최고 확률을 갖는 부루로 분류하는 전략을 쓴다면
#세 샘플은 각각 setosa versicolor  virginica 부류로 분류된다,

#4.결정 트리의 해석
#1.summary 함수로 결정 트리 해석하기
summary(r)  #Variable importance행은 설명 변수의 중요성을 순서대로 보여줌
#2.결정 트리 시각화하기
#결정 트리를 해석하는데 효과적인 시각화: rpart.plot라이브러리 사용
library(rpart.plot)
rpart.plot(r)
rpart.plot(r, type =4)

#5.랜덤 포리스트
#2.iris 데이터에 randomForest 함수 적용하기
#랜덤 포리스트를 이용한 iris 데이터 분류
library(randomForest)
f = randomForest(Species ~ ., data = iris)
#학습된 결정 트리에 대한 자세한 정보
f
#학습된 결정 트리에 대한 더 자세한 정보
summary(f)
plot(f)  #랜덤 포리스트를 plot로 시각화
#varlmpPlot 함수로 설명 변수의 중요도를 시각화
varUsed(f)
varImpPlot(f)
treesize(f)  #treesize함수는 결정 트리 각각에 대해 리프 노드의 개수를 출력
#3.랜덤 포리스트로 예측하기
newd = data.frame(Sepal.Length = c(5.11, 7.01, 6.32), Sepal.Width = c(3.51, 3.2, 3.31),
                  Petal.Length = c(1.4, 4.71, 6.02), Petal.Width = c(0.19, 1.4, 2.49))
predict(f, newdata = newd)
predict(f, newdata = newd, type = 'prob')
predict(f, newdata = newd, type = 'vote', norm.votes = FALSE)
#랜덤 포리스트의 하이퍼 매개변수
small_forest = randomForest(Species ~ ., data = iris, ntree = 20, nodesize = 6, maxnodes = 12)
treesize(small_forest)
small_forest

#6.SVM과 k-NN
#1.SVM
library(e1071)
s = svm(Species ~ ., data = iris)  #기본 커널은 redial basis 사용
print(s)
table(predict(s, iris), iris$Species)  #150개중 4개를 잘못 분류했으므로 오류율은 4/150=2.997%
s = svm(Species ~ ., data = iris, kernel = 'polynomial')  #polynomial 커널 사용
p = predict(s, iris)
table(p, iris$Species) #오류율이 7/150=4.667% 증가 redial basis커널 함수가 낫다는 사실을 알 수 있음
s = svm(Species ~ ., data = iris, cost = 100)  #기본 커널은 redial basis
p = predict(s, iris)
table(p, iris$Species)  #잘못 분류한 샘플이 2개 뿐이기 때문에 오류율은 1.333%다.
#2.k=NN
library(class)
train = iris
test = data.frame(Sepal.Length = c(5.11, 7.01, 6.32), Sepal.Width = c(3.51, 3.2, 3.31),
                  Petal.Length = c(1.4, 4.71, 6.02), Petal.Width = c(0.19, 1.4, 2.49))
k = knn(train[, 1:4], test, train$Species, k = 5)
k
#train 함수로 코딩하기
#train을 이용하여 결정 트리, 랜덤 포리스트, SVM, k-NN 모델 학습하는 예
library(caret)
r = train(Species ~ ., data = iris, method = 'rpart')
f = train(Species ~ ., data = iris, method = 'rf')
s = train(Species ~ ., data = iris, method = 'svmRadial')
k = train(Species ~ ., data = iris, method = 'knn')

#7.분류 모델의 다양한 적용
#1.UCLA admission 데이터
#UCLA admission 데이터
ucla = read.csv('https://stats.idre.ucla.edu/stat/data/binary.csv')
str(ucla)
ucla$admit = factor(ucla$admit)
#rpart를 적용하여 구한 결정 트리
r = rpart(admit ~ ., data = ucla)
par(mfrow = c(1, 1), xpd = NA)
plot(r)
text(r, use.n = TRUE)  #UCLA admission 데이터에 대해 rpart로 학습한 결정 트리
p = predict(r, ucla, type = 'class')
table(p, ucla$admit)  #결정 트리로 훈련 집합에 대해 예측을 시도한 결과를 보여준다. 정확률은(249+54)/400 = 80.5%
f = randomForest(admit ~ ., data = ucla)  #랜던 포리스트 적용
print(f)
#2.colon 데이터
library(survival)
str(colon)
clean_colon = na.omit(colon)  #전처리: 결측값 제거
clean_colon = clean_colon[c(TRUE, FALSE)]  #전처리: 홀수 번째 것만 뽑음
clean_colon$status = factor(clean_colon$status)
str(clean_colon)
r = rpart(status ~ rx + sex + age + obstruct + perfor + adhere + nodes + differ + extent + surg + node4, data = clean_colon)
p = predict(r, clean_colon, type = 'class')
table(p, clean_colon$status)
plot(r)
text(r, use.n = TRUE)  #colon 데이터에 대해 rpart로 학습한 결정 트리/ 정확률은 (319+283)/888 = 67.93%
summary(r)
#colon 데이터에 랜덤 포리스트 적용
f = randomForest(status ~ rx + sex + age + obstruct + perfor + adhere + nodes + differ + extent + surg + node4, data = clean_colon)
print(f)  #정확률은 (295+266)/888 = 63.18%
#3.voice 데이터
#voice 데이터
voice = read.csv('C:/Sources/voice.csv')
str(voice)
table(is.na(voice))
#voice데이터에 rpart 적용
r = rpart(label ~ ., data = voice)
par(mfrow = c(1, 1), xpd = NA)
plot(r)
text(r, use.n = TRUE)
p = predict(r, voice, type = 'class')
table(p, voice$label)  #정확률 (1551+1496)/3168 = 96.18%
#voice 데이터에 랜덤 포리스트 적용
f = randomForest(label ~ ., data = voice)
print(f)  #정확률 (1555+1549)/3168 = 97.98%
treesize(f)
