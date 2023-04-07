#10. 모델의 성능 평가
#2.정확률
library(rpart)
library(randomForest)
library(caret)
#iris 데이터에 결정 트리와 랜덤 포리스트 
r = rpart(Species ~ ., data = iris)  #결정 트리 모델
f = randomForest(Species ~ ., data = iris, ntree = 3)  #랜덤 포리스트 모델
r_pred = predict(r, iris, type = 'class')
confusionMatrix(r_pred, iris$Species)
f_pred = predict(f, iris)
confusionMatrix(f_pred, iris$Species)  #결정 트리의 정확률은 144/150 = 96%이고 오류률은 6/150 = 4%이다.

#3.일반화 능력 측정
#2.테스트를 훈련 집합과 테스트 집합으로 나누기
n = nrow(iris)  #iris의 샘플의 개수
i = 1:n   #1, 2, 3, ..., n을 가진 리스트 i
train_list = sample(i, n*0.6)  #60%를 랜덤하게 샘플링
test_list = setdiff(i, train_list)  #나머지 40%
iris_train = iris[train_list, ]  #훈련 집합 추출
iris_test = iris[test_list, ]  #테스트 집합 함수
f = randomForest(Species ~ ., data = iris_train)  #훈련 집합으로 랜덤 포리스트 학습
p = predict(f, newdata = iris_test)
p
iris_test$Species  #정확률은 59/60 = 98.33%
#3.caret 라이브러리로 구현하기
library(caret)
train_list = createDataPartition(y = iris$Species, p = 0.6, list = FALSE)
iris_train = iris[train_list, ]
itis_test = iris[-train_list, ]
f = randomForest(Species ~ ., data = iris_train)  #훈련 집합으로 랜덤 포리스트 학습
p = predict(f, newdata = iris_test)  

#4.교차 검증
#1.반복문으로 구현하기
library(caret)
data = iris[sample(nrow(iris)), ]  #iris 데이터의 순서를 섞는다.
k = 5
q = nrow(data)/k  #k개로 등분했을 때 부분 집합의 크기
l = 1:nrow(data)
accuracy = 0
for (i in 1:k) {
  test_list = ((i - 1)* q + 1) : (i*q)  #i번째를 테스트 집합으로 설정
  testData = data[test_list, ]
  train_list = setdiff(l, test_list)  #나머지를 훈련 집합으로 설정
  trainData = data[train_list, ]
  f = train(Species ~ ., data = trainData, method = 'rf')  #모델 학습(랜덤 포리스트)
  p = predict(f, newdata = testData)
  t = table(p, testData$Species)
  accuracy = accuracy + (t[1, 1] + t[2, 2] + t[3, 3])/length(test_list)  #정확률 측정&누적
}  
(average_accuracy = accuracy/k)  #평균 정확률(0.9466667)
#caret 라이브러리로 구현하기
control = trainControl(method = 'cv', number = 5)
f = train(Species ~ ., data = iris, method = 'rf', metrice = 'Accuracy', trControl = control)
confusionMatrix(f)  #정확률(0.9467)

#5.모델 선택
#1.4기본모델 선택하기: iris 데이터 예
#iris 데이터에 대해 5-겹 교차 검증을 결정 트리, 랜덤 포리스트, SVM, k-NN의 4개 모델 적용
control = trainControl(method = 'cv', number = 5)
r = train(Species ~ ., data = iris, method = 'rpart', metric = 'Accuracy', trControl = control)
f = train(Species ~ ., data = iris, method = 'rf', metric = 'Accuracy', trControl = control)
s = train(Species ~ ., data = iris, method = 'svmRadial', metric = 'Accuracy', trControl = control)
k = train(Species ~ ., data = iris, method = 'knn', metric = 'Accuracy', trControl = control)
resamp = resamples(list('결정트리' = r, '랜덤 포리스트' = f, 'SVM' = s, 'kNN' = k))
summary(resamp)
sort(resamp, decteasing = TRUE)
dotplot(resamp)  #resamples와 dotplot 함수를 이용해 모델의 성능을 시각화
#2.모델 선택하고 하이퍼 매개변수 최적화하기: colon 데이터
library(survival)
clean_colon = na.omit(colon)
clean_colon = clean_colon[c(TRUE, FALSE), ]
clean_colon$status = factor(clean_colon$status)
control = trainControl(method = 'cv', number = 10)
formula = status ~ rx + sex + age + obstruct + perfor + adhere + nodes + differ + extent + surg + node4
L = train(formula, data = clean_colon, method = 'svmLinear', metric = 'Accuray', trControl = control)
LW = train(formula, data = clean_colon, method = 'svmLinearWeights', metric = 'Accuray', trControl = control)
P = train(formula, data = clean_colon, method = 'svmPoly', metric = 'Accuray', trControl = control)
R = train(formula, data = clean_colon, method = 'svmRadial', metric = 'Accuray', trControl = control)
RW = train(formula, data = clean_colon, method = 'svmRadialWeights', metric = 'Accuray', trControl = control)
f100 = train(formula, data = clean_colon, method = 'rf', ntree = 100, metric = 'Accuray', trControl = control)
f300 = train(formula, data = clean_colon, method = 'rf', ntree = 300, metric = 'Accuray', trControl = control)
f500 = train(formula, data = clean_colon, method = 'rf', ntree = 500, metric = 'Accuray', trControl = control)
r = train(formula, data = clean_colon, method = 'rpart', metric = 'Accuray', trControl = control)
k = train(formula, data = clean_colon, method = 'knn', metric = 'Accuray', trControl = control)
g = train(formula, data = clean_colon, method = 'glm', metric = 'Accuray', trControl = control)
resamp = resamples(list('선형' = L, '선형가중치' = LW, '다항식' = P, 'RBF' = R, '가중치' = RW, 'rf100' = f100,
                        'rf300' = f300, 'rf500' = f500, 'tree' = r, 'knn' = k, 'glm' = g))
summary(resamp)
sort(resamp, decreasing = TRUE)
dotplot(resamp)  #resamples와 dotplot을 이용한 모델의 성능 시각화

#7.ROC 곡선과 AUC
#2.prediction과 performance 함수
library(ROCR)
#정답을 labels 변수에
#모델을 출력한 값을 predictions에
#prediction 함수의 첫 번째 매개변수에 예측값, 두 번째 매개변수에 정답을 주었는데 표와 유사한 정보가 추출되고 변수 p에 저장
#performance 함수는 표와 유사한 정보를 추출하여 변수 roc에 저장하고 measure옵션은 세로축 x.measure 가로축에 해당
#plot함수는 ROC곡선을 그려주고 abline 함수는 대각선을 표시해준다
#AUC를 계산하려면 performance 함수의 measure옵션을 'auc'로 설정하면 AUC의 계산 결과는 0.708333임을 알 수 있음

labels = c(0, 0, 1, 0, 1, 1, 0, 0, 0, 1)
predictions = c(0.26, 0.81, 0.73, 0.11, 0.20, 0.48, 0.23, 0.11, 0.61, 0.99)
p = prediction(predictions, labels)  
roc = performance(p, measure = 'tpr', x.measure = 'fpr')
plot(roc)  
abline(a = 0, b = 1)
auc = performance(p, measure = 'auc')
auc@y.values
