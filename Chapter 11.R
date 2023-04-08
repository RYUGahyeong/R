#11. 텍스트 마이닝
#2.DTM 구축 (DTM = 텍스트 문서를 일정한 크기의 벡터로 변환한 것)
#2.R을 이용한 전처리와 DTM 구축
library(RCurl)  #웹 서버와 접속할 수 있도록 도와줌
library(XML)  #윕 문서를 처리하는 기능 제공
t = readLines('https://en.wikipedia.org/wiki/Data_science')  #readLines함수는 지정된 URL에서 html 파일을 읽어오는 역할
d = htmlParse(t, asText = TRUE)  #htmlParse, xpathSApply 함수는 웹 문서를 R데이터형으로 변환해줌
clean_doc = xpathSApply(d, "//p", xmlValue)

library(tm)  #여러가지 유용한 텍스트 마이닝 함수 제공
library(SnowballC)  #어간을 추출하는 함수를 제공
doc = Corpus(VectorSource(clean_doc))
inspect(doc)
#텍스트 전처리
doc = tm_map(doc, content_transformer(tolower))  #소문자 변환
doc = tm_map(doc, removeNumbers)  #숫자 제거
doc = tm_map(doc, removeWords, stopwords('english'))  #영어 불용어 제거
doc = tm_map(doc, removePunctuation)  #구두점 제거
doc = tm_map(doc, stripWhitespace)  #공백 문자 제거

dtm = DocumentTermMatrix(doc)
dim(dtm)  #변수의 행과 열 개수(15 357)
inspect(dtm)  #상세 내용을 요약

#3.단어 구름
#1.wordcloud 라이브러리
library(wordcloud)
m = as.matrix(dtm)
v = sort(colSums(m), decreasing = TRUE)  #빈도가 높은 순서로 단어 정렬
d = data.frame(word = names(v), freq = v)  #데이터 프레임으로 변환
#max.words = 100은 빈도가 높은 상위 100개 단어를 보이게함, rot.per = 0.35는 세로로 배치한 단어의 비율을 35%
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words = 100, random.order = FALSE, rot.per = 0.35)
#단어 구름에 색상, 폰트 변경
library(RColorBrewer)
pal = brewer.pal(11, "Spectral")  #Spectral팔레트 사용
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words = 50, random.order = FALSE, 
          rot.per = 0.50, colors = pal)  #단어 개수 조절
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words = 50, random.order = FALSE, 
          rot.per = 0.50, colors = pal, family = "mono", font = 2)  #폰트 조절
#wordcloud2 라이브러리
library(wordcloud2)
wordcloud2(d)  #wordcloud2로 만든 단어 구름
d1 = d[1:200, ]  #200개 단어만 표시
wordcloud2(d1, shape = 'star')  #별 모양 단어 구름 
#pi/4로 회전 각도 설정, rotateRatio로 모든 단어가 한 방향을 향하게 하기
wordcloud2(d1, minRotation = pi/4, maxRotation = pi/4, rotateRatio = 1.0) 
#3.빈도 표시하기
findFreqTerms(dtm, lowfreq = 12)  #findFreqTerms함수는 단어의 빈도를 보여줌 발생 빈도가 12 이상인 단어
findAssocs(dtm, terms = 'harvard', corlimit = 0.7)  #findAssocs함수는 주어진 단어와 상관관계가 큰 단어순으로 보여줌 harvard과 상관관계가 0.7 이산인 단어
barplot(d[1:10, ]$freq, las = 2, names.arg = d[1:10, ]$word, col = 'lightblue', 
        main = '발생 빈도 상위 단어', ylab = '단어 빈도')  #중요도에 따라 단어를 나열한 막대그래프
#4.텍스트 이외의 응용: gapminder 예제
library(gapminder)
library(dplyr)
pop_siz = gapminder$filter(year == 2007) %>% group_by(continent) %>% summarize(sum(as.numeric(pop)))
d = data.frame(word = pop_siz[, 1], freq = pop_siz[, 2])
wordcloud(word = d[, 1], freq = d[, 2], min.freq = 1, max.words = 100, random.order = FALSE, rot.per = 0.35)
wordcloud2(d)

#4.문서 분류
#1.영화평 분류: movie_review 데이터 모델링
library(text2vec)
library(caret)
str(movie_review)
head(movie_review)
#데이터를 훈련 집합(mtrain)과 테스트집합(mtest)으로 나눔
train_list = createDataPartition(y = movie_review$sentiment, p = 0.6, list = FALSE)
mtrain = movie_review[train_list, ]
mtest = movie_review[-train_list, ]
#훈련 집합으로 DTM 구축
doc = Corpus(VectorSource(mtrain$review))
doc = tm_map(doc, content_transformer(tolower))
doc = tm_map(doc, removeNumbers)
doc = tm_map(doc, removeWords, stopwords('english'))
doc = tm_map(doc, removePunctuation)
doc = tm_map(doc, stripWhitespace)

dtm = DocumentTermMatrix(doc)
dim(dtm)
str(dim)
inspect(dtm)
dtm_small = removeSparseTerms(dtm, 0.90)  #제거할 단어의 비율 0.9로 설정
X = as.matrix(dtm_small)
dataTrain = as.data.frame(cbind(mtrain$sentiment, X))
dataTrain$V1 = as.factor(dataTrain$V1)
colnames(dataTrain)[1] = 'y'
#훈련 데이터인 dataTrain으로 결정 트리와 랜덤 포리스트를 학습
library(rpart)
r = rpart(y ~ ., data = dataTrain)
printcp(r)
par(mfrow = c(1, 1), xpd = NA)
plot(r)
text(r, use.n = TRUE)  #movie_review 데이터로 학습한 결정 트리
library(randomForest)
f = randomForest(y ~ ., data = dataTrain)
#2.예측과 성능 평가
docTest = Corpus(VectorSource(mtest$review))
docTest = tm_map(docTest, content_transformer(tolower))
docTest = tm_map(docTest, removeNumbers)
docTest = tm_map(docTest, removeWords, stopwords('english'))
docTest = tm_map(docTest, removePunctuation)
docTest = tm_map(docTest, stripWhitespace)
 
dtmTest = DocumentTermMatrix(docTest, control = list(dicrionary = dtm_small$dimnames$Terms))  #훈련 집합으로 만든 사전을 테스트 집합에 그대로 사용
dim(dtmTest)
str(dtmTest)
inspect(dtmTest)

X = as.matrix(dtmTest)
dataTast = as.data.frame(cbind(mtrain$sentiment, X))
dataTast$V1 = as.factor(dataTast$V1)
colnames(dataTast)[1] = 'y'
pr = predict(r, newdata = dataTast, type = 'class')
table(pr, dataTast$y)  
pf = predict(f, newdata = dataTast)  #결정 트리는 (570+934)/200 = 75.2%
table(pf, dataTast$y) #랜덤 포리스트는 (738+700)/2000 = 75.4%

#5.영어 텍스트 마이닝을 아용한 한국어 처리
library(tm)
library(XML)
library(wordcloud2)
library(SnowballC)
library(RCurl)
t = readLines('https://ko.wikipedia.org/wiki/%EB%B9%85_%EB%8D%B0%EC%9D%B4%ED%84%B0')
d = htmlParse(t, asText = TRUE)
clean_doc = xpathApply(d, "//p", xmlValue)

doc = Corpus(VectorSource(clean_doc))
inspect(doc)
doc = tm_map(doc, content_transformer(tolower))
doc = tm_map(doc, removeNumbers)
doc = tm_map(doc, removePunctuation)
doc = tm_map(doc, stripWhitespace)
#DTM 구축
dtm = DocumentTermMatrix(doc)
dim(dtm)
inspect(dtm)
#wordcloud2 함수 적용
m = as.matrix(dtm)
v = sort(colSums(m), decreasing = TRUE)
d = data.frame(word = names(v), freq = v)
d1 = d[1: 500, ]  #500개 단어만 표시
wordcloud2(d1)  #위키피디아의 '빅 데이터' 문서에서 추출한 단어 구름
