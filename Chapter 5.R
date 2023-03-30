#5 데이터 가공
#2.베이스 R을 이용한 데이터 가공
#gapminder 라이브러리 - 세계 각국의 기대 수명, 1인당 국내총생산, 인구 데이터 등을 집계해놓은 데이터 셋의 일부
library(gapminder) 
library(dplyr)
glimpse(gapminder)
#샘플과 속성 추출
gapminder[, c("country", "lifeExp")]  #각 나라의 기대수명
gapminder[, c("country", "lifeExp", "year")]  #각 나라의 기대수명, 측정 연도
gapminder[1:15, ]  #데이터 프레임의 행을 지정해 샘플 추출
gapminder[gapminder$country=="Croatia", ]  #Croatia인 샘플 추출
gapminder[gapminder$country=="Croatia", "pop"]   #Croatia 샘플에서 인구 속성만 추출
gapminder[gapminder$country=="Croatia", c("lifeExp", "pop")]  #Croatia 샘플에서 기대수명과 인구 추출
gapminder[gapminder$country=="Croatia"&gapminder$year>1990, c("lifeExp", "pop")]  # 1990년도 이후의 기대수명과 인구
#행/열 단위의 연산
apply(gapminder[gapminder$country=="Croatia", c("lifeExp", "pop")], 2, mean)

#3.dplyr 라이브러리를 이용한 데이터 가공
#샘플과 속성 추출
select(gapminder, country, year, lifeExp)
filter(gapminder, country=="Croatia")
#행/열 단위의 연산
#summarize 함수 사용 그룹별 통계 지표 산출
summarize(gapminder, pop_avg=mean(pop))
summarize(group_by(gapminder, continent), pop_avg=mean(pop))
summarize(group_by(gapminder, continent, country), pop_avg=mean(pop))
#%>%연산자를 이용한 연속 처리
gapminder%>%group_by(continent, country)%>%summarize(pop_avg=mean(pop))
temp1 = filter(gapminder, country=="Croatia")
temp2 = select(temp1, country, year, lifeExp)
temp3 = apply(temp2[ , c("lifeExp")], 2, mean)
temp3
gapminder%>%filter(country=="Croatia")%>%select(country, year, lifeExp)%>%summarize(lifeExp_avg = mean(lifeExp))

#4.데이터 가공의 실제
#avocado.csv
avocado <- read.csv("C:/Sources/avocado.csv", header = TRUE, sep=",")
str(avocado)
#그룹 단위 통계
(x_avg=avocado%>%group_by(region)%>%summarize(V_avg=mean(Total.Volume), P_avg = mean(AveragePrice)))
#유기농 여부를 기준으로 세분화
x_avg = avocado%>%group_by(region, year, type)%>%summarize(V_avg = mean(Total.Volume), P_avg = mean(AveragePrice))
x_avg
#연도별 아보카도 총 판매량의 변화를 시각화한 그래프
library(ggplot2)
x_avg%>%filter(region != "TotalUS") %>% ggplot(aes(year, V_avg, col = type)) + geom_line() + facet_wrap(~region)
#데이터 정렬과 검색
#arrange함수 사용 총 판매량 기준으로 순위, 최댓갑을 기록한 연도와 지역
arrange(x_avg, desc(V_avg))
x_avg = x_avg%>%filter(region !="TotalUS")
#TotalUS를 제외하고 나면 통계 함수를 직접 사용하여 처리할 수 있음
x_avg[x_avg$V_avg==max(x_avg$V_avg), ]
#Data형 데이터의 활용
library(lubridate)
(x_avg = avocado %>% group_by(region, year, month(Date), type) 
  %>% summarize(V_avg = mean(Total.Volume), P_avg = mean(AveragePrice)))

#데이터 구조 변경
#1인당 전기 생산량 데이터 파일
elex_gen = read.csv("C:/Sources/electricity_generation_per_person.csv", header = TRUE, sep = ",")
elex_gen
names(elex_gen)
names(elex_gen) = substr(names(elex_gen), 2, nchar(names(elex_gen)))
names(elex_gen)
#1인당 전기 사용량
elex_use = read.csv("C:/Sources/electricity_use_per_person.csv", header = TRUE, sep = ",")
elex_use
names(elex_use)[2:56] = substr(names(elex_use)[2:56], 2, nchar(names(elex_use)[2:56]))

library(tidyr)
elec_gen_df = gather(elex_gen, -country , key = "year", value = "ElectricityGeneration")
elec_use_df = gather(elex_use, -country , key = "year", value = "ElectricityGeneration")
elec_gen_use = merge(elec_gen_df, elec_use_df)  #1인당 전기 생산량과 전기 사용량(병합후)
