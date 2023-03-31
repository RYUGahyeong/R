#6. 데이터 시각화
#1.데이터 시각화란?
library(gapminder)
library(dplyr)
y <- gapminder %>% group_by(year, continent) %>% summarize(c_pop = sum(pop))
head(y, 20)
plot(y$year, y$c_pop)  #plot 함수를 이용한 기본 시각화 그래프
plot(y$year, y$c_pop, col = y$continent)  #그래프에 마커 색상 추가
plot(y$year, y$c_pop, col = y$continent, pch = c(1:5))  #숫자로 마커 추가
plot(y$year, y$c_pop, col = y$continent, pch = c(1:length(levels(y$continent))))  #출력하는 변수를 이용해 마커 출력
#범례 개수를 숫자로 자정
legend("topleft", legend = 5, pch = c(1:5), col = c(1:5))
#범례 개수를 데이터 개수에 맞게 지정
legend("topleft", legend = levels((y$continent)), pch = c(1:length(levels(y$continent))), col = c(1:length(levels(y$continent))))

#2.시각화의 기본 요소
#많은 양의 데이터를 효과적으로 관찰
plot(gapminder$gdpPercap, gapminder$lifeExp, col = gapminder$continent)
plot(log10(gapminder$gdpPercap), gapminder$lifeExp, col = gapminder$continent)  #로그 스케일을 이용한 그래프
library(ggplot2)
#ggplot 함수를 이용해 시각화
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, col = continent)) + geom_point() + scale_x_log10()
#pop 변수에 마커 표시
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, col = continent, size = pop)) + geom_point() + scale_x_log10()
#마커의 투명도를 이용한 정보 표시
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, col = continent, size = pop)) + geom_point(alpha = 0.5) + scale_x_log10()
#facet_wrap 함수를 이용해 자동으로 구분 생성한 그래프
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, col = continent, size = pop)) + geom_point(alpha = 0.5) + scale_x_log10() + facet_wrap(~year)

#2.데이터를 여러 관점에서 보게 함
#비교/순위
#국가명을 정확히 표시하기 우해 가로-세로 축의 위치를 바꾼 그래프
gapminder %>% filter(year == 1952&continent == "Asia") %>% ggplot(aes(reorder(country, pop), pop)) + geom_bar(stat = "identity") + coord_flip()
#아시아의 인구 순위를 보여주는 로그 스케일 막대그래프
gapminder %>% filter(year == 1952&continent == "Asia") %>% ggplot(aes(reorder(country, pop), pop)) + geom_bar(stat = "identity") + scale_y_log10() + coord_flip()
#변화 추세
#연도에 따른 대한민국 인규 변화를 시각화한 결과
gapminder %>% filter(country == "Korea, Rep.") %>% ggplot(aes(year, lifeExp, col = country)) + geom_point() +geom_line()
#continent의 lifeExp 변화에 추세선을 추가한 그래프
gapminder %>% ggplot(aes(x = year, y = lifeExp, col = continent)) + geom_point(alpha = 0.2) +geom_smooth()
#분포 혹은 구성 비율
#1952년의 lifeExp 분포 히스토그램(R의 기본 hist함수 이용)
x = filter(gapminder, year == 1952)
hist(x$lifeExp, main = "Histogram of lifeExp in 1952")
##1952년의 lifeExp 분포 히스토그램(ggplot 함수 이용)
x %>% ggplot(aes(lifeExp)) + geom_histogram()
#boxplot을 이용해 여러 대륙의 분포를 동시에 시각화한 결과
x %>% ggplot(aes(continent, lifeExp)) + geom_boxplot()
#상관관계
#lifeExp와 gdpPercap의 상관관계 파악을 위한 시각화
plot(log10(gapminder$gdpPercap), gapminder$lifeExp)

#3.시각화 도구
#plot 함수
head(cars)
plot(cars, type = "p", main = "cars")  #점 그래프
plot(cars, type = "l", main = "cars")  #type="l"은 선을 사용한 플롯
plot(cars, type = "b", main = "cars")  #type = "b"는 점과 선을 모두 사용한 플롯
plot(cars, type = "h", main = "cars")  #type = "h"는 히스토그램과 같은 막대그래프
#pie, barplot 함수
#pie와 barplot 함수를 이용해 시각화한 1952년 아시아 국가들의 gdp 구성과 순위
x = gapminder %>% filter(year == 1952 & continent == "Asia") %>% mutate(gdp = gdpPercap*pop) %>% select(country, gdp) %>% arrange(desc(gdp)) %>% head()
pie(x$gdp, x$country)
barplot(x$gdp, names.arg = x$country)
#pie와 barplot 함수를 이용해 시각화한 2007년 아시아 국가들의 gdp 구성과 순위
x = gapminder %>% filter(year == 2007 & continent == "Asia") %>% mutate(gdp = gdpPercap*pop) %>% select(country, gdp) %>% arrange(desc(gdp)) %>% head()
pie(x$gdp, x$country)
barplot(x$gdp, names.arg = x$country)
#matplot 함수
#matplot함수를 이용한 다중 플롯
matplot(iris[, 1:4], type = "l")
legend("topleft", names(iris)[1:4], lty = c(1, 2, 3, 4), col = c(1, 2, 3, 4))
#hist함수
#hist 함수를 이용한 히스토그램
hist(cars$speed)

#시각화에 특화된 ggplot2 라이브러리
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, col = continent)) + geom_point(alpha = 0.2)
#aes 함수
#ggplot2 라이브러리의 geom_bar 함수를 이용한 막대그래프
gapminder %>% filter(lifeExp>70) %>% group_by(continent) %>% summarize(n = n_distinct(country)) %>% ggplot(aes(x = continent, y = n)) + geom_bar(stat = "identity")
#geom_point 함수
#geom_histogram 함수를 이용한 히스토그램: 그룹의 분포를 수직으로 쌓아 표시
gapminder %>% filter(year == 2007) %>% ggplot(aes(lifeExp, col = continent)) + geom_histogram()
#geom_histogram 함수를 이용한 히스토그램: 그룹의 분포를 그룹별로 수평으로 펼쳐 표시
gapminder %>% filter(year == 2007) %>% ggplot(aes(lifeExp, col = continent)) + geom_histogram(position = "dodge")
#geom_boxplot 함수
#geom_boxplot 함수를 이용한 박스플롯
gapminder %>% filter(year == 2007) %>% ggplot(aes(continent, lifeExp, col = continent)) + geom_boxplot()
#scale_x_log10 scale_y_log10 함수
#scale_x_log10 함수를 이용해 가로축에 log10 스케일을 적용한 그래프
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, col = continent)) + geom_point(alpha = 0.2)
#가로축을 로그 스케일로 변환함
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, col = continent)) + geom_point(alpha = 0.2) + scale_x_log10()
#coord_flip 함수
gapminder %>% filter(continent == "Africa") %>% ggplot(aes(country, lifeExp)) + geom_bar(stat = "identity")  #기본 막대그래프
#플롯의 방향 전환
gapminder %>% filter(continent == "Africa") %>% ggplot(aes(country, lifeExp)) + geom_bar(stat = "identity") + coord_flip()
#scale_fill_brewer 함수
library(RColorBrewer)
#RColorBrewer 라이브러리에서 제공하는 다양한 색상 팔레트들의 이름과 구성
display.brewer.all()
#기본 팔레트를 적용한 그래프
gapminder %>% filter(lifeExp>70) %>% group_by(continent) %>% summarize(n = n_distinct(country)) %>% ggplot(aes(x = continent, y = n)) + geom_bar(stat = "identity", aes(fill = continent))
#Spectral 팔레트를 사용한 그래프
gapminder %>% filter(lifeExp>70) %>% group_by(continent) %>% summarize(n = n_distinct(country)) %>% ggplot(aes(x = continent, y = n)) + geom_bar(stat = "identity", aes(fill = continent)) + scale_fill_brewer(palette = "Spectral")
#Blues 팔레트를 적용한 그래프
gapminder %>% filter(lifeExp>70) %>% group_by(continent) %>% summarize(n = n_distinct(country)) %>% ggplot(aes(x = continent, y = n)) + geom_bar(stat = "identity", aes(fill = continent)) + scale_fill_brewer(palette = "Blues")
#Oranges 팔레트를 적용한 그래프
gapminder %>% filter(lifeExp>70) %>% group_by(continent) %>% summarize(n = n_distinct(country)) %>% ggplot(aes(x = continent, y = n)) + geom_bar(stat = "identity", aes(fill = continent)) + scale_fill_brewer(palette = "Oranges")
#순위 표시에 적합하도록 순서 정렬과 연속적 색상 팔레트를 적용한 그래프
gapminder %>% filter(lifeExp>70) %>% group_by(continent) %>% summarize(n = n_distinct(country)) %>% ggplot(aes(x = reorder(continent, n), y = n)) + geom_bar(stat = "identity", aes(fill = continent)) + scale_fill_brewer(palette = "Blues")

#4.시각화를 이용한 데이터 탐색
#gapminder 데이터의 시각적 탐구
#gapminder 데이터의 gdpPercap과 lifeExp를 연도별로 구분한 시각화
gapminder %>% ggplot(aes(gdpPercap, lifeExp, col = continent)) + geom_point(alpha = 0.2) + facet_wrap(~year) + scale_x_log10()
#쿠웨이트 경제지표 변화와 특성
gapminder %>% filter(year == 1952&gdpPercap > 10000 & continent == "Asia")
#쿠웨이트 gdpPercap과 pop의 연도별 변화 그래프
gapminder %>% filter(country == "Kuwait") %>% ggplot(aes(year, gdpPercap)) + geom_point() + geom_line()  #gdpPercap의 변화
gapminder %>% filter(country == "Kuwait") %>% ggplot(aes(year, pop)) + geom_point() + geom_line()  #pop의 변화
#대한민국과 비교
#대한민국 gdpPercap과 pop의 연도별 변화 그래프
gapminder %>% filter(country == "Korea, Rep.") %>% ggplot(aes(year, gdpPercap)) + geom_point() + geom_line()  #gdpPercap의 변화
gapminder %>% filter(country == "Korea, Rep.") %>% ggplot(aes(year, pop)) + geom_point() + geom_line()  #pop의 변화
#대한민국과 쿠웨이트의 연도에 따른 gdp 변화 비교 그래프
gapminder %>% filter(country == "Kuwait" | country == "Korea, Rep.") %>% mutate(gdp = gdpPercap*pop) %>% ggplot(aes(year, gdp, col = country)) + geom_point() + geom_line()
#산업 형태에 따른 경제지표 변화의 차이
#gdpPercap의 변화 비교
gapminder %>% filter(country == "Kuwait" | country == "Saudo Arabia" | country == "Iraq" | country == "Iran" | country == "Korea, Rep" | country == "China" | country == "Japan") %>% ggplot(aes(year, gdpPercap, col = country)) + geom_point()+geom_line()
#pop의 변화 비교
gapminder %>% filter(country == "Kuwait" | country == "Saudo Arabia" | country == "Iraq" | country == "Iran" | country == "Korea, Rep" | country == "China" | country == "Japan") %>% ggplot(aes(year, pop, col = country)) + geom_point()+geom_line()
#gdpPercap의 변화 비교
gapminder %>% filter(country == "Kuwait" | country == "Saudo Arabia" | country == "Iraq" | country == "Iran" | country == "Korea, Rep" | country == "China" | country == "Japan") %>% ggplot(aes(year, gdpPercap, col = country)) + geom_point()+geom_line() + scale_y_log10()


