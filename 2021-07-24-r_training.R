### 친절한 스포츠 데이터 with R

getwd()
setwd('C:/Users/kko/Desktop/R-training/0_전체_압축_파일')
batting <- read.csv('kbo_batting_qualified.csv')
length(ls('package:base'))
install.packages('tidyverse')
library('tidyverse')
install.packages('pacman')
library('pacman')
install.packages('tidymodels')
library('tidymodels')

batting <- read_csv('kbo_batting_qualified.csv',locale = locale('ko', encoding = 'euc-kr'))
glimpse(batting)
batting
class(batting)

df1 <- data.frame(x = c(1,2,3),
                  y = c(4,5,6))
df1

df2 <- tribble(~x, ~y,
               1, 4,
               2, 5,
               3, 6)
df2

df2 <- tribble(~x, ~y,
               #---:---
               1, 4,
               2, 5,
               3, 6)
df2
df1
df2

df3 <- read.csv(textConnection("
                               x, y
                               1, 4
                               2, 5
                               3, 6 
                               "))
df3

df3 <- read.csv(textConnection("
                               x, y
                               1, 4
                               2, 5
                               3, 6 "))
df3

1:10 %>% sum()
1:10 %>% mean()
1:10 %>% median()

batting %>% print()
batting %>% print(n=20)

batting %>% print(n = Inf)
batting %>% print(width = Inf)

options(dplyr.print_min = Inf)
options(tibble.width = Inf)
batting %>% View()

# <- 단축키 'Alt + -'
# %>% 단축키 'Ctrl + Shift + m'
# 주석으로 변환 'Ctrl + Shift + c'
# R session 초기화 'Ctrl + Shift + F10'
# 전체 단축기 목록 'Alt + Shift + k'

### 데이터 시각화

library(ggplot2)
ggplot(batting)
ggplot(data = batting)
ggplot(batting, mapping = aes(x = avg)) +
  geom_histogram()
ggplot(batting, aes(avg)) +
  geom_histogram()
ggplot(batting, aes(avg)) +
  geom_histogram(binwidth = .001)
batting %>% 
  ggplot(aes(avg)) +
  geom_histogram(bins = 30, fill = 'black', color = 'red')

# 사용가능한 색상 colors()
batting %>% 
  ggplot(aes(avg)) +
  geom_histogram(bins = 30,
                 fill = rgb(0.247, 0.513, 0.632),
                 color = 'white')
# rgb() : (red, green, blue)의 머리글자로 각 색깔에 대한 비율(0~1) 조합을 통해 특정 색상이 나오게 하는 함수. 해당 예를 통해, 빨강 24.7%, 초록 51.3%, 파랑 63.2% 비율로 섞은 것이다.
rgb(0.247, 0.513, 0.632)
# rgb()를 입력하면 해당 색상을 16진수 숫자 세 개로 이루어진 고유 코드가 출력된다.
batting %>% 
  ggplot(aes(avg)) +
  geom_histogram(bins = 30, fill = '#3F83A1', color = 'white')

batting %>% 
  ggplot(aes(throw_bat)) +
  geom_bar()

batting %>% 
  ggplot(aes(x = throw_bat,
             y = stat(count))) +
  geom_bar()

batting %>% 
  ggplot(aes(throw_bat)) +
  stat_count()

## 데이터 자체가 개수일 때
batting$throw_bat %>%
  table()
tribble(
  ~throw_bat, ~count,
  '우양', 30,
  '우우', 1001,
  '우좌', 155,
  '좌좌', 435
) -> bar_example

bar_example %>% 
  ggplot(aes(x = throw_bat, 
             y = count)) +
  geom_bar(stat = 'identity')

bar_example %>% 
  ggplot(aes(throw_bat, count)) +
  geom_col()

bar_example$throw_bat
bar_example$throw_bat %>% 
  as_factor()
# 궁금증: level이 의미하는 바? 가나다 순에서 처리순서 등급(?)

# 레벌 조정(수동)
bar_example$throw_bat %>% 
  as_factor() %>% 
  fct_relevel('우우', '좌좌', '우좌', '우양')
  
# 레벨 조정(자동) fct_reorder 함수 이용(작은 순서)
bar_example$throw_bat %>% 
  fct_reorder(bar_example$count)

bar_example %>% 
  ggplot(aes(x = throw_bat %>%  fct_reorder(count),
             y = count)) +
  geom_bar(stat = 'identity')

bar_example %>% 
  ggplot(aes(x = throw_bat %>%  fct_reorder(count),
             y = count)) +
  geom_col()

bar_example %>% 
  ggplot(aes(x = throw_bat %>%  fct_reorder(count, .desc = TRUE),
             y = count)) +
  geom_col()

bar_example %>% 
  ggplot(aes(x = throw_bat %>%  fct_reorder(-count),
             y = count)) +
  geom_col()

batting %>% 
  ggplot(aes(x = throw_bat %>%  fct_infreq())) +
  geom_bar()

batting %>% 
  ggplot(aes(x = throw_bat %>%  fct_infreq())) +
  geom_col()
# 오류: geom_bar()과 geom_col을 어떻게 적절히 사용할지? -> 데이터 개수를 이미 알고 있을 때는 geom_col 또는 geom_bar(stat= 'identity'), 데이터 개수를 현재 알고 있지 않은 상태(bar_example 처럼 객체로 저장되어 있지 않은 상태)라면 geom_bar()을 사용한다. 
