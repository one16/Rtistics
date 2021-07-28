### 친절한 스포츠 데이터 with R

## chapter 1. R 언어학 입문 & 2. tidyverse 입문
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

### 3. 데이터 시각화
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


## 3.5 선 그래프(2021-07-27)
batting[batting$rank == 1, ] # 시즌별 타율 1위인 선수들만을 뽑기

# 시즌별 타율 1위 기록 변화에 대한 그래프
batting[batting$rank == 1, ] %>% 
  ggplot(aes(x = year,
             y = avg)) +
  geom_line() # geom_line()dms x,y 두 가지 인수를 받는다.(x,y)

batting[batting$rank == 1, ] %>% 
  ggplot(aes(x = year,
             y = avg)) +
  geom_line(lwd = 1) # 선 굵기를 조절하고자 할 때는 lwd 인수를 사용한다.

batting[batting$rank == 1, ] %>% 
  ggplot(aes(x = year,
             y = avg)) +
  geom_line(lwd = 2)

# 선 종류를 고르고자 할 때는 linetype 인수를 사용한다. 다른 방법으로 lty인수를 사용하여 변경할 수 있다. 'dashed'는 lty = 2와 같다.
batting[batting$rank == 1, ] %>% 
  ggplot(aes(x = year,
             y = avg)) +
  geom_line(linetype = 'dashed') 

batting[batting$rank == 1, ] %>% 
  ggplot(aes(x = year,
             y = avg)) +
  geom_line(lty = 2)

batting[batting$rank == 1, ] %>% 
  ggplot(aes(x = year,
             y = avg)) +
  geom_line(lty = 3)

# 그 밖의 선은 ① geom_hline() ② geom_vline() ③ geom_abline() 등이 있고 모두 위와 같은 방식으로 선 두께와 종류를 변경할 수 있다.
# geom_hline()은 수평선으로, 특정한 y 지점을 기준으로 선을 그려야하기 때문에 yintercept(y절편) 인수가 필요하다.
# geom_vline()은 수직선으로, 특정한 x 지점을 기준으로 선을 그려야하기 때문에 xintercept(x절편) 인수가 필요하다.
batting[batting$rank == 1, ] %>% 
  ggplot(aes(x = year,
             y = avg)) +
  geom_line(lwd = 2, lty = 2) +
  geom_hline(yintercept = 0)

batting[batting$rank == 1, ] %>% 
  ggplot(aes(x = year,
             y = avg)) +
  geom_line(lwd = 2, lty = 2) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)

batting[batting$rank == 1, ] %>% 
  ggplot(aes(x = year,
             y = avg)) +
  geom_line(lwd = 2, lty = 2) +
  geom_hline(yintercept = 0.3) +
  geom_vline(xintercept = 1980) 

ryu <- read.csv('2020_ryu.csv')
ryu <- as_tibble(ryu)
ryu
ryu %>% 
  dim() # dim() 행과 열의 숫자

ryu %>% 
  names()
ryu$pitch_name
ryu$release_speed

## 구종별 구속에 대한 점 그래프 
# 점을 찍을 때는 geom_point()함수 사용
ryu %>% 
  ggplot(aes(x = pitch_name %>%  fct_reorder(release_speed),
             y = release_speed)) +
  geom_point()

ryu %>% 
  ggplot(aes(x = pitch_name %>%  fct_reorder(release_speed),
             y = release_speed)) +
  geom_jitter() # jitter(): 점을 옆으로 조금씩 움직이는 함수.

## 바이올린 그래프
ryu %>% 
  ggplot(aes(x = pitch_name %>% fct_reorder(release_speed),
             y = release_speed)) +
  geom_violin() +
  geom_jitter(alpha = .2) # alpha 인수는 점들의 투명도를 의미함.

ryu %>% 
  ggplot(aes(x = pitch_name %>% fct_reorder(release_speed),
             y = release_speed)) +
  geom_violin() +
  geom_jitter(alpha = .05)

## 상자 그래프(상자수염)
ryu %>% 
  ggplot(aes(x = pitch_name %>% fct_reorder(release_speed),
             y = release_speed)) +
  geom_boxplot()

## 3.7 산점도
ryu %>% 
  ggplot(aes(x = plate_x, plate_z)) +
  geom_point()

# 구종별 산점도를 그리고자 할 때는 facet_grid() 함수를 사용해서 나타낼 수 있다. () 안에 변수를 입력하여 해당 변수 각각의 산점도를 그릴 수 있다.
# (~ 변수)로 입력하면, 가로로 산점도를 나타내는 것이고,  (변수 .)으로 입력하면, 세로로 산점도를 나타내는 것이다. * 괄호 닫기 전 .을 반드시 찍어야한다.
ryu %>% 
  ggplot(aes(x = plate_x, plate_z)) +
  geom_point() +
  facet_grid( ~ pitch_name) +
  coord_fixed() # coord_fixed()는 가로 세로 비율을 일대일로 고정하라는 의미이다.

ryu %>% 
  ggplot(aes(x = plate_x, plate_z)) +
  geom_point() +
  facet_grid(pitch_name~ .) +
  coord_fixed()

# 입체감을 더해 보다 명확한 정보를 알고싶다면, geom_density_2d()함수를 사용한다.
ryu %>% 
  ggplot(aes(x = plate_x, plate_z)) +
  geom_density_2d_filled() +
  facet_grid(~ pitch_name) +
  coord_fixed() 
# 여기에 상대 타자가 오른손 타자인지 왼손 타자인지 구분하고자 stand 변수를 추가한다.
ryu %>% 
  ggplot(aes(x = plate_x, plate_z)) +
  geom_density2d_filled() +
  facet_grid(stand ~ pitch_name) +
  coord_fixed() 

ryu %>% 
  ggplot(aes(x = plate_x, plate_z)) +
  geom_density2d_filled() +
  facet_grid(stand ~ pitch_name) +
  coord_fixed() +
  guides(fill = FALSE) # guides() 함수를 통해 범례(legend)를 어떻게 처리할지 지정할 수 있다.guides(fill = FALSE)는 색칠(fill) 내용에 대해서는 범례를 표시하지 말라는 뜻이다.

# 스트라이크 존 그리기(annotate() 함수: 주석을 다는 함수)
ryu %>% 
  ggplot(aes(x = plate_x, plate_z)) +
  geom_density2d_filled() +
  annotate(
    geom = 'rect', # 사각형을 그려야하기 때문에  'rect'를 입력한다.
    xmin = -1, # 스트라이크 존은 대략 x축 (-1,1), y축 (1,3) 사이에 있다.
    xmax = 1,
    ymin = 1,
    ymax = 3,
    color = 'white', # 선 색상을 하얀색, 선 종류는 쇄선(dashed)으로 정한다
    alpha = .1,
    linetype = 'dashed' # lty = 2 와 같다.
  ) +
  facet_grid(stand ~ pitch_name) +
  coord_fixed() +
  guides(fill = FALSE)

# 구종을 직구와 체인지업으로 제한한다.
ryu[ryu$pitch_name %in% c('4-Seam Fastball', 'Changeup'), ] %>% 
  ggplot(aes(x = plate_x, plate_z)) +
  geom_density2d_filled() +
  annotate(
    geom = 'rect',
    xmin = -1,
    xmax = 1,
    ymin = 1,
    ymax = 3,
    color = 'white',
    alpha = .1,
    linetype = 'dashed'
  ) +
  facet_grid(pitch_name ~ stand) + # 왼손 타자, 오른손 타자로 보기 좋게 하기 위해 facet_grid()안의 변수들을 서로 바꿔준다.
  coord_fixed() +
  guides(fill = FALSE)

# 타율과 출루율 사이 관계에 대한 산점도
batting %>% 
  ggplot(aes(x = avg, y = obp)) +
  geom_point()

# aes() 안에 색 또는 모양 옵셥을 주어 변수 세 개 이상의 관계를 파악하기
batting %>% 
  ggplot(aes(x = avg, y = obp, color = slg)) +
  geom_point() +
  scale_color_gradient(low = 'gray75', high = '#53bfd4')

# 종류가 나뉘는 데이터는 모양으로 구분할 수 있다. 아래는 투타별로 다른 모양으로 점을 찍는다.
batting %>% 
  ggplot(aes(x = avg, y = obp, shape = throw_bat)) +
  geom_point()
