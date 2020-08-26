#  추가실습 1 

name = c('KIM','LEE','LIM','PARK')
korea = c(90,85,95,75)
math = c(65,80,90,95)
eng = c(85,75,85,90)
score = data.frame(name, korea, math, eng)

# 문제 1. 세 과목의 평균 성적을 구하시오.
sapply(score, mean)

# 문제 2. 수학, 영어 성적이 모두 80점 이상인 학생을 찾으시오.
subset(score, math>=80 & eng >=80)

# 문제 3. 이름과 국어 성적만 출력하시오.
subset(score, select=c('name','korea'))

# 추가 실습 2

# 문제 1. for문을 이용해 구구단 2-9단 만들기 

for (i in 2:9){
  for (j in 1:9){
    print(paste(i, "x", j, "=", i*j))
  }
}

# 문제 2. 1부터 100까지의 수 중에서 3의 배수이면서 4의 배수는 아닌 수의 합을 구하라.

func1 <-function(num){
  sum = 0
  for (i in 1:num){
    if (i %% 3 == 0 & i %%4 !=0) {
      sum = sum + i
    }
  }
  return(sum)
}


# 문제 3. x와 n을 입력하면 1 부터 n까지의 수 중에서 x의 배수의 합을 구하는 함수를 만들어라

multipsum <- function(x,n){
  sum=0
  for (i in 1:n){
    if (i %% x == 0 ){
      sum = sum + i
    }
  }
  return(sum)
}

multipsum(3,100)

# 추가실습 3 - gapminder 이용하기
install.packages("gapminder")
install.packages("dplyr")
library(gapminder) 
library(dplyr)

glimpse(gapminder) # dplyr : like a transposed version of print()
# country : 국가
# continent : 대륙 
# year : 5년주기의 연도, ranges from 1952 to 2007 in increments of 5 years 
# lifeExp : 기대수명
# pop : 인구 수
# gdpPercep : GDP 

str(gapminder)
levels(gapminder$country)

# 문제 1 : 우리나라의 1990년도 이후의 기대수명과 인구추출 "Korea, Rep."
subset(gapminder, country == 'Korea, Rep.' & year>1990 ,select=c('year','lifeExp','pop'))

# 문제 2 : 북한의 1990년ㄷ 이후의 기대 수명과 인구 추출  "Korea, Dem. Rep."
subset(gapminder, country =='Korea, Dem. Rep.' & year>1990, select = c('year', 'lifeExp','pop'))



