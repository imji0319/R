library(dplyr)
install.packages("hflights")
library(hflights)

# hflights DB : 미국 휴스턴에서 출발하는 모든 비행기의 2011년 이착륙기록이 수록

dim(hflights)

hflights_df <- tbl_df(hflights)
# tbl_df : dplyr 의 함수로 data frame 형태로 변환 
hflights_df

###  filter : dplyr 패키지의 기본 함수로 조건에 따라 row를 추출하고, 추출 대상이 되는 데이터 프레임과 추출하고 싶은 행의 조건 지정 
# 1월 데이터 추출
filter(hflights_df,Month == 1, DayofMonth == 1)

# 1, 2월 데이터 추출
filter(hflights_df,Month == 1 | Month ==2 )

###  arragne : dplyr 패키지 기본 함수로 지정한 열을 기준으로 기본 내림차순, 오름차순은 desc 옵션 사용
# ArrDelay, Month, Year 를 내림차순으로 정렬
arrange(hflights_df,ArrDelay, Month, Year)

# Month 오름차순 정렬
arrange(hflights_df,desc(Month))


###  select : dplyr 패키지 기본 함수로 colume을 추출하고 여러 개 추출할 떄는 , 로 구분, 인접한 열을 추출할 때는 : 를 사용 , 제외할 colume은 - 부호 사용
# Year,Month, DayOfWeek 열 추출
select(hflights_df,Year,Month,DayOfWeek )

# Year부터 DayOfWeek까지 Year, Month, DayOfMonth, DayOfWeek 추출
select(hflights_df,Year : DayOfWeek)

# Year 부터 DayOfWeek 를 제외한 나머지 열 추출
select(hflights_df, -(Year : DayOfWeek))

### mutate : dplyr 패키지 함수로 열을 추가하는 경우 사용 , 비슷한 함수 : transform , 새로 만든 열을 함수에서 바로 사용 가능 

mutate(hflights_df, gain = ArrDelay -DepDelay,
       gain_per_hour = gain/(AirTime/60))

# transform 은 새로 만든 열을 바로 사용 불가능 
transform(hflights_df, gain = ArrDelay -DepDelay,
       gain_per_hour = gain/(AirTime/60))


### summarise : dplyr 패키지 함수로 통계량 계산 가능
# 평균 출발 지연 시간
summarise(hflights_df, delay = mean(DepDelay, na.rm=TRUE))

### group_by : dplyr 패키지 함수로 지정한 열별로 그룹화

# 비행편수 20편 이상, 평균 비행 거리 2000마일 이하인 항공사별 평균 연착시간 
planes <- group_by(hflights_df, TailNum)
delay <- summarise(planes, count = n(),  # 비행편수
                   dist = mean(Distance, na.rm = TRUE),  # 평균 비행거리 
                   delay = mean(ArrDelay,na.rm=TRUE))    # 평균 연착시간 

delay<-filter(delay, count > 20, dist < 2000) # 비행편수 20 이상, 평균 비행거리 2000마일 이하 

library(ggplot2)

ggplot(delay,aes(dist, delay)) + 
  geom_point(aes(size = count ), alpha = 1/2) + 
  geom_smooth() +
  scale_size_area()
  
