
#인천공항 2019-01-01 ~ 2019-12-31 출발데이터 
#https://kuduz.tistory.com/1201

library('tidyverse')
library('lubridate')


#tibble 
#https://statkclee.github.io/data-science/data-handling-tibble.html

icn <- read.csv('icn.csv', fileEncoding="euc-kr" ) %>% as_tibble()
icn

# 날짜, 시간 데이터로 변경 
icn %>%
  mutate(날짜=paste(연, 월, 일, 계획) %>% ymd_hm()) %>%
  select(연, 월, 일, 계획, 날짜)

# 차이 : 출발시간 - 계획시간
icn %>% 
  mutate(계획시간=paste(연, 월, 일, 계획) %>% ymd_hm,
         출발시간=paste(연, 월, 일, 출발) %>% ymd_hm,
         차이=interval(계획시간, 출발시간)/minutes(1)) %>% # interval : 시간 차이, minutes : 분 단위 
  select(계획시간, 출발시간, 차이, 구분, 현황)

# 여객편만 골라내기 
icn %>% 
  filter(구분=='여객') %>%
  mutate(계획시간=paste(연, 월, 일, 계획) %>% ymd_hm,
         출발시간=paste(연, 월, 일, 출발) %>% ymd_hm,
         차이=interval(계획시간, 출발시간)/minutes(1)) %>%
  select(계획시간, 출발시간, 차이, 현황)

## 현황 : 지연 > 게획시간보다 출발시간이 늦을 경우 => 지연의 판단 기준 시간??

icn %>% 
  filter(구분=='여객') %>%
  mutate(계획시간=paste(연, 월, 일, 계획) %>% ymd_hm,
         출발시간=paste(연, 월, 일, 출발) %>% ymd_hm,
         차이=interval(계획시간, 출발시간)/minutes(1)) %>%
  select(계획시간, 출발시간, 차이, 현황) %>%
  filter(현황=='지연') %>%
  arrange(차이)

# => 오류를 제외하고 "30분" 이 지연 판단의 기준 

# 지연이 많은 달은?
icn %>% 
  filter(구분=='여객') %>%
  mutate(출발시간=paste(연, 월, 일, 출발) %>% ymd_hm,
         월=month(출발시간, label=T)) %>% #month : 월 추출 
  group_by(월, 현황) %>%
  summarise(count=n()) %>%
  mutate(비율=count/sum(count)) %>%
  filter(현황=='지연') %>%
  select(월, 비율)

#ggplot 
icn %>% 
  filter(구분=='여객') %>%
  mutate(출발시간=paste(연, 월, 일, 출발) %>% ymd_hm,
         월=month(출발시간, label=T)) %>%
  group_by(월, 현황) %>%
  summarise(count=n()) %>% 
  mutate(비율=count/sum(count)) %>%
  filter(현황=='지연') %>%
  ggplot(aes(x=월, y=비율)) +
  geom_bar(stat='identity') +
  geom_text(aes(y=비율-.003, label=format(비율*100, digits=2)), color='#ffffff')

# 요일별 지연율 
icn %>% 
  filter(구분=='여객') %>%
  mutate(출발시간=paste(연, 월, 일, 출발) %>% ymd_hm,
         요일=wday(출발시간, label=T)) %>%
  group_by(요일, 현황) %>%
  summarise(count=n()) %>% 
  mutate(비율=count/sum(count)) %>%
  filter(현황=='지연') %>%
  ggplot(aes(x=요일, y=비율)) +
  geom_bar(stat='identity') +
  geom_text(aes(y=비율-.003, label=format(비율*100, digits=2)), color='#ffffff')


#시간대 지연율 
icn %>% 
  filter(구분=='여객') %>%
  mutate(계획시간=paste(연, 월, 일, 계획) %>% ymd_hm,
          시간대=hour(계획시간)) %>%
  group_by(시간대, 현황) %>%
  summarise(count=n()) %>% 
  mutate(비율=count/sum(count)) %>%
  filter(현황=='지연') %>%
  ggplot(aes(x=시간대, y=비율)) +
  geom_bar(stat='identity')


# 각 시간대별 출발 예정 항공편의 수
icn %>% 
  filter(구분 == '여객') %>%
  mutate(계획시간 = paste(연, 월, 일, 계획) %>% ymd_hm,
         시간대 = hour(계획시간)) %>% 
  group_by(시간대) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=시간대, y=count)) + geom_bar(stat = 'identity')

# 분 단위 
icn %>% 
  filter(구분=='여객') %>%
  mutate(계획시간=paste(연, 월, 일, 계획) %>% ymd_hm,
         시간대=minute(계획시간)) %>% 
  group_by(시간대, 현황) %>%
  summarise(count=n()) %>%
  mutate(비율=count/sum(count)) %>%
  filter(현황=='지연') %>%
  ggplot(aes(x=시간대, y=비율)) +
  geom_bar(stat='identity')

# 10분 단위 > 내림 방식을 사용 -> floor_date(, '10 mins')
icn %>% 
  filter(구분=='여객') %>%
  mutate(계획시간=paste(연, 월, 일, 계획) %>% ymd_hm,
         시간대=floor_date(계획시간, '10 mins') %>% minute()) %>%
  group_by(시간대, 현황) %>%
  summarise(count=n()) %>%
  mutate(비율=count/sum(count)) %>%
  filter(현황=='지연') %>%
  ggplot(aes(x=시간대, y=비율)) +
  geom_bar(stat='identity')
