
#지도시각화 
install.packages("ggiraphExtra")
library(ggiraphExtra)


#1. 시각화 할 데이터
#1973년 미국 주별 강력 범죄율 
str(USArrests)
head(USArrests)


#데이터 중 변수로 지정되지 않은 값을 변수로 사용하기 위해 

library(tibble)

#행의 값을 데이터로 사용하기 위해 변수를 만들어 새로운 dataframe
crime<-rownames_to_column(USArrests,var = "state")
#지도데이터와 맞추기 위해 state 값를 소문자로 수정 
crime$state<-tolower(crime$state)
str(crime)


#2. 해당 위치 위경도 데이터 
#해당 위치의 위경도를 가지는 데이터프레임을 가져오기 위한 라이브러리 사용
library(ggplot2)
states_map<-map_data("state")
str(states_map)
head(states_map)

#3. 지도 시각화
#ggChoropleth(data=표현데이터, map=지도데이터 ) : 두개의 데이터 필요
ggChoropleth(data=crime,                    #표현할 데이터
             aes(fill=Murder,map_id=state), #색깔로 표현할 표현할 데이터의 변수 
             map=states_map,                #지도 데이터 : 위경도   
             interactive=T)                 #인터랙티브 T -> web page 저장 가능 




#대한민국 시도별 인구단계 구분도
install.packages("stringi")
install.packages("devtools")                #R에서 제공하는 미러 사이트외에 저장해 놓은 패키지를 사용하기 위한 라이브러리 
devtools::install_github("cardiomoon/kormaps2014") #github 에 있는 패키지를 불러오기 위한 install 방식 




library(kormaps2014)
str(changeCode(korpop1))


library(dplyr)
korpop1<-rename(korpop1,
                pop=총인구_명,
                name=행정구역별_읍면동)

str(changeCode(kormap1))
ggChoropleth(data=korpop1,
             aes(fill=pop,
                 map_id=code,
                 tooltip=name),   #지도 위에 표시할 지역명 
             map=kormap1,         #지도 데이터   
             interactive=T)       #인터렉티브 
