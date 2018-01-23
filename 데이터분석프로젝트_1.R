#데이터 분석 프로젝트 
##한국복지패널데이터

setwd("c:\\R_TEMP")
install.packages("foreign") #spss 파일을 읽을 수 잇도록 도와주는 라이브러리 
library(foreign)
library(dplyr)              #전처리
library(ggplot2)            #시각화
library(readxl)             #엑셀 파일 

###데이터 준비 단계


#원본데이터 -> 가구, 가구원, 아동 데이터를 merge한 데이터파일 
raw_welfare<-read.spss(file="Koweps_hpc10_2015_beta1.sav", #spss 파일 
                      to.data.frame=T)                     #to.data.frame=T : 옵션 사용하지 않으면 list 
#### 연습용 프로젝트 임으로 시스템으로 인해 적게 들어오는 한계 존재함을 인지 

#복사본 만들어 원본 데이터 보관
welfare<-raw_welfare

#데이터 확인 
str(welfare)       #각 변수의 속성 확인 
dim(welfare)       #데이터의 행열의 갯수 즉 크기 확인 
summary(welfare)   # 데이터의 요약 통계량 


#조사설계서 변수 설명 
#10차 한국복지패널 -가구용 파일

#h10_g3: 성별 => 23R
###1 : 남 ,2 : 여 

#h10_g4 : 태어난 연도 => 24R
### 1900~2014

#h10_g10 : 혼인상태 => 29R+
### 0.비해당(18세 미만) 1.유배우 2.사별 3.이혼
### 4.별거5.미혼(18세이상, 미혼모 포함) 6.기타(사망 등)

#h10_g11 : 종교 => 30R 
### 1 : 있음 2 : 없음

#h10_eco9 : 직종 => 251R
###sheet3의 직종 코드 ,4자리 


#h10_reg7 : 7개 권역 지역구분 => 11R
###1. 서울  2. 수도권(인천/경기)  3. 부산/경남/울산   
###4.대구/경북   5. 대전/충남   6. 강원/충북   7.광주/전남/전북/제주도"

##### h10_reg5 : 5개 권역 => 10R
##### 1. 서울    2.광역시   3.시   4.군   5.도농복합군


#10차 가구원용 beta 파일
#p1002_8aq1 : 소득=> 59R
###만원단위 1~9998

#사용할 변수 선택 및 변수명 바꾸기 

welfare<-rename(welfare,                #데이터
               sex=h10_g3,              #성별 
               birth=h10_g4,            #태어난 연도
               marriage=h10_g10,        #혼인상태
               income=p1002_8aq1,       #평균 월급
               religion=h10_g11,        #종교 여부
               code_job=h10_eco9,       #직종 코드
               code_region=h10_reg7)    #지역 코드 

#새로운 변수
#sex_income : 남녀의 월급 평균을 가지는 테이블  
#age_income : 나이별 월급 평균표
#ageg_income :연령대 월급 평균표
#sex_income :연령대와 성별 월급 평균표
#sex_age : 나이와 성별에 따른 월급 평균표 
#list_job : 직업코드명과 직업명 테이블
#job_income : 직업별 월급 분석표 
#top10: 상위 10 개 직업의 월급 
#tail10: 하위 10 개 직업의 월급 
#job_male : 남성 직업 빈도표 top10
#job_female : 여성 직업 빈도표 top10
#religion_marriage: 종교 유무에 따른 이혼율 표 
#divorce: 종교 유무에 따른 이혼율 표에서 이혼추출 표
#ageg_re_marriage : 연령대, 종교, 혼인상태를 확인 변수
#A_R_divorce : 연령층이 중년과 노년, 종교 유무에 의한 이혼 비율표
#list_region : 지역 코드 데이터 프레임
#region_ageg : 지역별 연령대 비율표
#list_order_old: 노년층 비율 내림차순 정렬표
#order : 노년층 지역 빈도에 의한 지역명 순서 변수 




###데이터 전처리 단계 
#####: 필요한 변수 검토 및 전처리, 변수 간의 관계 분석 

###데이터 분석 단계 
#####: 새로운 요약 테이블 생성, 그래프 시각화 



## Q1. 성별에 따라 월급이 다를까?

# 사용 변수 전처리 단계
### 성별 sex : range(남 : 1, 여 : 2)   * 응답 모름 : 9
#1. 변수 검토
class(welfare$sex)   #변수 속성
table(welfare$sex)   #변수 빈도


#2. 이상치 제거
welfare$sex <-ifelse(welfare$sex==9,NA,welfare$sex)
table(is.na(welfare$sex)) #NA 확인 


#3. 데이터 값 변경 1 -> 남 , 2 -> 여
welfare$sex<-ifelse(welfare$sex == 1 ,'male','female')
table(welfare$sex)
qplot(welfare$sex) # 빈도그래프로 데이터 확인 


### 월급 income : range(1,9998)  * 모름/ 무응답 : 9999 ,  0 : 아동 데이터  
#1. 변수 검토
class(welfare$income)                  #변수 속성
summary(welfare$income)                #데이터 요약 통계량 -> 이상치 확인 
qplot(welfare$income) + xlim(0,1000)   #데이터 확인을 위한 그래프로 범위 제한 (xlim)


#2. 이상치 제거
welfare$income<-ifelse(welfare$income %in% c(0,9999),NA,welfare$income)   #0,9999 -> NA
table(is.na(welfare$income))


#데이터 분석 단계
#필요한 데이터테이블 만들기 : 남녀의 월급 평균을 가지는 테이블 : sex_income
sex_income <- welfare %>% 
  filter(!is.na(income)) %>%           #income의 NA 값 제외
  group_by(sex) %>%                    #sex 에 따라 그룹화
  summarise(mean_income=mean(income))  #sex에 따른 income의 평균 


#성별에 따른 평균월급 그래프 
ggplot(data=sex_income,aes(x=sex, y=mean_income)) + 
  geom_col()                           #geom_col () : 집단 간 차이를 알 수 있는 막대 그래프 


###### 결과 ######
#성별에 따라 월급의 차이가 있다.





## Q2. 몇 살때 월급을 가장 많이 받을까?

# 사용 변수 전처리 단계
### 태어난 연도 birth : range(1900,2014)    * 모름 / 무응답 : 9999 
#1. 변수 검토
class(welfare$birth)                   #변수 속성
summary(welfare$birth)                 #데이터 요약 통계량 -> 이상치 확인 
qplot(welfare$birth)                   #데이터 확인을 위한 그래프
table(is.na(welfare$birth))


#2. 이상치 제거
welfare$birth<-ifelse(welfare$birth==9999,NA,welfare$birth)


#3. 파생 변수 만들기 : 나이 age 
welfare$age <- 2015 - welfare$birth +1
summary(welfare$age)                   #데이터 요약 통계량 -> 이상치 확인
qplot(welfare$age)                     #나이 범위 확인 


### 월급  - Q1.에서 이미 모름과 무응답에 해당하는 값은 NA로 처리
#1. 변수 검토
#2. 이상치 제거


#데이터 분석 단계
##나이 월급 평균표
age_income<-welfare %>% 
  filter(!is.na(income)) %>%           #income의 NA 값 제외
  group_by(age) %>%                    #age에 따라 그룹화 
  summarise(mean_income=mean(income))  #age에 따른 income의 평균

age_income


#나이에 따른 평균월급 그래프 
ggplot(data=age_income, aes(x=age,y=mean_income)) +
  geom_line()                          #geom_line() : 흐름에 따른 선 그래프 


###### 결과 ######
#소득은 서서히 점점 올라 40-50대 에서 가장 높으며 그 이후 60대 이후 하락세가 이어진다.





# Q3. 어떤 연령대의 평균월급이 가장 많을까?
# 사용 변수 전처리 단계

### 파생변수 나이 age 
#1. 변수 검토
#2. 이상치 제거
#3. 파생변수 만들기 agag : 초년 - 30세 미만 /중년 - 30~59세 / 노년 -  60세 이상 
welfare <- welfare %>% mutate(ageg=ifelse(age < 30 ,"young",
                                          ifelse(age<=59,"middle","old"))) 

table(welfare$ageg)
qplot(welfare$ageg)


### 월급 income 
#1. 변수 검토
#2. 이상치 제거

#데이터 분석 단계
##연령대 월급 평균표
ageg_income<- welfare %>%               
  filter(!is.na(income)) %>%            #NA 값 제외
  group_by(ageg) %>%                    #ageg에 따라 그룹화
  summarise(mean_income=mean(income))   #ageg에 따른 income 평균 

ageg_income


#연령대에 따른 평균월급 그래프 
ggplot(data=ageg_income, aes(x=ageg, y=mean_income)) +
  geom_col()                            
      

ggplot(data=ageg_income, aes(x=ageg, y=mean_income)) +
  geom_col() +                          
  scale_x_discrete(limits=c("young","middle","old"))  
  #그래프에 데이터가 나타나는 값의 순서를 지정할 때 scale_x_discrete(limits=c()) 


###### 결과 ######
#초년, 중년, 노년에 따라 월급의 차이가 있으며 중년(30~59세) 때 가장 높다. 





#========== < 정제 변수 : sex, birth, income, age, ageg > ============#




# Q4. 성별 ,연령대에 따른 월급에 차이?
# 사용 변수 전처리 단계 

### 월급 income , 성별 sex, 연령대 ageg  -> 전처리 완료
#1. 변수 검토
#2. 이상치 제거

# 데이터 분석 단계
##연령대와 성별 월급 평균표
sex_income <- welfare %>% filter(!is.na(income)) %>%
  group_by(ageg,sex) %>%                 #연령대와 성별에 따른 그룹화 , 그래프의 기준 
  summarise(mean_income=mean(income))
sex_income


#성별,연령대 에 따른 평균월급 누적 그래프 
ggplot(data=sex_income,aes(x=ageg ,y=mean_income,   # 두개의 그룹 => x 축 : group_by의 첫번째 칼럼  
                           fill=sex))+              # x축으로 사용하지 않는 변수를 fill 값으로 지정해 색 임의적으로 변경 
  geom_col() +          
  scale_x_discrete(limits=c("young","middle","old"))


## 막대 분리 그래프
ggplot(data=sex_income,aes(x=ageg ,y=mean_income,   
                           fill=sex))+              
  geom_col(position="dodge") +                      #그룹화한 것을 보여주기 위해 막대 분리 그래프  
  scale_x_discrete(limits=c("young","middle","old"))


###### 결과 ######
#연령대에 따라 평균 월급의 차이가 있으며 중년(30~59세) 가면 성별에 따른 임금의 차이가 크게 나타난다. 





#Q5. 나이 및 성별 월급 차이

# 사용 변수 전처리 단계 

### 월급 income , 성별 sex, 나이 age  -> 전처리 완료
#1. 변수 검토
#2. 이상치 제거

# 데이터 분석 단계
# 나이와 성별에 따른 월급 평균표 
sex_age<-welfare %>% filter(!is.na(income)) %>% 
  group_by(age,sex) %>% 
  summarise(mean_income=mean(income))

sex_age


#나이 및 성별에 따른 월급 그래프 
ggplot(data=sex_age,aes(x=age,y=mean_income,col=sex))+ #col : line의 색 옵션
  geom_line()



ggplot(data=sex_age,aes(x=age,y=mean_income,col=sex,fill=sex))+ 
  geom_col()



###### 결과 ######
#나이와 성별에 따라 월급의 차이가 있으며 그 차이는 40~60세에서 그 차이가 크다.






# Q6. 어떤 직업이 월급을 가장 많이 받을까?
# 사용 변수 전처리 단계 

### 직종 code_job : 4자리 수 
#1. 변수 검토

class(code_job)
table(code_job)

#2. 이상치 제거
#3. 데이터 값 변경 : 4자리 수 -> 직업명
list_job<-read_excel("Koweps_Codebook.xlsx",
                     col_name=T,sheet=2) #col_name : 칼럼 존재 , sheet : 번호 
                                         #칼럼 분리시 list 
                                                                
head(list_job)  
dim(list_job)

#code에 해당하는 직업명 데이터프레임 합치기 
welfare <- left_join(welfare,list_job,
                     id="code_job")     ##Joining, by = "code_job"

welfare %>% filter(!is.na(income)) %>% select(code_job,job)  %>%head(10)


### 월급 income  => 전처리 완료 
#1. 변수 검토
#2. 이상치 제거


#데이터 분석 단계
#직업별 월급 분석표 
job_income<-welfare %>% filter(!is.na(income)) %>% 
  group_by(job) %>%  
  summarise(mean_income=mean(income))

head(job_income)


#상위 10 개 직업의 월급 
top10<-job_income %>%  
  arrange(desc(mean_income)) %>% head(10) #arrange(desc()) : 상위 정열 



#직업별 월급 그래프
ggplot(data=top10,aes(x=reorder(job,mean_income),y=mean_income))+  #reorder(x축, 정렬 기준 ) : 정렬 
  geom_col()+
  coord_flip()                            #coord_filp() : 가로 그래프 


#하위 10 개 직업의 월급 
tail10<-job_income %>%  
  arrange(mean_income) %>% head(10)       #arrange() : 하위 정열 


#직업별 월급 그래프
ggplot(data=tail10,aes(x=reorder(job,-mean_income),y=mean_income))+ #reorder(x축, -정렬 기준 ) : 역정렬 
  geom_col()+
  coord_flip() +                          #coord_filp() : 가로 그래프 
  ylim (0,850)                            #값의 범위를 주어서 그래프가 의미하는 바를 확인하도록 도와줌 


###### 결과 ######
#금속 재료 공학 기술자 및 시험원 즉, 조선업 등 금속과 관련된 직업의 월급이 가장 높다.
#가사 및 육아 도우미 직업군이 가장 월급이 낮다.





#========== < 정제 변수 : sex, birth, income, age, ageg, code_job, job > ============#




# Q7. 성별로 어떤 직업이 가장 많을 까?

# 사용 변수 전처리 단계 

### 성별 sex  , 직업 job => 전처리 완료 
#1. 변수 검토
#2. 이상치 제거


# 데이터 분석 단계 

# 남성 직업 빈도표
job_male<- welfare %>% 
  filter(!is.na(job) & sex== "male" ) %>%       #남성이고 직업 있는 사람만 filtering
  group_by(job) %>% 
  summarise(n = n()) %>%                        # n(): 갯수                       
  arrange(desc(n)) %>% 
  head(10)

job_male


#여성 직업 빈도표
job_female<- welfare %>% 
  filter(!is.na(job) & sex== "female" ) %>%    #여성이고 직업 있는 사람만 filtering
  group_by(job) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(10)

job_female


#남성 직업 빈도 상위 10개 그래프 
ggplot(data=job_male,aes(x=reorder(job,n),y=n))+
  geom_col()+
  coord_flip()


#여성 직업 빈도 상위 10개 그래프 
ggplot(data=job_female,aes(x=reorder(job,n),y=n))+
  geom_col()+
  coord_flip()

###### 결과 ######
#남성과 여성 모두 작물 재배 종사자가 제일 많고 , 그 다음 남성은 자동차 운전원, 여성은 청소원 및 환경 미화원 이 가장 많다. 





#Q8. 종교 유무에 따른 이혼율 

# 사용 변수 전처리 단계 

### 종교 religion : 1 : 있음 / 2:  없음  * 무응답 : 9 

#1. 변수 검토
class(welfare$religion)           #numeric
table(welfare$religion)           #1-8047, 2 - 8617


#2. 이상치 제거
welfare$religion<-ifelse(welfare$religion==9,NA,welfare$religion) 
##### 이상치를 본래 실행해야 하는 코드 이지만 주어진 데이터 확인 결과 9 가 없기 때문에 실행하지 않는다. 


#3. 데이터에 이름 부여
welfare$religion <- ifelse(welfare$religion=="1","yes","no")
qplot(welfare$religion)


### 혼인상태 marriage 
### 0.비해당(18세 미만) 1.유배우 2.사별 3.이혼
### 4.별거5.미혼(18세이상, 미혼모 포함) 6.기타(사망 등)

#1. 변수 검토
class(welfare$marriage)
table(welfare$marriage)

#2. 이상치 제거

#3. 파생변수
welfare$group_marriage<-ifelse(welfare$marriage==1,"marriage",
                               ifelse(welfare$marriage==3,"divorce",NA))

table(welfare$group_marriage)
table(is.na(welfare$group_marriage))


# 데이터 분석 단계 
# 종교 유무에 따른 이혼율 표 
religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>%         # NA 값 제외 
  group_by(religion, group_marriage) %>%     # 종교와 기혼/이혼 에 따른 그룹
  summarise(n=n()) %>%                       # 종교 유무에 따른 기혼/이혼의 수    
  mutate(tot_group = sum(n),                 # tot_group : 종교에 따른 합
         pct=round(n/tot_group*100,1))       # pct : 종교에 따른 기혼/이혼의 비율 
religion_marriage  


#종교 유무에 따른 이혼율 표에서 이혼에 해당하는 데이터만  추출 
divorce <- religion_marriage %>% 
  filter(group_marriage=="divorce") %>% 
  select(religion,pct)
divorce           


ggplot(data=divorce,aes(x=religion,y=pct))+geom_col()


###### 결과 ######
#종교에 따른 이혼율의 차이가 잇지만 그래프상 크게 나타나진 않는다.






# Q9. 연령대 및 종교 유무에 따른 이혼율 분석 하기 

# 사용 변수 전처리 단계 

### 종교 religion , 연령대 : ageg ,혼인 상태 : marriage  => 전처리 완료 
#1. 변수 검토
#2. 이상치 제거 

# 데이터 분석 단계
# 연령대, 종교, 혼인상태를 확인 변수
ageg_re_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>%           #혼인상태가 NA가 아닌 것만 추출 
  group_by(ageg,religion,group_marriage) %>%   #연령대, 종교, 혼인상태에 따라 그룹화 
  summarise(n=n()) %>%                         #그룹별 개수
  mutate(tot_total=sum(n),                     #그룹별 합 
         pct=round(n/tot_total*100,1))         #그룹의 비율   


ageg_re_marriage

welfare %>% filter(ageg=="young") %>% group_by(marriage) %>% summarise(n=n())

#연령층이 중년과 노년, 종교 유무에 의한 이혼 비율표
A_R_divorce <- ageg_re_marriage %>% 
  filter(group_marriage=="divorce" & ageg != "young") %>%    #연령층이 "young" 은 제외 BUT 이혼상태, 2명 존재 
  select(ageg,religion,pct)
A_R_divorce


ggplot(data=A_R_divorce,aes(x=ageg,y=pct,fill=religion))+
  geom_col(position="dodge")


###### 결과 ######
#중년 층에서는 종교가 없는 사람들의 이혼율이 높은 반면 
#노년층에서는 종교를 가지고 있는 사람들의 이혼율이 더 높다. 그러나 노년 층에서의 차이가 뚜렷하다고 볼 수 는 없다.






#Q9. 노년층이 많은 지역은 어디인가?

# 사용 변수 전처리 단계 

### 연령대 : ageg => 전처리 완료 
#1. 변수 검토
#2. 이상치 제거 


### 지역  : code_region : range(1,7)
###1. 서울  2. 수도권(인천/경기)  3. 부산/경남/울산   
###4.대구/경북   5. 대전/충남   6. 강원/충북   7.광주/전남/전북/제주도"

#1. 변수 검토
class(welfare$code_region)
table(welfare$code_region)
table(is.na(welfare$code_region)) # 이상치 없음 

#2. 이상치 제거 
#3. 새로운 데이터 프레임
###지역 코드 데이터 프레임  -> joining 
list_region <-data.frame(code_region=c(1:7),
                         region=c("서울",
                                  "수도권(인천/경기)",
                                  "부산/경남/울산",
                                  "대구/경북",
                                  "대전/충남",
                                  "강원/충북",
                                  "광주/전남/전북/제주도"))


#데이터 분석 단계
#데이터 결합 
welfare<-left_join(welfare,list_region,
                   id="code_region")  #Joining, by = "code_region"   


#지역별 연령대 비율표
region_ageg<-welfare %>% 
  group_by(region,ageg) %>% 
  summarise(n=n()) %>% 
  mutate(tot_group=sum(n),
         pct=round(n/tot_group*100,2)) 
head(region_ageg)

##비율표 2.
region_ageg<-welfare %>% 
  count(region,ageg) %>% 
  group_by(region) %>% 
  mutate(pct=round(n/sum(n)*100,2))


#연령대별 지역 비율 그래프
ggplot(data=region_ageg, aes(x=region,y=pct,fill=ageg))+
  geom_col()+
  coord_flip()


#노년층 비율 높은 순 막대 그래프 
#노년층 비율 내림차순 정렬표
list_order_old <- region_ageg %>% 
  filter(ageg=="old") %>% 
  arrange(pct)
list_order_old


#지역명 순서 변수 
order<-list_order_old$region 


ggplot(data=region_ageg, aes(x=region,y=pct,fill=ageg))+
  geom_col()+
  coord_flip()+
  scale_x_discrete(limits=order)       # 리스트를 설정해 정렬이 가능 , 
                                       # 아래부터 그리게 되기 때문에 작은 값부터 넣음 


#연령대별 막대 색깔 나열 바꾸기 
region_ageg$ageg<-factor(region_ageg$ageg,                #데이터 프레임에 문자를 factor 로 들어감 
                         level=c("old","middle","young")) #level : 속성값 임의로 순서 변경 가능 


ggplot(data=region_ageg, aes(x=region,y=pct,fill=ageg))+
  geom_col()+
  coord_flip()+
  scale_x_discrete(limits=order)















