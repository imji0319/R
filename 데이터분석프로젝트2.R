#복지패널 데이터 분석 
setwd("../R_TEMP")
install.packages("foreign") #spss 파일을 읽을 수 잇도록 도와주는 라이브러리 
library(foreign)
library(dplyr)              #전처리
library(ggplot2)            #시각화
library(readxl)             #엑셀 파일 

#데이터 준비 
raw_welfare<-read.spss(file="Koweps_hpc10_2015_beta1.sav", #spss 파일 
                       to.data.frame=T)                     #to.data.frame=T : 옵션 사용하지 않으면 list 
#### 연습용 프로젝트 임으로 시스템으로 인해 적게 들어오는 한계 존재함을 인지 

#복사본 만들어 원본 데이터 보관
welfare<-raw_welfare

#사용할 변수명 바꾸기
welfare <- rename (welfare,
                   smoking=p1005_3aq5, #현재 흡연여부
                   income=p1002_8aq1,   #평균 월급 
                   sex=h10_g3,          #성별
                   code_school=p1007_3aq1,   #최종 학력 
                   code_major=p1007_3aq5     #대학전공계열 
                   )

# Q1. 교육 수준에 따른 월급 차이
#사용 변수 
#school : 최종학력 
#1. 중학교 졸업 이하                      
#2. 고등학교 중퇴, 졸업  
#3. 전문대학 재학, 중퇴, 졸업                                          
#4. 대학교(4년제) 재학, 중퇴, 졸업               
#5. 대학원 이상

class(welfare$code_school)
table(welfare$code_school)
table(is.na(welfare$code_school))

list_school<-data.frame(code_school=c(1:5),
                        school=c("중학교 졸업 이하",
                            "고등학교",
                            "전문대학",
                            "대학교(4년제)",
                            "대학원 이상"
                             ))
welfare<-left_join(welfare,list_school,id="code_school")
head(welfare$school)


#income : 월급
table(is.na(welfare$income))
welfare$income<-ifelse(welfare$income %in% c(0,9999),NA,welfare$income) 

#데이터 분석 단계
sch_income <- welfare %>% 
  filter(!is.na(school) & !is.na(income)) %>% 
  group_by(school) %>% 
  summarise(mean_income=mean(income))
head(sch_income)

ggplot(data=sch_income, aes(x=reorder(school,mean_income),y=mean_income)) +geom_col()


#====결론===#
# 중학교 졸업이하의 그룹의 임금이 낮다는 것은 일반적으로 받아들릴 수 있는 사실이지만 
# 확인결과 학력의 순에 따라 월급의 높은 정비례적인 상황은 성립하지 않음을 알 수 있다. 



# Q2. 대학 전공에 따른 월급 차이 
# 사용 변수 
# major
#1. 인문계열  2. 사회계열(경상계열)
#3. 사회계열(법학계열) 4. 사회계열(사회과학계열)
#5. 교육계열   6. 공학계열
#7. 자연계열   8. 의약계열(의학)
#9. 의약계열(약학) 10. 의약계열(간호,치료보건)
#11.예체능계열 12. 기타

table(welfare$code_major)
list_major=data.frame(code_major=c(1:12),
                      major=c("인문계열",
                              "경상계열",
                              "법학계열",
                              "사회과학계열",
                              "교육계열",
                              "공학계열",
                              "자연계열",
                              "의학",
                              "약학",
                              "간호,치료보건",
                              "예체능계열",
                              "기타"))
welfare<-left_join(welfare,list_major,id="code_major")

head(welfare$major)

#income

major<-welfare %>% 
  filter(!is.na(major) & !is.na(income)) %>% 
  group_by(major) %>% 
  summarise(mean_income=mean(income))
  
ggplot(data=major,aes(x=reorder(major,mean_income),y=mean_income))+geom_col()


#===결과 ====
#의학 계열의 전공이 월급이 높을 것 같다고 생각했지만 의외로 실제 가장 많은 전공은 경상계열이다.



# Q3. 성별에 따른 흡연여부
# 사용 변수
#smokeing 
table(welfare$smokeing)
table(is.na(welfare$smokeing))

welfare$smoking <- ifelse(welfare$smoking==1,"흡연","비흡연")
table(welfare$smoking)

#성별 sex
table(welfare$sex)
welfare$sex<-ifelse(welfare$sex==1,'male','female')

#데이터 분석 
sex_smk<-welfare %>% filter(smoking=="흡연") %>% 
  group_by(sex) %>% summarise(n=n())

sex_smk
ggplot(data=sex_smk,aes(x=sex,y=n))+geom_col()

