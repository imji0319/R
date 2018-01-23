---
title: "Q1. 성별에 따라 월급이 다를까? "
output: html_document
---

==================================================





####분석과정
1. 데이터 준비 단계  
-  데이터 구조 및 확인  
 
2. 데이터 전처리 단계   
-  필요한 변수 검토 및 전처리, 변수 간의 관계 분석 

3. 데이터 분석 단계  
-  새로운 요약 테이블 생성, 그래프 시각화 


한국복지패널데이터

```r
raw_welfare<-read.spss(file="Koweps_hpc10_2015_beta1.sav", #spss 파일 
                      to.data.frame=T)                     #to.data.frame=T : 옵션 사용하지 않으면 
```

```
Error in read.spss(file = "Koweps_hpc10_2015_beta1.sav", to.data.frame = T): unable to open file: 'No such file or directory'
```

```r
welfare<-raw_welfare
```

```
Error in eval(expr, envir, enclos): object 'raw_welfare' not found
```


```r
welfare<-rename(welfare,                #데이터
               sex=h10_g3,              #성별 
               birth=h10_g4,            #태어난 연도
               marriage=h10_g10,        #혼인상태
               income=p1002_8aq1,       #평균 월급
               religion=h10_g11,        #종교 여부
               code_job=h10_eco9,       #직종 코드
               code_region=h10_reg7)    #지역 코드 
```

```
Error in rename(welfare, sex = h10_g3, birth = h10_g4, marriage = h10_g10, : object 'welfare' not found
```

####사용 변수 전처리 단계  
성별 sex : range(남 : 1, 여 : 2)   * 응답 모름 : 9   

1. 변수 검토  

```r
class(welfare$sex)   #변수 속성
```

```
Error in eval(expr, envir, enclos): object 'welfare' not found
```

```r
table(welfare$sex)   #변수 빈도
```

```
Error in table(welfare$sex): object 'welfare' not found
```

2. 이상치 제거  

```r
welfare$sex <-ifelse(welfare$sex==9,NA,welfare$sex)
```

```
Error in ifelse(welfare$sex == 9, NA, welfare$sex): object 'welfare' not found
```

```r
table(is.na(welfare$sex)) #NA 확인 
```

```
Error in table(is.na(welfare$sex)): object 'welfare' not found
```

3. 데이터 값 변경 1 -> 남 , 2 -> 여  

```r
welfare$sex<-ifelse(welfare$sex == 1 ,'male','female')
```

```
Error in ifelse(welfare$sex == 1, "male", "female"): object 'welfare' not found
```

```r
table(welfare$sex)
```

```
Error in table(welfare$sex): object 'welfare' not found
```
4. 빈도그래프로 데이터 확인   

```r
qplot(welfare$sex) 
```

```
Error in eval(expr, envir, enclos): object 'welfare' not found
```


----

월급 income : range(1,9998)  * 모름/ 무응답 : 9999 ,  0 : 아동 데이터    
1. 변수 검토  

```r
class(welfare$income)                  #변수 속성
```

```
Error in eval(expr, envir, enclos): object 'welfare' not found
```

```r
summary(welfare$income)                #데이터 요약 통계량 -> 이상치 확인 
```

```
Error in summary(welfare$income): object 'welfare' not found
```

```r
qplot(welfare$income) + xlim(0,1000)   #데이터 확인을 위한 그래프로 범위 제한 (xlim)
```

```
Error in eval(expr, envir, enclos): object 'welfare' not found
```

2. 이상치 제거  

```r
welfare$income<-ifelse(welfare$income %in% c(0,9999),NA,welfare$income)   #0,9999 -> NA
```

```
Error in match(x, table, nomatch = 0L): object 'welfare' not found
```

```r
table(is.na(welfare$income))
```

```
Error in table(is.na(welfare$income)): object 'welfare' not found
```


-----
####데이터 분석 단계  

필요한 데이터테이블 만들기 : 남녀의 월급 평균을 가지는 테이블 : sex_income  

```r
sex_income <- welfare %>% 
  filter(!is.na(income)) %>%           #income의 NA 값 제외
  group_by(sex) %>%                    #sex 에 따라 그룹화
  summarise(mean_income=mean(income))  #sex에 따른 income의 평균 
```

```
Error in eval(expr, envir, enclos): object 'welfare' not found
```

성별에 따른 평균월급 그래프   

```r
ggplot(data=sex_income,aes(x=sex, y=mean_income)) + 
  geom_col()                           #geom_col () : 집단 간 차이를 알 수 있는 막대 그래프 
```

```
Error in ggplot(data = sex_income, aes(x = sex, y = mean_income)): object 'sex_income' not found
```

###결과   
-> 성별에 따라 월급의 차이가 있다.   


