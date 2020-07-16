#DART Text mining for Banks 
#2019년 12월 사업보고서 중 II.사업의 내용 텍스트 분석 

#실행 패키지 설치
install.packages("rJava")

#This package can't be downloaded by a method like install.packages().
#install.packages("KoNLP")

install.packages("stringr")
install.packages("wordcloud")
install.packages("RColorBrewer")


library(rJava)
library(stringr)
library(wordcloud)
library(KoNLP)
library(RColorBrewer)

#setwd
setwd("/Users/jihye/Documents/DA/Fintech/Fintech_Testing")

# 세종 한글 사전 시스템에 설치(명사) -> R이 한글을 인식할 수 있도록 설치 필수 
useSejongDic() 

# 세종 한글 사전에 단어 추가 
#mergeUserDic(data.frame("","ncn"))


#### 국민은행 Data 
kbstar <- readLines("kbstar.txt")

#keyword추출 
word_data <- sapply(kbstar, extractNoun,USE.NAMES = F) #sapply : 공백 기준으로 단어 추출 
res_data <- unlist(word_data) #unlist : -> vector


#불용어 제거 -> 사업특성상 빈번하게 사용되는 단어 제거
res_data <-gsub("[[:punct:]]","",res_data)
res_data <-gsub("당기","", res_data) 
res_data <-gsub("전기","",res_data)
res_data <-gsub("금액","",res_data)
res_data <-gsub("구분","",res_data)
res_data <-gsub("판매","",res_data)
res_data <-gsub("경제","",res_data)
res_data <-gsub("비즈니스","",res_data)
res_data <-gsub("관련","",res_data)
res_data <-gsub("이후","",res_data)
res_data <-gsub("분기","",res_data)
res_data <-gsub("각종","",res_data)
res_data <-gsub("은행","",res_data)
res_data <-gsub("기업","",res_data)
res_data <-gsub("확인","",res_data)
res_data <-gsub("발생","",res_data)
res_data <-gsub("가격","",res_data)
res_data <-gsub("합계","",res_data)
res_data <-gsub("준비","",res_data)
res_data <-gsub("스타","",res_data)
res_data <-gsub("이외","",res_data)
res_data <-gsub("인정","",res_data)
res_data <-gsub("해당","",res_data)
res_data <-gsub("이용안내","",res_data)
res_data <-gsub("업무","",res_data)
res_data <-gsub("단위","",res_data)
res_data <-gsub("노력","",res_data)
res_data <-gsub("회사","",res_data)
res_data <-gsub("특성","",res_data)

res_data <-gsub("내역","",res_data)
res_data <-gsub("이하","",res_data)
res_data <-gsub("이내","",res_data)
res_data <-gsub("종류","",res_data)
res_data <-gsub("메뉴","",res_data)
res_data <-gsub("하기","",res_data)
res_data <-gsub("업체","",res_data)
res_data <-gsub("현황","",res_data)
res_data <-gsub("특성","",res_data)
res_data <-gsub("재무제표","",res_data)
res_data <-gsub("특성","",res_data)
res_data <-gsub("년년","",res_data)
res_data <-gsub("사항","",res_data)

res_data <-gsub("금융","",res_data)
res_data <-gsub("계속","",res_data)
res_data <-gsub("제한없","",res_data)
res_data <-gsub("판단","",res_data)
res_data <-gsub("기존","",res_data)
res_data <-gsub("자유","",res_data)
res_data <-gsub("상담","",res_data)
res_data <-gsub("사실","",res_data)
res_data <-gsub("지식","",res_data)

res_data <-gsub("KB","",res_data)
res_data <-gsub("사회","",res_data)

res_data <-gsub("계좌","",res_data)
res_data <-gsub("이체","",res_data)
res_data <-gsub("전체","",res_data)
res_data <-gsub("당행","",res_data)

res_data <-gsub("[[:digit:]]","",res_data)


### 텍스트 마이닝 
# Filter함수를 이용해서 2-6글자 사이의 결과만 추출
res_data <- Filter(function(x){nchar(x)>=2 & nchar(x)<=10},res_data)

write(unlist(res_data),"keylist_data.txt")
keylist_data <-read.table("keylist_data.txt",na.strings=c("", "NA"))

# Keyword Frequency 
tabkey_data<-table(keylist_data)

# Keyword Frequency -> .csv 
# write.csv(tabkey_data,"kb_keyf_data.csv", fileEncoding = "utf8")

# Word Color 
palete <- brewer.pal(9,"Set1") # 단어 색상 부여 -> 숫자는 표현 가능한 색상 종류 

#그래픽 창 
x11()

#visualization
par(family="AppleGothic")

wordcloud(names(tabkey_data), 
          freq=tabkey_data, 
          scale = c(5,1), 
          rot.per = 0.5, 
          min.freq = 50000, #n번 이상 출현한 단어만 출력 
          random.order = T, 
          random.color = T, 
          colors = palete)

#Pie graph 
topic_word <- head(sort(tabkey_data, decreasing = T), 20)
pie(topic_word, col = palete, radius = 1)

# percent Pie graph
Perct <- round(topic_word/sum(topic_word)*100, 1) # percent 
names(topic_word)
lab <- paste(names(topic_word), "\n", Perct, "%") # making label
pie(topic_word, col= palete, cex = 0.8, labels = lab)

#donut 
par(new = T)
pie(topic_word, radius = 0.6, col = "white", labels = NA, border = NA)



