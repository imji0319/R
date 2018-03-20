library(dplyr)
install.packages("hflights")
library(hflights)

# hflights DB : 미국 휴스턴에서 출발하는 모든 비행기의 2011년 이착륙기록이 수록

dim(hflights)

hflights_df <- tbl_df(hflights)
# tbl_df : dplyr 의 함수로 data frame 형태로 변환 
hflights_df
