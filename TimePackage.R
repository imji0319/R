# Time Date package  

# Date

x <- as.Date("1970-01-01")
x

unclass(x)
unclass(as.Date("1970-01-03")) # 1970-01-01 기준으로 정수값 저장

# Date 기본 

today <- Sys.Date()

format(today, "%d %b %Y") # with month as word // %d : day , %b : month , %Y : year
weekdays(today) # weekdays() : 요일 return
months(today)   # months() : 월 return 

tenweeks <- seq(today, length.out = 10, by = "1 week") # next ten weeks 
tenweeks

months(tenweeks)

as.Date("2012-12-03")

as.Date("12/03/2012") #ISO8610 형태의 문자열을 쓰지 않을 경우 원하는 값으로 저장되지 않음 
 
as.Date("12/03/2012" , format = "%m/%d/%Y") #다른 형태일 경우 format 함수를 사용하여 처리.

# POSIXct , POSIXlt 

( z <- Sys.time())  # the current datetime, as class "POSIXct"
unclass(z)          # a large integer 
ceiling(unclass(z)/86400) #the number of days since 1970-01-01(UTC)
unclass(Sys.Date())

( now <- as.POSIXlt(Sys.time()))
unlist(unclass(now)) # a list shown as a named vector
now$year + 1900 
now$mon + 1
now$mday
months(now)
weekdays(now)

Sys.time() -3600 # an hour ago
as.POSIXlt(Sys.time(), "GMT") # the current time of GMT :그리니치 평균시

as.Date(ISOdate(2012,12,03))
ISOdate(2012,12,03)

# lubridate package 

install.packages('tidyverse')
library('tidyverse')
install.packages('lubridate')
library('lubridate')


as.Date('20120101')
ymd('20120101') 
mdy('January 10th 2020')
dmy('10-jan-2020')

ymd('820327') #1982-03-27
ymd('020327') #2020-03-27

Sys.timezone()


