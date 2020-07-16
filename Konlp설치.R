#KoNLP 설치 이슈 공유 _ facebook 
#https://www.facebook.com/notes/r-korea-krugkorean-r-user-group/konlp-설치-이슈-공유/1847510068715020/

#multilinguer 설치 중 fs 패키지 non-exist -> 수동 다운로드
#install.packages("~/Downloads/fs_1.4.1.tar", repos = NULL,type="source")

install.packages("multilinguer") 
library(multilinguer) 
install_jdk() 

#의존성 패키지 설치 
install.packages(c("hash", "tau", "Sejong", 
                   "RSQLite", "devtools", "bit", "rex", "lazyeval", 
                   "htmlwidgets", "crosstalk", "promises", "later", 
                   "sessioninfo", "xopen", "bit64", "blob", "DBI", 
                   "memoise", "plogr", "covr", "DT", "rcmdcheck", "rversions"), 
                 type = "binary")

# github 버전 설치
install.packages("remotes")

#openssl 패키지 non-exist -> 수동 다운로드
#install.packages("~/Downloads/openssl_1.4.1.tar", repos = NULL,type="source")

# 64bit 에서만 동작합니다.
remotes::install_github('haven-jeon/KoNLP', 
                        upgrade = "never", INSTALL_opts=c("--no-multiarch"))

library(KoNLP)
