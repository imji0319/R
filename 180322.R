
# clustering  -> kmeans of " stats " package  

# cluseter : clusering function package
install.packages("cluster")
library(cluster)

votes.repub[1:10,1:3]   # 1856's ~1976's 의 31개 선거에서 공화당 대통령 후보에 대한 각 주별 투표율애 대한 통계로 cluster 패키지 제공 데이터

# agnes : one of clustering methods , there are 6 kinds ( average , single (single linkage), complete ( complete linkage), ward ,weighted,flexible )
# 기본 방식 : average
# If data is standardized, stand is "TRUE" 
agnl <- agnes(votes.repub, metric = "manhattan", stand = TRUE)
agnl

plot(agnl)

op <- par(mfrow=c(2,2))

#daisy : dissimilarity 계산이 된 경우로 dissimilarity matrix 가 사용된 경우 무시 
agn2 <- agnes(daisy(votes.repub),diss=TRUE,method="complete") # one of clustering methods
plot(agn2)
agnS<-agnes(votes.repub,method="flexible",par.meth= 0.6) 
plot(agnS)
par(op)

#as.dendrogram : It can make plots of ( hierarchical clustering , classification tree )
(d2<-as.dendrogram(agn2)) 
d2[[1]]
d2[[2]]

d2[[1]][[1]]
str(d2)

