gcrm() # rm(list=ls())
gcHi(3) # Hello World!

library(data.table)
library(EMCluster) # em.init
# library(class) # knn
# parallel computing
library(doSNOW)
library(foreach)

setwd("C:/Users/gchlapinski/Desktop/Rrroboczy/UWr")
load("KrukUWr2018.RData")


#! zmieniæ przyk³ad (klasyfikacja kontaktu, przekazania do s¹du, wp³aty)

#!###################################################### klasyfikacja a regresja
# http://www-bcf.usc.edu/~gareth/ISL/

# mniejszy zbiór (u³atwienie wizualizacji)
casesTmp <- head(
  cases[!is.na(Gender) & Product == "Credit card" & TOA < 2000, ], 1000)
summary(casesTmp)

# standaryzacja danych
casesTmp[, `:=`(
  TOA_std=(TOA - mean(TOA))/sd(TOA), 
  Principal_std=(Principal - mean(Principal))/sd(Principal),
  DPD_std=(DPD - mean(DPD))/sd(DPD),
  Age_std=(Age - mean(Age))/sd(Age))] 

# normalizacja danych (uwaga: na wartoœci ujemne)
casesTmp[, `:=`(
  TOA_nrm=(TOA - min(TOA))/(max(TOA) - min(TOA)),
  Principal_nrm=(Principal - min(Principal))/(max(Principal) - min(Principal)),
  DPD_nrm=(DPD - min(DPD))/(max(DPD) - min(DPD)),
  Age_nrm=(Age - min(Age))/(max(Age) - min(Age)))]


v1 <- "TOA_std"
v2 <- "Principal_std"
v3 <- "DPD_std"
v4 <- "Age_std"

countTable <- table(casesTmp$Gender,
  cut(casesTmp[[v1]], breaks=quantile(casesTmp[[v1]], 
    probs=seq(from=0, to=1, by=0.1), include.lowest=TRUE)))
barplot(prop.table(countTable, 1), col=c("darkgreen","darkred"), 
  beside=TRUE, cex.names=0.6)

countTable <- table(casesTmp$Gender,
  cut(casesTmp[[v2]], breaks=quantile(casesTmp[[v2]],
    probs=seq(from=0, to=1, by=0.1), include.lowest=TRUE)))
barplot(prop.table(countTable, 1), col=c("darkgreen","darkred"),
  beside=TRUE, cex.names=0.6)

countTable <- table(casesTmp$Gender,
  cut(casesTmp[[v3]], breaks=unique(quantile(casesTmp[[v3]],
    probs=seq(from=0, to=1, by=0.1), include.lowest=TRUE))))
barplot(prop.table(countTable, 1), col=c("darkgreen","darkred"),
  beside=TRUE, cex.names=0.6)

#! ################################################### k-means (hard clustering)
k <- 5
kCluster <- kmeans(x=casesTmp[, .SD, .SDcols=c(v1, v2)],
  centers=k, iter.max=100, nstart=10)

# przypisanie klastra
casesTmp[, kClass:=kCluster$cluster]

# jaki procent kobiet w zbiorze
(F_prior <- casesTmp[Gender == "FEMALE", .N]/casesTmp[, .N])

# fancy graph
# jaka jest ró¿nica pomiêdzy library(pkg) vs pkg::fun
cluster::clusplot(casesTmp[, .SD, .SDcols=c(v1, v2)], kCluster$cluster,
  color=TRUE, shade=TRUE, labels=2, lines=0)

# nie taki "fancy" wykres
casesTmp[, colClass:=rainbow(k)[kCluster$cluster]]

plot(casesTmp[Gender == "FEMALE", ][[v1]], 
  casesTmp[Gender == "FEMALE", ][[v2]],
  pch=1, col=casesTmp[Gender == "FEMALE", ]$colClass, cex=0.7,
  xlab="", ylab="", main="",
  xlim = 1.05*range(casesTmp[[v1]]),
  ylim = 1.05*range(casesTmp[[v2]]))
points(casesTmp[Gender == "MALE", ][[v1]], 
  casesTmp[Gender == "MALE", ][[v2]],
  pch=6, col=casesTmp[Gender == "MALE", ]$colClass, cex=0.7)
points(kCluster$centers, pch=18, cex=2)

########################################################### macierz klasyfikacji
#             | Predicted Good | Predicted Bad  |
# Actual Good |      TP        |       FN       | = P
# Actual Bad  |      FP        |       TN       | = N
#
# sensitivity = true positive rate = TP/P = TP/(TP + FN)
# specificity = tru negative rate = TN/N = TN/(FP + TN)

tmp <- casesTmp[, .(.N,
  M_count=sum(ifelse(Gender == "MALE", 1,0)),
  F_count=sum(ifelse(Gender == "FEMALE", 1,0)),
  F_percent=sum(ifelse(Gender == "FEMALE", 1,0))/.N,
  Age_avg=mean(Age)), by=kClass][order(kClass)]

# kiedy uwa¿amy, ¿e klaster przewiduje kobietê??
# jaki wp³yw ma balansacja?? (jeszcze do tego wrócimy)
tmp[, F_predict:=ifelse(F_percent > F_prior, 1, 0)]

confMatrix <- matrix(
  c(tmp[F_predict == 1, sum(F_count)], tmp[F_predict == 0, sum(F_count)],
    tmp[F_predict == 1, sum(M_count)], tmp[F_predict == 0, sum(M_count)]),
  nrow=2, ncol=2, byrow=TRUE)
colnames(confMatrix) <- paste0("Pred: ", c("F", "M"))
rownames(confMatrix) <- paste0("Real: ", c("F", "M"))
confMatrix

############################################################## b³¹d klasyfikacji
(classError <- sum(diag(confMatrix))/sum(confMatrix))
####################################################### MSE - Mean Squared Error
casesTmp[tmp, on="kClass"][, .(MSE=mean((Age - Age_avg)^2))]

################################################################# wp³yw wyboru k
kClassError <- data.table()
for (k in 2:30) {
  kCluster <- kmeans(x=casesTmp[, .SD, .SDcols=c(v1, v2)],
    centers=k, iter.max=100, nstart=10)

  casesTmp[, kClass:=kCluster$cluster]

  tmp <- casesTmp[, .(.N,
    M_count=sum(ifelse(Gender == "MALE", 1,0)),
    F_count=sum(ifelse(Gender == "FEMALE", 1,0)),
    F_percent=sum(ifelse(Gender == "FEMALE", 1,0))/.N,
    Age_avg=mean(Age)), by=kClass][order(kClass)]
  tmp[, F_predict:=ifelse(F_percent > F_prior, 1, 0)]

  confMatrix <- matrix(
    c(tmp[F_predict == 1, sum(F_count)], tmp[F_predict == 0, sum(F_count)],
      tmp[F_predict == 1, sum(M_count)], tmp[F_predict == 0, sum(M_count)]),
    nrow=2, ncol=2, byrow=TRUE)

  kClassError <- rbindlist(list(kClassError, 
    data.table(K=k, ClassError=sum(diag(confMatrix))/sum(confMatrix),
      MSE=casesTmp[tmp, on="kClass"][, .(MSE=mean((Age - Age_avg)^2))])))
}

# poprawnie sklasyfikowane
kClassError[, max(ClassError)]

# Gini Impurity = 1 - p_i^2
1 - ((1-0.434)*0.434+0.434*(1-0.434))  

#! ################################### hierarchical clustering (hard clustering)
# dist method: "euclidean", "maximum", "manhattan", "minkowski", "canberra"
# hclust method: "complete", "single", "average"
k <- 5
pointsDist <- dist(casesTmp[, .SD, .SDcols=c(v1, v2)], method="euclidean")
hComplete <- hclust(pointsDist, method="complete")

# dendrogram wizualizacja
plot(hComplete) 

# przypisanie klastra
casesTmp[, hClass:=cutree(hComplete, k=k)]

# nie taki "fancy" wykres
casesTmp[, colClass:=rainbow(k)[hClass]]

plot(casesTmp[Gender == "FEMALE", ][[v1]], 
  casesTmp[Gender == "FEMALE", ][[v2]],
  pch=1, col=casesTmp[Gender == "FEMALE", ]$colClass, cex=0.7,
  xlab="", ylab="", main="",
  xlim = 1.05*range(casesTmp[[v1]]),
  ylim = 1.05*range(casesTmp[[v2]]))
points(casesTmp[Gender == "MALE", ][[v1]], 
  casesTmp[Gender == "MALE", ][[v2]],
  pch=6, col=casesTmp[Gender == "MALE", ]$colClass, cex=0.7)

########################################################### macierz klasyfikacji
tmp <- casesTmp[, .(.N,
  M_count=sum(ifelse(Gender == "MALE", 1,0)),
  F_count=sum(ifelse(Gender == "FEMALE", 1,0)),
  F_percent=sum(ifelse(Gender == "FEMALE", 1,0))/.N,
  Age_avg=mean(Age)), by=hClass][order(hClass)]

# kiedy uwa¿amy, ¿e klaster przewiduje kobietê??
tmp[, F_predict:=ifelse(F_percent > F_prior, 1, 0)]

confMatrix <- matrix(
  c(tmp[F_predict == 1, sum(F_count)], tmp[F_predict == 0, sum(F_count)],
    tmp[F_predict == 1, sum(M_count)], tmp[F_predict == 0, sum(M_count)]),
  nrow=2, ncol=2, byrow=TRUE)
colnames(confMatrix) <- paste0("Pred: ", c("F", "M"))
rownames(confMatrix) <- paste0("Real: ", c("F", "M"))
confMatrix

############################################################## b³¹d klasyfikacji
(classError <- sum(diag(confMatrix))/sum(confMatrix))
####################################################### MSE - Mean Squared Error
(casesTmp[tmp, on="hClass"][, .(MSE=mean((Age - Age_avg)^2))])

################################################################# wp³yw wyboru k
hClassError <- data.table()
for (k in 2:30) {
  casesTmp[, hClass:=cutree(hComplete, k=k)]

  tmp <- casesTmp[, .(.N,
    M_count=sum(ifelse(Gender == "MALE", 1,0)),
    F_count=sum(ifelse(Gender == "FEMALE", 1,0)),
    F_percent=sum(ifelse(Gender == "FEMALE", 1,0))/.N,
    Age_avg=mean(Age)), by=hClass][order(hClass)]
  tmp[, F_predict:=ifelse(F_percent > F_prior, 1, 0)]

  confMatrix <- matrix(
    c(tmp[F_predict == 1, sum(F_count)], tmp[F_predict == 0, sum(F_count)],
      tmp[F_predict == 1, sum(M_count)], tmp[F_predict == 0, sum(M_count)]),
    nrow=2, ncol=2, byrow=TRUE)

  hClassError <- rbindlist(list(hClassError, 
    data.table(K=k, ClassError=sum(diag(confMatrix))/sum(confMatrix),
      MSE=casesTmp[tmp, on="hClass"][, .(MSE=mean((Age - Age_avg)^2))])))
}

# poprawnie sklasyfikowane
hClassError[, max(ClassError)]

# Gini Impurity = 1 - p_i^2
1 - ((1-0.434)*0.434+0.434*(1-0.434))  

#! ################################################ EM cluster (soft clustering)
# https://cran.r-project.org/web/packages/EMCluster/vignettes/EMCluster-guide.pdf
# https://en.wikipedia.org/wiki/Expectation%E2%80%93maximization_algorithm
# method: "em.EM", "Rnd.EM", "Rnd.EM"
k <- 5
emCluster <- init.EM(casesTmp[, .SD, .SDcols=c(v1, v2)], 
  nclass=k, method="em.EM")

casesTmp[, emClass:=emCluster$class]

# nie taki "fancy" wykres
casesTmp[, colClass:=rainbow(k)[emClass]]

plot(casesTmp[Gender == "FEMALE", ][[v1]], 
  casesTmp[Gender == "FEMALE", ][[v2]],
  pch=1, col=casesTmp[Gender == "FEMALE", ]$colClass, cex=0.7,
  xlab="", ylab="", main="",
  xlim = 1.05*range(casesTmp[[v1]]),
  ylim = 1.05*range(casesTmp[[v2]]))
points(casesTmp[Gender == "MALE", ][[v1]], 
  casesTmp[Gender == "MALE", ][[v2]],
  pch=6, col=casesTmp[Gender == "MALE", ]$colClass, cex=0.7)
  
########################################################### macierz klasyfikacji
tmp <- casesTmp[, .(.N,
  M_count=sum(ifelse(Gender == "MALE", 1,0)),
  F_count=sum(ifelse(Gender == "FEMALE", 1,0)),
  F_percent=sum(ifelse(Gender == "FEMALE", 1,0))/.N,
  Age_avg=mean(Age)), by=emClass][order(emClass)]

# kiedy uwa¿amy, ¿e klaster przewiduje kobietê??
tmp[, F_predict:=ifelse(F_percent > F_prior, 1, 0)]

confMatrix <- matrix(
  c(tmp[F_predict == 1, sum(F_count)], tmp[F_predict == 0, sum(F_count)],
    tmp[F_predict == 1, sum(M_count)], tmp[F_predict == 0, sum(M_count)]),
  nrow=2, ncol=2, byrow=TRUE)
colnames(confMatrix) <- paste0("Pred: ", c("F", "M"))
rownames(confMatrix) <- paste0("Real: ", c("F", "M"))
confMatrix

############################################################## b³¹d klasyfikacji
(classError <- sum(diag(confMatrix))/sum(confMatrix))
####################################################### MSE - Mean Squared Error
(casesTmp[tmp, on="emClass"][, .(MSE=mean((Age - Age_avg)^2))])

################################################################# wp³yw wyboru k

# parallel computing
Sys.time()
cl <- makeCluster(3)
registerDoSNOW(cl)
# parallel computing

emClassError <- foreach (k=2:30, .init=data.table(), 
  .combine=function(x,y) rbindlist(list(x,y)), 
  .packages=c("data.table", "EMCluster"), 
  .inorder=FALSE) %dopar% {
  emCluster <- init.EM(casesTmp[, .SD, .SDcols=c(v1, v2)], 
    nclass=k, method="em.EM")

  casesTmp[, emClass:=emCluster$class]

  tmp <- casesTmp[, .(.N,
    M_count=sum(ifelse(Gender == "MALE", 1,0)),
    F_count=sum(ifelse(Gender == "FEMALE", 1,0)),
    F_percent=sum(ifelse(Gender == "FEMALE", 1,0))/.N,
    Age_avg=mean(Age)), by=emClass][order(emClass)]
  tmp[, F_predict:=ifelse(F_percent > F_prior, 1, 0)]

  confMatrix <- matrix(
    c(tmp[F_predict == 1, sum(F_count)], tmp[F_predict == 0, sum(F_count)],
      tmp[F_predict == 1, sum(M_count)], tmp[F_predict == 0, sum(M_count)]),
    nrow=2, ncol=2, byrow=TRUE)

  data.table(K=k, ClassError=sum(diag(confMatrix))/sum(confMatrix),
    MSE=casesTmp[tmp, on="emClass"][, .(MSE=mean((Age - Age_avg)^2))])
}

# parallel computing
stopCluster(cl)
rm(cl)
#registerDoSEQ()
# parallel computing
Sys.time()

# poprawnie sklasyfikowane
emClassError[, max(ClassError)]

# Gini Impurity = 1 - p_i^2
1 - ((1-0.434)*0.434+0.434*(1-0.434))
  
#! ################################################## knn - k nearest neighbours
k <- 5
kNearest <- class::knn(
  train=casesTmp[, .SD, .SDcols=c(v1, v2)],
  test=casesTmp[, .SD, .SDcols=c(v1, v2)],
  cl=casesTmp$Gender,
  k=k, use.all=FALSE)

cfMatrix <- table(kNearest, casesTmp$Gender)
sum(diag(cfMatrix))/sum(cfMatrix)

knnClassError <- data.table()
for (k in seq(from=1, to=29, by=2)) {
  kNearest <- class::knn(
    train=casesTmp[, .SD, .SDcols=c(v1, v2)],
    test=casesTmp[, .SD, .SDcols=c(v1, v2)],
    cl=casesTmp$Gender,
    k=k, use.all=FALSE)

  cfMatrix <- table(kNearest, casesTmp$Gender)

  knnClassError <- rbindlist(list(knnClassError, 
    data.table(K=k, ClassError=sum(diag(cfMatrix))/sum(cfMatrix))))
}

# poprawnie sklasyfikowane
knnClassError[, max(ClassError)]

# Gini Impurity = 1 - p_i^2
1 - ((1-0.434)*0.434+0.434*(1-0.434)) 
