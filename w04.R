gcrm() # rm(list=ls())
gcHi(4) # Hello World!

library(data.table)
# parallel computing
library(doSNOW)
library(foreach)

setwd("C:/Users/gchlapinski/Desktop/Rrroboczy/UWr")
load("KrukUWr2018.RData")

#! ##################################################### bias-variance trade-off
# generowane dane
n <- 100
x <- seq(from=0, to=1, length.out=n)
y1 <- 5*(10*(x - 0.5)^3 + (x - 0.5)^2 + (x - 0.5))
y <- y1 + rnorm(n)

trn <- sort(sample(1:n, 0.5*n))
tst <- (1:n)[-trn]

# wizualizacja przyk¹³du
plot(x, y1, type="l", lwd=3)
points(x, y, pch=15, col="darkred")

# treningowy b³¹d MSE
trnMSE <- vector(mode="numeric", length=20)
for (p in 1:20) {
  yMod <- lm(y[trn] ~ poly(x[trn], p))
  
  if (p %in% c(1, 3, 20)) {
    lines(x[trn], yMod$fitted.value, lty=3, lwd=4, col=rainbow(3)[p%%3 + 1])
  }
  
  trnMSE[p] <- mean((y[trn] - yMod$fitted.value)^2)
}

# szacowanie testowego b³êdu MSE (mo¿liwe tylko w przypadku, gdy znamy y1)
N <- 200
tstMSE <- vector(mode="numeric", length=20)
tstVar <- vector(mode="numeric", length=20)
tstBSQ <- vector(mode="numeric", length=20)
for (p in 1:20) { # p=1
  tmpMSE <- vector(mode="numeric", length=n)
  tmpMean <- vector(mode="numeric", length=n)
  tmpSquare <- vector(mode="numeric", length=n)
  
  for (i in 1:N) { # i=1
    y <- y1 + rnorm(n)  
    yMod <- lm(y[trn] ~ poly(x[trn], p))
    
    yPred <- predict(yMod, data.frame(x[tst]))
    
    tmpMSE <- tmpMSE + ((y[tst] - yPred)^2)/N
    tmpMean <- tmpMean + yPred/N
    tmpSquare <- tmpSquare + (yPred^2)/N
  }
  
  tstMSE[p] <- mean(tmpMSE)
  tstVar[p] <- mean(tmpSquare - tmpMean^2)  
  tstBSQ[p] <- mean((tmpMean - y1[tst])^2)
}

windows()
plot(1:20, trnMSE, type="b", col="darkred", 
  ylim=c(0.95*min(trnMSE, tstMSE), 1.05*max(trnMSE, tstMSE)))
lines(1:20, tstMSE, lty=2, lwd=3, col="darkgreen")

windows()
plot(1:20, tstMSE, type="b", col="darkred", ylim=c(0, 1.05*max(tstMSE)))
lines(1:20, tstVar, type="b", col="darkgreen")
lines(1:20, tstBSQ, type="b", col="darkblue")

tstMSE - (tstVar + tstBSQ) # wariancja zak³ócenia

#! ########################################## zbiór ucz¹cy, testowy, walidacyjny
# podzbiór do zabawy
summary(cases[Product == "Cash loan" & TOA < 65000, ])
casesTmp <- na.omit(cases[Product == "Cash loan" & TOA < 65000, ])
casesTmp <- casesTmp[sample(1:casesTmp[, .N], 10000), ]
summary(casesTmp)

# zmienne
variables <- c("TOA", "DPD", "Age", "MeanSalary", "PopulationInCity")

# normalizacja danych
for (v in variables) {
  vstd <- paste0(v, "_std")
  casesTmp[, eval(vstd):=(get(v) - min(get(v)))/(max(get(v)) - min(get(v)))]
} 

# podzia³ na zbiór ucz¹cy i testowy
n <- casesTmp[, .N]
K <- 29
trSample <- sample(1:n, 0.5*n)

kNearestError <- data.table()
for (k in seq(from=1, to=K, by=2)) {
  kNearest <- class::knn(
    train=casesTmp[trSample, .SD, .SDcols=variables],
    test=casesTmp[-trSample, .SD, .SDcols=variables],
    cl=casesTmp[trSample, ]$Bailiff,
    k=k, use.all=FALSE)

  cfMatrix <- table(kNearest, casesTmp[-trSample, ]$Bailiff)
  testErr <- sum(diag(cfMatrix))/sum(cfMatrix)

  kNearest <- class::knn(
    train=casesTmp[trSample, .SD, .SDcols=variables],
    test=casesTmp[trSample, .SD, .SDcols=variables],
    cl=casesTmp[trSample, ]$Bailiff,
    k=k, use.all=FALSE)

  cfMatrix <- table(kNearest, casesTmp[trSample, ]$Bailiff)
  trainErr <- sum(diag(cfMatrix))/sum(cfMatrix)

  kNearestError <- rbindlist(list(kNearestError, 
    data.table(K=k, TrainErr=trainErr, TestErr=testErr)))
}

plot(seq(from=1, to=K, by=2), kNearestError$TrainErr, 
  col="darkgreen", ylim=c(0.6, 1.05), type="b",
  xlab="k", ylab="Correctly Classified", main="Train vs Test") 
lines(seq(from=1, to=K, by=2), kNearestError$TestErr, 
  col="darkred", type="b")
legend("topright", legend=c("Train", "Test"), lty=c(1, 1), pch=c(1, 1),
  col=c("darkgreen", "darkred"))

for (i in 1:10) {
  kNearestError <- data.table()
  trSample <- sample(1:n, 0.5*n)
  for (k in seq(from=1, to=K, by=2)) {
    kNearest <- class::knn(
      train=casesTmp[trSample, .SD, .SDcols=variables],
      test=casesTmp[-trSample, .SD, .SDcols=variables],
      cl=casesTmp[trSample, ]$Bailiff,
      k=k, use.all=FALSE)

    cfMatrix <- table(kNearest, casesTmp[-trSample, ]$Bailiff)
    testErr <- sum(diag(cfMatrix))/sum(cfMatrix)

    kNearest <- class::knn(
      train=casesTmp[trSample, .SD, .SDcols=variables],
      test=casesTmp[trSample, .SD, .SDcols=variables],
      cl=casesTmp[trSample, ]$Bailiff,
      k=k, use.all=FALSE)

    cfMatrix <- table(kNearest, casesTmp[trSample, ]$Bailiff)
    trainErr <- sum(diag(cfMatrix))/sum(cfMatrix)

    kNearestError <- rbindlist(list(kNearestError, 
      data.table(K=k, TrainErr=trainErr, TestErr=testErr)))
  }
  
  lines(seq(from=1, to=K, by=2), kNearestError$TrainErr, 
    col="darkgreen", lty=3)
  lines(seq(from=1, to=K, by=2), kNearestError$TestErr, 
    col="darkred", lty=2)
}

#! ################################################################## resampling
######################################################## k-fold cross validation
kF <- 10
casesTmp[, kFold:=sample(1:kF, n, replace=TRUE)]

# parallel computing
Sys.time()
cl <- makeCluster(2)
registerDoSNOW(cl)
# parallel computing

kCVNearestError <- foreach (f=1:kF, .init=data.table(), 
  .combine=function(x,y) rbindlist(list(x,y)), 
  .packages=c("data.table"), .inorder=FALSE) %dopar% { 
  
  kCVNearErr <- data.table()
  
  for (k in seq(from=1, to=K, by=2)) {
    kNearest <- class::knn(
      train=casesTmp[kFold != f, .SD, .SDcols=variables],
      test=casesTmp[kFold == f, .SD, .SDcols=variables],
      cl=casesTmp[kFold != f, ]$Bailiff,
      k=k, use.all=FALSE)

    cfMatrix <- table(kNearest, casesTmp[kFold == f, ]$Bailiff)
    cvErr <- sum(diag(cfMatrix))/sum(cfMatrix)
    
    kCVNearErr <- rbindlist(list(kCVNearErr, 
      data.table(K=k, CVErr=cvErr)))
  }
  
  data.table(KF=f, kCVNearErr)
}

# parallel computing
stopCluster(cl)
rm(cl)
#registerDoSEQ()
# parallel computing
Sys.time()

lines(seq(from=1, to=K, by=2), 
  kCVNearestError[, mean(CVErr), by=K]$V1, lty=2)
kCVNearestError[, mean(CVErr), by=K]

###################################################################### bootstrap
B <- 100

# parallel computing
Sys.time()
cl <- makeCluster(2)
registerDoSNOW(cl)
# parallel computing

kBootNearestError <- foreach (b=1:B, .init=data.table(), 
  .combine=function(x,y) rbindlist(list(x,y)), 
  .packages=c("data.table"), .inorder=FALSE) %dopar% { 
  trBoot <- sample(1:n, n, replace=TRUE)
  kBootNearErr <- data.table()
  
  for (k in seq(from=1, to=K, by=2)) {
    kNearest <- class::knn(
      train=casesTmp[trBoot, .SD, .SDcols=variables],
      test=casesTmp[trBoot, .SD, .SDcols=variables],
      cl=casesTmp[trBoot, ]$Bailiff,
      k=k, use.all=FALSE)

    cfMatrix <- table(kNearest, casesTmp[trBoot, ]$Bailiff)
    bErr <- sum(diag(cfMatrix))/sum(cfMatrix)
    
    kBootNearErr <- rbindlist(list(kBootNearErr, 
      data.table(K=k, BootErr=bErr)))
  }
  
  data.table(B=b, kBootNearErr)
}

# parallel computing
stopCluster(cl)
rm(cl)
#registerDoSEQ()
# parallel computing
Sys.time()

lines(seq(from=1, to=K, by=2), 
  kBootNearestError[, mean(BootErr), by=K]$V1, lty=3)

windows()
boxplot(BootErr~K, data=kBootNearestError)

#! ############################################################### cel biznesowy
omówienie tematów projektów (pomys³y co mo¿e zostac wykonane)
jakie zmienne mo¿na modelowaæ

