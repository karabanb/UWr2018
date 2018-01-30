gcrm() # rm(list=ls())
gcHi(5) # Hello World!

library(data.table)
library(rpart) # drzewa regresyjne
library(tree) # drzewa regresyjne
# jest jeszcze wiele innych pakietów do drzew np. party
library(randomForest) # lasy losowe
library(gbm) # boosting
library(ada) # boosting
library(ROCR)
# recieving operating curve
rocplot <- function(pred, truth, ...) {
  predob <- prediction(pred, truth)
  perf <- performance(predob, "tpr", "fpr")
  gini <- 2*attributes(performance(predob, "auc"))$y.values[[1]] - 1

  plot(perf, main=paste0("Gini index: ", round(gini, 2)))
}

setwd("C:/Users/gchlapinski/Desktop/Rrroboczy/UWr")
load("KrukUWr2018.RData")

#!######################################################################## trees
# http://www-bcf.usc.edu/~gareth/ISL/
# https://cran.r-project.org/web/packages/rpart/vignettes/longintro.pdf
# http://www.learnbymarketing.com/481/decision-tree-flavors-gini-info-gain/
#
# https://gormanalysis.com/decision-trees-in-r-using-rpart/

summary(cases)

cases <- cases[Product == "Cash loan", ]

goals <- events[cases][Month <= 3, .(
  Reach=max(ifelse(gcisna(NumberOfVisitsWithClient, 0) > 0 
    | gcisna(NumberOfCallsWithClient, 0) > 0, 1, 0)),
  Concluded=max(ifelse(gcisna(NumberOfAgreementConcluded, 0) > 0, 1, 0)),
  Signed=max(ifelse(gcisna(NumberOfAgreementSigned, 0) > 0, 1, 0)),
  Payment=max(ifelse(gcisna(NumberOfPayment, 0) > 0, 1, 0))), 
  by=CaseId]
setDT(goals, key="CaseId")

# dla uproszczenia ominiemy NA (ró¿ne zachowanie pakietów tree i rpart)
# tematem uzupe³niania NA zajmiemy siê na nastêpnym wyk³adzie
# Uwaga: pakiet tree i tak to zrobi!!
casesTmp <- na.omit(cases[goals])

# pakiet tree lepiej toleruje (wymaga) factory
casesTmp[, `:=`(
  Reach=as.factor(Reach),
  Gender=as.factor(Gender),
  ExternalAgency=as.factor(ExternalAgency),
  Bailiff=as.factor(Bailiff),
  ClosedExecution=as.factor(ClosedExecution),
  Land=as.factor(Land))]
summary(casesTmp)

variables <- setdiff(names(casesTmp), c("CaseId", "Product",
  "Concluded", "Signed", "Payment"))
casesMod <- casesTmp[, .SD, .SDcols=variables]
summary(casesMod)

n <- casesMod[, .N]
train <- sample(1:n, 0.6*n)
testSet <- casesMod[-train, ]

##################################################### podzia³ na wêz³y w drzewie
# drzewo klasyfikacyjne (binarne - ka¿dy wêze³ ma conajwy¿ej dwa liœcie)
#
# miary podzia³u (najczêœciej spotykane):
# - gini impurity
#   gini = 1 - \sum_i p_mi^2 = \sum_i p_mi*(1 - p_mi)
#    
#   gini ~ mierzy poziom b³êdnej klasyfikacji
# - entropia (brak opcji w tree)
#   entopia = - \sum_i p_mi log_2 p_mi
#
#   entropia ~ miara "nieporz¹dku" (chaosu)
#   Jaki rozk³ad maksymalizuje entropiê:
#     - rozk³ad dyskretny o K punktach: rozk³ad jednostajny
#     - rozk³ad ci¹g³y na odcinku: rozk³ad jednostajny
#     - rozk³ad ci¹g³y na pó³prostej: rozk³ad wyk³adniczy
#     - rozk³ad ci¹g³y na prostej: rozk³ad normalny
# drzewo regresyjne
# - RSS
#   RSS = \sum_i (y_i - \hat{y}_i)^2
#
#   RSS ~ residual sum of squares

################################################################### tree package
tTree <- tree(Reach~., data=casesMod, subset=train, 
  split="gini", mincut=500, minsize=1000, mindev=1e-12)

plot(tTree)
text(tTree, pretty=0, cex=0.5)

# czy to drzewo jest kompletnie bezu¿yteczne?
table(testSet$Reach, predict(tTree, testSet, type="class"))

P_prior <- casesMod[train, ][ Reach == 1, .N]/casesMod[train, .N]
tmp <- testSet[, .(Reach)][, LeafPr:=predict(tTree, testSet, type="vector")[, 2]]
tmp[, IfBetter:=ifelse(LeafPr > P_prior, 1, 0)]
tmp[, .N, by=.(Reach, IfBetter)]

# krzywa ROC: Recieving Operating Curve
# https://en.wikipedia.org/wiki/Receiver_operating_characteristic
#
# zwi¹zek ROC z indeksem Gini 
# Gini = 2*AUROC - 1
# gdzie AUROC: Area Under ROC. 
rocplot(predict(tTree, testSet, type="vector")[, 2], testSet$Reach)

# przycinanie drzew
# http://www-bcf.usc.edu/~gareth/ISL/ # page: 309 (323) algorithm
tTreeCV <- cv.tree(tTree, FUN=prune.misclass)
tTreePruned <- prune.misclass(tTree, best=10)

rocplot(predict(tTreePruned, testSet, type="vector")[, 2], testSet$Reach)
table(testSet$Reach, predict(tTreePruned, testSet, type="class"))

plot(tTreePruned)
text(tTreePruned, pretty=0, cex=0.7)

################################################################## rpart package
rTree <- rpart(Reach~., data=casesMod, subset=train, 
  method="class", minsplit=1000, minbucket=500, cp=1e-12,
  parms=list(split="information"))

plot(rTree)
text(rTree, pretty=0, cex=0.7)

rocplot(predict(rTree, testSet, type="prob")[, 2], testSet$Reach)
table(testSet$Reach, predict(rTree, testSet, type="class"))

printcp(rTree)
plotcp(rTree)

# gdy nie ka¿dy b³¹d prognozy boli tak samo
rTreeLoss <- rpart(Reach~., data=casesMod, subset=train, 
  method="class", minsplit=1000, minbucket=500, cp=1e-12,
  parms=list(split="information", loss=matrix(c(0, 1, 3, 0), 2, 2, byrow=TRUE)))

rocplot(predict(rTreeLoss, testSet, type="prob")[, 2], testSet$Reach)
table(testSet$Reach, predict(rTreeLoss, testSet, type="class"))

#! maxcompete (rpart) ???

#! ###################################################################### forest
# gdy jedno drzewo nas nie satysfakcjonuje
# https://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm

## tune (e1071)???

######################################################################## bagging
# idea: bootstrapujemy próbê i na ka¿dej budujemy oddzielne drzewo
# wynikiem predykcji (klasyfikacji/regresji) jest uœredniony wynik po B drzewach
# zazwyczaj buduje siê drzewa du¿e (bez przycinania)
bagForest <- randomForest(Reach~., data=casesMod, 
  mtry=18, ntree=25, nodesize=500, 
  cutoff=casesMod[, .N/casesMod[, .N], by=Reach][order(Reach)]$V1,
  importance=TRUE, keep.inbag=TRUE, keep.forest=TRUE)

summary(bagForest)
bagForest

# jak malej¹ b³êdy wraz ze wzrostem liczby drzew
plot(bagForest)

# out of bag (podzbiór spraw s³u¿¹cy do estymacji b³êdu)
# obserwacje, które nie zosta³y wylosowane do budowy drzewa
bagForest$oob.times

# macierz liczba obserwacji x liczba drzew 
# które obserwacje by³y wziête do budowy drzewa  
bagForest$inbag

# uœrednione wyniki per obserwacja (³osowanie modeli/drzew)
bagForest$votes

# "dobroæ" zmiennych - jak wp³ywaj¹ na prognozê/klasyfikacjê
# MeanDecreseAccuracy:
# - policz b³¹d (classification error/MSE) dla obserwacji "out of bag"
# - spermutuj wartoœci dla m-tej zmiennej
# - policz b³¹d dla transformowanych danych (obserwacje out of bag)
# - policz ró¿nicê b³êdów
# - uœrednij ró¿nice po wszystkich drzewach i znormalizuj przez standardowe 
#   odchylenie ró¿nic
#
# MeanDecreaseGini:
# suma obni¿eñ wskaŸnika "nieczystoœci" (impurities: Gini Impurity 
# w klasyfikacji; RSS w regresji) dla danej zmiennej po wszystkich wêz³ach 
# w danym drzewie, a nastêpnie uœredniona po wszystkich drzewach w lesie
importance(bagForest)
varImpPlot(bagForest)

# rozmiar drzew
treesize(bagForest, terminal=TRUE)

# pojedyncze drzewo
getTree(bagForest, k=9, labelVar=TRUE)

# partial plot (wp³yw na logit)
partialPlot(bagForest, casesMod, DPD, "1")

################################################################## random forest
# idea: podobnie jak w baggingu buutstrapujemy próbê i na ka¿dej budujemy 
# oddzielne drzewo, tyle ¿e w ka¿dym drzewie u¿ywamy jedynie losowy podzbiór 
# zmiennych objaœniaj¹cych (sqrt(p) w klasyfikacji, p/3 w regresji - domyœlnie)
rndForest <- randomForest(Reach~., data=casesMod,
  mtry=5, ntree=500, nodesize=1000,
  cutoff=casesMod[, .N/casesMod[, .N], by=Reach][order(Reach)]$V1,
  importance=TRUE, keep.inbag=TRUE, keep.forest=TRUE) 

summary(rndForest)
rndForest

# jak malej¹ b³êdu wraz ze wzrostem liczby drzew
plot(rndForest)

# out of bag (podzbiór spraw s³u¿¹cy do estymacji b³êdu)
# obserwacje, które nie zosta³y wylosowane do budowy drzewa
rndForest$oob.times

# macierz liczba obserwacji x liczba drzew 
# które obserwacje by³y wziête do budowy drzewa  
rndForest$inbag

# uœrednione wyniki per obserwacja (g³osowanie modeli/drzew)
rndForest$votes

# "dobroæ" zmiennych - jak wp³ywaj¹ na prognozê/klasyfikacjê
importance(rndForest)
varImpPlot(rndForest)

# rozmiar drzew
treesize(rndForest, terminal=TRUE)

# pojedyncze drzewo
getTree(rndForest, k=8, labelVar=TRUE)

# partial plot (wp³yw na logit)
partialPlot(rndForest, casesMod, DPD, "1")

####################################################################### boosting
# # http://www-bcf.usc.edu/~gareth/ISL/ # page: 323 (337) algorithm
# idea: buduj kolejne drzewa, uzupe³niaj¹c informacjê zmiennych objaœniaj¹cych 
# informacj¹ o wyniku klasyfikacji poprzedniego drzewa
# czasmai zwany algorytmem powolnego uczenia

############################################################################ ada
# https://www.jstatsoft.org/article/view/v017i02/v17i02.pdf
adaBoost <- ada(...)


############################################################################ gbm
# http://allstate-university-hackathons.github.io/PredictionChallenge2016/GBM
casesMod[, .N, by=Reach]
casesMod[, Reach:=as.integer(Reach)-1]

boostForest <- gbm(Reach~., data=casesMod, 
  distribution="bernoulli",  #"gaussian", #
  n.trees=500, interaction.depth=3, shrinkage=0.01, n.minobsinnode=100,
  bag.fraction=0.5, train.fraction=0.5, cv.folds=5,
  var.monotone=rep(0, 18), verbose="CV", n.cores=3)

summary(boostForest)

# partial plot
plot(boostForest, "M_LastPaymentToImportDate")

# przegl¹danie drzew
pretty.gbm.tree(boostForest, 5)

#
gbm.perf(boostForest)
gbm.perf(boostForest, oobag.curve=TRUE)

#
predict.gbm