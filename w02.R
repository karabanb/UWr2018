gcrm() # rm(list=ls())
gcHi(2) # Hello World!

library(data.table)
# wizualizacja korelacji
#library(corrgram)
#library(corrplot)

setwd("C:/Users/gchlapinski/Desktop/Rrroboczy/UWr")
load("KrukUWr2018.RData")

#!######################################### eksploracja danych - poznanie danych
# z lotu ptaka
cases[, summary(.SD), .SDcols=setdiff(names(cases), "CaseId")] 

# szczeg??owy pogl?d na TOA
cases[, quantile(TOA, probs=seq(0, 1, 0.05))] # jedni wol? liczby
plot(sort(cases$TOA)) # inni wol? wykres
head(cases[order(-TOA), ], 15) # najwi?ksze salda (super z?ote karty)

# zr??nicowanie salda wzgl?dem produktu
cases[, .(.N,
  Min=quantile(TOA, probs=0),
  Q05=quantile(TOA, probs=0.05),
  Q1=quantile(TOA, probs=0.25),
  Med=quantile(TOA, probs=0.5),
  Q3=quantile(TOA, probs=0.75),
  Q95=quantile(TOA, probs=0.95),
  Max=quantile(TOA, probs=1)), by=Product]

# tabela kontyngencji
countTable <- table(cases$Product,
  cut(cases$TOA, breaks=quantile(cases$TOA, 
    probs=seq(from=0, to=1, by=0.2), include.lowest=TRUE)))
# wizualizacja cz?sto?ci tabeli kontyngencji
barplot(prop.table(countTable, 1), col=c("darkblue","darkred"), beside=TRUE)

# zr??nicowanie salda wzgl?dem produktu i p?ci
cases[, .(.N,
  Min=quantile(TOA, probs=0),
  Q05=quantile(TOA, probs=0.05),
  Q1=quantile(TOA, probs=0.25),
  Med=quantile(TOA, probs=0.5),
  Q3=quantile(TOA, probs=0.75),
  Q95=quantile(TOA, probs=0.95),
  Max=quantile(TOA, probs=1)), by=.(Gender, Product)][order(Gender, Product)] 

# zr??nicowanie salda wzgl?dem etapu egzekucji
cases[, .(.N,
  Min=quantile(TOA, probs=0),
  Q05=quantile(TOA, probs=0.05),
  Q1=quantile(TOA, probs=0.25),
  Med=quantile(TOA, probs=0.5),
  Q3=quantile(TOA, probs=0.75),
  Q95=quantile(TOA, probs=0.95),
  Max=quantile(TOA, probs=1)), by=.(Bailiff, ClosedExecution)]

# zale?no?? pomi?dzy zad?u?eniem a kapita?em
plot(cases$Principal, cases$TOA, pch='.')
abline(lm(cases$TOA~cases$Principal), col="tomato", lwd=3)

# zale?no?? pomi?dzy zad?u?eniem a DPD
plot(cases$DPD, cases$TOA, pch='.')
abline(lm(cases$TOA~cases$DPD), col="tomato", lwd=3)

# proste uzupe?nienie brak?w poprzez ?redni?
# brakami danych b?dziemy zajmowa? si? oddzielnie w przysz?o?ci 
cases[, MeanSalary:=ifelse(is.na(MeanSalary), 
  mean(MeanSalary, na.rm=TRUE), MeanSalary)]

plot(cases$MeanSalary, cases$TOA, pch='.')
abline(lm(cases$TOA~cases$MeanSalary), col="tomato", lwd=3)

# zale?no?? pomi?dzy zad?u?eniem a wiekiem
plot(cases$Age, cases$TOA, pch='.')
abline(lm(cases$TOA~cases$Age), col="tomato", lwd=3)

# dyskretyzacja (mo?e u?atwi? wizualizacj?)   
cases[, 
  TOAband:=cut(TOA, 
    breaks=c(0, 1000, 2000, 4000, 6000, 10000, 20000, 40000, 65000))]
cases[, .(.N, AvgTOA=mean(TOA)), by=TOAband][order(AvgTOA)]

plot(cases$TOAband, cases$Age)
# ?boxplot 

# a jak wygl?da to na poprzednich zmiennych??
plot(cases$TOAband, cases$Principal)
plot(cases$TOAband, cases$DPD)
plot(cases$TOAband, cases$MeanSalary)

# Uwaga: Land to raczej factor (nie ma porz?dku w tych warto?ciach)
plot(as.factor(cases$Land), cases$TOA)


########################################## zdarzenia maj? dodatkowy wymiar czasu
# dla ka?dej sprawy tabela events ma 12 wierszy z 12 miesi?cy obs?ugi
events[cases][, .N, by=CaseId][N != 12, ]

# NA w przypadku zdarze? oznacza, ?e zdarzenie nie wyst?pi?o
events[is.na(NumberOfCalls), NumberOfCalls:=0]
events[is.na(NumberOfCallsWithClient), NumberOfCallsWithClient:=0]
events[is.na(NumberOfPayment), NumberOfPayment:=0]
events[is.na(PaymentAmount), PaymentAmount:=0]

tmp <- events[cases][,
  .(NumberOfCases=.N,
    NumberOfCalls=sum(NumberOfCalls),
    SR=sum(PaymentAmount)/max(TOA)),
  by=.(Month, TOAband)][order(TOAband, Month)]

categories <- sort(unique(cases$TOAband))
colLine <- rainbow(length(categories))

# pr?by telefoniczne
plot(1:12, tmp[TOAband == categories[1], 
  ]$NumberOfCalls, ylim=c(0, 1.05*max(tmp$NumberOfCalls)), 
  type="l", col=colLine[1], lwd= 3,
  main="NumberOfCalls in TOA band", ylab="NumberOfCalls", xlab="MonthOfService")

for (i in 2:length(categories)) {
  lines(1:12, tmp[TOAband == categories[i], ]$NumberOfCalls, col=colLine[i],
  lty=ifelse(i %% 6 == 0, 6, i %% 6), lwd= 3)
}

legend("topright", legend=categories, 
  lty=c(1:6, 1:2), col=colLine, lwd=rep(3, 8))

# g??wny cel (kasa)
plot(1:12, tmp[TOAband == categories[1], 
  ]$SR, ylim=c(0, 1.05*max(tmp$SR)), 
  type="l", col=colLine[1], lwd= 3,
  main="SR in TOA band", ylab="SR", xlab="MonthOfService")

for (i in 2:length(categories)) {
  lines(1:12, tmp[TOAband == categories[i], ]$SR, col=colLine[i],
  lty=ifelse(i %% 6 == 0, 6, i %% 6), lwd= 3)
}

legend("topright", legend=categories, 
  lty=c(1:6, 1:2), col=colLine, lwd=rep(3, 8))

# ?eby zej?? na spraw? trzeba ustali? moment w czasie
tmp <- events[cases][Month <= 6,
  .(NumberOfCallsWithClient=sum(NumberOfCallsWithClient),
    NumberOfPayment=sum(ifelse(NumberOfPayment > 0, 1, 0))),
  by=.(CaseId, TOAband)]

plot(tmp$NumberOfCallsWithClient, tmp$NumberOfPayment)
abline(lm(tmp$NumberOfPayment~tmp$NumberOfCallsWithClient), col="tomato", lwd=3)

# lub okno czasowe
tmp <- events[cases][Month > 3 & Month <= 6,
  .(NumberOfCallsWithClient=sum(NumberOfCallsWithClient),
    NumberOfPayment=sum(ifelse(NumberOfPayment > 0, 1, 0))),
  by=.(CaseId, TOAband)]

plot(tmp$NumberOfCallsWithClient, tmp$NumberOfPayment)
abline(lm(tmp$NumberOfPayment~tmp$NumberOfCallsWithClient), col="tomato", lwd=3)

#!#################################################################### korelacje
# https://pl.wikipedia.org/wiki/Wsp%C3%B3%C5%82czynnik_korelacji_Pearsona
# https://pl.wikipedia.org/wiki/Wsp%C3%B3%C5%82czynnik_korelacji_rang_Spearmana
# https://pl.wikipedia.org/wiki/Tau_Kendalla

##################################### Uwaga: weryfikowana jest zale?no?? liniowa
x <- seq(from=-1, to=1, by=0.01)
y1 <- 2*x + rnorm(length(x), sd=0.1)
cor(x, y1)
y2 <- -2*x + rnorm(length(x), sd=0.1)
cor(x, y2)
y3 <- 2*x^2 + rnorm(length(x), sd=0.1)
cor(x, y3)

plot(x, y1, col="darkred", pch=16, main="Czy wszystkie zbiory s? skorelowane?")
lines(x, y2, col="darkblue", type="p", pch=16)
lines(x, y3, col="darkgreen", type="p", pch=16)
legend("bottom", legend=c("2x", "-2*x", "2*x^2"), pch=rep(16,3),
  col=c("darkred", "darkblue", "darkgreen"))

# Uwaga: Jakie s? r??nice pomi?dzy wsp??czynnikami korelacji Pearsona, 
#        Spearmana i Kendalla? Jakie to ma znaczenie?

# liczbowo
corMatrix <- cor(cases[, .SD, .SDcols=
  setdiff(names(cases), c("CaseId", "Product", "Gender", "Land", "TOAband"))], 
  use="pairwise.complete.obs", method="spearman") 

# wizualnie
corrgram::corrgram(corMatrix, order=FALSE, 
  lower.panel=corrgram::panel.shade, upper.panel=corrgram::panel.pie, 
  text.panel=corrgram::panel.txt, col.regions=colorRampPalette(
    c("darkgoldenrod4", "burlywood1", "white", "darkkhaki", "darkgreen")), 
  main="Korelacje", cor.method="spearman")
  
corrplot::corrplot(corMatrix, method="ellipse")

###################################################### test istotno?ci korelacji
cor.test(cases$LoanAmount, cases$Principal, 
  method="pearson", alternative="two.sided", conf.level=0.95)
  
cor.test(cases$TOA, cases$Age, 
  method="pearson", alternative="two.sided", conf.level=0.95)

# prymitywna podpr?bka
cor.test(cases[1:1000, ]$LoanAmount, cases[1:1000, ]$Principal, 
  method="pearson", alternative="two.sided", conf.level=0.95)
  
cor.test(cases[1:10000, ]$TOA, cases[1:10000, ]$Age, 
  method="pearson", alternative="two.sided", conf.level=0.95)

# dla danych porz?dkowych lepszym wyborem jest spearman
cor.test(cases$LoanAmount, as.integer(cases$TOAband), 
  method="spearman", alternative="two.sided", conf.level=0.95)

# na labork?....
#vars <- setdiff(names(casesTmp), 
#  c("CaseId", "Product", "Gender", "Land", "TOAband"))
#
#corrSignificance <- data.table()
#
#for (i in 1:(length(vars) - 1)) {
#  for (j in (i+1):length(vars)) {
#    test <- cor.test(casesTmp[[vars[i]]], casesTmp[[vars[j]]], 
#      method="pearson", alternative="two.sided", conf.level=0.95)
#  
#    corrSignificance <- rbindlist(list(corrSignificance, 
#      data.table(V1=vars[i], V2=vars[j], pValue=test$p.value)))
#  }
#}
#
#corrSignificance[pValue > 0.05, ]

# Uwaga: Globalne zale?no?ci/wnioski mog? by? nieprawdziwe lokalnie i odwrotnie.
# Uwaga: Do tematu wr?cimy przy okazji miar asocjacji.

#!############################## co mo?emy wycisn?? z danych - dodatkowe zmienne
# przekszta?cenia funkcyjne
cases[, LogTOA:=log(TOA)]

hist(cases$TOA) # odstaj?ca
hist(cases$LogTOA) # logarytm to szczeg??ny przypadek transfomacji Box'a-Cox'a

# zmienne pochodne - sp?acenie kredytu
cases[Product == "Cash loan", LoanRepayment:=1 - Principal/LoanAmount]

# warto?ci ujemne
plot(sort(cases[Product == "Cash loan", LoanRepayment]))
head(cases[Product == "Cash loan", ][order(LoanRepayment)], 30)

# ustawmy bariery (uwaga: lepiej nie tworzy? du?ych atom?w)
cases[LoanRepayment < 0, .N]
cases[LoanRepayment < 0, LoanRepayment:=0]

plot(cases$TOAband, cases$LoanRepayment, cex.axis=0.5)

# atom mo?e zaciemni? reszt? rozk?adu
hist(cases$M_LastPaymentToImportDate)

# pierwszy miesi?c kontaktu
tmp <- events[cases][NumberOfCallsWithClient > 0,
  .(MinMonth_CWC=min(Month)),
  by=.(CaseId, TOAband)]
  
plot(sort(tmp$MinMonth_CWC))  

# czy wp?ata po telefonie 2M
tmp <- events[NumberOfPayment > 0, .(CaseId, MO=Month)]
setDT(tmp, key="CaseId")

tmp <- events[tmp, allow.cartesian=TRUE][MO - Month <= 2 & Month <= MO, ]   
tmp <- tmp[, .(PaymentAfterCWC=
  ifelse(sum(NumberOfCallsWithClient, na.rm=T) > 0, 1L, 0L)), by=.(CaseId, MO)]

setnames(tmp, names(tmp), c("CaseId", "Month", "PaymentAfterCWC"))
setDT(tmp, key=c("CaseId", "Month"))

events <- tmp[events]

events[, .N, by=PaymentAfterCWC]


# i co Wam jeszcze przyjdzie do g?owy (pod warunkiem, ?e b?dzie u?yteczne)
