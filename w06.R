gcrm() # rm(list=ls())
gcHi(6) # Hello World!

library(data.table)
library(DMwR)
library(mvoutlier)
library(corrgram)
# library(MLR) # impute

setwd("C:/Users/gchlapinski/Desktop/Rrroboczy/UWr")
#gcsetwd()
load("KrukUWr2018.RData")

#! ############################################################## brakuj¹ce dane
# brakuj¹ce dane wystêpuj¹ tylko w zbiorze cases
summary(cases)
# w przypadku zbioru events NA oznacza, ¿e zdarzenie nie wyst¹pi³o
summary(events)

# czy wartoœæ NA zawsze oznacza brak danych?
cases[, .N, by=Gender]
# jak zmienna Gender wygl¹da w zestawieniu z Age (-1)
cases[, .(.N, MinusOne=sum(ifelse(Age == -1, 1, 0))), by=Gender]
# brak wartoœci Gender i Age jest wynikiem, ¿e jest to d³ug na firmê, czyli 
# tak naprawdê dodatkow¹ informacj¹

####################################### czy nale¿y uzupe³niaæ wartoœci brakuj¹ce
# najwiêcej braków mamy na Populacji
cases[is.na(PopulationInCity), .N]/cases[, .N]

# jednak¿e jak weŸmiemy tylko kompletne przypadki
woNAcases <- na.omit(cases)
# to odrzucimy 34% danych
woNAcases[, .N]/cases[, .N]

# poza tym mo¿emy zmieniæ rozk³ad zmiennych, na których mamy dane,
# lub zale¿noœci pomiêdzy zmiennymi
boxplot(list(cases[!is.na(DPD), ]$DPD, woNAcases$DPD))

# Uwaga: Istniej¹ przypadki, w których nie mo¿emy usuwaæ obserwacji ze wzglêdu
# na brak danych, np. wycena portfela (nie mo¿emy usun¹æ/nie wyceniæ spraw,
# w których pojawiaj¹ siê brakuj¹ce dane).

################################################ ile wartoœci NA mo¿na uzupe³niæ
# nie ma sztywno okreœlonej regu³y, jedynie przeczucie, ¿e uzupe³nianie wiêcej 
# 30%-40% braków jest generowaniem nowej zmiennej, a nie uzupe³nianiem starej

######################################################## uzupe³nianie eksperckie
cases[, .N, by=ExternalAgency]

# ró¿owy scenariusz: sprawa nie by³a przekazana do obs³ugi zewnêtrznej (Inkaso)
cases[is.na(ExternalAgency), ExternalAgency:=0]
# czarny scenariusz: sprawa by³a przekazana do obs³ugi zewnêtrznej (Inkaso)
cases[is.na(ExternalAgency), ExternalAgency:=1]
# w rzeczywistoœci raczej nie rozwa¿a siê ró¿owych scenariuszy, ale i w czarn¹
# rozpacz nie nale¿y popadaæ

################################################### uzupe³nianie œredni¹/median¹
# œrednia czêsto nie nale¿y do zbioru wartoœci zmiennej (mo¿na zaokr¹gliæ)
# œrednia jest czu³a na wartoœci odstaj¹ce (za chwilê szczegó³y)
cases[!is.na(DPD), .(Med=median(DPD), Avg=mean(DPD))]


################################################### uzupe³nianie wed³ug rozk³adu
tmp <- cases[!is.na(M_LastPaymentToImportDate), 
  .(Pr=.N/cases[!is.na(M_LastPaymentToImportDate), .N]), 
  by=M_LastPaymentToImportDate]
  
newValues <- sample(tmp$M_LastPaymentToImportDate, 
  size=cases[is.na(M_LastPaymentToImportDate), .N], 
  prob=tmp$Pr, replace=TRUE)  

# zachowamy rozk³ad 
table(newValues)/cases[is.na(M_LastPaymentToImportDate), .N]
tmp[order(M_LastPaymentToImportDate)]$Pr

cases[is.na(M_LastPaymentToImportDate), M_LastPaymentToImportDate:=newValues]

# ze zmienn¹ M_LastPaymentToImportDate powi¹zana jest zmienna LastPaymentAmount
# warto zachowaæ zale¿noœci pomiêdzy nimi
tmp <- cases[, mean(LastPaymentAmount, na.rm=TRUE), 
  by=M_LastPaymentToImportDate]

cases <- cases[tmp, on="M_LastPaymentToImportDate"]
cases[is.na(LastPaymentAmount), LastPaymentAmount:=V1]
cases[, V1:=NULL]
setDT(cases, key="CaseId")

########################################################### uzupe³nianie modelem
#! PopulationInCity   poprzez lasy


#! ########################################################## wartoœci odstaj¹ce
# estymator œredniej próbkowej jest bardzo czu³y na wartoœci odstaj¹ce
cases[, .(
  Avg=mean(TOA),
  Avg01=mean(TOA, trim=0.01),
  Avg05=mean(TOA, trim=0.05),
  Avg10=mean(TOA, trim=0.1),
  Mediana=median(TOA))]
  
statBP <- boxplot(cases$TOA)
attributes(statBP)

#################### weryfikacja wartoœci odstaj¹cych rozk³adów jednowymiarowych

################################################################## test Grubbs'a
# weryfikuje czy odstaj¹c¹ jest wartoœæ ekstremalna
# http://www.itl.nist.gov/div898/handbook/eda/section3/eda35h1.htm 

grubbs <- function(x, alpha=0.05) {
  N <- length(x)
  G <- max(abs(x - mean(x)))/sd(x)
  
  G > (N - 1)/sqrt(N)*sqrt((qt(0.5*alpha/N, N-2))^2/
    (N - 2 + (qt(0.5*alpha/N, N-2))^2))
}

head(cases[order(-TOA), ]$TOA, 10)
grubbs(cases$TOA)

############################################################# test Tietjen-Moore
# weryfikuje czy odstaj¹cymi s¹ k wartoœci ekstremalnych 
# http://www.itl.nist.gov/div898/handbook/eda/section3/eda35h2.htm
# zadanie na laborkê

########################################################### test Generalized ESD
# weryfikuje, które z k (górne ograniczenie) wartoœci ekstremalnych s¹ 
# wartoœciami odstaj¹cymi 
# http://www.itl.nist.gov/div898/handbook/eda/section3/eda35h3.htm

genESD <- function(x, r=1000, alpha=0.05) {
  tmp <- data.table(X=x, xTmp=abs(x - mean(x)))[order(xTmp)]
  
  n <- tmp[, .N]
  R <- c()
  lambda <- c()
  
  for (i in 1:r) {
    R <- c(R, tmp[, max(xTmp)/sd(X)]) 
    lambda <- c(lambda, (n - i)*qt(1-alpha/(2*(n - i + 1)), n - i - 1)/
      sqrt((n - i - 1 + (qt(alpha/(2*(n - i + 1)), n - i - 1))^2)*
        (n - i + 1)))

    tmp <- tmp[1:(tmp[, .N] - 1), ]
    tmp[, xTmp:=abs(X - mean(X))]
    tmp <- tmp[order(xTmp)]
  } 
  
  sum(R > lambda)
}

plot(sort(cases$TOA))
genESD(cases$TOA)

#################### weryfikacja wartoœci odstaj¹cych rozk³adów wielowymiarowych
# https://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Classification/Outliers
#
# u¿yteczna odleg³oœæ w statystyce
# https://en.wikipedia.org/wiki/Mahalanobis_distance
# mo¿na j¹ liczyæ pomiêdzy
# - obserwacjami
# - obserwacj¹, a rozk³adem 

# idea algorytmu:
# 1. Wyznacz odporny estymator macierzy kowariancji i œredniej (rozk³adu 
#    wielowymiarowego).
# 2. Wyznacz odleg³oœæ Mahalonobisa ka¿dej obserwacji od rozk³adu okreœlonego
#    przez parametry wyznaczone w pkt. 1.
# 3. Zweryfikuj, dla których obserwacji odleg³oœæ Mahalonobisa przekracza 
#    wartoœæ krytyczn¹

casesTmp <- na.omit(cases[Product == "Cash loan", .SD, 
  .SDcols=c("TOA", "Principal", "D_ContractDateToImportDate", "DPD", 
    "Age", "GDPPerCapita", "MeanSalary")])

results <- uni.plot(casesTmp)
table(results$outliers)
plot(sort(results$md))

#! dodaæ coœ o metryce Mahalonobisa


#! ############################################ porównanie jednorodnoœci zbiorów
# pos³uguj¹c siê metodologi¹ zbiór ucz¹cy/testowy, lub zbiór referencyjny i 
# zbiór prognozowany (gdy chcemy przenosiæ pewne zale¿noœci i/lub wnioskowania) 
# nale¿y siê upewniæ, ¿e zbiory s¹ jednorodne
# jednorodnoœæ zbiorów powinna te¿ byæ monitorowana w cyklu ¿ycia modelu, by 
# wykluczyæ sytuacjê, w której model zosta³ zbudowany na innych danych, ni¿ 
# dane, na których jest stosowany

#! dokoñczyæ

########################################## porównanie rozk³adów jednowymiarowych
compHist <- function(sampleA, sampleB, variable, nBin=20) {
  data <- rbindlist(list(sampleA, sampleB))
  vMax <- max(data[[variable]])
  vMin <- min(data[[variable]])  
  
  if (data[, .N, by=variable][, .N] < 20) {
    tmpA <- sampleA[, .N/sampleA[, .N], by=variable][order(get(variable))]
    tmpB <- sampleB[, .N/sampleB[, .N], by=variable][order(get(variable))]
  
    yMax <- max(tmpA$V1, tmpB$V1)
    plot(tmpA[[variable]], tmpA$V1, type="p", lwd=6, col="darkblue",
      ylim=c(0,yMax), xlim=c(vMin, vMax), xlab=variable, ylab="Probability",
      main="Distribution comparison")
    lines(tmpB[[variable]], tmpB$V1, type="p", lwd=3, col="gold")
    legend("topleft", legend=c("SampleA", "SampleB"), lwd=c(3, 3), lty=c(1, 1),
      col=c("darkblue", "gold"))
  } else {
    x <- vMin + (1:nBin)*(vMax - vMin)/nBin
    tmp <- data.table()
    for (i in 2:nBin) {
      tmp <- rbindlist(list(tmp, 
        data.table(
          X=x[i], 
          A=sampleA[x[i-1] < get(variable) & 
            get(variable) <= x[i], .N]/sampleA[, .N],
          B=sampleB[x[i-1] < get(variable) & 
            get(variable) <= x[i], .N]/sampleB[, .N])))
    }
  
    yMax <- max(tmp$A, tmp$B)  
    plot(tmp$X, tmp$A, type="p", lwd=6, col="darkblue",
      ylim=c(0,yMax), xlim=c(vMin, vMax), xlab=variable, ylab="Probability",
      main="Distribution comparison")
    lines(tmp$X, tmp$B, type="p", lwd=3, col="gold", lty=3)
    legend("topleft", legend=c("SampleA", "SampleB"), lwd=c(3, 3), lty=c(1, 1),
      col=c("darkblue", "gold"))
  }         

  invisible()
}

########################################################### porównanie korelacji
corrgram(caseTmpB, order=FALSE, lower.panel=panel.shade,
  upper.panel=panel.pie, text.panel=panel.txt,
  main="Car Milage Data in PC2/PC1 Order")
  
windows()

corrgram(testSampleB, order=FALSE, lower.panel=panel.shade,
  upper.panel=panel.pie, text.panel=panel.txt,
  main="Car Milage Data in PC2/PC1 Order")
  
############################################ porównanie rozk³adów dwuwymiarowych
caseTmpB[,AgeBond:=cut(Age,c(0,20,40,60,150))]
caseTmpB[,DPDBond:=cut(DPD,c(0,200,400,600,1500))]
tab1=table(caseTmpB$AgeBond,caseTmpB$DPDBond)

testSampleB[,AgeBond:=cut(Age,c(0,20,40,60,150))]
testSampleB[,DPDBond:=cut(DPD,c(0,200,400,600,1500))]
tab2=table(caseTmpB$AgeBond,caseTmpB$DPDBond)
mosaicplot(tab1) 
windows()
mosaicplot(tab2)

############################################# wspó³czynnik stabilnoœci populacji
# Niech: U - zbiór ucz¹cy, W - zbiór walidacyjny, k - liczba klas dla zmiennej
#
# Wsp = \sum^k_i=1 (%U_i - %W_i) * ln(%U_i/%W_i) 
#
# Regu³y kciuka:
# Wsp <= 0.1 - populacje podobne
# 0.1 < Wsp <= 0.25 - populacje nieznacznie siê ró¿ni¹ (lampka siê zapala)
# 0.25 < Wsp - populacje znacznie siê ró¿ni¹ (panika!!) 

