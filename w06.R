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

#! ############################################################## brakuj�ce dane
# brakuj�ce dane wyst�puj� tylko w zbiorze cases
summary(cases)
# w przypadku zbioru events NA oznacza, �e zdarzenie nie wyst�pi�o
summary(events)

# czy warto�� NA zawsze oznacza brak danych?
cases[, .N, by=Gender]
# jak zmienna Gender wygl�da w zestawieniu z Age (-1)
cases[, .(.N, MinusOne=sum(ifelse(Age == -1, 1, 0))), by=Gender]
# brak warto�ci Gender i Age jest wynikiem, �e jest to d�ug na firm�, czyli 
# tak naprawd� dodatkow� informacj�

####################################### czy nale�y uzupe�nia� warto�ci brakuj�ce
# najwi�cej brak�w mamy na Populacji
cases[is.na(PopulationInCity), .N]/cases[, .N]

# jednak�e jak we�miemy tylko kompletne przypadki
woNAcases <- na.omit(cases)
# to odrzucimy 34% danych
woNAcases[, .N]/cases[, .N]

# poza tym mo�emy zmieni� rozk�ad zmiennych, na kt�rych mamy dane,
# lub zale�no�ci pomi�dzy zmiennymi
boxplot(list(cases[!is.na(DPD), ]$DPD, woNAcases$DPD))

# Uwaga: Istniej� przypadki, w kt�rych nie mo�emy usuwa� obserwacji ze wzgl�du
# na brak danych, np. wycena portfela (nie mo�emy usun��/nie wyceni� spraw,
# w kt�rych pojawiaj� si� brakuj�ce dane).

################################################ ile warto�ci NA mo�na uzupe�ni�
# nie ma sztywno okre�lonej regu�y, jedynie przeczucie, �e uzupe�nianie wi�cej 
# 30%-40% brak�w jest generowaniem nowej zmiennej, a nie uzupe�nianiem starej

######################################################## uzupe�nianie eksperckie
cases[, .N, by=ExternalAgency]

# r�owy scenariusz: sprawa nie by�a przekazana do obs�ugi zewn�trznej (Inkaso)
cases[is.na(ExternalAgency), ExternalAgency:=0]
# czarny scenariusz: sprawa by�a przekazana do obs�ugi zewn�trznej (Inkaso)
cases[is.na(ExternalAgency), ExternalAgency:=1]
# w rzeczywisto�ci raczej nie rozwa�a si� r�owych scenariuszy, ale i w czarn�
# rozpacz nie nale�y popada�

################################################### uzupe�nianie �redni�/median�
# �rednia cz�sto nie nale�y do zbioru warto�ci zmiennej (mo�na zaokr�gli�)
# �rednia jest czu�a na warto�ci odstaj�ce (za chwil� szczeg�y)
cases[!is.na(DPD), .(Med=median(DPD), Avg=mean(DPD))]


################################################### uzupe�nianie wed�ug rozk�adu
tmp <- cases[!is.na(M_LastPaymentToImportDate), 
  .(Pr=.N/cases[!is.na(M_LastPaymentToImportDate), .N]), 
  by=M_LastPaymentToImportDate]
  
newValues <- sample(tmp$M_LastPaymentToImportDate, 
  size=cases[is.na(M_LastPaymentToImportDate), .N], 
  prob=tmp$Pr, replace=TRUE)  

# zachowamy rozk�ad 
table(newValues)/cases[is.na(M_LastPaymentToImportDate), .N]
tmp[order(M_LastPaymentToImportDate)]$Pr

cases[is.na(M_LastPaymentToImportDate), M_LastPaymentToImportDate:=newValues]

# ze zmienn� M_LastPaymentToImportDate powi�zana jest zmienna LastPaymentAmount
# warto zachowa� zale�no�ci pomi�dzy nimi
tmp <- cases[, mean(LastPaymentAmount, na.rm=TRUE), 
  by=M_LastPaymentToImportDate]

cases <- cases[tmp, on="M_LastPaymentToImportDate"]
cases[is.na(LastPaymentAmount), LastPaymentAmount:=V1]
cases[, V1:=NULL]
setDT(cases, key="CaseId")

########################################################### uzupe�nianie modelem
#! PopulationInCity   poprzez lasy


#! ########################################################## warto�ci odstaj�ce
# estymator �redniej pr�bkowej jest bardzo czu�y na warto�ci odstaj�ce
cases[, .(
  Avg=mean(TOA),
  Avg01=mean(TOA, trim=0.01),
  Avg05=mean(TOA, trim=0.05),
  Avg10=mean(TOA, trim=0.1),
  Mediana=median(TOA))]
  
statBP <- boxplot(cases$TOA)
attributes(statBP)

#################### weryfikacja warto�ci odstaj�cych rozk�ad�w jednowymiarowych

################################################################## test Grubbs'a
# weryfikuje czy odstaj�c� jest warto�� ekstremalna
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
# weryfikuje czy odstaj�cymi s� k warto�ci ekstremalnych 
# http://www.itl.nist.gov/div898/handbook/eda/section3/eda35h2.htm
# zadanie na labork�

########################################################### test Generalized ESD
# weryfikuje, kt�re z k (g�rne ograniczenie) warto�ci ekstremalnych s� 
# warto�ciami odstaj�cymi 
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

#################### weryfikacja warto�ci odstaj�cych rozk�ad�w wielowymiarowych
# https://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Classification/Outliers
#
# u�yteczna odleg�o�� w statystyce
# https://en.wikipedia.org/wiki/Mahalanobis_distance
# mo�na j� liczy� pomi�dzy
# - obserwacjami
# - obserwacj�, a rozk�adem 

# idea algorytmu:
# 1. Wyznacz odporny estymator macierzy kowariancji i �redniej (rozk�adu 
#    wielowymiarowego).
# 2. Wyznacz odleg�o�� Mahalonobisa ka�dej obserwacji od rozk�adu okre�lonego
#    przez parametry wyznaczone w pkt. 1.
# 3. Zweryfikuj, dla kt�rych obserwacji odleg�o�� Mahalonobisa przekracza 
#    warto�� krytyczn�

casesTmp <- na.omit(cases[Product == "Cash loan", .SD, 
  .SDcols=c("TOA", "Principal", "D_ContractDateToImportDate", "DPD", 
    "Age", "GDPPerCapita", "MeanSalary")])

results <- uni.plot(casesTmp)
table(results$outliers)
plot(sort(results$md))

#! doda� co� o metryce Mahalonobisa


#! ############################################ por�wnanie jednorodno�ci zbior�w
# pos�uguj�c si� metodologi� zbi�r ucz�cy/testowy, lub zbi�r referencyjny i 
# zbi�r prognozowany (gdy chcemy przenosi� pewne zale�no�ci i/lub wnioskowania) 
# nale�y si� upewni�, �e zbiory s� jednorodne
# jednorodno�� zbior�w powinna te� by� monitorowana w cyklu �ycia modelu, by 
# wykluczy� sytuacj�, w kt�rej model zosta� zbudowany na innych danych, ni� 
# dane, na kt�rych jest stosowany

#! doko�czy�

########################################## por�wnanie rozk�ad�w jednowymiarowych
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

########################################################### por�wnanie korelacji
corrgram(caseTmpB, order=FALSE, lower.panel=panel.shade,
  upper.panel=panel.pie, text.panel=panel.txt,
  main="Car Milage Data in PC2/PC1 Order")
  
windows()

corrgram(testSampleB, order=FALSE, lower.panel=panel.shade,
  upper.panel=panel.pie, text.panel=panel.txt,
  main="Car Milage Data in PC2/PC1 Order")
  
############################################ por�wnanie rozk�ad�w dwuwymiarowych
caseTmpB[,AgeBond:=cut(Age,c(0,20,40,60,150))]
caseTmpB[,DPDBond:=cut(DPD,c(0,200,400,600,1500))]
tab1=table(caseTmpB$AgeBond,caseTmpB$DPDBond)

testSampleB[,AgeBond:=cut(Age,c(0,20,40,60,150))]
testSampleB[,DPDBond:=cut(DPD,c(0,200,400,600,1500))]
tab2=table(caseTmpB$AgeBond,caseTmpB$DPDBond)
mosaicplot(tab1) 
windows()
mosaicplot(tab2)

############################################# wsp�czynnik stabilno�ci populacji
# Niech: U - zbi�r ucz�cy, W - zbi�r walidacyjny, k - liczba klas dla zmiennej
#
# Wsp = \sum^k_i=1 (%U_i - %W_i) * ln(%U_i/%W_i) 
#
# Regu�y kciuka:
# Wsp <= 0.1 - populacje podobne
# 0.1 < Wsp <= 0.25 - populacje nieznacznie si� r�ni� (lampka si� zapala)
# 0.25 < Wsp - populacje znacznie si� r�ni� (panika!!) 

