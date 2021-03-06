gcrm() # rm(list=ls())
gcHi(8) # Hello World!

library(data.table)
library(MASS)
library(car) # vif
library(randtests) # turning.point.test, difference.sign.test
library(olsrr) # const. var tests

load("KrukUWr2018.RData")

#! ############################################################ regresja liniowa
# http://www-bcf.usc.edu/~gareth/ISL/
# i wiele innych (to chyba najbardziej rozpowszechniona metoda statystyczna)

#! przygotowanie zbioru danych 
addV <- events[Month <= 6, .(
  LegalTransfer=max(gcisna(TransferToLegalProcess, 0)),
  SignedAgreement=ifelse(sum(gcisna(NumberOfAgreementSigned, 0)) > 0 , 1, 0),
  Reach=ifelse(sum(gcisna(NumberOfCalls, 0)) > 0,
    sum(gcisna(NumberOfCallsWithClient, 0))/sum(gcisna(NumberOfCalls, 0)), 0),
  NoCallClient=sum(gcisna(NumberOfCallsWithClient, 0)),
  NoLetRec=sum(gcisna(NumberOfLettersReceived, 0)),
  NoPay=sum(gcisna(NumberOfPayment, 0)),
  Pay6=sum(gcisna(PaymentAmount, 0))), by=CaseId]
goalV <- events[Month > 6, .(
  Pay712=sum(gcisna(PaymentAmount, 0))), by=CaseId]

set.seed(123)
n <- 5000 # liczba obserwacji

casesTmp <- na.omit(cases[Age != - 1 & TOA >= 100 
  & DPD >= 100 & LastPaymentAmount > 0, ])
casesTmp <- casesTmp[sample(casesTmp[, .N], n), ]
setDT(casesTmp, key="CaseId")

casesTmp <- casesTmp[addV, nomatch=0][goalV, nomatch=0]

# b�dziemy modelowa� zmienn�
par(mfrow=c(2, 1))
plot(casesTmp$Pay712, type="l")
hist(casesTmp$Pay712, 50)
# a dok�adniej (pozostaje spory atom w 0)
casesTmp[, Pay712log:=log(Pay712 + 1)]
windows()
par(mfrow=c(2, 1))
plot(casesTmp$Pay712log, type="l")
hist(casesTmp$Pay712log, 50)

# mamy do dyspozycji zmienne
summary(casesTmp)

#! regresja liniowa/wieloraka
# Model regresji: 
#
# Y = beta_0 + beta_1 X_1 + ... + beta_p X_p + e 
#
# lub wektorowo/macierzowo
# 
# Y = X Beta + E, X = [1, X_1, ..., X_p] (design matrix)
#
# Co trzeba za�o�y� o losowym zak��ceniu:
# - nieskorelowanie
# - �rednia 0 i sta�a wariancja sigma^2
#
# Co trzeba za�o�y� o predyktorach X_1, ..., X_p
# - niewsp�liniowo�� (macierz X'X ma rz�d p)

(lmFit <- lm(Pay712log ~ TOA + Principal + DPD + M_LastPaymentToImportDate + 
  LastPaymentAmount + MeanSalary + Age + SignedAgreement + Reach + NoCallClient + 
  NoLetRec + NoPay + Pay6 + LegalTransfer, data=casesTmp))

# co kryje w sobie obiekt lmFit
names(lmFit)

lmFit$coefficients # mo�na dobiera� si� do poszczeg�lnych slot�w listy
coef(lmFit) # lub u�ywa� przygotowanych funckji

# liczba parametr�w (zminnych)
p <- length(lmFit$coefficients) - 1

# czy ma znaczenia r�norodno�� skal na zmiennych modelu?
casesTmp[, TOA:=0.1*TOA]

(lmTmp <- lm(Pay712log ~ TOA + Principal + DPD + M_LastPaymentToImportDate + 
  LastPaymentAmount + MeanSalary + Age + SignedAgreement + Reach + NoCallClient + 
  NoLetRec + NoPay + Pay6 + LegalTransfer, data=casesTmp))

casesTmp[, TOA:=10*TOA]

(lmTmp)
(lmFit)

# dopasowane warto�ci 
# hatY_i = hatBeta_0 + hatBeta_1 x_1,i + ... + hatBeta_p x_p,i
head(fitted(lmFit), 10) # lmFit$fitted.values
# residua s� oszacowaniem zak��cenia losowego w modelu e
# hatE_i = r_i = y_i - hatY_i 
head(residuals(lmFit), 10) # lmFit$residuals

#! statystyki dopasowania
summary(lmFit)

# Residual sum of squares
# RSS = sum_i (y_i - hatY_i)^2 = sum_i (r_i)^2
(RSS <- sum(lmFit$residuals^2))
# Residual Standard Error
# RSE = sqrt(RSS/(n - p - 1))  
(RSE <- sqrt(RSS/(n - p - 1)))

# Total sum of squares
# TSS = sum_i (y_i - mean(y))^2 
(TSS <- sum((casesTmp$Pay712log - mean(casesTmp$Pay712log))^2))

# R^2 (jaki procent zmienno�ci wyja�nia model regresji)
#
# R^2 = 1 - RSS/TSS
#
# Uwaga: 
# - w przypadku regresji liniowej R^2 = ( hatCor(X, Y) )^2
# - w przypadku regresji liniowej R^2 = ( hatCor(Y, hatY) )^2
(R2 <- 1 - RSS/TSS) 

# adjusted R^2 - skorygowane (wzgl�dem liczby zmiennych w modelu) R^2 
#
# adjR^2 = 1 - (1 - R^2)*(n - 1)/(n - p - 1) 
#
# Uwaga: R^2 zawsze ro�nie wraz z kolejn� dodan� zmienn� do modelu
(adjR2 <- 1 - (1 - R2)*(n - 1)/(n - p - 1))

# F z (p , n - p - 1) stopniami swobody
#
# F = ( (TSS - RSS)/p )/( RSS/(n - p - 1) )
#
# Na bazie statystyki F weryfikowana jest hipoteza (czy Y zale�y od kt�rego� X_i) 
# H_0: beta_1 = ... = beta_p = 0
(Fs <- ( (TSS - RSS)/p )/( RSS/(n - p - 1) ))
(pValue <- pf(Fs, p, n - p - 1, lower.tail=FALSE))

# wp�yw zmiennych na R^2
lmTmp <- lm(Pay712log ~ TOA + Principal + DPD + M_LastPaymentToImportDate + 
  LastPaymentAmount + MeanSalary + Age + SignedAgreement + Reach + NoCallClient + 
  NoLetRec + NoPay + Pay6 + LegalTransfer, data=casesTmp)
summary(lmTmp)

#! istotno�� wsp�czynnik�w
# t test - testowanie czy wsp�czynniki s� r�wne 0 (brak zwi�zku ze zm. celu)
# H_0: beta_i = 0  
#
# t_i = hatBeta_i/std(hatBeta_i) ~ T(n - 2)
#
# Uwaga: Test weryfikuje istotno�� wsp�czynnika przy za�o�eniu, �e pozosta�e 
# s� w modelu - nie mo�na "bra� koniunkcji" 

summary(lmFit)
# macierz kowariancji parametr�w modelu (beta)
# hatSigma^2 (X'X)^(-1) = RSE^2 (X'X)^(-1)
X <- as.matrix(casesTmp[, .SD, .SDcols=c("TOA", "Principal", "DPD", "M_LastPaymentToImportDate",
  "LastPaymentAmount", "MeanSalary", "Age", "SignedAgreement", "Reach", 
  "NoCallClient", "NoLetRec", "NoPay", "Pay6", "LegalTransfer")])
X <- cbind(rep(1, 5000), X)

sqrt(diag(RSE^2 * solve(t(X) %*% X)))
sqrt(diag(vcov(lmFit)))

# przedzia�y ufno�ci dla parametr�w modelu
confint(lmFit)

# przy za�o�eniu normalno�ci e
cbind(coef(lmFit) - 1.96*sqrt(diag(vcov(lmFit))), 
      coef(lmFit) + 1.96*sqrt(diag(vcov(lmFit))))

# do kt�rego z przedzia��w nale�y 0
coef(lmFit) - 1.96*sqrt(diag(vcov(lmFit))) < 0 &
coef(lmFit) + 1.96*sqrt(diag(vcov(lmFit))) > 0

############################################### przy�adowe dane (nieskorelowane)
significNo <- c()

for (i in 1:100) {
  dataX <- data.frame(matrix(rnorm(101000), nrow=1000, ncol=101))
  names(dataX) <- c("Y", paste0("X", 1:100))

  lmDataX <- lm(Y ~ ., data=dataX)
  sLmDataX <- summary(lmDataX)

  # ile wsp�czynnik�w istotnych?
  significNo <- c(significNo, sum(sLmDataX$coefficients[, 4] < 0.05))
}

summary(significNo)
boxplot(significNo)

rm(i, significNo, dataX, lmDataX, sLmDataX)
################################################################################

# Statystyki F mo�emy u�y� do testowania hipotezy (istotno�� q wsp�czynnik�w)
# 
# H_0: beta_(p-q+1) = ... = beta_p = 0
#
# F = ( (RSS_0 - RSS)/q )/( RSS/(n - p - 1) ), 
# gdzie RSS_0 wyznaczone jest dla modelu z wyzerowanymi wsp�czynnikami 
summary(lmFit)

(lmFit2 <- lm(Pay712log ~ TOA + Principal + DPD + M_LastPaymentToImportDate 
  + SignedAgreement + Reach + NoCallClient + NoPay, data=casesTmp))

summary(lmFit2)

# nie mamy podstaw do odrzucenia 
# H_0: beta_LastPaymentAmount = beta_MeanSalary = beta_Age = 
#      beta_NoLetRec = beta_LegalTransfer = 0
(anovaRes <- anova(lmFit, lmFit2)) 

# F (drobne r�nice wynikaj� z brania zaokr�glonuch warto�ci z tabeli)
(Fs=( anovaRes[["Sum of Sq"]][2]/anovaRes[["Df"]][2] )/
 ( anovaRes[["RSS"]][2]/anovaRes[["Res.Df"]][2] ))
pf(Fs, 6, 4985, lower.tail=FALSE)

#! zmienne dyskretne (kategoryczne)
# dwustanowe
# wyniki/pognozy nie zale�� od kodowania; kodowanie ma jedynie wp�yw 
# na interpretacj� wsp�czynnika
casesTmp[, .N, by=Product]
casesTmp[, IfCard:=ifelse(Product == "Credit card", 1, 0)]

(lmFit3 <- lm(Pay712log ~ TOA + Principal + DPD + M_LastPaymentToImportDate 
  + SignedAgreement + Reach + NoCallClient + NoPay + Product, data=casesTmp))

summary(lmFit3)

(lmFit3 <- lm(Pay712log ~ TOA + Principal + DPD + M_LastPaymentToImportDate 
  + SignedAgreement + Reach + NoCallClient + NoPay + IfCard, data=casesTmp))

summary(lmFit3)

(anovaRes <- anova(lmFit2, lmFit3)) 

# wielostanowe

# czasami zmienna ci�g�a jest nieistotna, ale ma wp�yw na zmienn� modelowan� 
# w pewnych przedzia�ach
summary(casesTmp$LastPaymentAmount)
casesTmp[, LstPayBand:=cut(LastPaymentAmount, breaks=c(0, 50, 300, 60000))]
casesTmp[, .N, by=LstPayBand]

(lmFit4 <- lm(Pay712log ~ TOA + Principal + DPD + M_LastPaymentToImportDate + 
  SignedAgreement + Reach + NoCallClient + NoPay + IfCard + LstPayBand, 
  data=casesTmp))

summary(lmFit4)

(anovaRes <- anova(lmFit3, lmFit4)) 

# kodowanie zmiennej wielostanowej
contrasts(casesTmp$LstPayBand)

#! interakcje
# do modelu mo�emy doda� jedynie interakcj� 
# X_1:X_2
# lub doda� interakcj� wraz z efektami g��nymi
# X_1*X_2 ~ X_1 + X_2 + X_1*X_2 

(lmFit5 <- lm(Pay712log ~ TOA + Principal + DPD + M_LastPaymentToImportDate + 
  SignedAgreement + Reach * NoCallClient + NoPay + IfCard + LstPayBand, 
  data=casesTmp))

summary(lmFit5)

(anovaRes <- anova(lmFit4, lmFit5)) 

#! przekszta�cenia zmiennych - regresja wielomianowa
(lmFit6 <- lm(Pay712log ~ poly(TOA, 2) + Principal + DPD + 
  M_LastPaymentToImportDate + SignedAgreement + Reach * NoCallClient + 
  NoPay + IfCard + LstPayBand, data=casesTmp))

summary(lmFit6)

(anovaRes <- anova(lmFit5, lmFit6)) 

(lmFit6 <- lm(Pay712log ~ poly(TOA, 3) + Principal + DPD + 
  M_LastPaymentToImportDate + SignedAgreement + Reach * NoCallClient + 
  NoPay + IfCard + LstPayBand, data=casesTmp))

summary(lmFit6)

(anovaRes <- anova(lmFit5, lmFit6)) 

#! ################################ Poprawno�� dopasowania - weryfikacja za�o�e�
# kr�tka odpowied� 
# http://data.library.virginia.edu/diagnostic-plots/
par(mfrow =c(2,2))     
plot(lmFit6)

# d�uga odpowied� (rozszerzona)
#! wsp�liniowo��
# https://en.wikipedia.org/wiki/Variance_inflation_factor
# https://statisticalhorizons.com/multicollinearity
#
# Wsp�liniowo�� mo�emy zbada� weryfikuj�c czy dan� zmienn� mo�na modelowa� przy 
# pomocy pozosta�ych zmiennych
#
# X_1 = alpha_0 + alpha_2 X_2 + ... + alpha_p X_p + e 
#
# Vif_1 = 1/(1 - R^2_1)
#
# Regu�a kciuka najcz�ciej podaje 10 lub 5 jako warto�� progow�, 
# albo sqrt(Vif)> 2. 

# wizualizacja zale�no�ci pomi�dzy R^2 i Vif 
r2Val <- seq(from=0.05, to=0.95, by=0.05)
vifVal <- 1/(1-r2Val)
plot(r2Val, vifVal)

# Uwaga: Vif in (1, oo)
c(1/(1 - 0.9), 1/(1 - 0.99), 1/(1 - 0.999))

# Uwaga: Czym skutkuje wsp�liniowo�� zmiennych w modelu regresji? 
# Uwaga: Czy mo�e by� tylko jeden "du�y Vif"? 

# mo�emy zauwa�y�, �e w modelu lmFit2 zmienne TOA i Principal s� skorelowane
# (wsp�liniowe)
vif(lmFit2)

(lmFit2 <- lm(Pay712log ~ TOA + DPD + M_LastPaymentToImportDate 
  + SignedAgreement + Reach + NoCallClient + NoPay, data=casesTmp))
  
vif(lmFit2)
#summary(lmFit2)
  
# podobnie mamy dla modelu  lmFit4 (zauwa�cie, �e na Vif na�o�ona jest korekta 
# ze wzgl�du na zmienn� kategoryczn� LstPayBand)
vif(lmFit4)

(lmFit4 <- lm(Pay712log ~ TOA + DPD + M_LastPaymentToImportDate + 
  SignedAgreement + Reach + NoCallClient + NoPay + IfCard + LstPayBand, 
  data=casesTmp))

vif(lmFit4)
#summary(lmFit4)

# analogicznie dla modelu lmFit6
vif(lmFit6)

(lmFit6 <- lm(Pay712log ~ poly(TOA, 3) + DPD + 
  M_LastPaymentToImportDate + SignedAgreement + Reach * NoCallClient + 
  NoPay + IfCard + LstPayBand, data=casesTmp))

vif(lmFit6)
#summary(lmFit6)

#! weryfikacja zale�no�ci liniowej pomi�dzy predyktorami a zmienn� modelowan�
# weryfikacj� zale�no�ci liniowej mo�emy przeprowadzi� u�ywaj�c wykres�w
# punkt�w (x_i, r_i) dla r�nych predyktor�w x_i lub dla uproszczenia wykresu 
# punkt�w (hatY_i, r_i)
mod <- "lmFit6"

myPlot <- function(x, y, name, ...) {
  plot(x, y, xlab=name, ylab="hatE", ...)
  loessCurve <- lowess(x, y)
  lines(loessCurve$x, loessCurve$y, col="red", lwd=2)

  invisible()
}

windows()
myPlot(fitted(get(mod)), residuals(get(mod)), "hatY", 
  main="Residuals vs. Fitted")

# Uwaga: Na powy�szym wykresie nie chcemy widzie� "wzorca"

# wykresy (max. 9) punkt�w (x_i, r_i)
mod <- "lmFit6"

lmTmp <- get(mod)
#(lmTmp <- lm(Pay712log ~ poly(TOA, 3) + DPD + 
#  M_LastPaymentToImportDate + SignedAgreement + Reach * NoCallClient + 
#  IfCard + LstPayBand, data=casesTmp))

windows()
par(mfrow=c(3, 3))

variables <- names(lmTmp$model)[-1]
nCat <- unlist(lapply(lmTmp$model[, variables], function(x) length(unique(x))))
nCat <- sort(nCat, decreasing=TRUE)

plotCounter <- 0
for (v in names(nCat)) {
  if (is.null(dim(lmTmp$model[[v]]))) {
    if (plotCounter < 9) {
      myPlot(lmTmp$model[[v]], residuals(lmTmp), v)
      plotCounter <- plotCounter + 1
    }
  } else {
    for (i in 1:dim(lmTmp$model[[v]])[2]) {
      if (plotCounter < 9) {
        myPlot(lmTmp$model[[v]][, i], residuals(lmTmp), v)
        plotCounter <- plotCounter + 1
      }
    }
  }
}

#! obserwacje odstaj�ce/wp�ywowe
# https://en.wikipedia.org/wiki/Leverage_(statistics)
# https://en.wikipedia.org/wiki/Cook%27s_distance

################################################ obserwacje odstaj�ce wzgl�dem Y

# studentyzowane residua
# t_i = r_i/( RSE^2 (1 - h_i) )
# gdzie h_i i-ty element diagonali macierzy H (patrz ni�ej)
mod <- "lmFit6"

windows()
par(mfrow=c(2, 1))
plot(fitted(get(mod)), residuals(get(mod)),
  xlab="hatY", ylab="hatE")
plot(fitted(get(mod)), rstudent(get(mod)), 
  xlab="hatY", ylab="Studentized hatE")
abline(h=3, col="tomato", lty=3)
abline(h=-3, col="tomato", lty=3)

# Uwaga: Warto�ci odstaj�ce na Y mog� nie mie� du�ego wp�ywu na dopasowanie, 
# jednak�e b�d� mie� znacz�cy wp�yw na wnioskowanie o istotno�ci parametr�w
# oraz na wielko�ci statystyk dopasowania (poprzez wp�yw na warto�� RSE)

##################################### obserwacje odstaj�ce (wp�ywowe) wzgl�dem X

# leaverage statistics dla i-tej obserwacji h_i  
# H = X inv(X'X) X', hat matrix (projection matrix)
# h_i = diag(H)_i
mod <- "lmFit6"

windows()
plot(hatvalues(get(mod)), rstudent(get(mod)),
  xlab="Leaverage", ylab="Studentized hatE")
loessCurve <- lowess(x=hatvalues(get(mod)), y=rstudent(get(mod)))
lines(loessCurve$x, loessCurve$y, lwd=2, col="tomato")
abline(v=length(get(mod)$coefficients)/n, lty=3, col="yellow", lwd=2)

# Cook's distance
# D_i = r^2_i/(p RSE^2) h_i/(1 - h_i)^2 
summary(cooks.distance(get(mod)))
# regu�a kciuka podejrzewa obserwacje
sum(cooks.distance(get(mod)) > 4/n)

#! korelacja zak��cenia losowego
# Uwaga: W przypadku skorelowanych zak��ce� standardowe odchylenia 
# wsp�czynnik�w modelu b�d� niedoszacowane, a tym samym przedzia�y ufno�ci
# b�d� zbyt w�skie

# https://en.wikipedia.org/wiki/Durbin%E2%80%93Watson_statistic
durbinWatsonTest(lmFit6)

## to tu nie zadzia�a (jedynie w szeregach czasowych)                                                                                                       
## https://en.wikipedia.org/wiki/Turning_point_test
## weryfikuje istnienie korelacji pomi�dzy s�siednimi obserwacjami
#turning.point.test(residuals(lmFit6))
#
## https://en.wikipedia.org/wiki/Sign_test
## weryfikuje czy w danych jest rosn�cy, lub malej�cy trend
#difference.sign.test(residuals(lmFit6))

#! niesta�a wariancja (homoskedastyczno�� vs. heteroskedastyczno��)
# niesta�a wariancja b�dzie si� objawia� kszta�tem "lejka" na wykresie 
# punkt�w (hatY_i, r_i)
plot(residuals(lmFit6), fitted(lmFit6), main="lmFit6")

# dost�pne s� testy weryfikuj�ce hipotez� 
# H_0 : sta�a wariancja r_i
#
# https://cran.r-project.org/web/packages/olsrr/vignettes/heteroskedasticity.html

# Breusch-Pagan (przy za�o�eniu normalno�ci e)
# weryfikuje hipotez� o sta�o�ci wariancji w przedzia�ach zmiennej modelowanej
# (wykorzystuje chi2)
ncvTest(lmFit2)
ols_test_breusch_pagan(lmFit2)
ols_test_score(lmFit2) # nie ma za�o�enia normalno�ci
ols_test_f(lmFit2) # nie ma za�o�enia normalno�ci

ncvTest(lmFit4)
ols_test_breusch_pagan(lmFit4)
ols_test_score(lmFit4) # nie ma za�o�enia normalno�ci
ols_test_f(lmFit4) # nie ma za�o�enia normalno�ci

ncvTest(lmFit6)
#ols_test_breusch_pagan(lmFit6, rhs=TRUE)
#ols_test_score(lmFit6) # nie ma za�o�enia normalno�ci
#ols_test_f(lmFit6) # nie ma za�o�enia normalno�ci

# Uwaga: W przypadku niesta�ej wariancji warto pomy�le� nad transformacj� 
# zmiennej modelowanej (najcz�ciej log(Y), sqrt(Y)).

############################################### normalno�� oszacowanych zak��ce�
# https://en.wikipedia.org/wiki/Q%E2%80%93Q_plot
# https://pl.wikipedia.org/wiki/Test_Shapiro-Wilka
qqPlot(lmFit6, distribution="norm")                 

shapiro.test(residuals(lmFit6))
shapiro.test(rstudent(lmFit6))

# Uwaga: Czy za�o�enie normalno�ci jest kluczowe? Co z asymptotyk�?
# Uwaga: Mo�na stosowa� inne testy zgodno�ci rozk�adu, np. Ko�mogorowa-Smirnowa,
# lub testy normalno�ci Lilieforce, Bartleta, itd.

#! ################################## predykcja z wykorzystaniem modelu regresji
newData <- data.table(
  TOA=seq(from=min(casesTmp$TOA), to=max(casesTmp$TOA), length.out=100),
  DPD=rep(median(casesTmp$DPD), 100),
  M_LastPaymentToImportDate=rep(50, 100),
  SignedAgreement=rep(0, 100),
  Reach=rep(median(casesTmp$Reach), 100),
  NoCallClient=rep(median(casesTmp$NoCallClient), 100),
  NoPay=rep(median(casesTmp$NoPay), 100),
  IfCard=rep(0, 100),
  LstPayBand=rep(as.factor("(50,300]"), 100)
)

# Uwaga: Prognoza uzale�niona od TOA przy ustalonych warto�ciach pozosta�ych
# predyktor�w 

prInt <- predict(lmFit6, newdata=newData, interval ="prediction")
cfInt <- predict(lmFit6, newdata=newData, interval ="confidence")

plot(seq(from=min(casesTmp$TOA), to=max(casesTmp$TOA), length.out=100),
  prInt[, 1], lwd=3, type="l", 
  xlab="TOA", ylab="hatY",
  ylim=c(min(cfInt[, c(2, 3)], prInt[, c(2, 3)]), 
         max(cfInt[, c(2, 3)], prInt[, c(2, 3)])),
  main="Confidence vs. Prediction Intervals")
lines(seq(from=min(casesTmp$TOA), to=max(casesTmp$TOA), length.out=100),
  cfInt[, 2], lty=2, col="darkgreen", lwd=3)
lines(seq(from=min(casesTmp$TOA), to=max(casesTmp$TOA), length.out=100),
  cfInt[, 3], lty=2, col="darkgreen", lwd=3)    
lines(seq(from=min(casesTmp$TOA), to=max(casesTmp$TOA), length.out=100),
  prInt[, 2], lty=3, col="darkblue", lwd=3)
lines(seq(from=min(casesTmp$TOA), to=max(casesTmp$TOA), length.out=100),
  prInt[, 3], lty=3, col="darkblue", lwd=3)    
legend("bottomright", legend=c("hatY", "Cf. Int." , "Pr. Int."),
  lty=c(1, 2, 3), lwd=rep(3, 3), col=c("black", "darkgreen", "darkblue"))

summary(lmFit6)  

# Uwaga: Poza punktami przegi�cia mo�emy obserwowa� nieintuicyjne zachowanie
# na zmiennych, kt�re do modelu zosta�y dodane z zale�no�ci� funkcyjn� 

windows()
prInt <- predict(lmFit4, newdata=newData, interval ="prediction")
cfInt <- predict(lmFit4, newdata=newData, interval ="confidence")

plot(seq(from=min(casesTmp$TOA), to=max(casesTmp$TOA), length.out=100),
  prInt[, 1], lwd=3, type="l", 
  xlab="TOA", ylab="hatY",
  ylim=c(min(cfInt[, c(2, 3)], prInt[, c(2, 3)]), 
         max(cfInt[, c(2, 3)], prInt[, c(2, 3)])),
  main="Confidence vs. Prediction Intervals")
lines(seq(from=min(casesTmp$TOA), to=max(casesTmp$TOA), length.out=100),
  cfInt[, 2], lty=2, col="darkgreen", lwd=3)
lines(seq(from=min(casesTmp$TOA), to=max(casesTmp$TOA), length.out=100),
  cfInt[, 3], lty=2, col="darkgreen", lwd=3)    
lines(seq(from=min(casesTmp$TOA), to=max(casesTmp$TOA), length.out=100),
  prInt[, 2], lty=3, col="darkblue", lwd=3)
lines(seq(from=min(casesTmp$TOA), to=max(casesTmp$TOA), length.out=100),
  prInt[, 3], lty=3, col="darkblue", lwd=3)    
legend("bottomright", legend=c("hatY", "Cf. Int." , "Pr. Int."),
  lty=c(1, 2, 3), lwd=rep(3, 3), col=c("black", "darkgreen", "darkblue"))

summary(lmFit4)  
