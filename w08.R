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

# bêdziemy modelowaæ zmienn¹
par(mfrow=c(2, 1))
plot(casesTmp$Pay712, type="l")
hist(casesTmp$Pay712, 50)
# a dok³adniej (pozostaje spory atom w 0)
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
# Co trzeba za³o¿yæ o losowym zak³óceniu:
# - nieskorelowanie
# - œrednia 0 i sta³a wariancja sigma^2
#
# Co trzeba za³o¿yæ o predyktorach X_1, ..., X_p
# - niewspó³liniowoœæ (macierz X'X ma rz¹d p)

(lmFit <- lm(Pay712log ~ TOA + Principal + DPD + M_LastPaymentToImportDate + 
  LastPaymentAmount + MeanSalary + Age + SignedAgreement + Reach + NoCallClient + 
  NoLetRec + NoPay + Pay6 + LegalTransfer, data=casesTmp))

# co kryje w sobie obiekt lmFit
names(lmFit)

lmFit$coefficients # mo¿na dobieraæ siê do poszczególnych slotów listy
coef(lmFit) # lub u¿ywaæ przygotowanych funckji

# liczba parametrów (zminnych)
p <- length(lmFit$coefficients) - 1

# czy ma znaczenia ró¿norodnoœæ skal na zmiennych modelu?
casesTmp[, TOA:=0.1*TOA]

(lmTmp <- lm(Pay712log ~ TOA + Principal + DPD + M_LastPaymentToImportDate + 
  LastPaymentAmount + MeanSalary + Age + SignedAgreement + Reach + NoCallClient + 
  NoLetRec + NoPay + Pay6 + LegalTransfer, data=casesTmp))

casesTmp[, TOA:=10*TOA]

(lmTmp)
(lmFit)

# dopasowane wartoœci 
# hatY_i = hatBeta_0 + hatBeta_1 x_1,i + ... + hatBeta_p x_p,i
head(fitted(lmFit), 10) # lmFit$fitted.values
# residua s¹ oszacowaniem zak³ócenia losowego w modelu e
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

# R^2 (jaki procent zmiennoœci wyjaœnia model regresji)
#
# R^2 = 1 - RSS/TSS
#
# Uwaga: 
# - w przypadku regresji liniowej R^2 = ( hatCor(X, Y) )^2
# - w przypadku regresji liniowej R^2 = ( hatCor(Y, hatY) )^2
(R2 <- 1 - RSS/TSS) 

# adjusted R^2 - skorygowane (wzglêdem liczby zmiennych w modelu) R^2 
#
# adjR^2 = 1 - (1 - R^2)*(n - 1)/(n - p - 1) 
#
# Uwaga: R^2 zawsze roœnie wraz z kolejn¹ dodan¹ zmienn¹ do modelu
(adjR2 <- 1 - (1 - R2)*(n - 1)/(n - p - 1))

# F z (p , n - p - 1) stopniami swobody
#
# F = ( (TSS - RSS)/p )/( RSS/(n - p - 1) )
#
# Na bazie statystyki F weryfikowana jest hipoteza (czy Y zale¿y od któregoœ X_i) 
# H_0: beta_1 = ... = beta_p = 0
(Fs <- ( (TSS - RSS)/p )/( RSS/(n - p - 1) ))
(pValue <- pf(Fs, p, n - p - 1, lower.tail=FALSE))

# wp³yw zmiennych na R^2
lmTmp <- lm(Pay712log ~ TOA + Principal + DPD + M_LastPaymentToImportDate + 
  LastPaymentAmount + MeanSalary + Age + SignedAgreement + Reach + NoCallClient + 
  NoLetRec + NoPay + Pay6 + LegalTransfer, data=casesTmp)
summary(lmTmp)

#! istotnoœæ wspó³czynników
# t test - testowanie czy wspó³czynniki s¹ równe 0 (brak zwi¹zku ze zm. celu)
# H_0: beta_i = 0  
#
# t_i = hatBeta_i/std(hatBeta_i) ~ T(n - 2)
#
# Uwaga: Test weryfikuje istotnoœæ wspó³czynnika przy za³o¿eniu, ¿e pozosta³e 
# s¹ w modelu - nie mo¿na "braæ koniunkcji" 

summary(lmFit)
# macierz kowariancji parametrów modelu (beta)
# hatSigma^2 (X'X)^(-1) = RSE^2 (X'X)^(-1)
X <- as.matrix(casesTmp[, .SD, .SDcols=c("TOA", "Principal", "DPD", "M_LastPaymentToImportDate",
  "LastPaymentAmount", "MeanSalary", "Age", "SignedAgreement", "Reach", 
  "NoCallClient", "NoLetRec", "NoPay", "Pay6", "LegalTransfer")])
X <- cbind(rep(1, 5000), X)

sqrt(diag(RSE^2 * solve(t(X) %*% X)))
sqrt(diag(vcov(lmFit)))

# przedzia³y ufnoœci dla parametrów modelu
confint(lmFit)

# przy za³o¿eniu normalnoœci e
cbind(coef(lmFit) - 1.96*sqrt(diag(vcov(lmFit))), 
      coef(lmFit) + 1.96*sqrt(diag(vcov(lmFit))))

# do którego z przedzia³ów nale¿y 0
coef(lmFit) - 1.96*sqrt(diag(vcov(lmFit))) < 0 &
coef(lmFit) + 1.96*sqrt(diag(vcov(lmFit))) > 0

############################################### przy³adowe dane (nieskorelowane)
significNo <- c()

for (i in 1:100) {
  dataX <- data.frame(matrix(rnorm(101000), nrow=1000, ncol=101))
  names(dataX) <- c("Y", paste0("X", 1:100))

  lmDataX <- lm(Y ~ ., data=dataX)
  sLmDataX <- summary(lmDataX)

  # ile wspó³czynników istotnych?
  significNo <- c(significNo, sum(sLmDataX$coefficients[, 4] < 0.05))
}

summary(significNo)
boxplot(significNo)

rm(i, significNo, dataX, lmDataX, sLmDataX)
################################################################################

# Statystyki F mo¿emy u¿yæ do testowania hipotezy (istotnoœæ q wspó³czynników)
# 
# H_0: beta_(p-q+1) = ... = beta_p = 0
#
# F = ( (RSS_0 - RSS)/q )/( RSS/(n - p - 1) ), 
# gdzie RSS_0 wyznaczone jest dla modelu z wyzerowanymi wspó³czynnikami 
summary(lmFit)

(lmFit2 <- lm(Pay712log ~ TOA + Principal + DPD + M_LastPaymentToImportDate 
  + SignedAgreement + Reach + NoCallClient + NoPay, data=casesTmp))

summary(lmFit2)

# nie mamy podstaw do odrzucenia 
# H_0: beta_LastPaymentAmount = beta_MeanSalary = beta_Age = 
#      beta_NoLetRec = beta_LegalTransfer = 0
(anovaRes <- anova(lmFit, lmFit2)) 

# F (drobne ró¿nice wynikaj¹ z brania zaokr¹glonuch wartoœci z tabeli)
(Fs=( anovaRes[["Sum of Sq"]][2]/anovaRes[["Df"]][2] )/
 ( anovaRes[["RSS"]][2]/anovaRes[["Res.Df"]][2] ))
pf(Fs, 6, 4985, lower.tail=FALSE)

#! zmienne dyskretne (kategoryczne)
# dwustanowe
# wyniki/pognozy nie zale¿¹ od kodowania; kodowanie ma jedynie wp³yw 
# na interpretacjê wspó³czynnika
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

# czasami zmienna ci¹g³a jest nieistotna, ale ma wp³yw na zmienn¹ modelowan¹ 
# w pewnych przedzia³ach
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
# do modelu mo¿emy dodaæ jedynie interakcjê 
# X_1:X_2
# lub dodaæ interakcjê wraz z efektami g³ónymi
# X_1*X_2 ~ X_1 + X_2 + X_1*X_2 

(lmFit5 <- lm(Pay712log ~ TOA + Principal + DPD + M_LastPaymentToImportDate + 
  SignedAgreement + Reach * NoCallClient + NoPay + IfCard + LstPayBand, 
  data=casesTmp))

summary(lmFit5)

(anovaRes <- anova(lmFit4, lmFit5)) 

#! przekszta³cenia zmiennych - regresja wielomianowa
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

#! ################################ Poprawnoœæ dopasowania - weryfikacja za³o¿eñ
# krótka odpowiedŸ 
# http://data.library.virginia.edu/diagnostic-plots/
par(mfrow =c(2,2))     
plot(lmFit6)

# d³uga odpowiedŸ (rozszerzona)
#! wspó³liniowoœæ
# https://en.wikipedia.org/wiki/Variance_inflation_factor
# https://statisticalhorizons.com/multicollinearity
#
# Wspó³liniowoœæ mo¿emy zbadaæ weryfikuj¹c czy dan¹ zmienn¹ mo¿na modelowaæ przy 
# pomocy pozosta³ych zmiennych
#
# X_1 = alpha_0 + alpha_2 X_2 + ... + alpha_p X_p + e 
#
# Vif_1 = 1/(1 - R^2_1)
#
# Regu³a kciuka najczêœciej podaje 10 lub 5 jako wartoœæ progow¹, 
# albo sqrt(Vif)> 2. 

# wizualizacja zale¿noœci pomiêdzy R^2 i Vif 
r2Val <- seq(from=0.05, to=0.95, by=0.05)
vifVal <- 1/(1-r2Val)
plot(r2Val, vifVal)

# Uwaga: Vif in (1, oo)
c(1/(1 - 0.9), 1/(1 - 0.99), 1/(1 - 0.999))

# Uwaga: Czym skutkuje wspó³liniowoœæ zmiennych w modelu regresji? 
# Uwaga: Czy mo¿e byæ tylko jeden "du¿y Vif"? 

# mo¿emy zauwa¿yæ, ¿e w modelu lmFit2 zmienne TOA i Principal s¹ skorelowane
# (wspó³liniowe)
vif(lmFit2)

(lmFit2 <- lm(Pay712log ~ TOA + DPD + M_LastPaymentToImportDate 
  + SignedAgreement + Reach + NoCallClient + NoPay, data=casesTmp))
  
vif(lmFit2)
#summary(lmFit2)
  
# podobnie mamy dla modelu  lmFit4 (zauwa¿cie, ¿e na Vif na³o¿ona jest korekta 
# ze wzglêdu na zmienn¹ kategoryczn¹ LstPayBand)
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

#! weryfikacja zale¿noœci liniowej pomiêdzy predyktorami a zmienn¹ modelowan¹
# weryfikacjê zale¿noœci liniowej mo¿emy przeprowadziæ u¿ywaj¹c wykresów
# punktów (x_i, r_i) dla ró¿nych predyktorów x_i lub dla uproszczenia wykresu 
# punktów (hatY_i, r_i)
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

# Uwaga: Na powy¿szym wykresie nie chcemy widzieæ "wzorca"

# wykresy (max. 9) punktów (x_i, r_i)
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

#! obserwacje odstaj¹ce/wp³ywowe
# https://en.wikipedia.org/wiki/Leverage_(statistics)
# https://en.wikipedia.org/wiki/Cook%27s_distance

################################################ obserwacje odstaj¹ce wzglêdem Y

# studentyzowane residua
# t_i = r_i/( RSE^2 (1 - h_i) )
# gdzie h_i i-ty element diagonali macierzy H (patrz ni¿ej)
mod <- "lmFit6"

windows()
par(mfrow=c(2, 1))
plot(fitted(get(mod)), residuals(get(mod)),
  xlab="hatY", ylab="hatE")
plot(fitted(get(mod)), rstudent(get(mod)), 
  xlab="hatY", ylab="Studentized hatE")
abline(h=3, col="tomato", lty=3)
abline(h=-3, col="tomato", lty=3)

# Uwaga: Wartoœci odstaj¹ce na Y mog¹ nie mieæ du¿ego wp³ywu na dopasowanie, 
# jednak¿e bêd¹ mieæ znacz¹cy wp³yw na wnioskowanie o istotnoœci parametrów
# oraz na wielkoœci statystyk dopasowania (poprzez wp³yw na wartoœæ RSE)

##################################### obserwacje odstaj¹ce (wp³ywowe) wzglêdem X

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
# regu³a kciuka podejrzewa obserwacje
sum(cooks.distance(get(mod)) > 4/n)

#! korelacja zak³ócenia losowego
# Uwaga: W przypadku skorelowanych zak³óceñ standardowe odchylenia 
# wspó³czynników modelu bêd¹ niedoszacowane, a tym samym przedzia³y ufnoœci
# bêd¹ zbyt w¹skie

# https://en.wikipedia.org/wiki/Durbin%E2%80%93Watson_statistic
durbinWatsonTest(lmFit6)

## to tu nie zadzia³a (jedynie w szeregach czasowych)                                                                                                       
## https://en.wikipedia.org/wiki/Turning_point_test
## weryfikuje istnienie korelacji pomiêdzy s¹siednimi obserwacjami
#turning.point.test(residuals(lmFit6))
#
## https://en.wikipedia.org/wiki/Sign_test
## weryfikuje czy w danych jest rosn¹cy, lub malej¹cy trend
#difference.sign.test(residuals(lmFit6))

#! niesta³a wariancja (homoskedastycznoœæ vs. heteroskedastycznoœæ)
# niesta³a wariancja bêdzie siê objawiaæ kszta³tem "lejka" na wykresie 
# punktów (hatY_i, r_i)
plot(residuals(lmFit6), fitted(lmFit6), main="lmFit6")

# dostêpne s¹ testy weryfikuj¹ce hipotezê 
# H_0 : sta³a wariancja r_i
#
# https://cran.r-project.org/web/packages/olsrr/vignettes/heteroskedasticity.html

# Breusch-Pagan (przy za³o¿eniu normalnoœci e)
# weryfikuje hipotezê o sta³oœci wariancji w przedzia³ach zmiennej modelowanej
# (wykorzystuje chi2)
ncvTest(lmFit2)
ols_test_breusch_pagan(lmFit2)
ols_test_score(lmFit2) # nie ma za³o¿enia normalnoœci
ols_test_f(lmFit2) # nie ma za³o¿enia normalnoœci

ncvTest(lmFit4)
ols_test_breusch_pagan(lmFit4)
ols_test_score(lmFit4) # nie ma za³o¿enia normalnoœci
ols_test_f(lmFit4) # nie ma za³o¿enia normalnoœci

ncvTest(lmFit6)
#ols_test_breusch_pagan(lmFit6, rhs=TRUE)
#ols_test_score(lmFit6) # nie ma za³o¿enia normalnoœci
#ols_test_f(lmFit6) # nie ma za³o¿enia normalnoœci

# Uwaga: W przypadku niesta³ej wariancji warto pomyœleæ nad transformacj¹ 
# zmiennej modelowanej (najczêœciej log(Y), sqrt(Y)).

############################################### normalnoœæ oszacowanych zak³óceñ
# https://en.wikipedia.org/wiki/Q%E2%80%93Q_plot
# https://pl.wikipedia.org/wiki/Test_Shapiro-Wilka
qqPlot(lmFit6, distribution="norm")                 

shapiro.test(residuals(lmFit6))
shapiro.test(rstudent(lmFit6))

# Uwaga: Czy za³o¿enie normalnoœci jest kluczowe? Co z asymptotyk¹?
# Uwaga: Mo¿na stosowaæ inne testy zgodnoœci rozk³adu, np. Ko³mogorowa-Smirnowa,
# lub testy normalnoœci Lilieforce, Bartleta, itd.

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

# Uwaga: Prognoza uzale¿niona od TOA przy ustalonych wartoœciach pozosta³ych
# predyktorów 

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

# Uwaga: Poza punktami przegiêcia mo¿emy obserwowaæ nieintuicyjne zachowanie
# na zmiennych, które do modelu zosta³y dodane z zale¿noœci¹ funkcyjn¹ 

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
