
library(data.table)
library(car)
library(MASS)
load("KrukUWr2018.Rdata")


### Zadanie 1

#  Przygotuj rankę danych `cases_loanamount` bazując na tabeli `cases` tylko z przypadkami kredytów gotówkowych. 

cases_loanamount <- cases[Product == "Cash loan",]

# * Sprawdź miary pozycyjne wszystkich cech w utworzonej ramce.

summary(cases_loanamount)

# * Sprawdź liczność braków danych dla wszystkich cech.

sapply(cases_loanamount, function(x){sum(is.na(x))})

### Zadanie 2 

# Interesującą nas cechą będzie `LoanAmount`, której wartość będziemy modelować w celu zastpąpienia `NA's`.

# * Metodą losowania z rozkładu zastąp braki danych w cesze `Land`.

 dist <- prop.table(table(cases_loanamount$Land))
 sampled <- sample(x = unique(cases_loanamount[!is.na(Land), Land]),
                   size = sum(is.na(cases_loanamount[,Land])),
                   prob = dist,
                   replace = TRUE)
 
 cases_loanamount[is.na(Land) , Land := sampled]
 
# * Metodą ekspercką zastąp braki danych w cechach: `Other`,`Gender`, `MeanSalary` i `GDPPerCapita`.
 
 tmp <- cases[!is.na(MeanSalary), .(MS = min(MeanSalary), GDP= min(GDPPerCapita)), by = Land]
 cases_loanamount <- cases_loanamount[tmp, on = "Land"]
 cases_loanamount[, ':='(MeanSalary = MS, GDPPerCapita = GDP)]
 cases_loanamount[,':='(MS = NULL, GDP = NULL)]
 
 cases_loanamount[is.na(Other), Other:=TOA-Principal-Interest]
 cases_loanamount[is.na(Gender), Gender:="Company"]
 
# * Dokonaj dyskretyzacji cechy DPD a wartościom `NA` przypisz poziom `brak danych`, cechę zapisz jako `D_DPD`

 cases_loanamount[, D_DPD := cut(DPD, breaks = c(0,180, 360, 720, Inf))]
 
 cases_loanamount[is.na(D_DPD), "D_DPD"] <- "brak danych"
 cases_loanamount[,DPD:=NULL]
 
 ### Zadanie 3 
 
 # Na podstawie `cases_loanamount` przygotuj ramki danych:

 #   * `cases_loanamount_nas`, która zawiera wszystkie przypadki brakujacych wartości zmiennej `LoanAmount`.
 
 cases_loanamount_nas <- cases_loanamount[is.na(LoanAmount),]
 cases_loanamount_wonas <- cases_loanamount[!is.na(LoanAmount),]
 
 ix_trn <- sample(1:nrow(cases_loanamount_wonas), nrow(cases_loanamount_wonas)*0.7)
 ix_tst <- c(1:nrow(cases_loanamount_wonas))[-ix_trn]
 
 
 
 
 
 
 
 