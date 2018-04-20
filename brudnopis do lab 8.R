### Brudnopis

### wybiaramy zmienna modelowana
### sprawdzamy jej rozklad, jeze;i jest potrzeba logarytmujemy
### modelujemy
### liczymy RSS
### Liczymy RSE
### liczymy TSS
### Liczymy R^2

### Liczymy statystyke F i jej przedzal ufnosci
### Liczymy statystyke t - istitnosc kazdego wspolczynnika

### przedzialy ufnosci parametrow modelu
### sprawdzenie jak dyskretyzowana czy dyskretyzowana cecha moze miec wplyw
### sprawdzenie interakcji np. ExternalAgency:Gender

### Cooks Distance

### Sprawdzenie normalnosci rozkladu reszt itp

### predykkcja
load("KrukUWr2018.RData")
cases_loanamount <- cases[Product=="Cash loan",]

# przygotuj ramke danych 'cases_loanamount_nas' bazując na tabeli 'cases', która zawieraja
# wszytskie przypadki brakujacych danych zmiennej 'LoanAmount' dla kredytów gotówkowych 


## sprawdz ile brakow danych wystepuje w innych zmiennych i ewentualnie zaproponuj metode i zastap te braki, 

sapply(cases_loanamount, function(x){sum(is.na(x))})

# Land z rozkladu

land_distribution <- prop.table(table(cases_loanamount$Land))

sampled <- sample(unique(cases_loanamount[!is.na(Land),Land]),
                  size = sum(is.na(cases_loanamount$Land)),
                  prob = land_distribution,
                  replace = TRUE)

cases_loanamount[is.na(Land), Land:= sampled]

### ekspercko GDPPerCapita, MeanSalary, male, Other

tmp <- cases[!is.na(MeanSalary), .(MS = min(MeanSalary), GDP= min(GDPPerCapita)), by = Land]
cases_loanamount <- cases_loanamount[tmp, on = "Land"]
cases_loanamount[, ':='(MeanSalary = MS, GDPPerCapita = GDP)]
cases_loanamount[,':='(MS = NULL, GDP = NULL)]

cases_loanamount[is.na(Other), Other:=TOA-Principal-Interest]
cases_loanamount[is.na(Gender), Gender:="Company"]

cases_loanamount[, D_DPD := cut(DPD, breaks = c(0,180, 360, 720, Inf))]

cases_loanamount[is.na(D_DPD), "D_DPD"] <- "brak danych"
cases_loanamount[,DPD:=NULL]

rm("sampled", "tmp", "land_distribution")






