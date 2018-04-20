### Brudnopis

### wybiaramy zmienna modelowana
### sprawdzamy jej rozklad, jeze;i jest potrzeba logarytmujemy
### modelujemy
### liczymy RSS
### Liczymy RSE
### liczymy TSS
### Liczymy R^2

<<<<<<< HEAD
### Liczymy statystyke F i jej przedzal ufnosci
### Liczymy statystyke t - istitnosc kazdego wspolczynnika

### przedzialy ufnosci parametrow modelu
### sprawdzenie jak dyskretyzowana czy dyskretyzowana cecha moze miec wplyw
### sprawdzenie interakcji np. ExternalAgency:Gender

### Cooks Distance

### Sprawdzenie normalnosci rozkladu reszt itp

### predykkcja
=======
>>>>>>> 13a5428711dd929b0f9c4f29c2c62b8b8e421797
cases_loanamount <- cases[Product=="Cash loan",]

# przygotuj ramke danych 'cases_loanamount_nas' bazując na tabeli 'cases', która zawieraja
# wszytskie przypadki brakujacych danych zmiennej 'LoanAmount' dla kredytów gotówkowych 


## sprawdz ile brakow danych wystepuje w innych zmiennych i ewentualnie zaproponuj metode i zastap te braki, 

sapply(cases_loanamount, function(x){sum(is.na(x))})

# Lan z rozkladu

land_distribution <- prop.table(table(cases_loanamount$Land))

sampled <- sample(unique(cases_loanamount[!is.na(Land),Land]),
                  size = sum(is.na(cases_loanamount$Land)),
                  prob = land_distribution,
                  replace = TRUE)




cases_loanamount_nas[is.na(Other), Other:=TOA-Principal-Interest]
cases_loanamount_nas[is.na(Gender), Gender:="MALE"]

