### Brudnopis

### wybiaramy zmienna modelowana
### sprawdzamy jej rozklad, jeze;i jest potrzeba logarytmujemy
### modelujemy
### liczymy RSS
### Liczymy RSE
### liczymy TSS
### Liczymy R^2

cases_loanamount_nas <- cases[is.na(LoanAmount)&Product=="Cash loan",]

# przygotuj ramke danych 'cases_loanamount_nas' bazując na tabeli 'cases', która zawieraja
# wszytskie przypadki brakujacych danych zmiennej 'LoanAmount' dla kredytów gotówkowych 


## sprawdz ile brakow danych wystepuje w innych zmiennych i ewentualnie zaproponuj metode i zastap te braki, 

sapply(cases_loanamount_nas, function(x){sum(is.na(x))})


cases_loanamount_nas[is.na(Other), Other:=TOA-Principal-Interest]
cases_loanamount_nas[is.na(Gender), Gender:="MALE"]

