
### zadanie 1 #####

# Przeanalizuj wartości `NA` na zmiennej `LoanAmount`. Czy wszystkie wartości `NA` 
# w przypadku tej zmiennej oznaczają brak danych? 


summary(cases$LoanAmount)

head(cases[is.na(LoanAmount),]) #
