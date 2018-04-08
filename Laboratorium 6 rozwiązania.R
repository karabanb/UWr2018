

library(data.table)


load("KrukUWr2018.RData")

### zadanie 1 #####

# Przeanalizuj wartości `NA` na zmiennej `LoanAmount`. Czy wszystkie wartości `NA` 
# w przypadku tej zmiennej oznaczają brak danych? 


summary(cases$LoanAmount)

# jak można zauwazyć problem dotyczy głownie kart
cases[,.(.N), by = .(Product, is.na(LoanAmount))][order(Product)] 

# na pierwszy rzut oka wartość Principal jest równa LoanAmount
head(cases[!is.na(LoanAmount) & Product == "Credit card", 1:7])

# porównajmy statystyki opisowe

summary(cases[!is.na(LoanAmount) & Product == "Credit card" , LoanAmount])
summary(cases[!is.na(LoanAmount) & Product == "Credit card" , Principal])

# tak wyglada to wizualnie

plot(density(cases[!is.na(LoanAmount) & Product == "Credit card" , LoanAmount]))
lines(density(cases[!is.na(LoanAmount) & Product == "Credit card" , Principal]), col = "red")

# po odcięciu najwyższych 10%

q90 <- quantile(cases[!is.na(Principal),Principal], probs = 0.9)

plot(density(cases[Product == "Credit card" & LoanAmount < q90, LoanAmount],na.rm = TRUE))
lines(density(cases[Product == "Credit card" & LoanAmount < q90, Principal], na.rm = TRUE), col = "red")


#### w przypadku kart kredytowych wartośc LoanAmount możemy zastapić Principal #####

cases[Product == "Credit card" & is.na(LoanAmount), LoanAmount := Principal]
anyNA(cases[Product == "Credit card", LoanAmount]) # sprawdzamy czy w tym zbiorze zostaly NA

#### Zadanie 2 ######

# Podaj biznesowy (ekspercki) sposób uzupełnienia wartości `NA` dla zmiennej `Other`.

# jak na wykładzie nr 1 .... TOA = Prinicpal + Interest + Other, zatem...
# Other = TOA - Principal - Interest

cases[is.na(Other), Other := TOA - Principal - Interest]

anyNA(cases[,Other])

#### Zadanie 3 ######



