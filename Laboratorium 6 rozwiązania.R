

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

# Uzupełnij braki danych na zmiennej `Land` wykorzystując rozkład empiryczny. 
# Jak wykorzystać nowo pozyskane informacje w uzupełnieniu zmiennych `GDPPerCapita` i `MeanSalary`?


land_distribution <- prop.table(table(cases$Land)) #Pamietajmy, ze Land to factor!

barplot(land_distribution) #wizualizacja

# probkowanie z rozkladu Land

sampled <- sample(unique(cases[!is.na(Land),Land]),
                  size = sum(is.na(cases$Land)),
                  prob = land_distribution,
                  replace = TRUE)

cases[is.na(Land), Land:= sampled] # zastapienie brakow danych 

anyNA(cases$Land) #nie ma juz NA's

# Prosze zauwazyc, ze kazdy Land ma przypisana 1 wartosc MeanSalary i GDPPerCapita, 
# przyklad: wartosc min i max dla interesujacych nas cech jest taka sama

cases[!is.na(MeanSalary), .(minMeanSalry = min(MeanSalary, na.rm =T),
          maxMeanSalry = max(MeanSalary, na.rm = T),
          minGDPPerCapita = min(GDPPerCapita, na.rm =T),
          maxGDPPerCapita = max(GDPPerCapita, na.rm = T))
          , by = Land]

tmp <- cases[!is.na(MeanSalary), .(MS = min(MeanSalary), GDP= min(GDPPerCapita)), by = Land]

### Zadanie 5 ###
# 
# Zweryfikuj różnice pomiędzy wartościami średnich oraz przeciętnych dla rozkładów 
# poszczególnych zmiennych opisujących sprawy. Oceń jaki wpływ na różnice mają 
# wartości skrajne.

summary(cases)

# Wezmy pod lupe zmienna LoanAmount, tutaj widac ze srednia jest o wiele wieksza niz mediana

plot(density(cases$LoanAmount, na.rm=T))
boxplot(cases$LoanAmount ~ cases$Product)

cases[,.(avg = mean(LoanAmount, na.rm = TRUE),
         avg01 = mean(LoanAmount, na.rm = TRUE, trim = 0.01),
         avg05 = mean(LoanAmount, na.rm = TRUE, trim = 0.05),
         avg10 = mean(LoanAmount, na.rm = TRUE, trim = 0.1),
         median = median(LoanAmount, na.rm = TRUE))
      ]









