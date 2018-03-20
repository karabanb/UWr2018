
<<<<<<< HEAD
=======
library(data.table)
library(rpart)
library(rpart.plot)
library(ada)
library(gbm)
library(randomForest)
library(caret)

load("KrukUwr2018.RData")

## Zadanie 1


# Wykonaj samodzielnie (bez używania dedykowanych pakietów/funkcji) wykres ROC. 
# Wejściem będzie wektor prawdopodobieństw i wektor oznaczeń good/bad.

## Zadanie 2

# Zdefiniuj zmienną celu jako pojawienie się wpłaty w pierwszych 6ciu miesięcy obsługi. Następnie podziel zbiór danych 
# na uczący i testowy w proporcji 70% i 30% zachwoując taki sam rozkład zmiennej celu w zbiorze uczącycm i testowym. 
# Do wykonania zadania użyj funkcji createDataPartition z pakietu caret

events[is.na(events)] <- 0

dataset_classif <- cases[events][Month<=6, .(IfPayment6M = ifelse(sum(NumberOfPayment)>0, 1, 0)), by = CaseId][cases]




## Zadanie 3

# Utwórz drzewo klasyfikacyjne do modelowania zjawiska czy w sprawie pojawi się 
# wpłata w ciągu pierwszych 6 miesięcy obsługi. Zadanie wykonaj wykorzystując
# pakiet rpart.

## Zadanie 4

# Zmodyfikuj drzewo klasyfikacyjne z poprzedniego zadania zmieniając wartości parametrów cp, maxdepth, minsplit. 
# Co można zauważyć?

## Zadanie 5

# Wygeneruj macierz konfuzji dla wybranego przez Ciebie drzewa dla zbioru uczącego i testowego. 
# Jak kształtują się miary Accuracy, Precision i Sensitivity w obydwu macierzach?

## Zadanie 6

## Wygeneruj wykres ROC dla zbioru uczącego i testowego za pomocą napisanej przez Ciebie funkcji z zadania 1.


## Zadanie 7

# Zbuduj drzewo regresyjne w oparciu o dane aplikacyjne i behawioralne z pierwszych trzech miesięcy i 
# oszacuj skuteność skumulowaną od 4 do 12 miesiąca obsługi..

## Zadanie 8

# Zbuduj drzewa regresyjne do modelowania tego samego problemu jak w zadaniu 7. Tym razem modeluj
# dla wszystkich kombinacji wartości parametrów .....

# Do zbudowania siatki kombinacji użyj funkcji expand.grid. 

## Zadanie 9 

## Dla wygenerowanych w zadaniu 6 drzew porównaj błądd RMSE dla każdego z drzew na zbiorze treningowym i testowym. 
## Wskaż przy jakiej kombinacji parametrów uzyskujemy najlepszą jakość modelu mierzoną RMSE. 


## Zadanie 10

#  Do modelowania tego samego problemu regersyjnego użyj lasu losowego z pakietu randomForest. 
# Czy przy domyślnych parametrach wynik mierzony miarą RMSE jest lepszy od najlepszego wyniku z zdania 9?

## Zadanie 11

## 

