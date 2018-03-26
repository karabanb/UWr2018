
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

roc_plot <- function(GoodBad, Scores){
  tmp <- data.table(GoodBad = GoodBad, Score = Scores)[order(-Score)]
  tmp[,`:=`(TPR = cumsum(GoodBad)/sum(GoodBad), FPR = cumsum(!GoodBad)/sum(!GoodBad))]
  plot(tmp$FPR, tmp$TPR, type = "l", ylab = "sensitivity", xlab = "specifity")
  
}


## Zadanie 2

#  - Zdefiniuj zmienną celu `IfPayment6M` jako pojawienie się wpłaty w pierwszych 6ciu miesięcach 
# obsługi dla każdego CaseId. Poza nowo utworzoną zmienną zachowaj tylko i wyłącznie dane z tabeli cases.


events[is.na(events)] <- 0
dataset_classif <- cases[events][Month<=6, .(IfPayment6M = ifelse(sum(NumberOfPayment)>0, 1, 0)), by = CaseId][cases]

# Następnie podziel zbiór danych przy użyciu funkcji `createDataPartition` z pakietu `caret` 
# na uczący i testowy w proporcji 70% i 30%.


train_ix <- createDataPartition(dataset_classif$IfPayment6M, p= 0.7, list = FALSE)


#  Zachowaj tylko właściwe zmienne i skonwertuj do innego typu danych jeżeli jest taka potrzeba.

dataset_classif[, Land := as.factor(Land)][ , CaseId := NULL]

# - Podzielony i przygotowane zbiory danych zapisz jako `cases_train` i `cases_test`

cases_train <- dataset_classif[train_ix, ]
cases_test <- dataset_classif[-train_ix, ]


# - Sprawdź jak wygląda rozkład zmiennej celu `IfPayment6M` w zbiorze uczącym i testowym.

mean(dataset_classif[train_ix,]$IfPayment6M)
mean(dataset_classif[-train_ix,]$IfPayment6M)



## Zadanie 3

# Utwórz drzewo klasyfikacyjne do modelowania zjawiska dokonania wpłaty w  pierwszych 6 miesięcy obsługi. 
# Skorzystaj z przygotowanych danych z zadania 2. Zadanie wykonaj wykorzystując pakiet `rpart`. 

tree1 <- rpart(IfPayment6M~., data = cases_train, method = "class")



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

