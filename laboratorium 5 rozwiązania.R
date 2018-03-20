
<<<<<<< HEAD
=======

library(rpart)
library(rpart.plot)
library(ada)
library(gbm)
library(randomForest)
library(caret)

## Zadanie 1


# Wykonaj samodzielnie (bez używania dedykowanych pakietów/funkcji) wykres ROC. 
# Wejściem będzie wektor prawdopodobieństw i wektor oznaczeń good/bad.

## Zadanie 2

# Zdefiniuj zmienną celu jako pojawienie się wpłaty w pierwszych 6ciu miesięcy obsługi. Następnie podziel zbiór danych 
# na uczący i testowy w proporcji 70% i 30% zachwoując taki sam rozkład zmiennej celu w zbiorze uczącycm i testowym. 
# Do wykonania zadania użyj funkcji createDataPartition z pakietu caret

## Zadanie 3

# Utwórz drzewo klasyfikacyjne do modelowania zjawiska czy w sprawie pojawi się 
# wpłata w ciągu pierwszych 6 miesięcy obsługi. Zadanie wykonaj wykorzystując
# pakiet rpart.

## Zadanie 4

# Utwórz model lasu losowego do modelowania zjawiska okre

## Zadanie 4

# Zbuduj drzewo regresyjne do modelowania skumulowanej skuteczności na sprawie w 12 miesiącu obsługi.
# Drzewo modeluj wyłącznie w oparciu o dane aplikacyjne.

## Zadanie 5

# Zbuduj drzewo regresyjne w oparciu o dane aplikacyjne i behawioralne z pierwszych trzech miesięcy i 
# oszacuj skuteność skumulowaną od 4 do 12 miesiąca obsługi. Porównaj błąd szacunku tego drzewa
# z błędem z poprzedniego zadania.

## Zadanie 6

# Zbuduj drzewa regresyjne do modelowania tego samego problemu jak w zadaniu 5. Tym razem modeluj
# dla wszystkich kombinacji wartości parametrów .....

# Do zbudowania siatki kombinacji użyj funkcji expand.grid. 

## Zadanie 7 

## Dla wygenerowanych w zadaniu 6 drzew porównaj błądd RMSE dla każdego z drzew na zbiorze treningowym i testowym. 
## Wskaż przy jakiej kombinacji parametrów uzyskujemy najlepszą jakość modelu mierzoną RMSE. 


## Zadanie 8

#  U?yj lasu losowego do modelowania zjawiska pojawienia si? wp?aty w ci?gu 6 miesi?cy obs?ugi. Wyniki
# p?r?wnaj z usyzskanymi w zadaniu 3.

>>>>>>> ef8ddf521ab1a3c698964d1d105f0abcef52faac
