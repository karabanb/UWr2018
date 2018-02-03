Uwaga: W zadaniach można wybierać losowe podzbiory spraw (tak jak na wykładzie),
by czas obliczeń nie przekraczał czasu zajęć. Warto jednak zweryfikować czas 
obliczeń, by mieć świadomość ile czasu będą wykonywać się obliczenia do 
wykonania projektu.

## ! lab 1 ######
### Zadanie 1 ###
Stwórz strukturę danych x = data.table(U=runif(10)). Następnie napisz funkcję 
goodBadProp, która przyjmuje jako argumenty strukturę tab (data.table) oraz 
liczbę p z przedziału (0, 1). W ciele funkcji wykonaj operację
- dodaj do struktury tab kolumnę GoodBad, która przyjmuje wartości 1, gdy U > p, 
  a 0 w przeciwnym przypadku,
- jako wynik funkcji zwróć strukturę data.table z wyliczonymi częstościami
  goodów i badów.
Jak wygląda struktura danyh x po uruchomieniu funkcji? Jeśli inaczej niż 
pierwotnie, to czy można tego uniknąć?
Sparametryzuj funkcję, by jej kod nie zależał od wprost od U (nazwy kolumny 
w x). 

### rozwiazanie ###
library(data.table)

x <- data.table(U = runif(10))

goodBadProp <- function(tab, p){

  stopifnot(is.double(p), p >= 1, p <= 0)
  
  tab$GoodBad <- ifelse(tab$U > p, 1, 0)
  return(tab[, .N, by = GoodBad])
  
}


## Zadanie 2 ###
http://datatable.r-forge.r-project.org/datatable-intro.pdf
W materiale z powyższego linku można znaleźć przykłady wykorzystania data.table
wraz z czasami wykonania pewnych poleceń. Wykonajcie te przykłady zwracając 
uwagę na możliwośći data.table (Uwaga: W zależności od komputera mogą wystąpić
trudności z pomiarem czasu wykonania poprzez system.time. Polecam pakiet 
microbenchmark). 
                                                           
# Zadanie 3
Stwórz tabelę rndNumbers data.table o 100k wierszach i kolumnach 
- U z wartościami rozkładu jednostajnego na (0, 1)
- Z z wartościami rozkładu normalnego 
- E z wartościami rozkaldu eksponencjalnego
- G z wartościami 0, 1 
- P z wartościami z rozkładu Poissona o średniej 2
Wyznacz (korzystając z funckjonalności data.table) statystyki opisowe 
poszczególnych kolumn. 
Wyznacz (korzystając z funckjonalności data.table) statystyki opisowe 
kolumn U, Z, E w rozbiciu względem kolumny G. 
Wyznacz (korzystając z funckjonalności data.table) statystyki opisowe 
kolumn U, Z, E w rozbiciu względem czy P jest większe od swojej śedniej. Czy
można to zrobić bez dodawania nowych kolumn, wyliczania "na boku" średniej P?

# Zadanie 4
Do tabeli rndNumbers z zadania 3 dodaj kolumnę Id przypisując do niej ".I".
Stwórz tabelę rndNumbers2 jako podzbiór tabeli rndNumbers dla Id <= 10.
Stwórz tabelę tab (10 wierszy) z kolumnami
- Id o wartościach ze zbioru 1, 2, 3 przypisanymi losowo
- Id2 o wartościach ze zbioru 6, 7, 8 przypisanymi losowo
Nałóż klucz Id na tabelę rndNumbers2. Nałóż klucz Id na tabelę tab.
Wykonaj joiny tabel tab i rndNumbers w różnej kolejności (np. tab[rndNumbers2], 
lub rndNumbers2[tab]). Co otrzymujesz w wyniku (po jakiej kolumnie wykonywany
jest join; wymuś joina po drugiej kolumnie)? Do czego służą parametry 
nomatch, allow.cartesian?

# Zadanie 5
get, quote, eval


# Zadanie 6
Stwórzcie data.table x o 1mln wierszy i dwóch kolumnach z liczbami z rozkładu 
jendostajnego i normalnego. Zapiszcie tabelę x do pliku csv. Zweryfikujcie czas
zapisu przy użyciu funkcji standardowych funkcji zapisu do csv np. write.table,
write.csv oraz fwrite (data.table). 
Wczytajcie utworzony plik uzywając funckji read.table oraz fread porównując czas 
wczytywania danych.
Zwróćcie uwagę na ilość zajmowanego miejsca przez zmienną w R oraz wielkość 
pliku csv.

#! lab 2
# Zadanie 1
Wyznacz skuteczność (suma wpłat przez wartość zadłużenia) w różnych horyzontach
czasu (np. 3M, 6M, 12M) i wyznacz jej statystyki opisowe (kwantyle) 
w podziale na:  
- Gender
- ExtrernalAgency
- Bailiff
- ClosedExecution
- M_LastPaymentToImportDate (zaproponuj podział wg tej zmiennej)
- DPD (zaproponuj podział wg tej zmiennej)
- Age (zaproponuj podział wg tej zmiennej)
- TOA (zaproponuj podział wg tej zmiennej)
Wyniki zaprezentuj również na wykresie. Które zmienne najlepiej różnicują 
skuteczność (co rozumiesz poprzez "różnicują")?
Wyznacz korelacje pomiędzy zmienną skuteczności a wskazanymi zmiennymi. Czy
w zbiorze danych istnieją inne zmienne (od wskazanych) z dużą wartością 
korelacji.

# Zadanie 2
Wyznacz dotarcie per sprawa (czy był kontakt w sprawie telefoniczny, 
lub bezpośredni/wizyta) w różnych horyzontach czasu i wyznacz jej statystyki
opisowe (kwantyle) w podziale na wybrane zmienne (np. zmienne z zadania 1, 
lub zmienne, które róznicują dotarcie).

# Zadanie 3
Czy istnieje zależność pomiędzy ilością wykonywanych telefonów, wizyt, lub 
wysyłanych listów, a zmiennymi opisującymi sprawę (zmienne w cases).

# Zadanie 4
Dla wybranych zmiennych stwórz zmienne: 
- standaryzowane (o średniej zero i warinacji 1) 
- normalizowane (przekształcenie wartości zmienn,ej na odcinek [0, 1]). 
- logarytmowane
- pierwiatskowane 
Wyznacz korelację dla zmiennych orginalnych oraz korelację ich przekształconych 
odpowiedników. Co można zauważyć?

# Zadanie 5
Dla zadanej zmiennej stwórz dystrybuantę empiryczną (funkcja ecdf). Następnie 
stwórz zmienną przekształcając ją poprzez jej dystrybuantę empiryczną. Jakie
charakterystyki ma taka zmienna.

# Zadanie 6
Wyznacz wykres warstwowy pokazujący udział skuteczności w kolejnych miesiącach 
obsługi w podziale na:
1. SR w sprawach bez kontaktu (zarówno telefoniczny jak i wizyta)
2. SR w sprawach z kontaktem
3. SR w sprawach z ugodą
4. SR w sprawach przekazanych do sądu.
Powyższe zdarzenia narzucają hierarchię procesu, tzn. jeśli sprawa był kontakt 
w 3M to sprawa ta jest uważana za sprawę z kontaktem do 12M (do końca).
Jeśli w sprawie był kontakt w 2M oraz w tej sprawie została podpisana ugoda 
w 2M, to zaliczamy tą sprawę (jej skuteczność) do kategorii spraw z ugodą w 2M.

# Zadanie 7
Wyznacz istotność korelacji pomiędzy parami zmiennych.

#! lab 3
# Zadanie 1. 
Wykonaj twardą klasteryzację (k -średnich, hierarchiczny klastering) danych 
spraw pożyczkowych z zadłużeniem do 65k. wykorzystując zmienne przed zakupem
(zmienne ze zbioru cases). Dla uproszczenia brakujące dane uzupełnij wartością
średnią. Zaproponuj różne miary reprezentujące: 
- podobieństwa spraw w klastrze; 
- niepodobieństwa klastrów ("odległość" między klastrami).
Do jakich wyborów liczby klastrów k prowadzą te miary?
Czy jest możliwość wyboru k jako kompromis tych dwóch podejść?

# Zadanie 2 
Stwórz model twardej klasteryzacji spraw pożyczkowych z zadłużeniem do 65k 
prognozujący "czy sprawa z kontaktem w 3M" na podstawie zmiennych przed zakupem 
(zmienne ze zbioru cases). Które zmienne dają w wyniku model o najlepszych
właściwościach predykcyjnych? Ile zmiennych należy użyć do klasteryzacji?

# Zadanie 2 
Stwórz model miękkiej klasteryzacji spraw pożyczkowych z zadłużeniem do 65k 
prognozujący "czy sprawa z wpłatą w 3M" na podstawie zmiennych przed zakupem 
(zmienne ze zbioru cases). Które zmienne dają w wyniku model o najlepszych
właściwościach predykcyjnych? Ile zmiennych należy użyć do klasteryzacji?

# Zadanie 4
Stwórz model twardej klasteryzacji spraw kart kredytowych z zadłużeniem do 65k 
prognozujący wartość średniej pensji na podstawie zmiennych przed zakupem 
(zmienne ze zbioru cases). Które zmienne dają w wyniku model o najlepszych
właściwościach predykcyjnych? Ile zmiennych należy użyć do klasteryzacji?

# Zadanie 5
Stwórz model miękkiej klasteryzacji spraw kart kreydtowych z zadłużeniem do 65k 
prognozujący wartość średniej pensji na podstawie zmiennych przed zakupem  
(zmienne ze zbioru cases).  Które zmienne dają w wyniku model o najlepszych
właściwościach predykcyjnych? Czy pomocne są transformacje zmiennych (którym
"daleko" do normalności) takie jak logarytm, lub pierwiastek?  

# Zadanie 6
Napisz funkcję/procedurę wyboru k najbliższych sąsiadów (Wsk. Funkcja dist(x)
zwraca odległość pomiędzy punktami w zbiorze x).
Wykorzystaj tą funkcję w problemie regresyjnym prognozowania wartości średniej
pensji, gdzie wartość prognozy jest równa średniej wartości po sąsiadach.

#! lab 4
# Zadanie 1 
Wyznacz błędy MSE metodą cross-validation i k-fold cross validation 
dla przykładu z wykładu (przykład z danymi generowanymi). Porównaj te wartości 
z testowym błędem MSE. Powtórz obliczenia dla różnych funkcji "y1" (mniej, 
lub bardziej "pofalowanych").

# Zadanie 2 
Mając do dyspozycji zbiór spraw kart kredytowych o zadłużeniu do 1000 wyznacz 
oszacowanie rozkładu średniego zadłużenia oraz DPD pakietów takiego typu.  

# Zadanie 3
Mając próbę n elementową oszacuj ile różnych obserwacji z tej próby zawierają 
przecietnie poszczególne replikacje bootstrapowe. 

# Zadanie 4
MSE testowe i uczące dla knn   ??? co modelować

# Zadanie 5
Załóżmy, że sprawa rentowna to taka dla której suma wpłat wynosi więcej niż 200. 
Dla każdej sprawy wyznacz miesiąc, w którym przekracza ona próg rentowności. 
Jak wygląda rozkład wyznaczonych miesięcy? Jak wygląda rozkład skuteczności
spraw w momencie osiągnięcia progu rentowności? 

# Zadanie 6
Załóżmy, że sprawa rentowna to taka dla której skuteczność przekroczy 0.5% 
zadłużenia początkowego. Dla każdej sprawy wyznacz miesiąc, w którym przekracza 
ona próg skuteczności. Jak wygląda rozkład wyznaczonych miesięcy? Ile pieniędzy 
zostało wpłaconych na poszczególnych sprawach - zrób zestawienie w podziale
na różne bandy zadłużenia.

#! lab 5
# Zadanie 1
Wykonaj samodzielnie (bez używania dedykowanych pakietów/funkcji) wykres ROC. 
Wejściem będzie wektor parwdopodobieństw i wektor oznaczeń good/bad.

# Zadanie 2 
Uzyj drzewa klasyfikacyjnego do modelowania zjawiska czy w sprawie pojawi się 
wpłata w ciągu pierwszych 6 miesięcy obsługi. Zadanie wykonaj wykorzystując
pakiety tree i rpart.

# Zadanie 3 
Zbuduj drzewo regresyjne do modelowania skuteczności na sprawie od 4 do 12 
miesiąca obsługi (Wsk. Możesz wykorzystać zmienne behawioralne z pierwszych
trzech miesięcy obsługi). 

# Zadanie 4
Zbuduj dwa drzewa na dwóch połowach zbioru uczącego, a następnie wykonaj 
weryfikację obu drzew na tym samym zbiorze testowym. Czy zbudowane drzewa są 
"jednakowe"? Czy dają taką samą prognozę?

# Zadanie 5
zadanie z boostingu klasyfikacyjne

# Zadanie 6
zadanie z boostingu regresyjne???


#! lab 6
# Zadanie 1
Przeanalizuj wartości NA na zmiennej LoanAmount. Czy wszystkie wartości NA 
w przypadku tej zmiennej oznaczają brak danych? 

# Zadanie 2
Podaj biznesowy (ekspercki) sposób uzupełnienia wartości NA dla zmiennej Other.

# Zadanie 3
Uzupełnij braki danych na zmiennej Land wykorzystując rozkład empiryczny.
Jak wykorzystać nowo pozyskane informacje w uzupełnieniu zmiennych GDPPerCapita 
i MeanSalary? 

# Zadanie 4
Zweryfikuj dokładność uzupełniania braków danych dla zmiennej TOA poprzez modele
lasów losowych i najblizszych sąsiadów (Wsk. Braki danych w TOA należy
zasymulować).

# Zadnaie 5
Zweryfikuj różnice pomiędzy wartościami średnich oraz przeciętnych dla rozkładów 
poszczególnych zmiennych opisujących sprawy. Oceń jaki wpływ na różnice mają 
wartości skrajne.

# Zadanie 6
Posługując się wykresami typu boxplot zidentyfikuj wartości odstające (jaka 
reguła jest przyjęta w funkcji boxplot) na poszczgólnych zmiennych opisujących 
sprawy. Usuń przypadki z wartościami odstającymi, a następnie wykonaj wykres
ponownie. Czy nadal możesz zaobserwować wartości odstające?

# Zadanie 7
http://www.itl.nist.gov/div898/handbook/eda/section3/eda35h2.htm
Napisz funkcję wyznaczającą statystykę Tietjen-Moore (szczegóły w linku). Napisz
funkcję do wyznaczania wartości krytycznej testu Tietjen-Moore (Wsk. Empiryczną
wartość krytyczną testu szacujemy poprzez wyznaczenie odpowiedniego, zależnie
od poziomu istotności, kwantyla rozkładu statystyki testowej wyznaczonej przy 
prawdziwości hipotezy zerowej). 

# Zadnaie 8
# Techniczny
http://www.rdatamining.com/examples/outlier-detection
# Merytoryczny
http://www.dbs.ifi.lmu.de/Publikationen/Papers/LOF.pdf
Powtarzając kroki z przykładu (link techniczny) zidentyfikuj wartości odstające 
na zbiorze spraw kart kredytowych używając pakietu DMwR. Wyznacz zbiór wartości 
odstających uzywając pakietu mvoutlier, a następnie porównaj oba zbiory. Co 
możesz powiedzieć o tych dwóch zbiorach?

# Zadanie 9
Coś na metrykę Mahalonobisa   # transformacja danych na normalne???

# Zadanie 10 
Wyznacz wartości współczynnika stabilności populacji porównując zbiory pozyczek
oraz kart kredytowych. Jak podzielić ("pobandować na klasy") zmienne ciągłe?

#! lab 7



