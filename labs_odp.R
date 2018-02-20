Uwaga: W zadaniach mo¿na wybieraæ losowe podzbiory spraw (tak jak na wyk³adzie),
by czas obliczeñ nie przekracza³ czasu zajêæ. Warto jednak zweryfikowaæ czas 
obliczeñ, by mieæ œwiadomoœæ ile czasu bêd¹ wykonywaæ siê obliczenia do 
wykonania projektu.

## ! lab 1 ######
### Zadanie 1 ###
Stwórz strukturê danych x = data.table(U=runif(10)). Nastêpnie napisz funkcjê 
goodBadProp, która przyjmuje jako argumenty strukturê tab (data.table) oraz 
liczbê p z przedzia³u (0, 1). W ciele funkcji wykonaj operacjê
- dodaj do struktury tab kolumnê GoodBad, która przyjmuje wartoœci 1, gdy U > p, 
  a 0 w przeciwnym przypadku,
- jako wynik funkcji zwróæ strukturê data.table z wyliczonymi czêstoœciami
  goodów i badów.
Jak wygl¹da struktura danyh x po uruchomieniu funkcji? Jeœli inaczej ni¿ 
pierwotnie, to czy mo¿na tego unikn¹æ?
Sparametryzuj funkcjê, by jej kod nie zale¿a³ od wprost od U (nazwy kolumny 
w x). 

### rozwiazanie ###
library(data.table)

x <- data.table(U = runif(10))

goodBadProp <- function(tab, p){

  stopifnot(is.double(p), p < 1, p > 0)
  
  tab$GoodBad <- ifelse(tab[, 1] > p, 1, 0)
  #return(tab[, mean(.GoodBad), by = GoodBad])
  prop.table(table(tab$GoodBad))
  
  
}

z <- goodBadProp(x, 0.1)
z
class(z)



## Zadanie 2 ###
http://datatable.r-forge.r-project.org/datatable-intro.pdf
W materiale z powy¿szego linku mo¿na znaleŸæ przyk³ady wykorzystania data.table
wraz z czasami wykonania pewnych poleceñ. Wykonajcie te przyk³ady zwracaj¹c 
uwagê na mo¿liwoœæi data.table (Uwaga: W zale¿noœci od komputera mog¹ wyst¹piæ
trudnoœci z pomiarem czasu wykonania poprzez system.time. Polecam pakiet 
microbenchmark). 
                                                           
# Zadanie 3
Stwórz tabelê rndNumbers data.table o 100k wierszach i kolumnach 
- U z wartoœciami rozk³adu jednostajnego na (0, 1)
- Z z wartoœciami rozk³adu normalnego 
- E z wartoœciami rozkaldu eksponencjalnego
- G z wartoœciami 0, 1 
- P z wartoœciami z rozk³adu Poissona o œredniej 2
Wyznacz (korzystaj¹c z funckjonalnoœci data.table) statystyki opisowe 
poszczególnych kolumn. 
Wyznacz (korzystaj¹c z funckjonalnoœci data.table) statystyki opisowe 
kolumn U, Z, E w rozbiciu wzglêdem kolumny G. 
Wyznacz (korzystaj¹c z funckjonalnoœci data.table) statystyki opisowe 
kolumn U, Z, E w rozbiciu wzglêdem czy P jest wiêksze od swojej œedniej. Czy
mo¿na to zrobiæ bez dodawania nowych kolumn, wyliczania "na boku" œredniej P?

### rozwiazanie ###

n <- 100000

rndNumbers <- data.table(U = runif(n),
                         Z = rnorm(n),
                         E = rexp(n),
                         G = rbinom(n, 1, 0.6),
                         P = rpois(n, 2)
                         )

summary(rndNumbers)




# Zadanie 4
Do tabeli rndNumbers z zadania 3 dodaj kolumnê Id przypisuj¹c do niej ".I".
Stwórz tabelê rndNumbers2 jako podzbiór tabeli rndNumbers dla Id <= 10.
Stwórz tabelê tab (10 wierszy) z kolumnami
- Id o wartoœciach ze zbioru 1, 2, 3 przypisanymi losowo
- Id2 o wartoœciach ze zbioru 6, 7, 8 przypisanymi losowo
Na³ó¿ klucz Id na tabelê rndNumbers2. Na³ó¿ klucz Id na tabelê tab.
Wykonaj joiny tabel tab i rndNumbers2 w ró¿nej kolejnoœci (np. tab[rndNumbers2], 
lub rndNumbers2[tab]). Co otrzymujesz w wyniku (po jakiej kolumnie wykonywany
jest join; wymuœ joina po drugiej kolumnie)? Do czego s³u¿¹ parametry 
nomatch, allow.cartesian?

  
rndNumbers[, Id:= .I]

rndNumbers2 <- rndNumbers[Id <= 10]

tab <- data.table(Id = sample(c(1:3), 10, replace = TRUE),
                  Id2 = sample(c(6:8), 10, replace = TRUE))

setDT(rndNumbers2, key = c("Id"))
setDT(tab, key = c("Id"))

tab[rndNumbers2]
rndNumbers2[tab]



# Zadanie 5
get, quote, eval


# Zadanie 6
Stwórzcie data.table x o 1mln wierszy i dwóch kolumnach z liczbami z rozk³adu 
jendostajnego i normalnego. Zapiszcie tabelê x do pliku csv. Zweryfikujcie czas
zapisu przy u¿yciu funkcji standardowych funkcji zapisu do csv np. write.table,
write.csv oraz fwrite (data.table). 
Wczytajcie utworzony plik uzywaj¹c funckji read.table oraz fread porównuj¹c czas 
wczytywania danych.
Zwróæcie uwagê na iloœæ zajmowanego miejsca przez zmienn¹ w R oraz wielkoœæ 
pliku csv.

#! lab 2
# Zadanie 1
Wyznacz skutecznoœæ (suma wp³at przez wartoœæ zad³u¿enia) w ró¿nych horyzontach
czasu (np. 3M, 6M, 12M) i wyznacz jej statystyki opisowe (kwantyle) 
w podziale na:  
- Gender
- ExtrernalAgency
- Bailiff
- ClosedExecution
- M_LastPaymentToImportDate (zaproponuj podzia³ wg tej zmiennej)
- DPD (zaproponuj podzia³ wg tej zmiennej)
- Age (zaproponuj podzia³ wg tej zmiennej)
- TOA (zaproponuj podzia³ wg tej zmiennej)
Wyniki zaprezentuj równie¿ na wykresie. Które zmienne najlepiej ró¿nicuj¹ 
skutecznoœæ (co rozumiesz poprzez "ró¿nicuj¹")?
Wyznacz korelacje pomiêdzy zmienn¹ skutecznoœci a wskazanymi zmiennymi. Czy
w zbiorze danych istniej¹ inne zmienne (od wskazanych) z du¿¹ wartoœci¹ 
korelacji.

# Zadanie 2
Wyznacz dotarcie per sprawa (czy by³ kontakt w sprawie telefoniczny, 
lub bezpoœredni/wizyta) w ró¿nych horyzontach czasu i wyznacz jej statystyki
opisowe (kwantyle) w podziale na wybrane zmienne (np. zmienne z zadania 1, 
lub zmienne, które róznicuj¹ dotarcie).

# Zadanie 3
Czy istnieje zale¿noœæ pomiêdzy iloœci¹ wykonywanych telefonów, wizyt, lub 
wysy³anych listów, a zmiennymi opisuj¹cymi sprawê (zmienne w cases).

# Zadanie 4
Dla wybranych zmiennych stwórz zmienne: 
- standaryzowane (o œredniej zero i warinacji 1) 
- normalizowane (przekszta³cenie wartoœci zmienn,ej na odcinek [0, 1]). 
- logarytmowane
- pierwiatskowane 
Wyznacz korelacjê dla zmiennych orginalnych oraz korelacjê ich przekszta³conych 
odpowiedników. Co mo¿na zauwa¿yæ?

# Zadanie 5
Dla zadanej zmiennej stwórz dystrybuantê empiryczn¹ (funkcja ecdf). Nastêpnie 
stwórz zmienn¹ przekszta³caj¹c j¹ poprzez jej dystrybuantê empiryczn¹. Jakie
charakterystyki ma taka zmienna.

# Zadanie 6
Wyznacz wykres warstwowy pokazuj¹cy udzia³ skutecznoœci w kolejnych miesi¹cach 
obs³ugi w podziale na:
1. SR w sprawach bez kontaktu (zarówno telefoniczny jak i wizyta)
2. SR w sprawach z kontaktem
3. SR w sprawach z ugod¹
4. SR w sprawach przekazanych do s¹du.
Powy¿sze zdarzenia narzucaj¹ hierarchiê procesu, tzn. jeœli sprawa by³ kontakt 
w 3M to sprawa ta jest uwa¿ana za sprawê z kontaktem do 12M (do koñca).
Jeœli w sprawie by³ kontakt w 2M oraz w tej sprawie zosta³a podpisana ugoda 
w 2M, to zaliczamy t¹ sprawê (jej skutecznoœæ) do kategorii spraw z ugod¹ w 2M.

# Zadanie 7
Wyznacz istotnoœæ korelacji pomiêdzy parami zmiennych.

#! lab 3
# Zadanie 1. 
Wykonaj tward¹ klasteryzacjê (k -œrednich, hierarchiczny klastering) danych 
spraw po¿yczkowych z zad³u¿eniem do 65k. wykorzystuj¹c zmienne przed zakupem
(zmienne ze zbioru cases). Dla uproszczenia brakuj¹ce dane uzupe³nij wartoœci¹
œredni¹. Zaproponuj ró¿ne miary reprezentuj¹ce: 
- podobieñstwa spraw w klastrze; 
- niepodobieñstwa klastrów ("odleg³oœæ" miêdzy klastrami).
Do jakich wyborów liczby klastrów k prowadz¹ te miary?
Czy jest mo¿liwoœæ wyboru k jako kompromis tych dwóch podejœæ?

# Zadanie 2 
Stwórz model twardej klasteryzacji spraw po¿yczkowych z zad³u¿eniem do 65k 
prognozuj¹cy "czy sprawa z kontaktem w 3M" na podstawie zmiennych przed zakupem 
(zmienne ze zbioru cases). Które zmienne daj¹ w wyniku model o najlepszych
w³aœciwoœciach predykcyjnych? Ile zmiennych nale¿y u¿yæ do klasteryzacji?

# Zadanie 2 
Stwórz model miêkkiej klasteryzacji spraw po¿yczkowych z zad³u¿eniem do 65k 
prognozuj¹cy "czy sprawa z wp³at¹ w 3M" na podstawie zmiennych przed zakupem 
(zmienne ze zbioru cases). Które zmienne daj¹ w wyniku model o najlepszych
w³aœciwoœciach predykcyjnych? Ile zmiennych nale¿y u¿yæ do klasteryzacji?

# Zadanie 4
Stwórz model twardej klasteryzacji spraw kart kredytowych z zad³u¿eniem do 65k 
prognozuj¹cy wartoœæ œredniej pensji na podstawie zmiennych przed zakupem 
(zmienne ze zbioru cases). Które zmienne daj¹ w wyniku model o najlepszych
w³aœciwoœciach predykcyjnych? Ile zmiennych nale¿y u¿yæ do klasteryzacji?

# Zadanie 5
Stwórz model miêkkiej klasteryzacji spraw kart kreydtowych z zad³u¿eniem do 65k 
prognozuj¹cy wartoœæ œredniej pensji na podstawie zmiennych przed zakupem  
(zmienne ze zbioru cases).  Które zmienne daj¹ w wyniku model o najlepszych
w³aœciwoœciach predykcyjnych? Czy pomocne s¹ transformacje zmiennych (którym
"daleko" do normalnoœci) takie jak logarytm, lub pierwiastek?  

# Zadanie 6
Napisz funkcjê/procedurê wyboru k najbli¿szych s¹siadów (Wsk. Funkcja dist(x)
zwraca odleg³oœæ pomiêdzy punktami w zbiorze x).
Wykorzystaj t¹ funkcjê w problemie regresyjnym prognozowania wartoœci œredniej
pensji, gdzie wartoœæ prognozy jest równa œredniej wartoœci po s¹siadach.

#! lab 4
# Zadanie 1 
Wyznacz b³êdy MSE metod¹ cross-validation i k-fold cross validation 
dla przyk³adu z wyk³adu (przyk³ad z danymi generowanymi). Porównaj te wartoœci 
z testowym b³êdem MSE. Powtórz obliczenia dla ró¿nych funkcji "y1" (mniej, 
lub bardziej "pofalowanych").

# Zadanie 2 
Maj¹c do dyspozycji zbiór spraw kart kredytowych o zad³u¿eniu do 1000 wyznacz 
oszacowanie rozk³adu œredniego zad³u¿enia oraz DPD pakietów takiego typu.  

# Zadanie 3
Maj¹c próbê n elementow¹ oszacuj ile ró¿nych obserwacji z tej próby zawieraj¹ 
przecietnie poszczególne replikacje bootstrapowe. 

# Zadanie 4
MSE testowe i ucz¹ce dla knn   ??? co modelowaæ

# Zadanie 5
Za³ó¿my, ¿e sprawa rentowna to taka dla której suma wp³at wynosi wiêcej ni¿ 200. 
Dla ka¿dej sprawy wyznacz miesi¹c, w którym przekracza ona próg rentownoœci. 
Jak wygl¹da rozk³ad wyznaczonych miesiêcy? Jak wygl¹da rozk³ad skutecznoœci
spraw w momencie osi¹gniêcia progu rentownoœci? 

# Zadanie 6
Za³ó¿my, ¿e sprawa rentowna to taka dla której skutecznoœæ przekroczy 0.5% 
zad³u¿enia pocz¹tkowego. Dla ka¿dej sprawy wyznacz miesi¹c, w którym przekracza 
ona próg skutecznoœci. Jak wygl¹da rozk³ad wyznaczonych miesiêcy? Ile pieniêdzy 
zosta³o wp³aconych na poszczególnych sprawach - zrób zestawienie w podziale
na ró¿ne bandy zad³u¿enia.

#! lab 5
# Zadanie 1
Wykonaj samodzielnie (bez u¿ywania dedykowanych pakietów/funkcji) wykres ROC. 
Wejœciem bêdzie wektor parwdopodobieñstw i wektor oznaczeñ good/bad.

# Zadanie 2 
Uzyj drzewa klasyfikacyjnego do modelowania zjawiska czy w sprawie pojawi siê 
wp³ata w ci¹gu pierwszych 6 miesiêcy obs³ugi. Zadanie wykonaj wykorzystuj¹c
pakiety tree i rpart.

# Zadanie 3 
Zbuduj drzewo regresyjne do modelowania skutecznoœci na sprawie od 4 do 12 
miesi¹ca obs³ugi (Wsk. Mo¿esz wykorzystaæ zmienne behawioralne z pierwszych
trzech miesiêcy obs³ugi). 

# Zadanie 4
Zbuduj dwa drzewa na dwóch po³owach zbioru ucz¹cego, a nastêpnie wykonaj 
weryfikacjê obu drzew na tym samym zbiorze testowym. Czy zbudowane drzewa s¹ 
"jednakowe"? Czy daj¹ tak¹ sam¹ prognozê?

# Zadanie 5
zadanie z boostingu klasyfikacyjne

# Zadanie 6
zadanie z boostingu regresyjne???


#! lab 6
# Zadanie 1
Przeanalizuj wartoœci NA na zmiennej LoanAmount. Czy wszystkie wartoœci NA 
w przypadku tej zmiennej oznaczaj¹ brak danych? 

# Zadanie 2
Podaj biznesowy (ekspercki) sposób uzupe³nienia wartoœci NA dla zmiennej Other.

# Zadanie 3
Uzupe³nij braki danych na zmiennej Land wykorzystuj¹c rozk³ad empiryczny.
Jak wykorzystaæ nowo pozyskane informacje w uzupe³nieniu zmiennych GDPPerCapita 
i MeanSalary? 

# Zadanie 4
Zweryfikuj dok³adnoœæ uzupe³niania braków danych dla zmiennej TOA poprzez modele
lasów losowych i najblizszych s¹siadów (Wsk. Braki danych w TOA nale¿y
zasymulowaæ).

# Zadnaie 5
Zweryfikuj ró¿nice pomiêdzy wartoœciami œrednich oraz przeciêtnych dla rozk³adów 
poszczególnych zmiennych opisuj¹cych sprawy. Oceñ jaki wp³yw na ró¿nice maj¹ 
wartoœci skrajne.

# Zadanie 6
Pos³uguj¹c siê wykresami typu boxplot zidentyfikuj wartoœci odstaj¹ce (jaka 
regu³a jest przyjêta w funkcji boxplot) na poszczgólnych zmiennych opisuj¹cych 
sprawy. Usuñ przypadki z wartoœciami odstaj¹cymi, a nastêpnie wykonaj wykres
ponownie. Czy nadal mo¿esz zaobserwowaæ wartoœci odstaj¹ce?

# Zadanie 7
http://www.itl.nist.gov/div898/handbook/eda/section3/eda35h2.htm
Napisz funkcjê wyznaczaj¹c¹ statystykê Tietjen-Moore (szczegó³y w linku). Napisz
funkcjê do wyznaczania wartoœci krytycznej testu Tietjen-Moore (Wsk. Empiryczn¹
wartoœæ krytyczn¹ testu szacujemy poprzez wyznaczenie odpowiedniego, zale¿nie
od poziomu istotnoœci, kwantyla rozk³adu statystyki testowej wyznaczonej przy 
prawdziwoœci hipotezy zerowej). 

# Zadnaie 8
# Techniczny
http://www.rdatamining.com/examples/outlier-detection
# Merytoryczny
http://www.dbs.ifi.lmu.de/Publikationen/Papers/LOF.pdf
Powtarzaj¹c kroki z przyk³adu (link techniczny) zidentyfikuj wartoœci odstaj¹ce 
na zbiorze spraw kart kredytowych u¿ywaj¹c pakietu DMwR. Wyznacz zbiór wartoœci 
odstaj¹cych uzywaj¹c pakietu mvoutlier, a nastêpnie porównaj oba zbiory. Co 
mo¿esz powiedzieæ o tych dwóch zbiorach?

# Zadanie 9
Coœ na metrykê Mahalonobisa   # transformacja danych na normalne???

# Zadanie 10 
Wyznacz wartoœci wspó³czynnika stabilnoœci populacji porównuj¹c zbiory pozyczek
oraz kart kredytowych. Jak podzieliæ ("pobandowaæ na klasy") zmienne ci¹g³e?

#! lab 7



