Uwaga: W zadaniach mo?na wybiera? losowe podzbiory spraw (tak jak na wyk?adzie),
by czas oblicze? nie przekracza? czasu zaj??. Warto jednak zweryfikowa? czas 
oblicze?, by mie? ?wiadomo?? ile czasu b?d? wykonywa? si? obliczenia do 
wykonania projektu.

#! lab 1
# Zadanie 1
Stw?rz struktur? danych x = data.table(U=runif(10)). Nast?pnie napisz funkcj? 
goodBadProp, kt?ra przyjmuje jako argumenty struktur? tab (data.table) oraz 
liczb? p z przedzia?u (0, 1). W ciele funkcji wykonaj operacj?
- dodaj do struktury tab kolumn? GoodBad, kt?ra przyjmuje warto?ci 1, gdy U > p, 
  a 0 w przeciwnym przypadku,
- jako wynik funkcji zwr?? struktur? data.table z wyliczonymi cz?sto?ciami
  good?w i bad?w.
Jak wygl?da struktura danyh x po uruchomieniu funkcji? Je?li inaczej ni? 
pierwotnie, to czy mo?na tego unikn???
Sparametryzuj funkcj?, by jej kod nie zale?a? od wprost od U (nazwy kolumny 
w x). 

# Zadanie 2
http://datatable.r-forge.r-project.org/datatable-intro.pdf
W materiale z powy?szego linku mo?na znale?? przyk?ady wykorzystania data.table
wraz z czasami wykonania pewnych polece?. Wykonajcie te przyk?ady zwracaj?c 
uwag? na mo?liwo??i data.table (Uwaga: W zale?no?ci od komputera mog? wyst?pi?
trudno?ci z pomiarem czasu wykonania poprzez system.time. Polecam pakiet 
microbenchmark). 
                                                           
# Zadanie 3
Stw?rz tabel? rndNumbers data.table o 100k wierszach i kolumnach 
- U z warto?ciami rozk?adu jednostajnego na (0, 1)
- Z z warto?ciami rozk?adu normalnego 
- E z warto?ciami rozkaldu eksponencjalnego
- G z warto?ciami 0, 1 
- P z warto?ciami z rozk?adu Poissona o ?redniej 2
Wyznacz (korzystaj?c z funckjonalno?ci data.table) statystyki opisowe 
poszczeg?lnych kolumn. 
Wyznacz (korzystaj?c z funckjonalno?ci data.table) statystyki opisowe 
kolumn U, Z, E w rozbiciu wzgl?dem kolumny G. 
Wyznacz (korzystaj?c z funckjonalno?ci data.table) statystyki opisowe 
kolumn U, Z, E w rozbiciu wzgl?dem czy P jest wi?ksze od swojej ?edniej. Czy
mo?na to zrobi? bez dodawania nowych kolumn, wyliczania "na boku" ?redniej P?

# Zadanie 4
Do tabeli rndNumbers z zadania 3 dodaj kolumn? Id przypisuj?c do niej ".I".
Stw?rz tabel? rndNumbers2 jako podzbi?r tabeli rndNumbers dla Id <= 10.
Stw?rz tabel? tab (10 wierszy) z kolumnami
- Id o warto?ciach ze zbioru 1, 2, 3 przypisanymi losowo
- Id2 o warto?ciach ze zbioru 6, 7, 8 przypisanymi losowo
Na??? klucz Id na tabel? rndNumbers2. Na??? klucz Id na tabel? tab.
Wykonaj joiny tabel tab i rndNumbers w r??nej kolejno?ci (np. tab[rndNumbers2], 
lub rndNumbers2[tab]). Co otrzymujesz w wyniku (po jakiej kolumnie wykonywany
jest join; wymu? joina po drugiej kolumnie)? Do czego s?u?? parametry 
nomatch, allow.cartesian?

# Zadanie 5
get, quote, eval


# Zadanie 6
Stw?rzcie data.table x o 1mln wierszy i dw?ch kolumnach z liczbami z rozk?adu 
jendostajnego i normalnego. Zapiszcie tabel? x do pliku csv. Zweryfikujcie czas
zapisu przy u?yciu funkcji standardowych funkcji zapisu do csv np. write.table,
write.csv oraz fwrite (data.table). 
Wczytajcie utworzony plik uzywaj?c funckji read.table oraz fread por?wnuj?c czas 
wczytywania danych.
Zwr??cie uwag? na ilo?? zajmowanego miejsca przez zmienn? w R oraz wielko?? 
pliku csv.

#! lab 2
# Zadanie 1
Wyznacz skuteczno?? (suma wp?at przez warto?? zad?u?enia) w r??nych horyzontach
czasu (np. 3M, 6M, 12M) i wyznacz jej statystyki opisowe (kwantyle) 
w podziale na:  
- Gender
- ExtrernalAgency
- Bailiff
- ClosedExecution
- M_LastPaymentToImportDate (zaproponuj podzia? wg tej zmiennej)
- DPD (zaproponuj podzia? wg tej zmiennej)
- Age (zaproponuj podzia? wg tej zmiennej)
- TOA (zaproponuj podzia? wg tej zmiennej)
Wyniki zaprezentuj r?wnie? na wykresie. Kt?re zmienne najlepiej r??nicuj? 
skuteczno?? (co rozumiesz poprzez "r??nicuj?")?
Wyznacz korelacje pomi?dzy zmienn? skuteczno?ci a wskazanymi zmiennymi. Czy
w zbiorze danych istniej? inne zmienne (od wskazanych) z du?? warto?ci? 
korelacji.

# Zadanie 2
Wyznacz dotarcie per sprawa (czy był kontakt w sprawie telefoniczny, lub bezpośredni)
w róznych horyzontach czasu i wyznacz jej statystyki opisowe (kwantyle) w podziale na wybrane zmienne (np. zmienne z zadania 1, 
lub zmienne, które różnicują dotarcie).

# Zadanie 3
Czy istnieje zale?no?? pomi?dzy ilo?ci? wykonywanych telefon?w, wizyt, lub 
wysy?anych list?w, a zmiennymi opisuj?cymi spraw? (zmienne w cases).

# Zadanie 4
Dla wybranych zmiennych stw?rz zmienne: 
- standaryzowane (o ?redniej zero i warinacji 1) 
- normalizowane (przekszta?cenie warto?ci zmienn,ej na odcinek [0, 1]). 
- logarytmowane
- pierwiatskowane 
Wyznacz korelacj? dla zmiennych orginalnych oraz korelacj? ich przekszta?conych 
odpowiednik?w. Co mo?na zauwa?y??

# Zadanie 5
Dla zadanej zmiennej stw?rz dystrybuant? empiryczn? (funkcja ecdf). Nast?pnie 
stw?rz zmienn? przekszta?caj?c j? poprzez jej dystrybuant? empiryczn?. Jakie
charakterystyki ma taka zmienna.

# Zadanie 6
Wyznacz wykres warstwowy pokazuj?cy udzia? skuteczno?ci w kolejnych miesi?cach 
obs?ugi w podziale na:
1. SR w sprawach bez kontaktu (zar?wno telefoniczny jak i wizyta)
2. SR w sprawach z kontaktem
3. SR w sprawach z ugod?
4. SR w sprawach przekazanych do s?du.
Powy?sze zdarzenia narzucaj? hierarchi? procesu, tzn. je?li sprawa by? kontakt 
w 3M to sprawa ta jest uwa?ana za spraw? z kontaktem do 12M (do ko?ca).
Je?li w sprawie by? kontakt w 2M oraz w tej sprawie zosta?a podpisana ugoda 
w 2M, to zaliczamy t? spraw? (jej skuteczno??) do kategorii spraw z ugod? w 2M.

# Zadanie 7
Wyznacz istotno?? korelacji pomi?dzy parami zmiennych.

#! lab 3
# Zadanie 1. 
Wykonaj tward? klasteryzacj? (k -?rednich, hierarchiczny klastering) danych 
spraw po?yczkowych z zad?u?eniem do 65k. wykorzystuj?c zmienne przed zakupem
(zmienne ze zbioru cases). Dla uproszczenia brakuj?ce dane uzupe?nij warto?ci?
?redni?. Zaproponuj r??ne miary reprezentuj?ce: 
- podobie?stwa spraw w klastrze; 
- niepodobie?stwa klastr?w ("odleg?o??" mi?dzy klastrami).
Do jakich wybor?w liczby klastr?w k prowadz? te miary?
Czy jest mo?liwo?? wyboru k jako kompromis tych dw?ch podej???

# Zadanie 2 
Stw?rz model twardej klasteryzacji spraw po?yczkowych z zad?u?eniem do 65k 
prognozuj?cy "czy sprawa z kontaktem w 3M" na podstawie zmiennych przed zakupem 
(zmienne ze zbioru cases). Kt?re zmienne daj? w wyniku model o najlepszych
w?a?ciwo?ciach predykcyjnych? Ile zmiennych nale?y u?y? do klasteryzacji?

# Zadanie 2 
Stw?rz model mi?kkiej klasteryzacji spraw po?yczkowych z zad?u?eniem do 65k 
prognozuj?cy "czy sprawa z wp?at? w 3M" na podstawie zmiennych przed zakupem 
(zmienne ze zbioru cases). Kt?re zmienne daj? w wyniku model o najlepszych
w?a?ciwo?ciach predykcyjnych? Ile zmiennych nale?y u?y? do klasteryzacji?

# Zadanie 4
Stw?rz model twardej klasteryzacji spraw kart kredytowych z zad?u?eniem do 65k 
prognozuj?cy warto?? ?redniej pensji na podstawie zmiennych przed zakupem 
(zmienne ze zbioru cases). Kt?re zmienne daj? w wyniku model o najlepszych
w?a?ciwo?ciach predykcyjnych? Ile zmiennych nale?y u?y? do klasteryzacji?

# Zadanie 5
Stw?rz model mi?kkiej klasteryzacji spraw kart kreydtowych z zad?u?eniem do 65k 
prognozuj?cy warto?? ?redniej pensji na podstawie zmiennych przed zakupem  
(zmienne ze zbioru cases).  Kt?re zmienne daj? w wyniku model o najlepszych
w?a?ciwo?ciach predykcyjnych? Czy pomocne s? transformacje zmiennych (kt?rym
"daleko" do normalno?ci) takie jak logarytm, lub pierwiastek?  

# Zadanie 6
Napisz funkcj?/procedur? wyboru k najbli?szych s?siad?w (Wsk. Funkcja dist(x)
zwraca odleg?o?? pomi?dzy punktami w zbiorze x).
Wykorzystaj t? funkcj? w problemie regresyjnym prognozowania warto?ci ?redniej
pensji, gdzie warto?? prognozy jest r?wna ?redniej warto?ci po s?siadach.

#! lab 4
# Zadanie 1 
Wyznacz b??dy MSE metod? cross-validation i k-fold cross validation 
dla przyk?adu z wyk?adu (przyk?ad z danymi generowanymi). Por?wnaj te warto?ci 
z testowym b??dem MSE. Powt?rz obliczenia dla r??nych funkcji "y1" (mniej, 
lub bardziej "pofalowanych").

# Zadanie 2 
Maj?c do dyspozycji zbi?r spraw kart kredytowych o zad?u?eniu do 1000 wyznacz 
oszacowanie rozk?adu ?redniego zad?u?enia oraz DPD pakiet?w takiego typu.  

# Zadanie 3
Maj?c pr?b? n elementow? oszacuj ile r??nych obserwacji z tej pr?by zawieraj? 
przecietnie poszczeg?lne replikacje bootstrapowe. 

# Zadanie 4
MSE testowe i ucz?ce dla knn   ??? co modelowa?

# Zadanie 5
Za???my, ?e sprawa rentowna to taka dla kt?rej suma wp?at wynosi wi?cej ni? 200. 
Dla ka?dej sprawy wyznacz miesi?c, w kt?rym przekracza ona pr?g rentowno?ci. 
Jak wygl?da rozk?ad wyznaczonych miesi?cy? Jak wygl?da rozk?ad skuteczno?ci
spraw w momencie osi?gni?cia progu rentowno?ci? 

# Zadanie 6
Za???my, ?e sprawa rentowna to taka dla kt?rej skuteczno?? przekroczy 0.5% 
zad?u?enia pocz?tkowego. Dla ka?dej sprawy wyznacz miesi?c, w kt?rym przekracza 
ona pr?g skuteczno?ci. Jak wygl?da rozk?ad wyznaczonych miesi?cy? Ile pieni?dzy 
zosta?o wp?aconych na poszczeg?lnych sprawach - zr?b zestawienie w podziale
na r??ne bandy zad?u?enia.

#! lab 5
# Zadanie 1
Wykonaj samodzielnie (bez u?ywania dedykowanych pakiet?w/funkcji) wykres ROC. 
Wej?ciem b?dzie wektor parwdopodobie?stw i wektor oznacze? good/bad.

# Zadanie 2 
Uzyj drzewa klasyfikacyjnego do modelowania zjawiska czy w sprawie pojawi si? 
wp?ata w ci?gu pierwszych 6 miesi?cy obs?ugi. Zadanie wykonaj wykorzystuj?c
pakiety tree i rpart.

# Zadanie 3 
Zbuduj drzewo regresyjne do modelowania skuteczno?ci na sprawie od 4 do 12 
miesi?ca obs?ugi (Wsk. Mo?esz wykorzysta? zmienne behawioralne z pierwszych
trzech miesi?cy obs?ugi). 

# Zadanie 4
Zbuduj dwa drzewa na dw?ch po?owach zbioru ucz?cego, a nast?pnie wykonaj 
weryfikacj? obu drzew na tym samym zbiorze testowym. Czy zbudowane drzewa s? 
"jednakowe"? Czy daj? tak? sam? prognoz??

# Zadanie 5
zadanie z boostingu klasyfikacyjne

# Zadanie 6
zadanie z boostingu regresyjne???


#! lab 6
# Zadanie 1
Przeanalizuj warto?ci NA na zmiennej LoanAmount. Czy wszystkie warto?ci NA 
w przypadku tej zmiennej oznaczaj? brak danych? 

# Zadanie 2
Podaj biznesowy (ekspercki) spos?b uzupe?nienia warto?ci NA dla zmiennej Other.

# Zadanie 3
Uzupe?nij braki danych na zmiennej Land wykorzystuj?c rozk?ad empiryczny.
Jak wykorzysta? nowo pozyskane informacje w uzupe?nieniu zmiennych GDPPerCapita 
i MeanSalary? 

# Zadanie 4
Zweryfikuj dok?adno?? uzupe?niania brak?w danych dla zmiennej TOA poprzez modele
las?w losowych i najblizszych s?siad?w (Wsk. Braki danych w TOA nale?y
zasymulowa?).

# Zadnaie 5
Zweryfikuj r??nice pomi?dzy warto?ciami ?rednich oraz przeci?tnych dla rozk?ad?w 
poszczeg?lnych zmiennych opisuj?cych sprawy. Oce? jaki wp?yw na r??nice maj? 
warto?ci skrajne.

# Zadanie 6
Pos?uguj?c si? wykresami typu boxplot zidentyfikuj warto?ci odstaj?ce (jaka 
regu?a jest przyj?ta w funkcji boxplot) na poszczg?lnych zmiennych opisuj?cych 
sprawy. Usu? przypadki z warto?ciami odstaj?cymi, a nast?pnie wykonaj wykres
ponownie. Czy nadal mo?esz zaobserwowa? warto?ci odstaj?ce?

# Zadanie 7
http://www.itl.nist.gov/div898/handbook/eda/section3/eda35h2.htm
Napisz funkcj? wyznaczaj?c? statystyk? Tietjen-Moore (szczeg??y w linku). Napisz
funkcj? do wyznaczania warto?ci krytycznej testu Tietjen-Moore (Wsk. Empiryczn?
warto?? krytyczn? testu szacujemy poprzez wyznaczenie odpowiedniego, zale?nie
od poziomu istotno?ci, kwantyla rozk?adu statystyki testowej wyznaczonej przy 
prawdziwo?ci hipotezy zerowej). 

# Zadnaie 8
# Techniczny
http://www.rdatamining.com/examples/outlier-detection
# Merytoryczny
http://www.dbs.ifi.lmu.de/Publikationen/Papers/LOF.pdf
Powtarzaj?c kroki z przyk?adu (link techniczny) zidentyfikuj warto?ci odstaj?ce 
na zbiorze spraw kart kredytowych u?ywaj?c pakietu DMwR. Wyznacz zbi?r warto?ci 
odstaj?cych uzywaj?c pakietu mvoutlier, a nast?pnie por?wnaj oba zbiory. Co 
mo?esz powiedzie? o tych dw?ch zbiorach?

# Zadanie 9
Co? na metryk? Mahalonobisa   # transformacja danych na normalne???

# Zadanie 10 
Wyznacz warto?ci wsp??czynnika stabilno?ci populacji por?wnuj?c zbiory pozyczek
oraz kart kredytowych. Jak podzieli? ("pobandowa? na klasy") zmienne ci?g?e?

#! lab 7



