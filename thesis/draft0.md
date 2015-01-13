Algorytm ewolucji różnicowej - wpływ wykorzystania osobników z archiwum do mutacji różnicowej na jakość wyników.
# Abstract

# Wstęp
	Problem optymalizacji jest jednym z najczęściej pojawiających się w praktycznych zastosowaniach problemów [Źródło/Zmiękczenie]. Jego powszechność a także rosnący stopień skomplikowania konkretnych zadań stawianych przed systemami informatycznymi rozwiązującymi ten problem skłania do poszukiwania coraz doskonalszych metod jego rozwiązywania [ListaPublikacji dt. problemu optymalizacji)].

## Definicja problemu
	Praca ta, umiejsciowiona została w kontekście rozwiązywania problemów optymalizacji gdzie analityczna forma funkcji optymalizowanej nie jest znana lub analityczne wyznaczenie jej ekstremum jest niemożliwe. Zakłada się bowiem, że zadanie jest zdefiniowane następująco:
	f: X -> R
	min f(x); x należy do X
Gdzie: 
 	- f - funkcja optymalizowana
	- X - dziedzina funkcji f (zbiór potencjalnych rozwiązań)
Dodatkowo zakłada się także możliwość zbadania wartości funkcji f w dowolnym punkcie ze zbioru X, jednak brak możliwości bezpośredniego zbadania jakichkolwiek innych własności tejże funkcji.

Przy tych założeniach, należy zauważyć, że jeśli zbiór X jest nieskończenie liczny (nawet przeliczalny), nie jest możliwe zweryfikowanie, czy wyznaczony punkt jest rzeczywistym rozwiązaniem zadanego problemu. Co więcej, w wielu przypadkach (brak istnienia hiperpłaszczyzny X, takiej, że w każdym jej punkcie wartość funkcji jest równa poszukiwanemu ekstremum) prawdopodobieństwo odnalezienia rozwiązania problemu (niezależnie od metody), wynosi zero.
Mając to na względzie by problem można było uznać za rozwiązany, należy go przeformułować tak, że rozwiązaniem jest dowolny element x ze zbioru X, jednak jego jakość wyznaczana jest przez wartość funkcji f w tym punkcie. Dla tak sformułowanego problemu możliwe jest porównywanie różnych metod przeglądania przestrzeni X pod względem jakości otrzymanego rozwiązania.
Warto zwrócić uwagę, że pomimo iż między różnymi rozwiązaniami problemu istnieje relacja częściowego porządku, to przy przyjętych założeniach wartość bezwzględna funkcji f w punkcie nie niesie żadnej informacji poza wygenerowaniem relacji częściowego porządku między elementami badanego podzbioru X. Z tego też powodu, wszelkie badania porównawcze przeprowawdzane na poszczególnych metodykach muszą być oparte na tej właśnie relacji porządku.
// To chyba można sobie darować - nie ma znaczenia dla wywodu ?
// Ważnym faktem jest to, że jeśli metodę rozwiązania problemu wraz z jej parametrami uznamy za element zbioru dopuszczalnych metod rozwiązywania D. To problem wyboru metody optymalizacji staje się problemem optymalizacyjnym.


## Proponowana modyfikacja (? tutaj?)

# Przegląd literatury
## Problem optymalizacji
(Definicja problemu)
### Podejścia do rozwiązania
### Klasyczne algorytmy
#### Rozwiązania analityczne
#### Rozwiązania dokładne
### Algorytmy heurystyczne
(Czym jest, dlaczego jest stosowane, odnieśnie do klasycznych algorytmów)
(Algorytm ewolucyjny jako przykład rozwiązania heurystycznego)
#### Algorytmy ewolucyjne
(Czemu heurystyczny?)
(Idea i Schemat działania)
(Problemy)
(Odmiany i schematy?)
(Parametry)
(Alg. genetyczne - wspomnienie, bez rozwijania wątku)
(Eksploracja vs. Eksplatacja)
#### Ewolucja różnicowa
(Ogólny opis)
(Czym się różni od klasycznych algorytmów ewolucyjnych)
(Zalety i wady w kontekście problemu optymalizacji)
(Parametry)
(Wyniki w kontekście innych metod)
#### Podejście do archiwum
(Odniesienia w literaturze)
(Dotychczasowe wnioski)
## Modelowanie statystyczne
(Czemu służy, dlaczego potrzebne do tych badań)
### Testy statystyczne
(Ogólna koncepcja, p-value)
#### Testy parametryczne
(Co to jest i kiedy się stosuje)
##### Test T-studenta
(Jak działa, ZAŁOŻENIA)
(Wady zalety).
#### Testy nieparametryczne
(Co to jest i kiedy się stosuje)
##### Test wilcoxona
(Opis, działanie, ZAŁOŻENIA)
(Wady zalety)
## Benchmarki blackboxowe
(Czemu służą)
(Dlaczego są dobrym odniesieniem)
### CEC
(Opis benchmarka, funkcje testowe)
(Implementacja w R)

# Badania
## Cel

## Plan eksperymentów
### Dobór parametrów
### Zbierane rezulataty
#### Środek ciężkości populacji (DE/mid)
### Sposób analizy

## Wyniki
## Analiza wyników (wnioski)

# Podsumowanie
