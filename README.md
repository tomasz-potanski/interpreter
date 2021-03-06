#Interpreter of my own programming language
#####An university project build in Haskell.
---
- **Technologies**: Haskell
- **Date**: May, 2014
- **Assessment**: 22/24 (24 was the maximum)
- **GitHub**: https://github.com/tomasz-potanski/interpreter

Designed simplistic programming language (OO, statically-typed, recursive, functions as parameters,
IO, loops, lambda abstractions etc.). Created interpreter that verifies and executes programs.

folder: good contains exemples of valid programs in this language.

ORIGINAL TASK DESCRIPTION BELOW [Polish]
>>>>>>>>>>>>>>>>>>>>>>>


Jezyki i paradygmaty programowania
Zad 2 (lab), interpreter
---------

Tomasz Potanski, 321 150
tomasz@potanski.pl
------------

Interpretuje język imperatywny bazujący na pascalu.

Rozwiązanie celuje w 16 - 18 punktów, tzn. zdaje mi się, 
że zrealizowałem wszystko, co było wymagane na 16 
a dodatkowo zrobiłem ok. 4 funkcjonalności
z listy zadań z treści zadania. 

Nietypowe konstrukcje:
print <-- (zmienna);
print <- (exp/bexp/String/Integer)

średnik na końcu argumentów procedury:
	procedure wart ( zm : Integer; ) ;

if (wraz z odmianami) kończy się endifem

-----------------------------------------------
Funkcjonalności zrealizowane (część pierwsza)

./good/	- zawiera przykłady poprawnych programów
./bad/	- zawiera przykłady niepoprawnych programów
Wszystki programy są opisane.
Dopuszczalne komentarze w języku:
-Jednoliniowe: //, #
-Wieloliniowe: /* */, { }, {* *}

---------------------------------------------
PONIŻEJ WYPISUJĘ FUNKCJONALNOŚCI, KTÓRE SĄ ZREALIZOWANE:

Cały komplet na 8 punktów:
1. Jeden typ wartości: Integer				OK (jest nawet więcej typów)
2. Zmienne, operacja przypisania			OK 
	+ operacje znane z c++ np. ++zmienna, --zmienna, +=, -=, /=, *=
	+ można deklarować wiele zmiennych o tym samym typie w jednej linii: x, y : Integer;
3. If							OK
4. While						OK
5. Wyrażenia z arytmetyką + - * / ( )			OK
6. Porównania						OK

-----------------------------------------
Na 12 pkt:
7. Funkcje lub procedury z				OK 			
   parametrami przez wartość, rekurencja

----------------------------

Funkcjonalności zrealizowane (w częsci drugiej):

Wymaganie na 16 pkt.
1. Dwa typy wartości: Integer i Boolean			OK
2. Arytmetyka, porównania				OK
3. While, if (if _ elif _ else _ endif)			OK
4. Funkcje/procedury bez zagnieżdżania, rekurencja	OK
5. Jawne wypisywanie wartości na wyjście: print/write	OK

Dwa sposór następujących
6. Operatory preinkrementacji, +=, -=, /= itp.		OK 	
7. Pętla for						OK
8. String, literały napisowe, rzutowanie napis <> int	OK
9. Dwa sposoby przekazywa param (zmienna/wartosc)	++

-----------------------
Wymagania na 20 puntków:
1. Przesłanianie identyfikatorów ze statycznym 		OK (powiedziałbym, że 
   ich wiązaniem					   działa - patrz: przesProc.pas)
							  

2. Statyczne typowanie					++
3. Jawne obsłużenie dynamicznych błędów wykonania	OK
4. Funkcje zwracające wartość				OK

Dwie dodatkowe rzeczy z listy:
1. rekordy						++
2. tablice indeksowane int lub listy			OK
3. tablice/slowniki indeksowane dowolnymi		Brak
   porównywalnymi wartościami; typ klucza
   należy uwzględnić w typie słownika
4. krotki z przypisanie jak w Pythonie			Brak 
5. Funkcje jako parametry				OK
6. Zwracanie funkcji w wyniku; domkniecie		OK
   jak w JS
7. Funkcje anonimowe					++

------------------------
Wymagania na 24 pkty:
1. Dowolnie zagieżdżone definicje funkcji/procedur	++
   z zachowanie poprawności statycznego wiązania
   identyfikatorów (jak w Pascalu)
2. Dodatko jedna lub dwie funkcjonalnosci z		++ (nawet więcej)
   listy na 20 pktów
