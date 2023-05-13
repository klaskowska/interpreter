## Interpreter
Interpreter kompilujemy poleceniem `make` w katalogu `karolina_laskowska`.  
Uruchamiamy poleceniem `./interpreter program`, gdzie `program` oznacza nazwę pliku z programem do interpretacji.  
Interpreter nie działa interaktywnie, możemy podawać jedynie wejście z pliku.  

### Opis języka programowania, dla którego działa interpreter

Jest to język imperatywny w składni opartej o C++. 
Program w tym języku składa się z deklaracji funkcji. Jeśli zostanie zadeklarowana funkcja `int main()`, to jej ciało się wykona.
Wartości wyrażeń są typów: `int`, `bool`, `string`, `function<Type>([Type])`.
Jest dostępne obliczanie działań arytmetycznych oraz porównań, a także przypisywanie wartości wyrażeń do zmiennych.
Można wypisywać wartości napisów oraz liczb na standardowe wyjście.
Dostępne są instrukcje `if`, `if else` oraz `while`.
Definiując funkcję musimy określić jej typ - funkcja może zwracać wartość dowolnego z 4 typów. Jeżeli funkcja nie ma nic zwracać, to określamy jej typ jako `void`.
Argumenty do funkcji możemy przekazywać przez wartość lub przez zmienną. Argumenty również mogą być dowolnego z 4 typów.
Definicje funkcji można dowolnie zagnieżdżać, zachowując poprawność statycznego wiązania identyfikatorów.
Można tworzyć funkcje anonimowe i przekazywać je jako wartość bądź przypisywać na zmienną odpowiedniego typu.
Dostępne są także domknięcia funkcji.
Błędy wykonania są obsługiwane poprzez wypisanie komunikatu oraz zatrzymanie interpretera.
Język ten jest statycznie typowany.


Poniżej opisuję najważniejsze oraz nietypowe konstrukcje. 
1. Dla funkcji `int main()` nie podajemy argumentów. Ta funkcja wymaga zwrócenia liczby.
2. Nie ma możliwości definiowania zmiennych globalnych. 
3. Funkcje globalne muszą mieć unikalne nazwy. Wszystkie inne nazwy zmiennych mogą się powtarzać, przy czym następuje nadpisanie typu.
4. Wartość warunków logicznych to tylko true/false (nie mogą być liczbami ani napisami).
5. Funkcje typu `void` muszą zwracać (muszą mieć `return`).
6. Przekazywanie parametrów przez wartość standardowo, a przez zmienną z użyciem `&` (jak w C++). 
7. Aby zadeklarować funkcję lokalnie, trzeba przypisać lambdę do zmiennej odpowiedniego typu (typ: `function<Type>([Type])`, lambda: `lambda([Arg]) -> Type Block`).
8. Obliczanie wyrażeń bez przypisania na zmienną: poprzedzając obliczenie słowem `calc`.
9. Konstrukcje if, if else zakończone słowem `endif`. Konstrukcja `while` zakończona słowem `endwhile`.
10. Wypisywanie wartości na wyjście przy pomocy instrukcji `print_int[Expr]` dla liczb oraz `print_str[Expr]` dla napisów.
12. Jeśli mamy wyrażenia wieloargumentowe, to najpierw obliczamy typ każdego z wyrażeń po kolei, a następnie sprawdzamy po kolei, czy każdy z obliczonych typów pasuje do oczekiwań.
13. Funkcje globalne nie muszą być w zdefiniowane w kolejności przed wywołaniem, ale lokalne już muszą być (wszystkie funkcje globalne widzą siebie nawzajem).
14. Przy wywoływaniu funkcji najpierw odczytywana i zapamiętywana jest definicja funkcji, następnie obliczane są wartości podanych argumentów od lewej do prawej, a na koniec wykonuje się ciało funkcji zapamiętanej w chwili wywoływania.
15. W operacjach `and` i `or` zawsze oblicza wartości obu składników (nawet jeśli zna wynik po obliczeniu tylko jednego składnika).
16. Przy wywoływaniu funkcji w statycznym typowaniu najpierw sprawdzamy, czy dana funkcja została zadeklarowana, następnie sprawdzamy, czy liczba przekazanych argumentów się zgadza, a następnie po kolei obliczamy typy przekazanych argumentów i sprawdzamy, czy zgadzają się z definicją tej funkcji.
17. Typy sprawdzamy też w nieosiągalnej części kodu (np. po "return");
18. W `while` oraz `if` najpierw obliczamy typ warunku i sprawdzamy, czy się zgadza, a następnie obliczamy typ instrukcji.
19. Przy przypisywaniu wartości na zmienną najpierw sprawdzamy, czy dana zmienna została zadeklarowana, a następnie sprawdzamy, czy typ wyrażenia jest odpowiedni.
20. Funkcje nie mogą się nazywać słówkami kluczowymi, tj. print_int, print_str, calc, if, while, return, True, False, int, string, bool, void, function.