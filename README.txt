Język oparty jest na Pascalu (nieznacznie zmieniłam składnię od deklaracji,
głównie naprawiałam błędy, nowa gramatyka jest w pliku Docinterpreter.pdf).
Do zaimplementowania użyłam BNFC. Zmienne oraz funkcje trzymam w zmiennej
reprezentującej środowisko.

Z mojej deklaracji zaimplementowałam wszystko i ponadto:
- jawne obsłużenie błędów wykonania
- dowolne zagnieżdżenie funkcji/procedur

W następnej iteracji na pewno chciałabym dodać statyczne typowanie (na razie
język działa bardziej jak np Python, deklaracje typów (poza tablicami) nic
nie wnoszą i działanie "wbrew" typom jest wyłapywane w trakcie wykonywania
programu).

Interpeter uruchamia się domyślnie, powinien ubsługiwać i pliki i podawanie
programu na standardowym wejściu.

Załączam następujące programy:

W katalogu good:
- test_dec.txt - deklaracje + arytmetyka
- test_for.txt - pętla for
- test_while.txt - pętle while
- test_collatz.txt - while z if
- test_array.txt - przypisanie, deklaracja i dostęp do elementów w tablicy

