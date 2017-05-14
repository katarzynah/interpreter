Język oparty jest na Pascalu (nieznacznie zmieniłam składnię od deklaracji,
głównie naprawiałam błędy, nowa gramatyka jest w pliku Docinterpreter.pdf).
Do zaimplementowania użyłam BNFC. Korzystam z transforamtora monad StateT,
w której trzymam środowisko (wszystko z tym związane w pliku ProgramState.hs).

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
- test_str_int.txt - string i rzutowanie
- test_for.txt - pętla for
- test_while.txt - pętle while
- test_collatz.txt - while z if
- test_array.txt - przypisanie, deklaracja i dostęp do elementów w tablicy
- test_proc_env.txt - procedura bez parametrów nadpisująca zmienne globalne
- test_embedded_fun.txt - zagnieżdżone procedury i rekurencja
- test_array_fun.txt - funkcja zwracająca tablicę
(print jest w większości tych przykładów)

W katalogu bad:
- test_double_var_block.txt - blad skaldniowy, dwa bloki deklarujące zmienne kolo siebie
- test_no_begin.txt - blad skladniowy, brak begin
- test_wrong_symbol.txt - blad skladniowy, użyty nieorozpoznawany token
- test_unknown_id.txt - nieznany identyfikator
- test_unknown_fun_id.txt - nieznany id funkcji, pokazuje, że funckja zdefiniowana
    w innej funckji nie jest widoczna na zewnątrz
- test_bad_arg_number.txt - zła liczba argumentów (funkcja może być wywołana bez
    przypisania, wtedy zwracana przez nią wartość jest tracona)
- test_div_zero.txt - dzielenie przez 0
- test_index_out_of_bounds.txt - index out of bounds w tablicy
- test_proc_assign.txt - przypisanie zmiennej wywołania procedury
- test_for_not_int.txt - wywołanie pętlie for dla wartości nie będącej intem
- test_while_not_bool.txt - wywołanie pętli while dla warunku nie będącym bool
- test_arithm_not_int.txt - arytmetyka nie na intach

