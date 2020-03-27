{- SPOP. Lab 0. Nieoceniane -}

import Data.Char  -- funkcje 'ord' i 'chr' do zadania 5.

{- Zadanie 1. Napisz funkcję, która zwraca środkowy element podanej listy.
Wykorzystaj funkcje standardowe: 'div' i 'length' oraz operator (!!). Przykład:

ghci> middle "Haskell"
'k'

-}

middle :: [a] -> a
middle = undefined

{- Zadanie 2. Napisz funkcję, która usuwa z listy występujące bezpośrednio
po sobie duplikaty danego elementu. Nie korzystaj z funkcji standardowych.
Przykład:

ghci> removeDuplicates [9, 3, 3, 3, 4, 5, 5, 3, 5]
[9,3,4,5,3,5]

Wskazówka: spójrz na definicję funkcji 'maximum' z wykładu. -}

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = undefined

{- Zadanie 3. Napisz funkcję, która wstawia do danej listy nowy element
na podanej pozycji. Nie korzystaj z funkcji standardowych. Przykład:

ghci> insertAt "askell" 'H' 0
"Haskell"

Wskazówka: por. z definicji operatora (!!) z wykładu
-}

insertAt :: [a] -> a -> Int -> [a]
insertAt = undefined

{- Zadanie 4. Napisz funkcję, która usuwa z listy wszystkie występujące
dalej duplikaty poszczególnych elementów. Przykład:

ghci> removeAllDuplicates [9, 3, 3, 3, 4, 5, 5, 3, 5]
[9,3,4,5]

WskazÃ³wka: spójrz na definicję funkcji 'reverse' z wykładu. W akumulatorze
przechowuj elementy napotykane po raz pierwszy. Użyj funkcji 'elem' do
sprawdzenia, czy element jest już w akumulatorze. -}

removeAllDuplicates :: Eq a => [a] -> [a]
removeAllDuplicates = undefined

{- Zadanie 5. Zadanie dotyczy szyfrowania tekstów. Prosty kod Cezara polega
na tym, że w miejsce danej litery wstawiamy literę o kodzie większym np.
o 3 (liczbę™ tą nazywamy kluczem w kodzie Cezara). Końcowe litery alfabetu
zastępujemy literami z początku alfabetu. Np. w miejsce 'A' wstawiamy 'D',
w miejsce 'X' wstawiamy 'A'. Napisz dwie funkcje, które odpowiednio kodują
i dekodują napis szyfrem Cezara o podanym kluczu. Przykład:

ghci> codeCezar "Koty" 3
"Nrwb"
ghci> decodeCezar "Nrwb" 3
"Koty"

Wskazówka: kod ASCII danego znaku zwraca funkcja 'ord :: Char -> Int', natomiast
znak odpowiadający podanemu kodowi ASCII zwraca funkcja 'chr :: Int -> Char'.
Przykład:

ghci> ord 'A'
65
ghci> chr 65
'A' -}

codeCezar :: String -> Int -> String
codeCezar = undefined

decodeCezar :: String -> Int -> String
decodeCezar = undefined