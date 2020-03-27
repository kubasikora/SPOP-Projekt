{- SPOP. Lab 0. Nieoceniane -}

import Data.Char  -- funkcje 'ord' i 'chr' do zadania 5.

{- Zadanie 1. Napisz funkcję, która zwraca środkowy element podanej listy.
Wykorzystaj funkcje standardowe: 'div' i 'length' oraz operator (!!). Przykład:

ghci> middle "Haskell"
'k'
-}

middle :: [a] -> a
middle [] = error "Empty list"
middle list = list !! middleIndex
    where   listLength = length list
            middleIndex = if mod listLength 2 == 0 
                            then error "No middle element"
                            else div listLength 2

{- Zadanie 2. Napisz funkcję, która usuwa z listy występujące bezpośrednio
po sobie duplikaty danego elementu. Nie korzystaj z funkcji standardowych.
Przykład:

ghci> removeDuplicates [9, 3, 3, 3, 4, 5, 5, 3, 5]
[9,3,4,5,3,5]

Wskazówka: spójrz na definicję funkcji 'maximum' z wykładu. -}

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [x] = [x]
removeDuplicates (x:y:ys) | x == y = removeDuplicates(x:ys)
                          | otherwise = x:removeDuplicates(y:ys)

{- Zadanie 3. Napisz funkcję, która wstawia do danej listy nowy element
na podanej pozycji. Nie korzystaj z funkcji standardowych. Przykład:

ghci> insertAt "askell" 'H' 0
"Haskell"

Wskazówka: por. z definicji operatora (!!) z wykładu
-}

insertAt :: [a] -> a -> Int -> [a]
insertAt list element 0 = element:list
insertAt [] element index = error "Index out of range"
insertAt (x:xs) element index = x:(insertAt xs element (index-1))

{- Zadanie 4. Napisz funkcję, która usuwa z listy wszystkie występujące
dalej duplikaty poszczególnych elementów. Przykład:

ghci> removeAllDuplicates [9, 3, 3, 3, 4, 5, 5, 3, 5]
[9,3,4,5]

Wskazówka: spójrz na definicję funkcji 'reverse' z wykładu. W akumulatorze
przechowuj elementy napotykane po raz pierwszy. Użyj funkcji 'elem' do
sprawdzenia, czy element jest już w akumulatorze. -}

removeAllDuplicates :: Eq a => [a] -> [a]
removeAllDuplicates [] = []
removeAllDuplicates list = rmDupl list []
    where rmDupl [] acc = acc 
          rmDupl (x:xs) acc = if not (x `elem` acc)
                              then rmDupl xs (acc ++ [x])
                              else rmDupl xs acc

{- Zadanie 5. Zadanie dotyczy szyfrowania tekstów. Prosty kod Cezara polega
na tym, że w miejsce danej litery wstawiamy literę o kodzie większym np.
o 3 (liczbę tą nazywamy kluczem w kodzie Cezara). Końcowe litery alfabetu
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
codeCezar [] _ = []
codeCezar (x:xs) shift = [cypherLetter x shift] ++ codeCezar xs shift
    where cypherLetter letter shift | letter >= 'a' && letter <= 'z' = if (ord letter) + shift > (ord 'z') 
                                                                       then chr ((ord letter) + shift - ((ord 'z') - (ord 'a') + 1))
                                                                       else chr ((ord letter) + shift)
                                    | letter >= 'A' && letter <= 'Z' = if (ord letter) + shift > (ord 'Z') 
                                                                       then chr ((ord letter) + shift - ((ord 'Z') - (ord 'A') + 1))
                                                                       else chr ((ord letter) + shift)

decodeCezar :: String -> Int -> String
decodeCezar [] _ = []
decodeCezar (x:xs) shift = [decypherLetter x shift] ++ decodeCezar xs shift
    where decypherLetter letter shift | letter >= 'a' && letter <= 'z' = if (ord letter) - shift < (ord 'a') 
                                                                         then chr ((ord letter) - shift + ((ord 'z') - (ord 'a') + 1))
                                                                         else chr ((ord letter) - shift)
                                      | letter >= 'A' && letter <= 'Z' = if (ord letter) - shift < (ord 'A') 
                                                                         then chr ((ord letter) - shift + ((ord 'Z') - (ord 'A') + 1))
                                                                         else chr ((ord letter) - shift)
