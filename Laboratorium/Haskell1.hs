{- SPOP. Lab 1. Nieoceniane -}

import Data.Char  -- funkcja 'toUpper'

{- Zadanie 1. Napisz funkcję, która koduje ciąg znaków alfabetem Morse'a.
Poszczególne znaki wynikowego kodu Morse'a powinny być oddzielone spacją.
Przykład:

ghci> morse "Haskell"
".... .- ... -.- . .-.. .-.."

Wykorzystaj zdefiniowaną niżej listę kodów liter. Znaki spoza tej listy
funkcja powinna ignorować. Funkcja 'toUpper :: Char -> Char' konwertuje
literę na dużą. NIE korzystaj z innych funkcji standardowych (ewentualnie
'fst' i 'snd'). -}

morseCodes = [('A',".-"),('B',"-..."),('C',"-.-."),('D',"-.."),
    ('E',"."),('F',"..-."),('G',"--."),('H',"...."),('I',".."),
    ('J',".---"),('K',"-.-"),('L',".-.."),('M',"--"),('N',"-."),
    ('O',"---"),('P',".--."),('Q',"--.-"),('R',".-."),('S',"..."),
    ('T',"-"),('U',"..-"),('V',"...-"),('W',".--"),('X',"-..-"),
    ('Y',"-.--"),('Z',"--..")]

morse :: String -> String
morse [] = []
morse (x:xs) = encode x ++ " "  ++ morse xs
    where encode x = findCode (toUpper x) morseCodes
          findCode _ [] = []
          findCode x (c:cs) = if x == fst c then snd c else findCode x cs          

{- Zadanie 2. Napisz funkcję, jak w zadaniu 1, ale w rozwiązaniu
wykorzystaj funkcje standardowe: 'filter', 'map' i 'concat'. -}

morse' :: String -> String
morse' w = concat(map encode w)
    where encode x = (if length(code x) /= 0  then  snd(head(code x))  else []) ++ " " 
          code x = filter (\c -> (fst c) == toUpper x) morseCodes


{- Sprawdź wartość 'check'. -}

check = morse msg == morse' msg 
msg   = "Houston, mamy problem!"