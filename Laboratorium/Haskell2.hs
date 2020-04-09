{- SPOP. Lab 2. Nieoceniane -}

{- Obiekt w systemie plików jest plikiem o określonej nazwie, lub
katalogiem o określonej nazwie, zawierającym inne obiekty (które są
plikami lub katalogami): -}

data FSObject = File String | Directory String [FSObject] deriving (Eq, Show)

{- Przykładowa wartość: -}

fs :: FSObject
fs = Directory "Root" [(File "root.txt"),
                       (Directory "Tmp" [(File "tmp.txt"),
                                         (File "tmp.exe"),
                                         (Directory "haskell" [(File "main.hs")])
                                        ]),
                       (File "root.exe")
                      ]

file :: FSObject
file = File "main.hs"

{- Zadanie 1. Napisz funkcję, która sprawdza, czy plik o podanej nazwie
istnieje w danym systemie plików. -}

find :: String -> FSObject -> Bool
find name (File f) = f == name
find name (Directory dir content) = foldl (||) False (map (find name) content)

{- Zadanie 2. Napisz funkcję, która szuka pliku o podanej nazwie w danym
systemu plików. Jeśli taki plik istnieje, funkcja zwraca ścieżkę do
tego pliku postaci "nazwa katalogu/nazwa katalogu/.../nazwa pliku". -}

-- search' :: String -> FSObject -> Maybe String
-- search' name (File f) = if f == name then Just name else Nothing
-- search' name (Directory dir content) = searchName' name "/" (Directory dir content)
--     where searchName' name acc (Directory dir content) = Just (acc ++ dir ++ "/" ++ (foldl (safeConcat) "" (map (search name) content)))


search :: String -> FSObject -> Maybe String
search name (File f) = if f == name then Just name else Nothing
search name dir = findFile name "" dir
    where takeFirst [] = Nothing
          takeFirst (x:xs) = x
          findFile name acc (File f) = if f == name then Just (acc ++ "/" ++ f) else Nothing
          findFile name acc (Directory dir content) = takeFirst (filter (\z -> z /= Nothing) (map (findFile name (acc ++ "/" ++ dir)) content))