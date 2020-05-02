module Main where

type Cell = Int

type Board = [[Cell]]

type Clue = Maybe Int

data CluesSet = Clues [Clue] [Clue] [Clue] [Clue] deriving Show -- upper right bottom left

left :: [Clue]
left = [Nothing, Nothing, Just 4, Nothing]

-- funkcja służąca do obliczenia ile piramid jest widocznych w danym rzędzie
getVisiblePyramids :: [Cell] -> Int
getVisiblePyramids row = let nextPyramid [] height acc = acc
                             nextPyramid (x:xs) height acc = if x > height then nextPyramid xs x (acc+1)
                                                             else nextPyramid xs height acc
                         in nextPyramid row 0 0  

-- wyciagnij wartosc z just 
fromJust :: Maybe a -> a 
fromJust (Just x) = x

-- sprawdz czy dany wiersz jest zgodny ze wskazówką, jesli rzad jest jeszcze nieskonczony to jest poprawny
checkRowClue :: [Cell] -> Clue -> Bool
checkRowClue row clue = if clue == Nothing || elem 0 row then True
                        else getVisiblePyramids row == fromJust clue

-- sprawdz wszystkie wskazowki z lewej strony -> wskazowka | wiersz
checkLeftClues :: Board -> [Clue] -> Bool
checkLeftClues board clues = let nextRow [] [] = True
                                 nextRow (x:xs) (c:cs) = checkRowClue x c && nextRow xs cs 
                             in nextRow board clues

-- reverse rows in board
reverseRowsInBoard :: Board -> Board
reverseRowsInBoard board = let reverseRow [] = []
                               reverseRow (x:xs) = reverse x : reverseRow xs
                           in reverseRow board

-- sprawdz wszystkie wskazowki z prawej strony -> odwroc wiersze i sprawdz jak z lewej
checkRightClues :: Board -> [Clue] -> Bool
checkRightClues board clues = checkLeftClues (reverseRowsInBoard board) clues
                             

-- sprawdz wszystkie wskazowki z gory -> transpozycja planszy i sprawdz jak z lewej
checkUpperClues :: Board -> [Clue] -> Bool
checkUpperClues board clues = checkLeftClues (transpose board) clues

-- sprawdz wszystkie wskazowki z dolu -> transpozycja planszy + odwroc wiersze i sprawdz jak z lewej
checkBottomClues :: Board -> [Clue] -> Bool 
checkBottomClues board clues = checkLeftClues (reverseRowsInBoard (transpose board)) clues

-- stworz pustą plansze o zadanym rozmiarze
createEmptyBoard :: Int -> Board
createEmptyBoard size | size <= 0 = error "Invalid board size"
                      | otherwise = [ [ 0 | x <- [1..size] ] | y <- [1..size] ] 

-- ile razy element wystepuje w liscie
numTimesFound :: (Eq a) => a -> [a] -> Int
numTimesFound x xs = (length . filter (== x)) xs

-- transpozycja planszy - kolumny do rzedow
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:xs)   = transpose xs
transpose ((x:xs):xss) = (x:[ h | (h:_) <- xss ]):transpose (xs:[ t | (_:t) <- xss ])

-- czy dany wiersz planszy jest poprawny
isRowValid :: [Cell] -> Bool
isRowValid [] = True
isRowValid row = iterOverRow row row
                 where iterOverRow og [] = True
                       iterOverRow og (x:xs) = if x == 0 then iterOverRow og xs else numTimesFound x og == 1 && iterOverRow og xs

-- sprawdz poprawnosc wierszy planszy
checkRowsForUniqueness :: Board -> Bool
checkRowsForUniqueness board = nextRow board (size - 1)
                               where size = length board
                                     nextRow board 0 = isRowValid (board !! 0)
                                     nextRow board index = isRowValid (board !! index) && nextRow board (index - 1)
                                 
-- sprawdz czy dana plansza jest poprawna, na razie wartosci unikalne w wierszach i kolumnach
isValidBoard :: Board -> CluesSet -> Bool 
isValidBoard board (Clues u r b l) = let areRowsValid = checkRowsForUniqueness board
                                         areColsValid = checkRowsForUniqueness (transpose board) 
                                         areLeftCluesMet = checkLeftClues board l
                                         areRightCluesMet = checkRightClues board r
                                         areUpperCluesMet = checkUpperClues board u
                                         areBottomCluesMet = checkBottomClues board b
                                         areCluesMet = areLeftCluesMet && areRightCluesMet && areUpperCluesMet && areBottomCluesMet
                                     in areRowsValid && areColsValid && areCluesMet

-- oblicz nastepny wymieniany element
nextPosition :: Int -> (Int, Int) -> (Int, Int)
nextPosition size (row, column) = if column + 1 == size then (row + 1, 0)
                               else (row, column + 1)

-- dokonaj wymiany elementu
replace :: Int -> a -> [a] -> [a]
replace _ _ [] = error "Index out of range"
replace 0 value (_:xs) = value:xs
replace index value (x:xs) = x:replace (index - 1) value xs

-- wymien element w planszy na inny
replaceValueInBoard :: Board -> (Int, Int) -> Cell -> Board
replaceValueInBoard board (row, col) value = let rowToReplace = board !! row
                                                 modifiedRow = replace col value rowToReplace
                                             in replace row modifiedRow board

-- stworz nowe rozwiazania, zmieniajac jeden element w planszy
createPossibleBoards :: Board -> (Int, Int) -> [Board]
createPossibleBoards board position = createNext board position (length board)
                                      where createNext _ _ 0 = []
                                            createNext board pos n = [replaceValueInBoard board pos n] ++ createNext board pos (n-1) 

-- stworz liste mozliwych rozwiazan
fillSquare :: Board -> CluesSet -> (Int, Int) -> [Board]
fillSquare board clues (row, column) = let size = length board
                                       in if row == size then [board]
                                          else let nextCell = nextPosition size (row, column)
                                                   nextBoards = createPossibleBoards board (row, column) 
                                                   checkNextBoard [] = []
                                                   checkNextBoard (x:xs) = (if isValidBoard x clues then fillSquare x clues nextCell else []) ++ checkNextBoard xs
                                               in checkNextBoard nextBoards 

convertNumberToClue :: Int -> Clue
convertNumberToClue num = if num == 0 then Nothing else Just num

askForClues :: IO [Clue]
askForClues = do line <- getLine
                 let numbers = map read $ words line :: [Int]
                 let clues = map convertNumberToClue numbers
                 return clues

askForCluesSet :: IO CluesSet
askForCluesSet = do putStrLn "Podaj wskazówki, jeśli nie istnieje wpisz 0"
                    putStrLn "Podaj górne wskazówki: "
                    upper <- askForClues
                    putStrLn "Podaj prawe wskazówki: "
                    right <- askForClues
                    putStrLn "Podaj dolne wskazówki: "
                    bottom <- askForClues
                    putStrLn "Podaj lewe wskazówki: "
                    left <- askForClues
                    return (Clues upper right bottom left)

askForBoardSize :: IO Int
askForBoardSize = do putStrLn "Podaj rozmiar planszy:"
                     line <- getLine
                     let size = read (takeWhile (/=' ') line) :: Int
                     return size

-- main - pobierz plansze od użytkownika i rozwiąż łamigłówkę
main :: IO ()
main = do putStrLn "SPOP-Projekt: Piramidy"
          size <- askForBoardSize
          clues <- askForCluesSet
          let board = createEmptyBoard size
          -- let clues = Clues [Just 3, Nothing, Just 1, Nothing] [Nothing, Just 3, Nothing, Nothing] [Nothing, Nothing, Nothing, Nothing] [Nothing, Nothing, Just 4, Nothing]
          -- let clues = Clues [Nothing, Nothing, Nothing, Nothing] [Nothing, Just 3, Nothing, Nothing] [Nothing, Just 2, Nothing, Just 2] [Nothing, Nothing, Just 4, Nothing]
        --   let clues = Clues [Just 1, Just 2, Just 3, Just 2] [Just 4, Just 1, Just 2, Just 2] [Just 2, Just 2, Just 1, Just 3] [Just 1, Just 4, Just 2, Just 2]   
          let solutions = fillSquare board clues (0,0) -- zacznij od elementu (0,0) 
          print (head solutions)