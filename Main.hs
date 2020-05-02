module Main where

type Cell = Int
type Board = [[Cell]]

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
transpose ((x:xs):xss) = (x:[ h | (h:_) <- xss]):transpose (xs:[ t | (_:t) <- xss])


isRowValid :: [Cell] -> Bool
isRowValid [] = True
isRowValid row = iterOverRow row row
                 where iterOverRow og [] = True
                       iterOverRow og (x:xs) = if x == 0 then iterOverRow og xs else numTimesFound x og == 1 && iterOverRow og xs

checkRowsForUniqueness :: Board -> Bool
checkRowsForUniqueness board = nextRow board (size - 1)
                               where size = length board
                                     nextRow board 0 = isRowValid (board !! 0)
                                     nextRow board index = isRowValid (board !! index) && nextRow board (index - 1)
                                 
-- sprawdz czy dana plansza jest poprawna, na razie dummy check
isValidBoard :: Board -> Bool 
isValidBoard board = let areRowsValid = checkRowsForUniqueness board
                         areColsValid = checkRowsForUniqueness (transpose board) 
                     in areRowsValid && areColsValid

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
fillSquare :: Board -> (Int, Int) -> [Board]
fillSquare board (row, column) = let size = length board
                                 in if row == size then [board]
                                 else let nextCell = nextPosition size (row, column)
                                          nextBoards = createPossibleBoards board (row, column) 
                                          checkNextBoard [] = []
                                          checkNextBoard (x:xs) = (if isValidBoard x then fillSquare x nextCell else []) ++ checkNextBoard xs
                                 in checkNextBoard nextBoards 


main :: IO ()
main = do
    putStrLn "SPOP-Projekt: Piramidy"
    putStrLn "Podaj rozmiar planszy:"
    line <- getLine
    let size = (read (takeWhile (/= ' ') line) :: Int)
    let board = createEmptyBoard size
    let solutions = fillSquare board (0,0) -- zacznij od elementu (0,0) 
    print solutions