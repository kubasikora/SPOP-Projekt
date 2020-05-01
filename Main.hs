module Main where

type Cell = Int
type Board = [[Cell]]

-- stworz pustą plansze o zadanym rozmiarze
createEmptyBoard :: Int -> Board
createEmptyBoard size | size <= 0 = error "Invalid board size"
                      | otherwise = [ [ 0 | x <- [1..size] ] | y <- [1..size] ] 

-- sprawdz czy dana plansza jest poprawna, na razie dummy check
isValidBoard :: Board -> Bool 
isValidBoard board = (board !! 0) !! 0 == 1

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