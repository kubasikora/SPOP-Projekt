module Main where

{-
okej, pomysł na to jest taki, każda komórka jest listą i ma wartości od 1 do size, 
i na podstawie każdego clue/constraint, wykluczamy z tych list wartosci zabronione 
iterujemy po clue/constraint do momentu az kazda komorka bedzie length == 1
 -}

type Cell = [Int]
type Board = [[Cell]]
type EdgeClues = [Maybe Int]
data Clues = Clues EdgeClues EdgeClues EdgeClues EdgeClues -- upper, right, bottom, left

createEmptyBoard :: Int -> Board
createEmptyBoard size | size <= 0 = error "Invalid board size"
                      | otherwise = [ [ [z | z <-[1..size] ] | x <- [1..size] ] | y <- [1..size] ] 

main :: IO ()
main = do
    putStrLn "SPOP-Projekt: Piramidy"
    putStrLn "Podaj rozmiar planszy:"
    line <- getLine
    let size = (read (takeWhile (/= ' ') line) :: Int)
    print $ createEmptyBoard size
