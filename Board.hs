-- Module: Board
-- Provides functions for managing the game board.

module Board where

import Data.List (transpose)

-- Constants representing player types
mkPlayer :: Int
mkPlayer = 1

mkOpponent :: Int
mkOpponent = 2

-- Prints the header of the game board
printBoardHeader :: IO ()
printBoardHeader = putStrLn " x 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15\ny ------------------------------"

-- Type synonym for the game board
type Board = [[String]]

-- Creates a new empty game board of size n x n
mkBoard :: Int -> Board
mkBoard n = replicate n (replicate n ". ")

-- Returns the size of the board
size :: Board -> Int
size bd = length bd

-- Gets a specific row from a list of lists
getRow :: [[a]] -> Int -> [a]
getRow listOfLists index
  | index < length listOfLists = listOfLists !! index
  | otherwise = error "Index out of bounds"

-- Gets a specific column from a list of lists
getCol :: Int -> [[a]] -> [a]
getCol _ [] = []
getCol columnIndex lists = map (!! columnIndex) lists

-- Prints the game board
printBoard :: Board -> IO ()
printBoard board = mapM_ printRow (zip [1..] board)
  where
    printRow (rowNum, row) = do
      putStr $ showRowNumber rowNum
      putStr "| "
      putStrLn $ concat row
    showRowNumber n = show (n `mod` 10)

-- Updates the game board with a new value at the specified position
updateBoard :: Board -> Int -> Int -> String -> Board
updateBoard board row col value =
    let (before, currentRow : after) = splitAt (row - 1) board
        (left, _ : right) = splitAt (col - 1) currentRow
    in before ++ [left ++ [value] ++ right] ++ after

-- Checks if a specified position on the board is empty
isEmpty :: Int -> Int -> Board -> Bool
isEmpty x y updatedBoard
    | x < 0 || x >= length updatedBoard || y < 0 || y >= length (head updatedBoard) = False
    | otherwise = (updatedBoard !! x) !! y == ". "

-- Checks if a player has won
checkWin :: String -> [String] -> Bool
checkWin _ [] = False
checkWin target xs = any (\sublist -> all (== target) sublist) (consecutiveSublists xs)
  || any (\sublist -> all (== target) sublist) (consecutiveSublists (transpose xs))
  || any (\sublist -> all (== target) sublist) (consecutiveSublists (diagonals xs))
  || any (\sublist -> all (== target) sublist) (consecutiveSublists (diagonals (map reverse xs)))
  where
    consecutiveSublists [] = []
    consecutiveSublists xs'@(x:xs'')
      | length xs' >= 5 = take 5 xs' : consecutiveSublists xs''
      | otherwise = []

    diagonals :: [[a]] -> [[a]]
    diagonals [] = []
    diagonals ([]:_) = []
    diagonals xss = zipWith (!!) xss [0..] : diagonals (map tail xss)

-- Checks if the game is a draw
checkDraw :: String -> Board -> Bool
checkDraw element board = any (elem element) board

-- Gets the player symbol based on the count
getP :: Int -> String
getP count
  | even count = "x "
  | otherwise = "o "    

-- Gets the diagonal starting from the specified row and column
getDiagonal1 :: Board -> Int -> Int -> [String]
getDiagonal1 board row col =
    let diagStart = max 0 (col - row)
        diagEnd = min 14 (14 + col - row)
    in map (\i -> (board !! (row - col + i)) !! i) [diagStart .. diagEnd]

-- Gets the diagonal starting from the specified row and column
getDiagonal2 :: Board -> Int -> Int -> [String]
getDiagonal2 board row col =
    let diagStart = max 0 (row + col - 14)
        diagEnd = min 14 (row + col)
    in map (\i -> (board !! (row + col - i)) !! i) [diagStart .. diagEnd]