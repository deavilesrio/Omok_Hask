module Main where

import System.IO
import Board

count :: Int
count = 0

main :: IO ()
main = do
    -- Initialize an empty board 
    let initialBoard = mkBoard 15
    -- Display the initial board
    putStrLn "Initial Board:"
    printBoardHeader
    printBoard initialBoard
    -- Start the game loop
    gameLoop initialBoard count

gameLoop :: Board -> Int -> IO ()
gameLoop board count = do
    -- Prompt the user to enter row and column indices
    putStrLn "Enter row and column indices (separated by a space):"

    input <- getLine
    let [row, col] = map read (words input) :: [Int]
    -- Update the board with 'X' or 'O' based on the count
    let updatedBoard = if count `mod` 2 == 0
                          then updateBoard board row col "x "
                          else updateBoard board row col "o "
    -- Display the updated board
    putStrLn "Updated Board:"
    printBoardHeader
    printBoard updatedBoard
    -- Continue the game loop
    gameLoop updatedBoard (count + 1)
