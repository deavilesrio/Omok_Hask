-- Board.hs

-- | This module provides functions for managing the game board.
module Board where

-- | This is the header for the game board.
-- It displays the column indices.
-- Example:
--
-- >>> printBoardHeader
-- x 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5
printBoardHeader :: IO ()
printBoardHeader = putStrLn "1 2 3 4 5 6 7 8 9 0 1 2 3 4 5\n-----------------------------"


type Board = [[String]]  -- Change the type to use String instead of Int

mkBoard :: Int -> Board
mkBoard n = replicate n (replicate n ". ")

size :: Board -> Int
size bd = length bd

printBoard :: Board -> IO ()
printBoard board = mapM_ putStrLn (map concat board)-- need for concatMap show
updateBoard :: Board -> Int -> Int -> String -> Board  -- Accept String instead of Int
updateBoard board row col value =
    let (before, currentRow : after) = splitAt (row - 1) board
        (left, _ : right) = splitAt (col - 1) currentRow
    in before ++ [left ++ [value] ++ right] ++ after