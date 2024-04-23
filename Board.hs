-- Board.hs

-- | This module provides functions for managing the game board.
module Board where

mkPlayer :: Int
mkPlayer = 1

mkOppenent :: Int
mkOppenent = 2
printBoardHeader :: IO ()
printBoardHeader = putStrLn " x 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5\ny ------------------------------"


type Board = [[String]]  -- Change the type to use String instead of Int

mkBoard :: Int -> Board
mkBoard n = replicate n (replicate n ". ") -- creates the board game of n times

size :: Board -> Int
size bd = length bd

getRow :: [[a]] -> Int -> [a] --get specific row function
getRow listOfLists index = if index < length listOfLists
                                    then listOfLists !! index
                                    else error "Index out of bounds"
getCol :: Int -> [[a]] -> [a] -- get specific  col
getCol _ [] = []
getCol columnIndex lists = map (!! columnIndex) lists
                                    
-- printBoard :: Board -> IO ()
-- printBoard board = mapM_ putStrLn (map concat board)-- need for concatMap show
printBoard :: Board -> IO ()
printBoard board = mapM_ printRow (zip [1..] board)
  where
    printRow (rowNum, row) = do
      putStr $ showRowNumber rowNum
      putStr "| "
      putStrLn $ concat row

    showRowNumber n = show (n `mod` 10)
updateBoard :: Board -> Int -> Int -> String -> Board  -- Accept String instead of Int
updateBoard board row col value =
    let (before, currentRow : after) = splitAt (row - 1) board
        (left, _ : right) = splitAt (col - 1) currentRow
    in before ++ [left ++ [value] ++ right] ++ after

isEmpty :: Int->Int->Board -> Bool
isEmpty x y updatedBoard | x < 0 || x >= length updatedBoard || y < 0 || y >= length (head updatedBoard) = False  -- Check if indices are out of bounds
    | otherwise = (updatedBoard !! x) !! y == ". " -- checks if cells is empty by checking if postion at x,y is . 


checkWin :: String -> [String] -> Bool -- check win function receives the specific col or row and then checks if there are a 5 consecutive stones placed
checkWin _ [] = False
checkWin target xs = any (\sublist -> all (== target) sublist) (consecutiveSublists xs)
  where
    consecutiveSublists [] = []
    consecutiveSublists xs'@(x:xs'')
      | length xs' >= 5 = take 5 xs' : consecutiveSublists xs''
      | otherwise = []
checkDraw ::String ->Board -> Bool
checkDraw element board = any (elem element) board

getP :: Int -> String --get players stone function
getP count
  | even count = "x " --if even count
  | otherwise  = "o " --if odd count