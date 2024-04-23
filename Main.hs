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
    putStrLn ""
    -- Start the game loop
    gameLoop initialBoard count

gameLoop :: Board -> Int -> IO ()
gameLoop board count = do
    -- Prompt the user to enter row and column indices
    if even count
        then putStr "x's turn: enter x y (1-15)? "
        else putStr "0's turn: enter x y (1-15)? "

    input <- getLine
    let [row, col] = map read (words input) :: [Int]
    -- Update the board with 'X' or 'O' based on the count
    putStrLn $ if row >15 || col > 15 || row < 1 || col < 1 -- checks if x or y is outbounds
        then "Position its out bounds: "++show row ++ " " ++ show col
        else ""
    if row >15 || col > 15 || row < 1 || col < 1 -- if its outbounds loops again
        then gameLoop board count
        else putStr ""
    let empty = isEmpty (row-1) (col-1) board -- checks if postion at x,y is empty
    let updatedBoard = if even count && empty then do updateBoard board row col "x " -- if empty and p1 turn 
        else if odd count && empty then updateBoard board row col "o " -- if empty and p2 turn
        else board -- else meaning not empty store the actual board with no changes
    
    let p = getP count -- get the player stone

    if empty -- prints error message or the player movement
            -- Display the updated board
            then do 
                putStrLn "Updated Board:"
                printBoardHeader
                printBoard updatedBoard
            else putStrLn "Position Invalid , there is a stone already in place"
    let rowat = getRow updatedBoard (row-1)
    let colat = getCol (col-1) updatedBoard
    -- let rowat = updatedBoard !! (row - 1) -- Get the row at the specified index (assuming row is 1-indexed)
    --putStrLn $ "Col " ++ show row ++ ": " ++ show colat

    let rowWon = checkWin p rowat --check if player has won in row
    let colWon = checkWin p colat --check if player has won in col
    let isDraw = checkDraw ". " updatedBoard
    if rowWon || colWon then putStrLn $ p++" Won!!" -- if a player won message will show up 
    else if not isDraw then putStrLn "Is a Draw!!" --if is a draw messafe will show up
    else putStrLn ""

    if empty && (not rowWon && not colWon) then gameLoop updatedBoard (count + 1) --loop and selects which player turns is only if no one has won
    else if not empty && (not rowWon && not colWon) then gameLoop updatedBoard count
    else putStrLn ""


