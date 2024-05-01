module Main where

import System.IO
import Board

-- The count of moves made in the game
count :: Int
count = 0

-- The main function to start the game
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

-- The game loop function
gameLoop :: Board -> Int -> IO ()
gameLoop board count = do
    -- Prompt the user to enter row and column indices
    if even count
        then putStr "x's turn: enter x y (1-15)? "
        else putStr "o's turn: enter x y (1-15)? "

    input <- getLine
    let coords = words input

    case mapM readMaybe coords of
        Just [row, col] ->
            if isValidMove row col
                then processMove row col
                else invalidMove
        _ -> invalidInput

  where
    -- Attempts to read a string as an integer, returning Nothing if unsuccessful
    readMaybe :: String -> Maybe Int
    readMaybe s = case reads s of
        [(x, "")] -> Just x
        _         -> Nothing

    -- Checks if a move is valid
    isValidMove :: Int -> Int -> Bool
    isValidMove row col = row >= 1 && row <= 15 && col >= 1 && col <= 15

    -- Processes the player's move
    processMove :: Int -> Int -> IO ()
    processMove row col = do
        let empty = isEmpty (row - 1) (col - 1) board
            updatedBoard = if even count then updateBoard board row col "x " else updateBoard board row col "o "
            p = getP count

        -- Display the updated board
        putStrLn "Updated Board:"
        printBoardHeader
        printBoard updatedBoard

        -- Check win conditions
        let rowat = getRow updatedBoard (row - 1)
            colat = getCol (col - 1) updatedBoard
            diagonal1 = getDiagonal1 updatedBoard (row - 1) (col - 1)
            diagonal2 = getDiagonal2 updatedBoard (row - 1) (col - 1)
            rowWon = checkWin p rowat
            colWon = checkWin p colat
            diag1Won = checkWin p diagonal1
            diag2Won = checkWin p diagonal2
            isDraw = checkDraw ". " updatedBoard

        let totalMoves = size updatedBoard * size updatedBoard
            movesMade = length (filter (\row -> any (== ". ") row) updatedBoard)
            isDraw = not (rowWon || colWon || diag1Won || diag2Won) && movesMade == totalMoves

        if rowWon || colWon || diag1Won || diag2Won then putStrLn $ p ++ " Won!!"
        else if isDraw then putStrLn "It's a Draw!!"
        else gameLoop updatedBoard (count + 1)

    -- Prints a message for an invalid move
    invalidMove :: IO ()
    invalidMove = do
        putStrLn "Position Invalid, there is a stone already in place."
        gameLoop board count

    -- Prints a message for invalid input
    invalidInput :: IO ()
    invalidInput = do
        putStrLn "Invalid input! Please enter two integers separated by space."
        gameLoop board count