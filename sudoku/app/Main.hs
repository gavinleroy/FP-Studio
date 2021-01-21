module Main where

import System.IO
import Control.Monad
import Sudoku

-- All boards verified using https://www.sudoku-solutions.com/

errormsg :: String
errormsg = "Unsolvable or incorrectly formatted board :("

menumsg :: String
menumsg  = unlines
  [ ""
  , "Select a menu option:"
  , "---------------------"
  , "1. Solve Easy Board"
  , "2. Solve Hard Board"
  , "3. Solve Wikipedia Board"
  , "4. Solve 3x2 Board"
  , "5. Solve 50,000 3x3 Boards"
  , "6. Solve Custom board from file"
  , "7. Generate MxN Sudoku board"
  ]

board3x2 :: String
board3x2
  = unlines
  [ "3 2"
  , "1 . . . 5 6"
  , ". . . . . ."
  , ". 3 . . . ."
  , ". . 1 . . 4"
  , ". . . . 1 2"
  , "6 . . . . ."]

easyboard :: String
easyboard 
  = unlines 
  [ "3 3"
  , ". . . 4 6 . . 3 ."
  , "3 9 . . . 1 7 . 5"
  , "2 8 4 . . . . 9 ."
  , "5 . . 8 7 . 6 1 3"
  , "8 3 1 . 9 . . . ."
  , ". . 2 5 1 . . 8 ."
  , ". 6 . . . . . . 9"
  , "4 . 5 . 2 6 3 . ."
  , ". . . . 4 7 5 6 1"]

hardboard :: String
hardboard 
  = unlines 
  [ "3 3"
  , "5 . . 9 . 4 . 7 ."
  , ". . . . . 7 . . ."
  , ". 2 . . 1 . . . ."
  , "9 . . . . . . . ."
  , ". . . . 6 1 . 2 ."
  , ". 8 5 4 . . . . ."
  , "4 . . 8 . 2 . 5 ."
  , ". . . 7 . . 3 9 6"
  , ". . . . . . . . ."]

mysteryboard :: String
mysteryboard 
  = unlines
  [ "3 3" -- 'Mystery' = taken from Wikipedia
  , ". . . . 4 . . . ."
  , "1 2 . . . . . 7 3"
  , ". 3 . . . 8 . . ."
  , ". . 4 . . . 6 . ."
  , ". . . 2 . 3 . . ."
  , ". . 5 . . . . . ."
  , ". . 6 . 9 . 5 . ."
  , ". 7 . . . . . 2 ."
  , ". . . . . . . . ."]

readFileMain :: IO ()
readFileMain = do 
  contents <- readFile "./sudoku3x3tests.txt"
  putStrLn . unlines . map (maybe errormsg id . solveFromString . makeString3x3) . lines $ contents

main' :: String -> String -> IO ()
main' msg board = do
  putStrLn msg
  putStrLn board
  putStrLn $ maybe errormsg id $ solveFromString board

main :: IO ()
main = do
  putStrLn menumsg
  option <- read <$> getLine
  case option of
    1 -> main' "Easy Board" easyboard
    2 -> main' "Hard Board" hardboard
    3 -> main' "Wikipedia Board" mysteryboard
    4 -> main' "3x2 Board" board3x2
    5 -> readFileMain
    6 -> undefined
    7 -> undefined
    _ -> putStrLn "Invalid option :("

