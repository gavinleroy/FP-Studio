module Main where

import System.IO
import Control.Monad
import Sudoku

-- All boards verified using https://www.sudoku-solutions.com/

errormsg :: String
errormsg = "Unsolvable or incorrectly formatted board :("

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
  , "9 1 . 3 . . . . 8"
  , ". 8 4 . . 1 . 7 ."
  , ". . . . 9 . 6 . ."
  , ". . . 8 7 6 . 3 ."
  , ". . 6 . . . 7 . ."
  , ". 7 . 9 5 3 . . ."
  , ". . 8 . 3 . . . ."
  , ". 6 . 7 . . 8 5 ."
  , "4 . . . . 9 . 2 7"]

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
-- main = main' "3x2 Board" board3x2
-- main = main' "Easy Board" easyboard
-- main = main' "Hard Board" hardboard
main = main' "Mystery Board" mysteryboard
-- main = readFileMain
