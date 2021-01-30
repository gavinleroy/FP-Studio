{- 
 - Gavin Gray, University of Utah SP21
 - Sudoku FP-Studio CS 6963
 - -}

module Main where

import Sudoku
import Text.Read                  (readMaybe)
import System.IO                  (readFile)
import Data.List                  (transpose)

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
  , "4. Solve Invalid Board"
  , "5. Solve 3x2 Board"
  , "6. Solve File of 3x3 Boards"
  , "7. Solve Random MxN Board"
  , "8. Generate MxN Sudoku Board"
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

nosolboard :: String
nosolboard 
  = unlines 
  [ "3 3"
  , "3 . . 4 6 . . 3 ."
  , "3 9 . . . 1 7 . 5"
  , "2 8 4 . . . . 9 ."
  , "5 . . 8 7 . 6 1 3"
  , "8 3 1 . 9 . . . ."
  , ". . 2 5 1 . . 8 ."
  , ". 6 . . . . . . 9"
  , "4 . 5 . 2 6 3 . ."
  , ". . . . 4 7 5 6 1"]

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

 -- 'Mystery' = taken from Wikipedia
mysteryboard :: String
mysteryboard 
  = unlines
  [ "3 3"
  , ". . . . 4 . . . ."
  , "1 2 . . . . . 7 3"
  , ". 3 . . . 8 . . ."
  , ". . 4 . . . 6 . ."
  , ". . . 2 . 3 . . ."
  , ". . 5 . . . . . ."
  , ". . 6 . 9 . 5 . ."
  , ". 7 . . . . . 2 ."
  , ". . . . . . . . ."]

interleave :: [a] -> [a] -> [a]
interleave xs ys = concat (transpose [xs, ys])

maybeError :: Maybe String -> String
maybeError = maybe errormsg id

readFileMain :: IO ()
readFileMain = do 
  contents <- readFile "./sudoku3x3tests.txt"
  let inputboards = map makeString3x3 . lines $ contents
  let solvedboards = map (maybeError . solveFromString) inputboards
  putStrLn . unlines $ interleave inputboards solvedboards

generateMain :: IO String
generateMain = do
  putStrLn "Enter block size M N: "
  [m, n] <- map read . words <$> getLine
  putStrLn "building random board ..."
  generateBoardAsString m n

prog' :: String -> String -> IO ()
prog' msg board = do
  putStrLn msg
  putStrLn board
  putStrLn . maybeError $ solveFromString board

prog :: IO ()
prog = do
  putStrLn menumsg
  option <- read <$> getLine
  case option of
    1 -> prog' "Easy Board" easyboard
    2 -> prog' "Hard Board" hardboard
    3 -> prog' "Wikipedia Board" mysteryboard
    4 -> prog' "Invalid Board" nosolboard
    5 -> prog' "3x2 Board" board3x2
    6 -> readFileMain
    7 -> generateMain >>= prog' "Random Board"
    8 -> generateMain >>= putStrLn
    _ -> putStrLn "Invalid option :("
  prog

main :: IO ()
main = prog

