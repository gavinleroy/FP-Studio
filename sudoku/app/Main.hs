module Main where

import System.IO
import Control.Monad
import Sudoku

errormsg :: String
errormsg = "Unsolvable or incorrectly formatted board :("

easyboard :: String
easyboard = unlines ["3 3"
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
hardboard = unlines ["3 3"
                    , "9 1 . 3 . . . . 8"
                    , ". 8 4 . . 1 . 7 ."
                    , ". . . . 9 . 6 . ."
                    , ". . . 8 7 6 . 3 ."
                    , ". . 6 . . . 7 . ."
                    , ". 7 . 9 5 3 . . ."
                    , ". . 8 . 3 . . . ."
                    , ". 6 . 7 . . 8 5 ."
                    , "4 . . . . 9 . 2 7"]

hardmain :: IO ()
hardmain = do
  putStrLn "Hard board before solving ..."
  putStrLn easyboard
  putStrLn "Hard board solved :" 
  putStrLn $ maybe errormsg id $ solveFromString hardboard

easymain :: IO ()
easymain = do
  putStrLn "Easy board before solving ..."
  putStrLn easyboard
  putStrLn "Easy board solved :" 
  putStrLn $ maybe errormsg id $ solveFromString easyboard

mysteryboard :: String
mysteryboard 
  = unlines
  [ "3 3"
  , ". . . . . . . 1 ."
  , "4 . . . . . . . ."
  , ". 2 . . . . . . ."
  , ". . . . 5 . 4 . 7"
  , ". . 8 . . . 3 . ."
  , ". . 1 . 9 . . . ."
  , "3 . . 4 . . 2 . ."
  , ". 5 . 1 . . . . ."
  , ". . . 8 . 6 . . ."]

mysterymain :: IO ()
mysterymain = do
  putStrLn "Mystery board before solving ..."
  putStrLn easyboard
  putStrLn "Mystery board solved :" 
  putStrLn $ maybe errormsg id $ solveFromString mysteryboard

rfm :: String -> IO String
rfm cs = do
  putStrLn cs
  return $! maybe errormsg id $ solveFromString cs

readFileMain :: IO ()
readFileMain = do 
  contents <- readFile "./sudoku3x3tests.txt"
  putStrLn . unlines . map (maybe errormsg id . solveFromString . make3x3) . lines $ contents

main :: IO ()
main = readFileMain
-- main = easymain
-- main = hardmain
-- main = mysterymain