{- Gavin Gray u1040250
 - University of Utah
 - Spring 2021, CS 6963
 - Assignment 2, Santorini
 - -}

module PlayerStrategy where

import SantoriniDefs
import SantoriniUtils

rankBoard :: BState -> (Int, BState)
rankBoard bs@(_,_,_) 
  | isWin bs  = (2, bs)
  | otherwise = (1, bs)

determineNewPlayerPos :: [Pos] -> Player
determineNewPlayerPos = take 2

