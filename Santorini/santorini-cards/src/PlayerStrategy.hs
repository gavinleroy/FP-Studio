{-# LANGUAGE NamedFieldPuns #-}
-- {-# LANGUAGE RecordWildCards #-}

{- Gavin Gray u1040250
 - University of Utah
 - Spring 2021, CS 6963
 - Assignment 2, Santorini
 - -}

module PlayerStrategy 
  ( rankboard
  , determineNewPlayerPos
  ) where

import Data.List                                 (minimumBy)
import Data.Function                             (on)
import SantoriniDefs
import SantoriniUtils

type Rule = ((Int, GameBoard) -> (Int, GameBoard))

winscore          =  100
losescore         = -50
poswinscore       =  7
closescore        =  4
oplowscore        =  2

heightmultiplier  = 3

rules :: Rule
rules 
  = setupwin
  . dontlose
  . stayclose
  . dontfall
  -- . keepoplow -- I lose more with this enabled

stayclose :: Rule
stayclose t@(n, gb@GB{players=[Player{tokens=[p1, p2]}, _]}) 
  | dist p1 p2 == 4
    || dist p1 p2 == 2 = (n + closescore `div` 2, gb)
  | dist p1 p2 == 3    = (n + closescore, gb)
  | otherwise          = t

dontlose :: Rule
dontlose t@(n, gb) 
  | couldWin (swapPlayers gb) = (n + losescore, gb)
  | otherwise                = t 

setupwin :: Rule
setupwin t@(n, gb) 
  | couldWin gb = (n + poswinscore, gb)
  | otherwise   = t 

dontfall :: Rule
dontfall (n, gb@GB{players=[Player{tokens=[p1, p2]}, _],spaces}) 
  = (n + heightmultiplier * (getPos p1 spaces + getPos p2 spaces), gb)

keepoplow :: Rule
keepoplow t@(n, gb)
  | couldElevate (swapPlayers gb) = (n + oplowscore, gb)
  | otherwise                    = t

rankboard :: GameBoard -> (Int, GameBoard)
rankboard = rules . (,) 0

-- Placing Strategy --

center :: Pos
center = (3, 3)

dist :: Pos -> Pos -> Int
dist (x, y) (x', y') 
  = abs (x' - x) + abs (y' - y)

pair :: a -> a -> [a]
pair x y = [x, y]

determineNewPlayerPos :: [Pos] -> [Pos]
determineNewPlayerPos ps 
  = minimumBy (compare `on` (\[p1, p2] 
      -> dist p1 center ^ 2 + dist p2 center ^ 2)) 
        $ filter (\[p1, p2] 
          -> p1 /= p2 && p1 < p2
          && dist p1 p2 == 3) 
          $ pair <$> ps <*> ps

