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

winscore          =  10
losescore         = -50
trickypushscore   =  9
poswinscore       =  7
closescore        =  4
oplowscore        =  2

heightmultiplier  = 2

rules :: Rule
rules 
  = setupwin
  . dontlose
  . stayclose
  . dontfall
  . minotaurpush

-- Try to push people to level 3
-- or Pan to level 0
minotaurpush :: Rule
minotaurpush t@(n, 
  gb@GB{players=[Player{card}
  , Player{tokens=[op1, op2]}]
  , spaces})
  | card /= "Minotaur"      = t
  | any ((==3) . flip getPos spaces) 
      (foldr (:) 
        (mNeighborsOP1 gb) 
        (mNeighborsOP2 gb)) = (n + trickypushscore, gb)
  | otherwise               = t

stayclose :: Rule
stayclose t@(n, gb@GB{players=[Player{tokens=[p1, p2]}, _]}) 
  | dist p1 p2 == 4
    || dist p1 p2 == 2 = (n + closescore `div` 2, gb)
  | dist p1 p2 == 3    = (n + closescore, gb)
  | otherwise          = t

dontlose :: Rule
dontlose t@(n, gb) 
  | couldWin (swapPlayers gb) 
    || cantMove gb = (n + losescore, gb)
  | otherwise      = t 

setupwin :: Rule
setupwin t@(n, gb) 
  | couldWin gb = (n + poswinscore, gb)
  | otherwise   = t 

dontfall :: Rule
dontfall (n, gb@GB{players=[Player{tokens=[p1, p2]}, _],spaces}) 
  = (n + heightmultiplier * (getPos p1 spaces + getPos p2 spaces), gb)

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

