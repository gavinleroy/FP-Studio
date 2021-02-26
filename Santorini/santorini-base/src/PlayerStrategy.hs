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

type Rule = ((Int, BState) -> (Int, BState))

winscore          =  100
losescore         = -50
poswinscore       =  7
closescore        =  4
oplowscore        =  2

rules :: Rule
rules 
  = winrule
  . setupwin
  . dontlose
  . stayclose
  . dontfall
  -- . keepoplow -- I lose more with this enabled

winrule :: Rule
winrule t@(n, bs)
  | isWin bs  = (n + winscore, bs)
  | otherwise = t

stayclose :: Rule
stayclose t@(n, bs@([p1, p2],_,_)) 
  | dist p1 p2 == 4
    || dist p1 p2 == 2 = (n + closescore `div` 2, bs)
  | dist p1 p2 == 3    = (n + closescore, bs)
  | otherwise          = t

dontlose :: Rule
dontlose t@(n, bs) 
  | couldWin (flipBS bs) = (n + losescore, bs)
  | otherwise          = t 

setupwin :: Rule
setupwin t@(n, bs) 
  | couldWin bs = (n + poswinscore, bs)
  | otherwise          = t 

dontfall :: Rule
dontfall (n, bs@([p1,p2],m,_)) 
  = (n + 3 * (getPos p1 m + getPos p2 m), bs)

keepoplow :: Rule
keepoplow t@(n, bs)
  | couldElevate (flipBS bs) = (n + oplowscore, bs)
  | otherwise                = t

rankboard :: BState -> (Int, BState)
rankboard = rules . (,) 0

-- Placing Strategy --

center :: Pos
center = (3, 3)

dist :: Pos -> Pos -> Int
dist (x, y) (x', y') 
  = abs (x' - x) + abs (y' - y)

pair :: a -> a -> [a]
pair x y = [x, y]

determineNewPlayerPos :: [Pos] -> Player
determineNewPlayerPos ps 
  = minimumBy (compare `on` (\[p1, p2] 
      -> dist p1 center + dist p2 center)) 
        $ filter (\[p1, p2] 
          -> p1 /= p2 && p1 < p2
          && dist p1 p2 == 3) 
          $ pair <$> ps <*> ps

