{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

{- Gavin Gray u1040250
 - University of Utah
 - Spring 2021, CS 6963
 - Assignment 2, Santorini
 - -}

module SantoriniUtils where

import           Data.Char                       (intToDigit)
import           Data.Matrix                     (Matrix)
import           Data.List                       ((\\), intersect)
import qualified Data.Matrix as Matrix

import SantoriniDefs

-- Utilities --

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n ns = take n ns : chunksOf n (drop n ns)

inbounds :: Pos -> Bool
inbounds (x, y) 
  = 0 < x 
  && x < 6 
  && 0 < y 
  && y < 6

boardPositions :: [Pos]
boardPositions 
  = (,) 
  <$> [1..5] 
  <*> [1..5]

getPos :: Pos -> Matrix a -> a
getPos = uncurry Matrix.unsafeGet

setPos :: a -> Pos -> Matrix a -> Matrix a
setPos = Matrix.unsafeSet

incNPos :: Num a => a -> Matrix a -> Pos -> Matrix a
incNPos n m p = setPos ((+n) $ getPos p m) p m 

incPos :: Num a => Matrix a -> Pos -> Matrix a
incPos = incNPos 1

capPos :: Num a => Matrix a -> Pos -> Matrix a
capPos m p = setPos 4 p m 

-- Neighbor Helpers --

neighbors :: Int -> Pos -> [Pos]
neighbors d (x, y)
  = [(x + x', y + y') 
  | x' <- [-d..d]
  , y' <- [-d..d]
  , (x' /= 0 || y' /= 0) 
  && inbounds (x + x', y + y')]

bNeighbors' :: Int -> Matrix Int -> Pos -> [Pos] -> [Pos]
bNeighbors' d m p op
  = flip (\\) op
  $ filter ((< 4) 
    . flip getPos m) 
  . neighbors d $ p

bNeighbors :: GameBoard -> [Pos]
bNeighbors gb@GB{spaces}
  = bNeighbors' 1 spaces p1 (p2:op)
  where
    [p1, p2] = myplayer gb
    op       = opplayer gb

mNeighbors' :: Int -> Matrix Int -> Pos -> [Pos] -> [Pos]
mNeighbors' d m p
  = filter ((< getPos p m + 2) 
    . flip getPos m ) 
  . bNeighbors' d m p

mNeighborsN :: Int -> GameBoard -> [Pos]
mNeighborsN d gb@GB{spaces}
  = mNeighbors' d spaces p1 (p2:op)
  where 
    [p1, p2] = myplayer gb
    op       = opplayer gb

mNeighbors :: GameBoard -> [Pos]
mNeighbors = mNeighborsN 1

occupiedNeighborsP1 :: GameBoard -> [Pos]
occupiedNeighborsP1 gb@GB{spaces}
  = mNeighbors' 1 spaces p1 p2 `intersect` opplayer gb
  where (p1:p2) = myplayer gb

-- Using Neighbor Helpers with BState --
-- mNeighborsXXX :: GameBoard -> [Pos]
mNeighborsP1  = mNeighbors 
mNeighborsP2  = mNeighbors . swapMyPlayerPos
mNeighborsOP1 = mNeighbors . swapPlayers
mNeighborsOP2 = mNeighbors . swapMyPlayerPos . swapPlayers
-- Optional helpers for a move distance of d
mNeighborsP1N    = mNeighborsN
mNeighborsP2N  d = mNeighborsN d . swapMyPlayerPos
mNeighborsOP1N d = mNeighborsN d . swapPlayers
mNeighborsOP2N d = mNeighborsN d . swapMyPlayerPos . swapPlayers

-- GameBoard Utilities --

myplayer :: GameBoard -> [Pos]
myplayer GB{players= [ Player{ tokens } , _ ]} = tokens

opplayer :: GameBoard -> [Pos]
opplayer GB{players= [ _, Player{ tokens } ]} = tokens

swapPlayers :: GameBoard -> GameBoard
swapPlayers GB{ players = [p1, p2], .. } = GB { players = [p2, p1], .. }

newMyPlayerPos :: GameBoard -> [Pos] -> GameBoard
newMyPlayerPos GB{ players = [ Player { card, tokens } , op] , ..} newtoks
  = GB { players=[Player{card, tokens=newtoks}, op], .. }

swapMyPlayerPos :: GameBoard -> GameBoard
swapMyPlayerPos GB{ players = [ Player { card, tokens = [p1, p2] } , op] , ..} 
  = GB { players=[Player{card, tokens=[p2, p1]}, op], .. }

newOpPlayerPos :: GameBoard -> [Pos] -> GameBoard
newOpPlayerPos GB{ players = [ p, Player { card, tokens } ] , ..} newtoks 
  = GB { players=[p, Player{card, tokens=newtoks}], .. }

swapOpPlayerPos :: GameBoard -> GameBoard
swapOpPlayerPos = swapPlayers . swapMyPlayerPos . swapPlayers

newSpaces :: GameBoard -> Matrix Height -> GameBoard
newSpaces GB{..} nm = GB{ spaces=nm, .. }

incTurn :: GameBoard -> GameBoard
incTurn GB{turn,..} = GB{turn = turn+1, ..}

mycard :: GameBoard -> String
mycard = card . head . players

-- Strategy Utilities --

isWin :: GameBoard -> GameBoard -> Bool
isWin
  ib@GB -- initial board
  { players = 
    [ Player
      { tokens = ps@[p1, p2] }, _ ]
  , spaces }
  gb@GB -- new position
  { players = 
    [ Player
      { card = mycard
      , tokens = ps'@[p1', p2'] }
    , Player
      { card = opcard
      , tokens = [op1, op2] } ]
  , spaces = spaces' }
  =  (not againstminotaur && p1h' == 3)
  || (not againstminotaur && p2h' == 3)
  || (againstminotaur && p1h' == 3 && p1' `notElem` ps)
  || (againstminotaur && p2h' == 3 && p2' `notElem` ps)
  || (iampan && (p1h - p1h') >= 2)
  || (iampan && (p2h - p2h') >= 2)
  || null ((++)
      (take 1 (mNeighborsOP1 gb)) 
      (take 1 (mNeighborsOP2 gb)))
  where 
    iampan = mycard == "Pan"
    againstminotaur = opcard == "Minotaur"
    p1h  = getPos p1 spaces
    p2h  = getPos p2 spaces
    p1h' = getPos p1' spaces'
    p2h' = getPos p2' spaces'

couldWin :: GameBoard -> Bool
couldWin GB{} -- bs@(p@[p1, p2],m,op@[op1, op2]) 
 = False
  -- = any ((==3) . flip getPos m) 
  --   (foldr (:) 
  --     (mNeighborsP1 bs) 
  --     (mNeighborsP2 bs))
  -- || null ((++)
  --     (take 1 (mNeighborsOP1 bs)) 
  --     (take 1 (mNeighborsOP2 bs)))

couldElevate :: GameBoard -> Bool
couldElevate GB{} -- (p@[p1,p2],m,op)
  = False
  -- = not . null . (++) (f p1 p2) $ f p2 p1
  -- where f p p' =
  --         filter ((> getPos p m) 
  --           . flip getPos m) 
  --         $ mNeighbors' m p (p':op)

-- Position manipulations -- 

padd :: Pos -> Pos -> Pos
padd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

psub :: Pos -> Pos -> Pos
psub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

-- IO GameBoard --

gbshow :: GameBoard -> String
gbshow GB
  { players=[Player{tokens=[p1, p2]}
    , Player{tokens=[p1', p2']}]
  , spaces }
  = show 
  $ Matrix.setElem '#' p2'
  $ Matrix.setElem '#' p1'
  $ Matrix.setElem '#' p2
  $ Matrix.setElem '#' p1
  $ Matrix.fromLists 
  $ map (map intToDigit)
  $ Matrix.toLists spaces

