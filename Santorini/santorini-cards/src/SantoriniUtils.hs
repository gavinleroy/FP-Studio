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

-- testgb1 :: GameBoard
-- testgb1 = 
--   ( [[(1, 1), (3, 2)]]
--   , Matrix.fromLists 
--     $ replicate 5 
--     $ replicate 5 0
--   , 1 )

-- testgb2 :: GameBoard
-- testgb2 = 
--   ( [[(2, 3), (4, 4)], [(2, 5), (3, 5)]]
--   , Matrix.fromLists
--     [ [0, 0, 0, 0, 2]
--     , [1, 1, 2, 0, 0]
--     , [1, 0, 0, 3, 0]
--     , [0, 0, 3, 0, 0]
--     , [0, 0, 0, 1, 4] ]
--   , 18 )

-- testgb3 :: GameBoard
-- testgb3 = 
--   ( [[(1, 1), (1, 2)], [(1, 3), (1, 4)]]
--   , Matrix.fromLists 
--     $ replicate 5 
--     $ replicate 5 0
--   , 0 )

-- testgb4 :: GameBoard
-- testgb4 = 
--   ( [[(1, 1), (1, 2)], [(2, 1), (2, 2)]]
--   , Matrix.fromLists
--     [ [0, 0, 4, 0, 2]
--     , [1, 1, 4, 0, 0]
--     , [4, 4, 4, 3, 0]
--     , [0, 0, 3, 0, 0]
--     , [0, 0, 0, 1, 4] ]
--   , 18 )

testmatr :: Matrix Int
testmatr = Matrix.fromLists
  [ [0, 3, 0, 3, 2]
  , [2, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0] ]

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

neighbors :: Pos -> [Pos]
neighbors (x, y)
  = [(x + x', y + y') 
  | x' <- [-1..1]
  , y' <- [-1..1]
  , (x' /= 0 || y' /= 0) 
  && inbounds (x + x', y + y')]

bNeighbors' :: Matrix Int -> Pos -> [Pos] -> [Pos]
bNeighbors' m p op
  = flip (\\) op
  $ filter ((< 4) 
    . flip getPos m) 
  . neighbors $ p

bNeighbors :: GameBoard -> [Pos]
bNeighbors gb@GB{spaces}
  = bNeighbors' spaces p1 (p2:op)
  where
    [p1, p2] = myplayer gb
    op       = opplayer gb

mNeighbors' :: Matrix Int -> Pos -> [Pos] -> [Pos]
mNeighbors' m p
  = filter ((< getPos p m + 2) 
    . flip getPos m ) 
  . bNeighbors' m p

mNeighbors :: GameBoard -> [Pos]
mNeighbors gb@GB{spaces}
  = mNeighbors' spaces p1 (p2:op)
  where 
    [p1, p2] = myplayer gb
    op       = opplayer gb

occupiedNeighborsP1 :: GameBoard -> [Pos]
occupiedNeighborsP1 gb@GB{spaces}
  = mNeighbors' spaces p1 p2 `intersect` opplayer gb
  where (p1:p2) = myplayer gb

-- Using Neighbor Helpers with BState --
-- mNeighborsXXX :: GameBoard -> [Pos]
mNeighborsP1  = mNeighbors
mNeighborsP2  = mNeighbors . swapMyPlayerPos
mNeighborsOP1 = mNeighbors . swapPlayers
mNeighborsOP2 = mNeighbors . swapMyPlayerPos . swapPlayers

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
isWin _ gb@GB{spaces}
  = getPos p1 spaces  == 3 
  || getPos p2 spaces == 3
  || null ((++)
      (take 1 (mNeighborsOP1 gb)) 
      (take 1 (mNeighborsOP2 gb)))
  where [p1,p2] = myplayer gb

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

