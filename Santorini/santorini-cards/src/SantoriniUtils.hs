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
import           Data.List                       ((\\))
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

flipBS :: BState -> BState
flipBS (p,m,op) = (op,m,p)

flipPlrsGB :: GameBoard -> GameBoard
flipPlrsGB GB{players=[p1, p2], ..} 
  = GB{players=[p2, p1], ..}

swapPPos :: GameBoard -> GameBoard
swapPPos GB{players=[Player{card, tokens=[p1, p2]}, p], ..} 
  = GB{ players = [Player{card, tokens=[p2, p1]}, p], ..}

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

incPos :: Num a => Matrix a -> Pos -> Matrix a
incPos m p = setPos ((+1) $ getPos p m) p m 

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

-- Using Neighbor Helpers with BState --
-- mNeighborsXXX :: GameBoard -> [Pos]
mNeighborsP1  = mNeighbors
mNeighborsP2  = mNeighbors . swapPPos
mNeighborsOP1 = mNeighbors . flipPlrsGB
mNeighborsOP2 = mNeighbors . swapPPos . flipPlrsGB

-- GameBoard Utilities --

myplayer :: GameBoard -> [Pos]
myplayer GB{players= [ Player{ tokens } , _ ]} = tokens

opplayer :: GameBoard -> [Pos]
opplayer GB{players= [ _, Player{ tokens } ]} = tokens

-- Strategy Utilities --

-- isWin :: BState -> Bool
-- isWin bs@([p1, p2],m,_) 
--   = getPos p1 m == 3 
--   || getPos p2 m == 3
--   || null ((++)
--       (take 1 (mNeighborsOP1 bs)) 
--       (take 1 (mNeighborsOP2 bs)))

-- couldWin :: BState -> Bool
-- couldWin bs@(p@[p1, p2],m,op@[op1, op2]) 
--   = any ((==3) . flip getPos m) 
--     (foldr (:) 
--       (mNeighborsP1 bs) 
--       (mNeighborsP2 bs))
--   || null ((++)
--       (take 1 (mNeighborsOP1 bs)) 
--       (take 1 (mNeighborsOP2 bs)))

-- couldElevate :: BState -> Bool
-- couldElevate (p@[p1,p2],m,op)
--   = not . null . (++) (f p1 p2) $ f p2 p1
--   where f p p' =
--           filter ((> getPos p m) 
--             . flip getPos m) 
--           $ mNeighbors' m p (p':op)

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

