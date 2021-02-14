{-# LANGUAGE TupleSections #-}

{- Gavin Gray u1040250
 - University of Utah
 - Spring 2021, CS 6963
 - Assignment 2, Santorini
 - -}

module Player where

import GHC.Generics
import Control.Monad                           (join)
import Data.Matrix                             (Matrix)
import Data.Char                               (intToDigit)
import Data.List                               ((\\), maximumBy)

import qualified Data.Matrix as Matrix

-- Data Definitions --
type Pos       = (Int, Int)
type Player    = [Pos]
type Players   = [Player]
type Height    = Int
type Turn      = Int
type Board     = Matrix Height
type GameBoard = (Players, Board, Turn)

type BState = (Player, Board)
newtype Move = M ([Move] -> BState -> [BState])

appMove :: Move -> ([Move] -> BState -> [BState])
appMove (M f) = f

-- Utilities --

testgb1 :: GameBoard
testgb1 = 
  ( [[(1, 1), (3, 2)]]
  , Matrix.fromLists 
    $ replicate 5 
    $ replicate 5 0
  , 1 )

testgb2 :: GameBoard
testgb2 = 
  ( [[(2, 3), (4, 4)], [(2, 5), (3, 5)]]
  , Matrix.fromLists
    [ [0, 0, 0, 0, 2]
    , [1, 1, 2, 0, 0]
    , [1, 0, 0, 3, 0]
    , [0, 0, 3, 0, 0]
    , [0, 0, 0, 1, 4] ]
  , 18 )

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
boardPositions = (,) <$> [1..5] <*> [1..5]

neighbors :: Pos -> [Pos]
neighbors (x, y)
  = [(x + x', y + y') 
  | x' <- [-1..1]
  , y' <- [-1..1]
  , (x' /= 0 || y' /= 0) 
  && inbounds (x + x', y + y')]

bNeighbors :: Matrix Int -> Pos -> [Pos]
bNeighbors m 
  = filter ((< 4) 
    . flip getPos m) 
  . neighbors

mNeighbors :: Matrix Int -> Pos -> [Pos]
mNeighbors m p 
  = filter ((< getPos p m + 1) 
    . flip getPos m ) 
  . bNeighbors m $ p

getPos :: Pos -> Matrix a -> a
getPos = uncurry Matrix.unsafeGet

setPos :: a -> Pos -> Matrix a -> Matrix a
setPos = Matrix.unsafeSet

incPos :: Num a => Pos -> Matrix a -> Matrix a
incPos p m = setPos ((+1) $ getPos p m) p m 

-- Moves --

idM :: Move
idM = M (\_ bs -> [bs]) 

build :: Move
build = M (\(c:cs) ([p1, p2], mat) -> 
    let g = appMove c cs . (, mat) . uncurry (:)
        p1s= bNeighbors mat p1 in
        concatMap (g . (,[p2])) p1s)

move :: Move
move = M (\(c:cs) ([p1, p2], mat) -> 
    let g = appMove c cs . (, mat) . uncurry (:)
        p1s= mNeighbors mat p1
        p2s= mNeighbors mat p2 in
        foldr (:) (concatMap (g . (,[p2])) p1s) (concatMap (g . (,[p1])) p2s))

rankBoard = undefined

determineNewPlayerPos :: [Pos] -> Player
determineNewPlayerPos = take 2

addPlayer :: Players -> Players
addPlayer [] = [determineNewPlayerPos boardPositions]
addPlayer ps@[p] = determineNewPlayerPos (boardPositions \\ p) : ps

-- ENTRY POINTS --

turn :: [Move] -> GameBoard -> GameBoard
turn (c:cs) ([p1, op@[o1, o2]], matr, t) 
  = ([op, p1'], matr, t + 1)
  where 
    (p1', matr') = maximumBy rankBoard $ appMove c cs (p1, matr)

-- TODO fix this somehow 
initplayer :: [Move] -> GameBoard -> GameBoard
initplayer cs (ps, b, t) 
  = (addPlayer ps, b, t + 1) 

-- Testing Functions --

testCont :: [Move] -> GameBoard -> [String]
testCont (c:cs) (p1:_, matr, _)
  = map bsshow $ appMove c cs (p1, matr)

pCont :: [Move] -> GameBoard -> IO ()
pCont cs = mapM_ putStrLn . testCont cs

-- IO GameBoard --

gbshow :: GameBoard -> String
gbshow ([[p1, p2], [p1', p2']], mat, _) 
  = show 
  $ Matrix.setElem '#' p2'
  $ Matrix.setElem '#' p1'
  $ Matrix.setElem '#' p2
  $ Matrix.setElem '#' p1
  $ Matrix.fromLists 
  $ map (map intToDigit)
  $ Matrix.toLists mat

bsshow :: BState -> String
bsshow ([p1, p2], mat) 
  = show 
  $ Matrix.setElem '#' p2
  $ Matrix.setElem '#' p1
  $ Matrix.fromLists 
  $ map (map intToDigit)
  $ Matrix.toLists mat
