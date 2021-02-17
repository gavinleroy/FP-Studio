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
import Data.Function                           (on)
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

type BState = (Player, Board, Player)
newtype Move = M ([Move] -> BState -> [BState])

appMove :: Move -> ([Move] -> BState -> [BState])
appMove (M f) = f

doneCont :: [Move]
doneCont = [idM]

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

testgb3 :: GameBoard
testgb3 = 
  ( [[(1, 1), (1, 2)], [(1, 3), (1, 4)]]
  , Matrix.fromLists 
    $ replicate 5 
    $ replicate 5 0
  , 0 )

testgb4 :: GameBoard
testgb4 = 
  ( [[(1, 1), (1, 2)], [(2, 1), (2, 2)]]
  , Matrix.fromLists
    [ [0, 0, 4, 0, 2]
    , [1, 1, 4, 0, 0]
    , [4, 4, 4, 3, 0]
    , [0, 0, 3, 0, 0]
    , [0, 0, 0, 1, 4] ]
  , 18 )

testmatr :: Matrix Int
testmatr = Matrix.fromLists
  [ [0, 3, 0, 3, 2]
  , [2, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0] ]

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

bNeighbors :: Matrix Int -> Pos -> [Pos] -> [Pos]
bNeighbors m p op
  = flip (\\) op
  $ filter ((< 4) 
    . flip getPos m) 
  . neighbors $ p

mNeighbors :: Matrix Int -> Pos -> [Pos] -> [Pos]
mNeighbors m p
  = filter ((< getPos p m + 2) 
    . flip getPos m ) 
  . bNeighbors m p

getPos :: Pos -> Matrix a -> a
getPos = uncurry Matrix.unsafeGet

setPos :: a -> Pos -> Matrix a -> Matrix a
setPos = Matrix.unsafeSet

incPos :: Num a => Matrix a -> Pos -> Matrix a
incPos m p = setPos ((+1) $ getPos p m) p m 

isWin :: BState -> Bool
isWin ([p1, p2],m,_) 
  = getPos p1 m == 3 
  || getPos p2 m == 3

-- Moves --

idM :: Move
idM = M (\_ bs -> [bs]) 

build :: Move
build = M (\(c:cs) (p@[p1, p2], mat, op) -> 
    let g = appMove c cs . (p,,op)
        mats' = map (incPos mat) (bNeighbors mat p1 (p2:op)) in 
        concatMap g mats')

move :: Move
move = M (\(c:cs) ([p1, p2], mat, op) -> 
    -- let g = appMove c cs . (, mat,op) . uncurry (:)
    let g = (\p2' p1' -> 
              let bs = ([p1',p2'],mat,op) in
              if isWin bs then [bs]
              else appMove c cs bs)
        p1s = mNeighbors mat p1 (p2:op)
        p2s = mNeighbors mat p2 (p1:op) in
        -- foldr (:) (concatMap (g . (,[p2])) p1s) (concatMap (g . (,[p1])) p2s))
        foldr (:) (concatMap (g p2) p1s) (concatMap (g p1)  p2s))

rankBoard :: BState -> (Int, BState)
rankBoard bs@(_,_,_) 
  | isWin bs  = (2, bs)
  | otherwise = (1, bs)

determineNewPlayerPos :: [Pos] -> Player
determineNewPlayerPos = take 2

addPlayer :: Players -> Players
addPlayer [] = [determineNewPlayerPos boardPositions]
addPlayer ps@[p] = [p, determineNewPlayerPos (boardPositions \\ p)]

-- ENTRY POINTS --

turn :: [Move] -> GameBoard -> GameBoard
turn (c:cs) ([p1, op], matr, t) 
  = ([op, p1'], matr', t + 1)
  where 
    (_, (p1', matr',_)) 
      = maximumBy (compare `on` fst) 
      -- = head
      . map rankBoard
      . appMove c cs $ (p1, matr, op)

initplayer :: Players -> Players
initplayer = addPlayer

-- initplayer :: [Move] -> GameBoard -> GameBoard
-- initplayer cs (ps, b, t) 
--   = (addPlayer ps, b, t + 1) 

-- Testing Functions --

testCont :: [Move] -> GameBoard -> [String]
testCont (c:cs) (p1:op:_, matr,_)
  = map bsshow $ appMove c cs (p1, matr, op)

pCont :: [Move] -> GameBoard -> IO ()
pCont cs = mapM_ putStrLn . testCont cs

testTurn :: [Move] -> GameBoard -> String
testTurn cs = gbshow . turn cs

pTurn :: [Move] -> GameBoard -> IO ()
pTurn cs = putStrLn . testTurn cs

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
bsshow ([p1, p2], mat,_) 
  = show 
  $ Matrix.setElem '#' p2
  $ Matrix.setElem '#' p1
  $ Matrix.fromLists 
  $ map (map intToDigit)
  $ Matrix.toLists mat
