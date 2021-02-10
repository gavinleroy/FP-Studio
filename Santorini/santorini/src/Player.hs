{-
 - Gavin Gray u1040250
 - University of Utah
 - Spring 2021, CS 6963
 - Assignment 2, Santorini
 - -}

module Player where

import Data.Matrix                             (Matrix)
import Data.List                               ((\\))

import qualified Data.Matrix as Matrix

-- Data Definitions --
type Pos       = (Int, Int)
type Player    = [Pos]
type Players   = [Player]
type Height    = Int
type Turn      = Int
type Board     = Matrix Height
type GameBoard = (Players, Board, Turn)

-- Utilities --

inbounds :: Pos -> Bool
inbounds (x, y) = 0 < x && x < 6 && 0 < y && y < 6

boardPositions :: [Pos]
boardPositions = (,) <$> [1..5] <*> [1..5]

neighbors :: Pos -> [Pos]
neighbors (x, y)
  = [(x + x', y + y') 
  | x' <- [-1..1]
  , y' <- [-1..1]
  , (x' /= 0 || y' /= 0) 
  && inbounds (x + x', y + y')]

getPos :: Pos -> Matrix a -> a
getPos = uncurry Matrix.unsafeGet

setPos :: a -> Pos -> Matrix a -> Matrix a
setPos = Matrix.unsafeSet

incPos :: Num a => Pos -> Matrix a -> Matrix a
incPos p m = setPos ((+1) $ getPos p m) p m 

-- Board Altering --

fullMove :: GameBoard -> GameBoard
fullMove ([[p1, p2], _], matr, _) = undefined
  where
    n1 = neighbors p1
    n2 = neighbors p2

determineNewPlayerPos :: [Pos] -> Player
determineNewPlayerPos = take 2

addPlayer :: Players -> Players
addPlayer [] 
  = [determineNewPlayerPos boardPositions]
addPlayer plrs@[player]
  = determineNewPlayerPos (boardPositions \\ player) : plrs
addPlayer _ = error "attempt to add more than two players"

