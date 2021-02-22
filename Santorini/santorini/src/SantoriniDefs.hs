{- Gavin Gray u1040250
 - University of Utah
 - Spring 2021, CS 6963
 - Assignment 2, Santorini
 - -}

module SantoriniDefs where

import Data.Matrix                               (Matrix)

-- Data Definitions --

type Pos       = (Int, Int)
type Player    = [Pos]
type Players   = [Player]
type Height    = Int
type Turn      = Int
type Board     = Matrix Height
type GameBoard = (Players, Board, Turn)

-- NOTE the first player in the tuple has the turn
type BState = (Player, Board, Player)
newtype Move = M ([Move] -> BState -> [BState])

appMove :: [Move] -> BState -> [BState]
appMove [] = pure
appMove ((M f) : cs) = f cs

