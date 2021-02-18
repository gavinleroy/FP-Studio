{- Gavin Gray u1040250
 - University of Utah
 - Spring 2021, CS 6963
 - Assignment 2, Santorini
 - -}

module SantoriniDefs where

import Data.Matrix                               (Matrix)

-- Data Definitions --
--
type Pos       = (Int, Int)
type Player    = [Pos]
type Players   = [Player]
type Height    = Int
type Turn      = Int
type Board     = Matrix Height
type GameBoard = (Players, Board, Turn)

type BState = (Player, Board, Player)
newtype Move = M ([Move] -> BState -> [BState])

appMove :: Move -> [Move] -> BState -> [BState]
appMove (M f) = f

