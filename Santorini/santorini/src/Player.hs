{-# LANGUAGE TupleSections #-}

{- Gavin Gray u1040250
 - University of Utah
 - Spring 2021, CS 6963
 - Assignment 2, Santorini
 - -}

module Player 
  ( module SantoriniDefs 
  , chunksOf
  , initplayer
  , turn
  , move
  , build
  , idM
  ) where

import            Control.Monad                  (join)
import            Data.Function                  (on)
import            Data.List                      ((\\), maximumBy)
import            Data.Matrix                    (Matrix)
import            GHC.Generics
import            SantoriniDefs
import            SantoriniUtils
import            PlayerStrategy
import qualified Data.Matrix as Matrix

-- Moves --

idM :: Move
idM = M (\_ bs -> [bs]) 

build :: Move
build = M (\cs (p@[p1, p2], mat, op) -> 
    let g = appMove cs . (p,,op)
        mats' = map (incPos mat) (bNeighbors mat p1 (p2:op)) in 
        concatMap g mats')

move :: Move
move = M (\cs ([p1, p2], mat, op) -> 
    let g = (\p2' p1' -> 
              let bs = ([p1',p2'],mat,op) in
              if isWin bs then [bs]
              else appMove cs bs)
        p1s = mNeighbors mat p1 (p2:op)
        p2s = mNeighbors mat p2 (p1:op) in
        foldr (:) (concatMap (g p2) p1s) (concatMap (g p1)  p2s))

addPlayer :: Players -> Players
addPlayer [] = [determineNewPlayerPos boardPositions]
addPlayer ps@[p] = [p, determineNewPlayerPos (boardPositions \\ p)]

-- Entry Points --

turn :: [Move] -> GameBoard -> GameBoard
turn cs ([p1, op], matr, t) 
  = ([op, p1'], matr', t + 1)
  where 
    (_, (p1', matr',_)) 
      = maximumBy (compare `on` fst) 
      . map rankboard
      . appMove cs $ (p1, matr, op)

initplayer :: Players -> Players
initplayer = addPlayer

-- Testing Helpers --

testCont :: [Move] -> GameBoard -> [String]
testCont cs (p1:op:_, matr,_)
  = map bsshow $ appMove cs (p1, matr, op)

pCont :: [Move] -> GameBoard -> IO ()
pCont cs = mapM_ putStrLn . testCont cs

testTurn :: [Move] -> GameBoard -> String
testTurn cs = gbshow . turn cs

pTurn :: [Move] -> GameBoard -> IO ()
pTurn cs = putStrLn . testTurn cs

