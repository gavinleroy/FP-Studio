{-# LANGUAGE NamedFieldPuns #-}
-- {-# LANGUAGE RecordWildCards #-}

{- Gavin Gray u1040250
 - University of Utah
 - Spring 2021, CS 6963
 - Assignment 2, Santorini
 - -}

module Player 
  ( module SantoriniDefs 
  , PAction
  , chunksOf
  , initplayer
  , playerturn
  , move
  , build
  ) where

import           Control.Monad                  (join)
import           Data.Function                  (on)
import           Data.List                      ((\\), maximumBy)
import           Data.Matrix                    (Matrix)
import           GHC.Generics
import           SantoriniDefs
import           SantoriniUtils
import           PlayerStrategy
import qualified Data.Matrix as Matrix

type PAction = Action BState

-- Moves --

build :: PAction
build = undefined
-- build = Action $ \bstate -> 
--   let expandbmats = \bs@(_,mat,_) -> (swapsnd, map (incPos mat) (bNeighbors bs))
--   in nextS $ expandS expandbmats bstate

move :: PAction
move = undefined
-- move = Action $ \bstate -> 
--   let p1f = (,) swapfst . getplayerpos
--       p2f = (,) swapfst . getplayerpos . swapPPos
--       -- previously above this was the body of `getplayerpos` but hlint suggested the
--       -- above as more conventional. :shrug:
--       -- zipWith (\x y -> [x, y]) (mNeighbors bs') (repeat p2)
--       getplayerpos bs'@([_, p2],_,_) = map (\x_ -> (\x y -> [x, y]) x_ p2) (mNeighborsP1 bs')
--       fused = fuseS (expandS p1f bstate) (expandS p2f bstate)
--       in if any isWin fused then exitS fused else nextS fused 

addPlayer :: Players -> Players
addPlayer [] = error "received empty list"
addPlayer [PrePlayer{card}, op@PrePlayer{}] 
  = [ op
  , Player
    { card
    , tokens = determineNewPlayerPos boardPositions } ]

addPlayer [PrePlayer{card}, op@Player{tokens}] 
  = [ op
  , Player
    { card
    , tokens = determineNewPlayerPos (boardPositions \\ tokens) } ]
addPlayer [Player{}, Player{}] = error "received two players during setup"


-- Entry Points --

playerturn :: [PAction] -> GameBoard -> GameBoard
playerturn cs = id
-- turn cs ([p, op], matr, t) 
--   = ([op, p'], matr', t + 1)
--   where 
--     (_, (p', matr',_)) 
--       = maximumBy (compare `on` fst) 
--       . map rankboard
--       . nextS 
--       . state cs 
--       $ (p, matr, op)

initplayer :: Players -> Players
initplayer = addPlayer

-- Testing Helpers --

-- testCont :: [PAction] -> GameBoard -> [String]
-- testCont cs (p1:op:_, matr,_)
--   = map bsshow $ nextS $ state cs (p1, matr, op)

-- pCont :: [PAction] -> GameBoard -> IO ()
-- pCont cs = mapM_ putStrLn . testCont cs

-- testTurn :: [PAction] -> GameBoard -> String
-- testTurn cs = gbshow . turn cs

-- pTurn :: [PAction] -> GameBoard -> IO ()
-- pTurn cs = putStrLn . testTurn cs

