{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

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
  , basicmove
  , apollomove
  -- , getplayerpos
  -- , testgb
  , basicbuild
  ) where

import           Control.Monad                  (join)
import           Data.DList                     (DList)
import           Data.Function                  (on)
import           Data.List                      ((\\), maximumBy)
import           Data.Matrix                    (Matrix)
import           GHC.Generics
import           SantoriniDefs
import           SantoriniUtils
import           PlayerStrategy
import qualified Data.Matrix as Matrix

type PAction = Action GameBoard

-- BUILDING --

basicbuild :: PAction
basicbuild = Action $ \gb -> 
  let expandSpaces = \gb' -> (newSpaces, map (incPos $ spaces gb') (bNeighbors gb'))
  in nextS $ expandS expandSpaces gb

atlasbuild :: PAction
atlasbuild = Action $ \gb ->
  undefined

demeterbuild :: PAction
demeterbuild = Action $ \gb ->
  undefined

hephastusbuild :: PAction
hephastusbuild = Action $ \gb ->
  undefined

prometheusbuild :: PAction
prometheusbuild = Action $ \gb ->
  undefined

-- MOVING --

basicmove' :: State GameBoard -> State GameBoard
basicmove' = applyS [p1f, p2f]
  where
    p1f = (,) newMyPlayerPos . getplayerpos
    p2f = p1f . swapMyPlayerPos
    getplayerpos :: GameBoard -> [[Pos]]
    getplayerpos gb' = map 
      (\x_ -> 
        (\x y -> 
          [x, y]) x_ ((!! 1) $ myplayer gb')) 
      (mNeighborsP1 gb') -- [Pos]

basicmove :: PAction
basicmove = Action $ \sgb -> 
  let sgb' = basicmove' sgb
  in if any isWin sgb' 
  then exitS isWin sgb' 
  else nextS sgb' 

apolloswap :: GameBoard -> Pos -> GameBoard
apolloswap GB
  { players = 
    [ Player {card=c1, tokens=[p1, p2]}
    , Player {card=c2, tokens=[op1, op2]} ]
  , ..} p
  | p == op1 = GB {players=
    [ Player{card=c1, tokens=[op1, p2]}
    , Player{card=c2, tokens=[p1, op2]} ], ..}
  | p == op2 = GB {players=
    [ Player{card=c1, tokens=[op2, p2]}
    , Player{card=c2, tokens=[op1, p1]} ], ..}
  | otherwise = error "misuse of apolloswap"

apollomove :: PAction
apollomove = Action $ \sgb ->
  let sgb' = basicmove' sgb
      swapOpPos = \gb' -> (apolloswap, occupiedNeighborsP1 gb') 
      sgb'' = fuseS sgb' $ expandS swapOpPos sgb
  in if any isWin sgb'' 
  then exitS isWin sgb'' 
  else nextS sgb'' 

-- let expandSpaces = \gb' -> (newSpaces, map (incPos $ spaces gb') (bNeighbors gb'))
-- in nextS $ expandS expandSpaces gb

artemismove :: PAction
artemismove = Action $ \gb ->
  undefined

minotaurmove :: PAction
minotaurmove = Action $ \gb ->
  undefined

prometheusmove :: PAction
prometheusmove = Action $ \gb ->
  undefined

-- ADD PLAYER INTERFACE --

addPlayer :: Players -> Players
addPlayer [] = error "received empty list"
addPlayer [PrePlayer{card}, op@PrePlayer{}] 
  = [ op , Player { card
    , tokens = determineNewPlayerPos boardPositions } ]
addPlayer [PrePlayer{card}, op@Player{tokens}] 
  = [ op , Player { card
    , tokens = determineNewPlayerPos (boardPositions \\ tokens) } ]
addPlayer [Player{}, Player{}] = error "received two players during setup"

-- ENTRY POINTS --

terminate :: DList GameBoard -> GameBoard
terminate 
  = snd 
  . maximumBy (compare `on` fst) 
  . fmap rankboard

playerturn :: [PAction] -> GameBoard -> GameBoard
playerturn ks = nextS . state ks terminate

initplayer :: Players -> Players
initplayer = addPlayer

-- TEST GB --

testgb = GB
    { players = 
      [ Player{ card = "Artemis", tokens = [(2, 3), (4, 4)] }
      , Player{ card = "Prometheus", tokens = [(2, 5), (3, 5)] } ]
    , spaces = Matrix.fromLists [[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]]
    , turn = 18 }

