{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

{- Gavin Gray u1040250
 - University of Utah
 - Spring 2021, CS 6963
 - Assignment 2, Santorini
 - -}

module Player 
  ( module SantoriniDefs 
  , initplayer
  , playerturn
  ) where

import           Control.Monad                  (join)
import           Data.DList                     (DList)
import           Data.Function                  (on)
import           Data.List                      ((\\), maximumBy)
import           Data.Matrix                    (Matrix)
import           Data.Maybe                     (fromMaybe)
import           GHC.Generics
import           SantoriniDefs
import           SantoriniUtils
import           PlayerStrategy
import qualified Data.Map    as Map    
import qualified Data.Matrix as Matrix

type PAction = Action GameBoard

lookupCM :: String -> [PAction]
lookupCM
  = fromMaybe [basicmove, basicbuild]
  . flip Map.lookup cardmap

cardmap :: Map.Map String [PAction]
cardmap
  = Map.fromList 
  [ ("Apollo"     , [apollomove     , basicbuild]                 )
  , ("Artemis"    , [artemismove    , basicbuild]                 )
  , ("Atlas"      , [basicmove      , atlasbuild]                 )
  , ("Demeter"    , [basicmove      , demeterbuild]               )
  , ("Hephastus"  , [basicmove      , hephastusbuild]             )
  , ("Minotaur"   , [minotaurmove   , basicbuild]                 )
  , ("Pan"        , [basicmove      , basicbuild]                 )
  , ("Prometheus" , [prometheusbuild, prometheusmove, basicbuild] ) ]

-- BUILDING --

basicbuild' :: State GameBoard -> State GameBoard
basicbuild' = expandS $ \gb' ->
  (newSpaces, map (incPos $ spaces gb') (bNeighbors gb'))

basicbuild :: PAction
basicbuild = Action $ \sgb -> 
  exitIfS isWin $ basicbuild' sgb

atlasbuild :: PAction
atlasbuild = Action $ \sgb ->
  let sgb' = basicbuild' sgb
      expandSpaces = \gb' -> 
        (newSpaces, map (capPos $ spaces gb') (bNeighbors gb'))
  in exitIfS isWin $ fuseS sgb' $ expandS expandSpaces sgb

demeterbuild :: PAction
demeterbuild = Action $ \sgb ->
  let expandSpaces = \gb' ->
        let bs = bNeighbors gb' 
            m = spaces gb' in
            ( newSpaces
            , concatMap (\p -> 
                let m' = incPos m p in
                m' : map (incPos m') (filter (/=p) bs))
              bs )
  in exitIfS isWin $ expandS expandSpaces sgb

hephastusbuild :: PAction
hephastusbuild = Action $ \sgb ->
  let sgb' = basicbuild' sgb
      expandSpaces = \gb' -> 
        ( newSpaces
        , map (incNPos 2 $ spaces gb') 
            (filter (\p -> getPos p (spaces gb') < 2) $ bNeighbors gb')
          ++
          map (incPos $ spaces gb') (bNeighbors gb'))
  in exitIfS isWin $ fuseS sgb' $ expandS expandSpaces sgb

prometheusbuild :: PAction
prometheusbuild = Action $ \sgb ->
  let sgb' = mapS swapMyPlayerPos sgb
      sgb'' =  basicbuild' sgb
      sgb''' = basicbuild' sgb' in
  exitIfS isWin $ foldl1 fuseS [sgb, sgb', sgb'', sgb''']

-- MOVING --

getplayerpos :: (Pos -> Bool) -> GameBoard -> [[Pos]]
getplayerpos valid gb' = map 
  (\x_ -> 
    (\x y -> 
      [x, y]) x_ ((!! 1) $ myplayer gb')) 
  (filter valid $ mNeighborsP1 gb')

p1basicmove' :: (Pos -> Bool) -> State GameBoard -> State GameBoard
p1basicmove' valid 
  = expandS 
  $ (,) newMyPlayerPos 
  . getplayerpos valid

p2basicmove' :: (Pos -> Bool) -> State GameBoard -> State GameBoard
p2basicmove' valid 
  = expandS 
  $ (,) newMyPlayerPos 
  . getplayerpos valid
  . swapMyPlayerPos

basicmove' :: (Pos -> Bool) -> State GameBoard -> State GameBoard
basicmove' valid sgb 
  = fuseS 
    (p1basicmove' valid sgb) 
    (p2basicmove' valid sgb)

basicmove :: PAction
basicmove = Action $ \sgb -> 
  exitIfS isWin $ basicmove' (const True) sgb

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
  let sgb' = basicmove' (const True) sgb
      swapOpPos = \gb' -> (apolloswap, occupiedNeighborsP1 gb') 
  in exitIfS isWin $ fuseS sgb' $ expandS swapOpPos sgb

artemismove :: PAction
artemismove = Action $ \sgb@ST{init=igb} ->
  let originalpos = myplayer igb
      sgb' = basicmove' (const True) sgb
      sgb'' = p1basicmove' (not . flip elem originalpos) sgb'
  in exitIfS isWin $ fuseS sgb' sgb''

minotaurpush :: GameBoard -> Pos -> GameBoard
minotaurpush GB
  { players = 
    [ Player {card = c1, tokens = [p1, p2]}
    , Player {card = c2, tokens = [op1, op2]} ]
  , ..} p 
  | p == op1 = GB { players =
    [ Player{ card = c1, tokens = [p, p2]}
    , Player{ card = c2, tokens = [np, op2]} ], ..}
  | p == op2 = GB {players =
    [ Player{card = c1, tokens = [p, p2]}
    , Player{card = c2, tokens = [op1, np]} ], ..}
  | otherwise = error "invalid use of minotaurpush"
  where np = padd p $ psub p p1

minotaurmove :: PAction
minotaurmove = Action $ \sgb ->
  let sgb' = basicmove' (const True) sgb
      swapOpPos = \gb' -> 
        let filf = \p ->
              let mp = head $ myplayer gb'
                  np = padd p $ psub p mp
                  m = spaces gb'
              in inbounds np                 -- position is on the board
              && (getPos np m < 4)           -- not a capped tower
              && (np `notElem` myplayer gb') -- not occupied by my players
              && (np `notElem` opplayer gb') -- not occupied by opposing players
        in (minotaurpush, filter filf $ occupiedNeighborsP1 gb') 
  in exitIfS isWin $ fuseS sgb' $ expandS swapOpPos sgb
 
-- assume only the player in first position can move --
prometheusmove :: PAction
prometheusmove = Action $ \sgb@ST{init=gb} ->
  let om = spaces gb
      p1f = (,) newMyPlayerPos . getplayerpos
      getplayerpos = \gb'-> 
        let m = spaces gb'
        in map (\x_ -> 
             (\x y -> 
               [x, y]) x_ ((!! 1) $ myplayer gb')) 
           (filter 
             (if om == spaces gb' 
              then const True
              else (\p -> 
                getPos p m <=
                getPos (head $ myplayer gb') m))
           $ mNeighborsP1 gb')
  in exitIfS isWin $ expandS p1f sgb

-- ADD PLAYER INTERFACE --

addPlayer :: Players -> Players
addPlayer [] = error "received empty list in addPlayer"
addPlayer [PrePlayer{card}, op@PrePlayer{}] 
  = [ op , Player { card
    , tokens = determineNewPlayerPos boardPositions } ]
addPlayer [PrePlayer{card}, op@Player{tokens}] 
  = [ op , Player { card
    , tokens = determineNewPlayerPos (boardPositions \\ tokens) } ]
addPlayer [Player{}, Player{}] = error "received two players during setup"

terminate :: DList GameBoard -> GameBoard
terminate 
  = snd 
  . maximumBy (compare `on` fst) 
  . fmap rankboard

playerturn' :: [PAction] -> GameBoard -> GameBoard
playerturn' ks 
  = incTurn 
  . swapPlayers 
  . nextS 
  . state ks terminate

-- ENTRY POINTS --

playerturn :: GameBoard -> GameBoard
playerturn gb 
  = playerturn' (lookupCM $ mycard gb) gb

initplayer :: Players -> Players
initplayer = addPlayer

