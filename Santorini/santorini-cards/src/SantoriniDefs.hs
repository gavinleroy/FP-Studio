{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

{- Gavin Gray u1040250
 - University of Utah
 - Spring 2021, CS 6963
 - Assignment 3, Santorini Cards
 - -}

module SantoriniDefs where

import           Data.Aeson
import           Data.DList                                (DList)
import           Data.Maybe                                (fromMaybe)
import           Data.Foldable                             (find)
import           Data.HashMap.Internal                     (keys)
import           Data.Matrix                               (Matrix)
import           Relude.Extra.Tuple                        (fmapToSnd)
import qualified Data.DList  as DList 
import qualified Data.Matrix as Matrix

import Prelude hiding (init)

-- Data Definitions --

type Height    = Int
type Turn      = Int
type Pos       = (Int, Int)

data Player = Player
  { card   :: String
  , tokens :: [(Int, Int)] } 
  | PrePlayer { card :: String }
  deriving (Eq, Show)

instance ToJSON Player where
  toJSON Player{..} = object 
    [ "card"   .= card
    , "tokens" .= tokens ]
  toJSON PrePlayer{..} = object 
    [ "card"   .= card ]

instance FromJSON Player where
  parseJSON = withObject "player" $ \o -> do
    if "tokens" `elem` keys o
      then Player <$> o .: "card" <*> o .: "tokens" 
      else PrePlayer <$> o .: "card"

type Players   = [Player]
type Board     = Matrix Height
data GameBoard = GB
  { players :: [Player]
  , spaces  :: Matrix Height
  , turn    :: Turn 
  } deriving (Eq, Show)

instance ToJSON GameBoard where
  toJSON GB{players, spaces=sps, turn} = object
    [ "players" .= players
    , "spaces"  .= Matrix.toLists sps
    , "turn"    .= turn ]

instance FromJSON GameBoard where
  parseJSON = withObject "gameboard" $ \o -> do
    players <- o .: "players"
    sps <- o .: "spaces"
    let spaces = Matrix.fromLists sps
    turn <- o .: "turn"
    return GB{..}

newtype Action a = Action (State a -> a)

data State a = ST
  { kont   :: [Action a]
  , init   :: a
  , states :: DList a 
  , term   :: DList a -> a }

instance Foldable State where
  foldMap f ST{states} = foldMap f states
  foldr f z ST{states} = foldr f z states

state :: [Action a] -> (DList a -> a)  -> a -> State a
state ks f a = ST 
  { kont   = ks
  , init   = a
  , states = DList.singleton a
  , term   = f }

minBy :: Ord b => (a -> b) -> a -> a -> a
minBy f a1 a2
  | f a1 < f a2 = a1
  | otherwise   = a2

applyS :: Eq b => [b -> (b -> a -> b, [a])] -> State b -> State b
applyS fs sb = foldr1 fuseS . map (`expandS` sb) $ fs

-- The shorter cont stack is chosen in case of fast return
-- The first termination function is chosen for simplicity
fuseS :: Eq a => State a -> State a -> State a
fuseS ST{kont=ks1,init=i1,states=as,term} 
      ST{kont=ks2,init=i2,states=bs} 
  | i1 /= i2 
    = error "cannot fuse two states with different initial states"
  | length ks1 /= length ks2 
    = error "cannot fuse two states with different continuation sizes"
  | otherwise = ST 
    { kont    = ks1
    , init    = i1
    , states  = as `DList.append` bs
    , term }

mapS :: (a -> a) -> State a -> State a
mapS f ST{states=ls, ..} 
  = ST {states = DList.map f ls, ..}

mapZip :: Functor f => (a -> b) -> f a -> f (a, b)
mapZip  = fmapToSnd

expandS :: (b -> (b -> a -> b, [a])) -> State b -> State b
expandS g ST {states=bs, ..} 
  = ST { states = 
    DList.concat 
    $ DList.toList
    $ (\(bs', (g', as)) ->
        fmap (g' bs') (DList.fromList as)) 
    <$> mapZip g bs
  , .. }

nextS :: State a -> a
nextS ST{kont=[], states=as, term} 
  = term as
nextS ST{kont = (Action f) : ks, ..} 
  = f ST{ kont=ks, .. }

 -- MUST HAVE AT LEAST ONE ELEMENT

exitIfS :: (a -> a -> Bool) -> State a -> a
exitIfS g sa
  | any f sa = exitS f sa
  | otherwise = nextS sa
  where f = g $ init sa
 
exitS :: (a -> Bool) -> State a -> a
exitS f ST {states} 
  = fromMaybe (error "invalid exit on a false state") 
  $ find f states

