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
import           Data.HashMap.Internal                     (keys)
import           Data.Matrix                               (Matrix)
import           Relude.Extra.Tuple                        (fmapToSnd)
import qualified Data.DList  as DList 
import qualified Data.Matrix as Matrix

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
data GameBoard = GameBoard
  { players :: [Player]
  , spaces  :: Matrix Height
  , turn    :: Turn 
  } deriving (Eq, Show)

instance ToJSON GameBoard where
  toJSON GameBoard{players, spaces=sps, turn} = object
    [ "players" .= players
    , "spaces"  .= Matrix.toLists sps
    , "turn"    .= turn ]

instance FromJSON GameBoard where
  parseJSON = withObject "gameboard" $ \o -> do
    players <- o .: "players"
    sps <- o .: "spaces"
    let spaces = Matrix.fromLists sps
    turn <- o .: "turn"
    return GameBoard{..}

-- NOTE the first player in the tuple has the turn
type BState = (Player, Board, Player)

swapfst :: BState -> Player -> BState
swapfst (p,b,op) p' = (p',b,op)

swapsnd :: BState -> Board -> BState
swapsnd (p,b,op) b' = (p,b',op)

swaptrd :: BState -> Player -> BState
swaptrd (p,b,op) op' = (p,b,op')

newtype Action a = Action (State a -> [a])

newtype State a = ST ([Action a], DList a)

-- instance Functor State where
--   fmap f (ST (_, dlist)) = ST ([], fmap f dlist)
instance Foldable State where
  foldMap f (ST (_, dlist)) = foldMap f dlist
  foldr f z (ST (_, dlist)) = foldr f z dlist

state :: [Action a] -> a -> State a
state cs a = ST (cs, DList.singleton a)

getstateS :: State a -> DList a
getstateS (ST s) = snd s

minBy :: Ord b => (a -> b) -> a -> a -> a
minBy f a1 a2
  | f a1 < f a2 = a1
  | otherwise   = a2

-- The shorter cont stack is chosen in case of fast return
fuseS :: State a -> State a -> State a
fuseS (ST (cs1, as)) (ST (cs2, bs)) 
  = ST (cs, as `DList.append` bs)
  where cs = minBy length cs1 cs2

mapS :: (a -> a) -> State a -> State a
mapS f (ST (cs, ls)) 
  = ST (cs, DList.map f ls)

mapZip :: Functor f => (a -> b) -> f a -> f (a, b)
mapZip  = fmapToSnd

expandS :: (b -> (b -> a -> b, [a])) -> State b -> State b
expandS g (ST (cs, bs)) 
  = ST $ (,) cs
  $ DList.concat 
  $ DList.toList
  $ (\(bs', (g', as)) ->
      fmap (g' bs') (DList.fromList as)) 
  <$> mapZip g bs

nextS :: State a -> [a]
nextS (ST ((Action f) : cs, as))   
  = f $ ST (cs, as)
nextS (ST ([], as)) 
  = DList.toList as

exitS :: State a -> [a]
exitS (ST (_, as)) = DList.toList as

