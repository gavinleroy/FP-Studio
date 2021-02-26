{- Gavin Gray u1040250
 - University of Utah
 - Spring 2021, CS 6963
 - Assignment 2, Santorini
 - -}

module SantoriniDefs where

import           Relude.Extra.Tuple                        (fmapToSnd)
import           Data.Matrix                               (Matrix)
import           Data.DList                                (DList)
import qualified Data.DList as DList 

-- Data Definitions --

type Height    = Int
type Turn      = Int
type Pos       = (Int, Int)
type Player    = [Pos]
type Players   = [Player]
type Board     = Matrix Height
type GameBoard = (Players, Board, Turn)

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

