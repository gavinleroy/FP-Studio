{-# LANGUAGE DeriveGeneric #-}

{-
 - Gavin Gray u1040250
 - University of Utah
 - Spring 2021, CS 6963
 - Assignment 2, Santorini
 - -}

module Main where

import System.IO
import Data.Aeson
import Data.Maybe
import Data.Matrix                            (fromLists, toLists)
import GHC.Generics
import Data.Functor                           ((<&>))
import Data.ByteString.Lazy.Internal          (unpackChars, packChars)

import qualified Player as P

data GB = GB
  { players :: [[[Int]]]
  , spaces  :: [[Int]]
  , turn    :: Int 
  } deriving (Generic, Show)
instance ToJSON GB 
instance FromJSON GB 

-- Player Cont Types --
basicI :: [P.Move]
basicI = [P.idM]

basicM :: [P.Move]
basicM = [P.move, P.idM]

-- TODO get rid of this and handle stuff on your own
initialboard :: GB
initialboard = GB
  { players = []
  , spaces  = replicate 5 $ replicate 5 0
  , turn    = 0 }

lt :: [a] -> (a, a)
lt [x, y] = (x, y)

lf :: (a, a) -> [a]
lf (x, y) = [x, y]

convert2 :: GB -> P.GameBoard
convert2 GB{players=ps, spaces=sp, turn=t} =
  ( P.chunksOf 2 . map lt . concat $ ps
  , fromLists sp
  , t)

convertF :: P.GameBoard -> GB
convertF (ps, sp, t) = GB
  { players = P.chunksOf 2 . map lf . concat $ ps
  , spaces = toLists sp 
  , turn = t }

doAct :: (P.GameBoard -> P.GameBoard) -> IO ()
doAct f = readJSONBoard >>= printJSONBoard . doPlay f 

doPlay :: (P.GameBoard -> P.GameBoard) -> GB -> GB
doPlay f = convertF . f . convert2

readJSONBoard :: IO GB
readJSONBoard
  = getLine <&> 
  fromMaybe (error "player got invalid board")
  . decode 
  . packChars

printJSONBoard :: GB -> IO ()
printJSONBoard gb
  = sequence_ 
  [ putStrLn . unpackChars . encode $ gb
  , hFlush stdout ]

play :: [P.Move] -> IO ()
play cs = sequence_ [ doAct (P.turn cs) , play cs ]

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin LineBuffering
  sequence_ [ doAct (P.initplayer basicI), play basicM ]
  
