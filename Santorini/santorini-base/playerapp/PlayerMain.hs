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

newtype Player = Player
  { ps :: [(Int, Int)]
  } deriving (Generic, Show)

data GB = GB
  { players :: [[[Int]]]
  , spaces  :: [[Int]]
  , turn    :: Int 
  } deriving (Generic, Show)
instance ToJSON GB 
instance FromJSON GB 

-- Player Cont Types --
basicaction :: [P.PAction]
basicaction = [P.move, P.build]

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

doAct :: (ToJSON b, FromJSON b) 
  => (b -> a) -> (a -> b) -> (a -> a) 
  -> IO ()
doAct c2 cf f = readOBJ >>= printOBJ . doPlay c2 cf f 

doPlay :: (b -> a) -> (a -> b) 
  -> (a -> a) -> b -> b
doPlay c2 cf f = cf . f . c2

readOBJ :: FromJSON a => IO a
readOBJ
  = getLine <&> 
  fromMaybe (error "invalid input")
  . decode 
  . packChars -- (String -> ByteString)

printOBJ :: ToJSON a => a -> IO ()
printOBJ x 
  = (putStrLn . unpackChars . encode $ x)
  >> hFlush stdout 

play :: [P.PAction] -> IO ()
play cs 
  = doAct convert2 convertF (P.turn cs) 
  >> play cs

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin LineBuffering
  doAct id id P.initplayer
  >> play basicaction
  
