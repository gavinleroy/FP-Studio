{-# LANGUAGE OverloadedStrings #-}

{-
 - Gavin Gray u1040250
 - University of Utah
 - Spring 2021, CS 6963
 - Assignment 3, Santorini Cards
 - -}

module Main where

import           Data.Aeson
import           Data.ByteString.Lazy.Internal          (unpackChars, packChars)
import           Data.Functor                           ((<&>))
import           GHC.Generics
import           Data.Map                               (Map)
import           Data.Maybe
import           System.IO
import qualified Data.Matrix as Matrix 
import qualified Data.Map    as Map
import qualified Player      as P

-- Player Cont Types -- -- TODO REMOVE
basicaction :: [P.PAction]
basicaction = [P.basicmove, P.basicbuild]

cardmap :: Map String [P.PAction]
cardmap -- TODO create continuations for each card
  = Map.insert "Apollo"     basicaction
  . Map.insert "Artemis"    basicaction
  . Map.insert "Atlas"      basicaction
  . Map.insert "Demeter"    basicaction
  . Map.insert "Hephastus"  basicaction
  . Map.insert "Minotaur"   basicaction
  . Map.insert "Pan"        basicaction
  . Map.insert "Prometheus" basicaction
  $ Map.empty

doAct :: (ToJSON a, FromJSON a) => (a -> a) -> IO ()
doAct f = readOBJ >>= printOBJ . f 

readOBJ :: FromJSON a => IO a
readOBJ
  = getLine <&> 
  fromMaybe (error "invalid input")
  . decode 
  . packChars

printOBJ :: ToJSON a => a -> IO ()
printOBJ x 
  = (putStrLn . unpackChars . encode $ x)
  >> hFlush stdout 

play :: [P.PAction] -> IO ()
play cs 
  = doAct (P.playerturn cs) 
  >> play cs

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin LineBuffering
  doAct P.initplayer
  >> play basicaction
  
