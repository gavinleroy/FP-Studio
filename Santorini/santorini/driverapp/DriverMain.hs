{-# LANGUAGE DeriveGeneric #-}

module Main where

-- import Driver
import System.Environment
import System.Process
import System.IO
import GHC.IO.Handle
import GHC.Generics
import Data.Aeson

type Player = (Handle, Handle, Int)

data GameBoard = GameBoard 
  { players   :: [[[Int]]]
  , spaces    :: [[Int]]
  , turn      :: Int
  } deriving (Generic, Show)
instance ToJSON GameBoard 
instance FromJSON GameBoard 

initialboard :: GameBoard
initialboard = GameBoard
  { players = []
  , spaces  = replicate 5 $ replicate 5 0
  , turn    = 0 }

inbounds :: [Int] -> Bool
inbounds [x, y] = 1 <= x && x <= 5 && 1 <= y && y <= 5
inbounds _      = False

unique :: Eq a => [a] -> Bool
unique []     = True
unique (n:ns) = notElem n ns && unique ns

isValid :: GameBoard -> Bool
isValid GameBoard{players=ps, spaces=bs, turn=t} 
  = all (<5) (concat bs)
  -- Check player positions
  && all inbounds (concat ps)
  && unique (concat ps)

playerWon :: GameBoard -> Bool
playerWon GameBoard{players=ps, spaces=bs} 
  = undefined

play' :: Player -> Player -> GameBoard -> IO Int
play' p1@(hin, hout, id1) p2@(_,_,id2) gb = do
  -- TODO insert some actual game logic to modify the board
  play' p2 p1 gb

play :: Player -> Player -> GameBoard -> IO Int
play p1@(hin, hout, id1) p2@(_,_,id2) gb = do
  -- TODO insert some actual game logic to modify the board
  play p2 p1 gb

main :: IO ()
main = do
  args <- getArgs
  case args of
    [a1, a2] -> do
      (Just hin1, Just hout1, _, ph) <- 
        createProcess (shell a1) 
          { std_out = CreatePipe
          , std_in = CreatePipe}
      (Just hin2, Just hout2, _, ph) <- 
        createProcess (shell a2) 
          { std_out = CreatePipe
          , std_in = CreatePipe}
      winner <- play (hin1, hout1, 1) (hin2, hout2, 2) initialboard 
      if winner == 1 then
        putStrLn "Player 1 wins!"
      else putStrLn "Player 2 wins!"
    _        -> error "USAGE: driver <exec1> <exec2>"

