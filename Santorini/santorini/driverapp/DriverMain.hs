{-# LANGUAGE DeriveGeneric #-}

module Main where

-- import Driver
import System.Environment
import System.Process.Typed
import System.IO
import GHC.IO.Handle
import GHC.Generics
import Data.Aeson
import Data.Maybe
import Control.Monad
import Data.ByteString.Lazy.Internal                   (unpackChars, packChars)

type Player = (Handle, Handle, Int)

data GB = GB
  { players   :: [[[Int]]]
  , spaces    :: [[Int]]
  , turn      :: Int
  } deriving (Generic, Show)
instance ToJSON GB 
instance FromJSON GB 

initialboard :: GB
initialboard = GB
  { players = []
  , spaces  = replicate 5 $ replicate 5 0
  , turn    = 0 }

inbounds :: [Int] -> Bool
inbounds [x, y] = 1 <= x && x <= 5 && 1 <= y && y <= 5
inbounds _      = False

unique :: Eq a => [a] -> Bool
unique []     = True
unique (n:ns) = notElem n ns && unique ns

isValid :: GB -> Bool
isValid GB{players=ps, spaces=bs, turn=t} 
  = all (<5) (concat bs)
  -- Check player positions
  && all inbounds (concat ps)
  && unique (concat ps)

playerWon :: GB -> Bool
playerWon GB{players=ps, spaces=bs} 
  = undefined

initValid :: GB -> Bool
initValid = undefined

sendAndRead :: Player -> GB -> IO GB
sendAndRead (hin, hout, _) gb = do
  hPutStrLn hin . unpackChars . encode $ gb
  hGetLine hout >>= return . maybe initialboard id . decode . packChars

-- For the rest of the gameplay
play' :: Player -> Player -> GB -> IO Int
play' p1 p2 gb = do
  gb' <- sendAndRead p1 gb
  -- TODO insert validity and winning check logic
  play' p2 p1 gb'

-- For initial player adding
play :: Player -> Player -> IO Int
play p1 p2 = do
  gb <- sendAndRead p1 initialboard
  -- TODO logic to check validity
  gb' <- sendAndRead p2 gb
  -- TODO logic to check validity
  play' p1 p2 gb'

main :: IO ()
main = do
  args <- getArgs
  case args of
    [a1, a2] -> do
      let p1' = setStdin createPipe
              $ setStdout createPipe
              $ shell a1
      let p2' = setStdin createPipe
              $ setStdout createPipe
              $ shell a2
      w <- withProcessWait_ p1' $ \p1 -> do
        withProcessWait_ p2' $ \p2 -> do
          hSetBuffering (getStdin p1)  LineBuffering
          hSetBuffering (getStdout p1) LineBuffering
          hSetBuffering (getStdin p2)  LineBuffering
          hSetBuffering (getStdout p2) LineBuffering
          play (getStdin p1, getStdout p1, 1) (getStdin p2, getStdout p2, 2)
      if w == 1 then
        putStrLn "Player 1 wins!"
      else putStrLn "Player 2 wins!"
    _ -> error "USAGE: driver <exec1> <exec2>"

