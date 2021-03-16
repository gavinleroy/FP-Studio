{-
 - Gavin Gray, SP 21
 - University of Utah
 - CS 6963, FP Studio
 - -}

module Main where

import           System.Environment
import           System.Process                  (readProcessWithExitCode)
import           System.Exit                     (ExitCode)
import           Control.Monad
import           Data.List.NonEmpty hiding (last)
import           Text.Regex.Posix
import qualified Data.Text as Tx

winlineregex :: String
winlineregex = "(Win by|No move possible for) (1|2)"

cardlist :: [String]
cardlist = [ "Apollo"    
           , "Artemis"
           , "Atlas"     
           , "Demeter"   
           , "Hephastus" 
           , "Minotaur"  
           , "Pan"       
           , "Prometheus" ]

callCmd :: NonEmpty String -> IO (ExitCode, String, String)
callCmd (cmd :| args) = readProcessWithExitCode cmd args stdIn
  where stdIn = ""

runtests :: String -> String -> String -> [(String, String)] -> IO ()
runtests _ _ _ [] = return ()
runtests r p1 p2 ((a, b) : cs) = do
  putStr $ a ++ " vs " ++ b ++ " ~> "
  (_, stdout, stderr) <-
    callCmd $ r :| ["--cards", a, b, p1, p2]
  let llerr = last . lines $ stderr
  let llout = last . lines $ stdout
  if llerr =~ winlineregex || llout =~ llout
  then putStrLn "good"  >> runtests r p1 p2 cs
  else putStrLn "error" >> error stderr

main :: IO ()
main = do
  [r, p1, p2] <- getArgs
  runtests r p1 p2 
    [(x, y) | x <- cardlist, y <- cardlist, x /= y]
  putStrLn "no bad moves :)"

