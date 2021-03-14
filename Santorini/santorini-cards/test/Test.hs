
import Test.HUnit
import Decode       (decodeTestList)
import Encode       (encodeTestList)
import MoveT        (moveTestList)

main :: IO ()
main = do
  putStrLn "~~~~~~~~~~~~ Running Decode Tests ~~~~~~~~~~~~>"
  runTestTT decodeTestList
  putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~>"
  putStrLn "~~~~~~~~~~~~ Running Encode Tests ~~~~~~~~~~~~>"
  runTestTT encodeTestList
  putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~>"
  putStrLn "~~~~~~~~~~~~~ Running Move Tests ~~~~~~~~~~~~~>"
  runTestTT moveTestList
  putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~>"
  return ()
