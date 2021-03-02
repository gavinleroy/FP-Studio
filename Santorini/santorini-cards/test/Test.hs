
import Test.HUnit
import Decode       (decodeTestList)
import Encode       (encodeTestList)

main :: IO ()
main = do
  putStrLn "~~~~~~~~~~~~ Running Decode Tests ~~~~~~~~~~~~>"
  runTestTT decodeTestList
  putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~>"
  putStrLn "~~~~~~~~~~~~ Running Encode Tests ~~~~~~~~~~~~>"
  runTestTT encodeTestList
  putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~>"
  return ()
