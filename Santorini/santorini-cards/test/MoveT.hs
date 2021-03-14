module MoveT 
  (moveTestList
  ) where

import Test.HUnit
import TestUtils
import SantoriniDefs
import Player

import qualified Data.Matrix as Matrix

moveTestList = TestList 
  [ TestLabel "move-test1" test1
  ]

test1_inp = GB 
  { players =
    [ Player{ card = "Pan",tokens = [(2,3),(4,5)] }
    , Player{ card = "Minotaur",tokens = [(2,1),(4,2)] } ]
  , spaces = Matrix.fromLists [
    [0,0,0,0,0],
    [1,0,0,0,0],
    [0,1,0,1,1],
    [2,2,0,0,2],
    [1,0,0,2,1]]
  , turn = 14}
test1_out = GB 
  { players =
    [ Player{ card = "Minotaur",tokens = [(2,1),(4,2)] }
    , Player{ card = "Pan",tokens = [(4,4),(2,3)] } ]
  , spaces = Matrix.fromLists [
    [0,0,0,0,0],
    [1,0,0,0,0],
    [0,1,0,1,1],
    [2,2,0,0,2],
    [1,0,0,2,1]]
  , turn = 15}
test1 = TestCase (assertEqual 
  "Winning move Pan" 
  test1_out 
  (playerturn test1_inp))
