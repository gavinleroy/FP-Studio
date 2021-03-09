module Encode
  ( encodeTestList
  ) where

import Test.HUnit
import TestUtils
import SantoriniDefs

import qualified Data.Matrix as Matrix

encodeTestList = TestList 
  [ TestLabel "encode-test1" test1
  , TestLabel "encode-test2" test2
  , TestLabel "encode-test3" test3
  ]

test1_inp = [PrePlayer { card = "Artemis" }, PrePlayer{ card = "Prometheus" }]
test1_out = "[{\"card\":\"Artemis\"},{\"card\":\"Prometheus\"}]"
test1 = TestCase (assertEqual 
  "Encoding 2 PrePlayers" 
  test1_out 
  (obj_json test1_inp))

test2_inp = 
  [ PrePlayer { card = "Prometheus" }
  , Player{ card = "Artemis", tokens = [(2, 3), (4, 4)] } ]
test2_out = "[{\"card\":\"Prometheus\"},{\"card\":\"Artemis\",\"tokens\":[[2,3],[4,4]]}]"
test2 = TestCase (assertEqual 
  "Encoding 1 PrePlayer and 1 Player" 
  test2_out 
  (obj_json test2_inp))

test3_inp = GB
    { players = 
      [ Player{ card = "Artemis", tokens = [(2, 3), (4, 4)] }
      , Player{ card = "Prometheus", tokens = [(2, 5), (3, 5)] } ]
    , spaces = Matrix.fromLists [[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]]
    , turn = 18 }
test3_out = "{\"turn\":18,\"spaces\":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]],\"players\":[{\"card\":\"Artemis\",\"tokens\":[[2,3],[4,4]]},{\"card\":\"Prometheus\",\"tokens\":[[2,5],[3,5]]}]}"
test3 = TestCase (assertEqual 
  "Encoding GameBoard"
  test3_out 
  (obj_json test3_inp))

