module Decode 
  ( decodeTestList
  ) where

import Test.HUnit
import TestUtils
import SantoriniDefs

import qualified Data.Matrix as Matrix

decodeTestList = TestList 
  [ TestLabel "decode-test1" test1
  , TestLabel "decode-test2" test2
  , TestLabel "decode-test3" test3
  ]

test1_inp = "[{\"card\":\"Artemis\"},{\"card\":\"Prometheus\"}]"
test1_out = [PrePlayer { card = "Artemis" }, PrePlayer{ card = "Prometheus" }]
test1 = TestCase (assertEqual 
  "Decoding 2 PrePlayers" 
  test1_out 
  (json_obj test1_inp))

test2_inp = "[{\"card\":\"Prometheus\"}, {\"card\":\"Artemis\",\"tokens\":[[2,3],[4,4]]}]"
test2_out = 
  [ PrePlayer { card = "Prometheus" }
  , Player{ card = "Artemis", tokens = [(2, 3), (4, 4)] } ]
test2 = TestCase (assertEqual 
  "Decoding 1 PrePlayer and 1 Player" 
  test2_out 
  (json_obj test2_inp))

test3_inp = "{\"players\": \
  \ [{\"card\":\"Artemis\", \"tokens\":[[2,3],[4,4]]}, \
  \ {\"card\":\"Prometheus\",\"tokens\":[[2,5],[3,5]]}], \
  \ \"spaces\":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]], \
  \ \"turn\":18}"
test3_out = GB
    { players = 
      [ Player{ card = "Artemis", tokens = [(2, 3), (4, 4)] }
      , Player{ card = "Prometheus", tokens = [(2, 5), (3, 5)] } ]
    , spaces = Matrix.fromLists [[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]]
    , turn = 18 }
test3 = TestCase (assertEqual 
  "Decoding GameBoard"
  test3_out 
  (json_obj test3_inp))

