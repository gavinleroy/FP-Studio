module Encode
  ( encodeTestList
  ) where

import Test.HUnit
import TestUtils
import SantoriniDefs

encodeTestList = TestList 
  [ TestLabel "encode-test1" test1
  , TestLabel "encode-test2" test2
  ]

test1_inp = [PrePlayer { card = "Artemis" }, PrePlayer{ card = "Prometheus" }]
test1_out = "[{\"card\":\"Artemis\"},{\"card\":\"Prometheus\"}]"
test1 = TestCase (assertEqual 
  "Decoding 2 PrePlayers" 
  test1_out 
  (obj_json test1_inp))

test2_inp = 
  [ PrePlayer { card = "Prometheus" }
  , Player{ card = "Artemis", tokens = [(2, 3), (4, 4)] } ]
test2_out = "[{\"card\":\"Prometheus\"},{\"card\":\"Artemis\",\"tokens\":[[2,3],[4,4]]}]"
test2 = TestCase (assertEqual 
  "Decoding 1 PrePlayer and 1 Player" 
  test2_out 
  (obj_json test2_inp))

