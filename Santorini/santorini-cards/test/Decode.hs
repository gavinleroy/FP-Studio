module Decode 
  ( decodeTestList
  ) where

import Test.HUnit
import TestUtils
import SantoriniDefs

decodeTestList = TestList 
  [ TestLabel "decode-test1" test1
  , TestLabel "decode-test2" test2
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

