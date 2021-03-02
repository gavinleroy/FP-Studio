{-
 - Gavin Gray u1040250
 - University of Utah
 - Spring 2021, CS 6963
 - Assignment 2, Santorini
 - -}

module TestUtils 
  ( module SantoriniDefs 
  , obj_json
  , json_obj
  ) where

import Data.Aeson
import Data.ByteString.Lazy.Internal             (unpackChars, packChars)
import Data.Maybe
import SantoriniDefs

obj_json :: ToJSON a => a -> String
obj_json = unpackChars . encode

json_obj :: FromJSON a => String -> a
json_obj = fromMaybe (error "DECODE FAIL") . decode . packChars

