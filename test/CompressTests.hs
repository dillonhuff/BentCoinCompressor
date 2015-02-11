module CompressTests() where

import Data.BitString
import Data.ByteString

import Compress
import TestUtils

allTests = do
  oneBitLocationsTests

oneBitLocationsTests =
  testFunction oneBitLocations oneBitLocationsTestCases

oneBitLocationsTestCases =
  Prelude.map (\(x, y) -> (bitString $ pack x, y))
  [([], (0, [])),
   ([0], (0, [])),
   ([255], (8, [0, 1, 2, 3, 4, 5, 6, 7])),
   ([32, 1, 1, 3], (5, [5, 8, 16, 24, 25]))]
