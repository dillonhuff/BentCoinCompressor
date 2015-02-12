module CompressTests() where

import Data.BitString as BitStr
import Data.ByteString as ByteStr

import Compress
import TestUtils

allTests = do
  oneBitLocationsTests
  truncateTo14BitsTests
  compressTests

oneBitLocationsTests =
  testFunction oneBitLocations oneBitLocationsTestCases

oneBitLocationsTestCases =
  Prelude.map (\(x, y) -> (bitString $ pack x, y))
  [([], (0, [])),
   ([0], (0, [])),
   ([255], (8, [0, 1, 2, 3, 4, 5, 6, 7])),
   ([32, 1, 1, 3], (5, [5, 8, 16, 24, 25]))]

truncateTo14BitsTests =
  testFunction truncateTo14Bits truncateTo14BitsTestCases


truncateTo14BitsTestCases =
  [(16384, BitStr.take 14 $ bitString $ pack [0, 0]),
   (0, BitStr.take 14 $ bitString $ pack [0, 0]),
   (1, BitStr.take 14 $ bitString $ pack [1, 0]),
   (255, BitStr.take 14 $ bitString $ pack [255, 0]),
   (1024, BitStr.take 14 $ bitString $ pack [0, 4])]

compressTests =
  testFunction compress compressTestCases

compressTestCases =
  [(ByteStr.empty, pack [0]),
   (pack [1, 0, 0], pack [1, 0, 0, 0]),
   (pack [2, 0, 0], pack [1, 64, 0, 0]),
   (pack [0, 1, 33], pack [3, 0, 2, 0, 1, 84, 0])]
