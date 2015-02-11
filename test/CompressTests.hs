module CompressTests() where

import Data.BitString
import Data.ByteString

import Compress
import TestUtils

allTests = do
  numberOfOneBitsTests

numberOfOneBitsTests =
  testFunction numberOfOneBits numberOfOneBitsTestCases

numberOfOneBitsTestCases =
  Prelude.map (\(x, y) -> (bitString $ pack x, y))
  [([], 0),
   ([0], 0),
   ([255], 8),
   ([32, 1, 1, 3], 5)]
