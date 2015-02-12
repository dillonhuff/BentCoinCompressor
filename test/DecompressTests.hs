module DecompressTests() where

import Data.ByteString
import Test.QuickCheck

import Compress
import Decompress

allDecompressTests =
  quickCheck compressDecompressAreIdentity

compressDecompressAreIdentity bytes =
  (decompress $ compress byteStr) == byteStr
  where
    byteStr = pack bytes
