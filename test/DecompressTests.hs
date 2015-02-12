module DecompressTests() where

import Data.ByteString as ByteStr
import Test.QuickCheck

import Compress
import Decompress

allDecompressTests =
  quickCheck compressDecompressAreIdentity

compressDecompressAreIdentity bytes =
  (decompress $ compress byteStr) == byteStr
  where
    byteStr = ByteStr.take 16383 $ pack bytes
