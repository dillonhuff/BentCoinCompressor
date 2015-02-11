module Compress(numberOfOneBits) where

import Data.Word
import Data.BitString as BitStr
import Data.ByteString as ByteStr

oneBit = BitStr.take 1 $ bitString $ singleton 1

numberOfOneBits :: BitString -> Word32
numberOfOneBits bitStr = case BitStr.length bitStr == 0 of
  True -> 0
  False -> case (BitStr.take 1 bitStr) == oneBit of
    True -> 1 + (numberOfOneBits $ BitStr.drop 1 bitStr)
    False -> numberOfOneBits $ BitStr.drop 1 bitStr
