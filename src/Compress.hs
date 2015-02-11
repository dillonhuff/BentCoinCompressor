module Compress(numberOfOneBits) where

import Data.Word
import Data.BitString as BitStr
import Data.ByteString as ByteStr

oneBit = BitStr.take 1 $ bitString $ singleton 1

oneBitLocations :: BitString -> [Word32]
oneBitLocations bitStr = recOneBitLocations 0 bitStr

recOneBitLocations :: Word32 -> BitString -> [Word32]
recOneBitLocations index bitStr = case BitStr.length bitStr == 0 of
  True -> []
  False -> case (BitStr.take 1 bitStr) == oneBit of
    True -> 1 + (oneBitLocations $ BitStr.drop 1 bitStr)
    False -> oneBitLocations $ BitStr.drop 1 bitStr
