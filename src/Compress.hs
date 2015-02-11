module Compress(oneBitLocations) where

import Data.Word
import Data.BitString as BitStr
import Data.ByteString as ByteStr

oneBit = BitStr.take 1 $ bitString $ singleton 1

oneBitLocations :: BitString -> (Word32, [Word32])
oneBitLocations bitStr = recOneBitLocations 0 0 bitStr

recOneBitLocations :: Word32 -> Word32 -> BitString -> (Word32, [Word32])
recOneBitLocations numOnes index bitStr = case BitStr.length bitStr == 0 of
  True -> (0, [])
  False -> case (BitStr.take 1 bitStr) == oneBit of
    True -> ((fst res2) + 1, index : (snd res2))
    False -> res1
    where
      res1 = recOneBitLocations numOnes (index + 1) $ BitStr.drop 1 bitStr
      res2 = recOneBitLocations (numOnes + 1) (index + 1) $ BitStr.drop 1 bitStr
