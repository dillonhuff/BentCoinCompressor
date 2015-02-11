module Compress(oneBitLocations,
                truncateTo14Bits) where

import Data.Bits
import Data.BitString as BitStr
import Data.ByteString as ByteStr
import Data.Word

oneBit = BitStr.take 1 $ bitString $ singleton 1
zeroBit = BitStr.take 1 $ bitString $ singleton 0

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

truncateTo14Bits :: Word32 -> Maybe BitString
truncateTo14Bits word = case word >= 16384 of
  True -> Nothing
  False -> Just $ truncateBits 14 word

truncateBits :: Int -> Word32 -> BitString
truncateBits 0 word = BitStr.empty
truncateBits n word = BitStr.append nextBit restOfBits
  where
  nextBit = case word .&. 1 == 1 of
    True -> oneBit
    False -> zeroBit
  restOfBits = truncateBits (n - 1) (shiftR word 1)
