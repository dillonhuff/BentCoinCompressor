module Compress(oneBitLocations,
                truncateTo14Bits,
                compress) where

import Data.Bits
import Data.BitString as BitStr
import Data.ByteString as ByteStr
import Data.Word

import Utils

oneBitLocations :: BitString -> [Word32]
oneBitLocations bitStr = recOneBitLocations 0 bitStr

recOneBitLocations :: Word32 -> BitString -> [Word32]
recOneBitLocations index bitStr = case BitStr.length bitStr == 0 of
  True -> []
  False -> case (BitStr.take 1 bitStr) == oneBit of
    True -> index : rest
    False -> rest
    where
      rest = recOneBitLocations (index + 1) $ BitStr.drop 1 bitStr

{-oneBitLocations :: BitString -> (Word32, [Word32])
oneBitLocations bitStr = recOneBitLocations 0 0 bitStr

recOneBitLocations :: Word32 -> Word32 -> BitString -> (Word32, [Word32])
recOneBitLocations numOnes index bitStr = case BitStr.length bitStr == 0 of
  True -> (0, [])
  False -> case (BitStr.take 1 bitStr) == oneBit of
    True -> ((fst res2) + 1, index : (snd res2))
    False -> res1
    where
      res1 = recOneBitLocations numOnes (index + 1) $ BitStr.drop 1 bitStr
      res2 = recOneBitLocations (numOnes + 1) (index + 1) $ BitStr.drop 1 bitStr-}

truncateTo14Bits :: Word32 -> BitString
truncateTo14Bits word = truncateBits 14 word

truncateBits :: Int -> Word32 -> BitString
truncateBits 0 word = BitStr.empty
truncateBits n word = BitStr.append nextBit restOfBits
  where
  nextBit = case word .&. 1 == 1 of
    True -> oneBit
    False -> zeroBit
  restOfBits = truncateBits (n - 1) (shiftR word 1)

compress :: ByteString -> ByteString
compress byteStr = case numOneBits > 0 of
  True -> realizeBitStringStrict $ BitStr.concat (tNumBits : tNumOneBits : tOneBitLocs)
  False -> ByteStr.singleton 0
  where
    oneBitLocs = oneBitLocations $ bitString byteStr
    numBits = fromIntegral (BitStr.length $ bitString byteStr) :: Word32
    tNumBits = truncateTo14Bits numBits
    numOneBits = fromIntegral (Prelude.length oneBitLocs) :: Word32
    tNumOneBits = truncateTo14Bits numOneBits
    tOneBitLocs = Prelude.map truncateTo14Bits oneBitLocs
