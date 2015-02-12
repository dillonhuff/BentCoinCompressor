module Decompress(decompress) where

import Data.BitString as BitStr
import Data.ByteString as ByteStr
import Data.Word

import Utils

data CompressedFormat = CompressedFormat {
     fileLength :: Word32,
     numOneBits :: Word32,
     oneBitPositoins :: [Word32]
  } deriving (Eq, Ord, Show)
  
decompress :: ByteString -> ByteString
decompress byteStr = generateDecompressedFile fileDescription
  where
    bitStr = bitString byteStr
    fileDescription = readFormat bitStr

readFormat :: BitString -> CompressedFormat
readFormat bitStr = CompressedFormat len numOnes onePositions
  where
    len = toWord32 $ BitStr.take 14 bitStr
    numOnes = toWord32 $ BitStr.take 14 $ BitStr.drop 14 bitStr
    onePositions = getOnePositions $ BitStr.drop 28 bitStr

getOnePositions :: BitString -> [Word32]
getOnePositions bitStr = case (BitStr.length bitStr) == 0 of
  True -> []
  False -> (toWord32 $ BitStr.take 14 bitStr) : (getOnePositions $ BitStr.drop 14 bitStr)

toWord32 :: BitString -> Word32
toWord32 bitStr = case BitStr.length bitStr == 0 of
  True -> 0
  False -> case (BitStr.take 1 bitStr) == oneBit of
    True -> 1 + rest
    False -> rest
    where
      rest = 2*(toWord32 $ BitStr.drop 1 bitStr)

generateDecompressedFile :: CompressedFormat -> ByteString
generateDecompressedFile (CompressedFormat len numOnes onePositions) =
  realizeBitStringStrict uncompressedBits
  where
    uncompressedBits = recGenerate 0 len onePositions

recGenerate :: Word32 -> Word32 -> [Word32] -> BitString
recGenerate index max onePositions = case index >= max of
  True -> BitStr.empty
  False -> case (Prelude.length onePositions > 0) && index == (Prelude.head onePositions) of
    True -> BitStr.append oneBit (recGenerate (index + 1) max (Prelude.tail onePositions))
    False -> BitStr.append zeroBit (recGenerate (index + 1) max onePositions)
