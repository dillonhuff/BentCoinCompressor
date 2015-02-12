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
    onePositions = []

toWord32 :: BitString -> Word32
toWord32 bitStr = 0

generateDecompressedFile :: CompressedFormat -> ByteString
generateDecompressedFile (CompressedFormat len numOnes onePositions) =
  ByteStr.empty
