module Utils(oneBit,
             zeroBit) where

import Data.BitString as BitStr
import Data.ByteString

oneBit = BitStr.take 1 $ bitString $ singleton 1
zeroBit = BitStr.take 1 $ bitString $ singleton 0
