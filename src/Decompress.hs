module Decompress(decompress) where

import Data.BitString as BitStr
import Data.ByteString as ByteStr

decompress :: ByteString -> ByteString
decompress bitStr = bitStr
