module BenchmarkCompressor() where

import Data.ByteString

import Compress

compressionFactor :: ByteString -> Double
compressionFactor byteString = 0.5
