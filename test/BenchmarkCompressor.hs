module BenchmarkCompressor() where

import Control.Monad
import Control.Monad.Random
import Data.BitString as Bit
import Data.ByteString as B

import Compress
import Utils

benchmarkMeanCompressionFactor :: Int -> Int -> Double -> IO Double
benchmarkMeanCompressionFactor numStrs strLen probOfOne = do
  strs <- evalRandIO $ sequence $ Prelude.replicate numStrs $ randBitString strLen probOfOne
  return $ avgCompressionFactor $ Prelude.map realizeBitStringStrict strs

randByteString :: Int -> Double -> ByteString
randByteString length probOfOne = B.empty

randBit :: (RandomGen g) => Double -> Rand g BitString
randBit probOfOne = do
  val <- getRandomR (0, 1.0)
  return $ case val <= probOfOne of
    True -> oneBit
    False -> zeroBit

randBitString :: (RandomGen g) => Int -> Double -> Rand g BitString
randBitString numBits probOfOne =
  liftM Bit.concat $ sequence $ Prelude.replicate numBits $ randBit probOfOne

compressionFactor :: ByteString -> Double
compressionFactor byteStr = compressFactor
  where
    compressedBytes = compress byteStr
    compressedLength = B.length compressedBytes
    normalLength = B.length byteStr
    compressFactor = (fromIntegral compressedLength) / (fromIntegral normalLength)

avgCompressionFactor :: [ByteString] -> Double
avgCompressionFactor byteStrs = meanCompFactor
  where
    sumOfCompFactors = Prelude.foldl (+) 0 $ Prelude.map compressionFactor byteStrs
    meanCompFactor = sumOfCompFactors / (fromIntegral $ Prelude.length byteStrs)
