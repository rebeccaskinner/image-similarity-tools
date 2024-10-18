{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
module Data.PerceptualHash where
import Data.Bits
import Data.Word
import Text.Printf
import Text.Read
import Data.Vector.Sized (SizedVector2D)
import Data.Vector qualified as Vec
import Data.Vector.Sized qualified as SizedVec

newtype PerceptualHash = PerceptualHash { getHashBits :: Word64 }
  deriving newtype (Show, Eq, Ord, Bits)

perceptualHash :: SizedVector2D 64 64 Double -> PerceptualHash
perceptualHash dctVals = PerceptualHash word
  where
    lowFrequencies = SizedVec.take2D @16 @16 dctVals
    mean = Vec.sum (SizedVec.fromSizedVector2D lowFrequencies) / 256
    bitForBlock (bitIdx, blockMean) acc
      | blockMean > mean = setBit acc bitIdx
      | otherwise = acc
    word :: Word64
    word = foldr bitForBlock 0 indexBlockMeans
    indexBlockMeans = zip [0..] blockMeans
    blockMeans = Vec.toList . SizedVec.fromSizedVector2D . fmap calcBlockMean $ SizedVec.subsample2D @2 @2 lowFrequencies
    calcBlockMean blockData = Vec.sum (SizedVec.fromSizedVector2D blockData) / 4

showHex :: PerceptualHash -> String
showHex = printf "%016x" . getHashBits

parseHex :: String -> Maybe PerceptualHash
parseHex = fmap PerceptualHash . readMaybe

perceptualHashDistance :: PerceptualHash -> PerceptualHash -> Int
perceptualHashDistance a b = popCount $ a `xor` b
