{-# LANGUAGE DerivingStrategies #-}
module Data.PerceptualHash where
import Data.Bits
import Data.Word
import Text.Printf

newtype PerceptualHash = PerceptualHash { getHashBits :: Word64 }
  deriving newtype (Show, Eq, Ord, Bits)

showHex :: PerceptualHash -> String
showHex (PerceptualHash p) = printf "%016x" p

parseHex :: String -> Maybe PerceptualHash
parseHex hexString = undefined
