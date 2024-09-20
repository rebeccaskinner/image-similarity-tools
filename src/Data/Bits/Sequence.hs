module Data.Bits.Sequence where
import Data.Bits

fromBitList :: forall a. FiniteBits a => [a] -> a
fromBitList bits = foldr (.|.) zeroBits offsetBits
  where
    width = finiteBitSize @a undefined
    offsetStart = width * (length bits - 1)
    offsetBits = [b `shiftL` n | (b, n) <- zip bits [offsetStart, offsetStart - width .. ]]

getOffsetBits :: forall a. FiniteBits a => [a] -> [Int]
getOffsetBits bits = [n | (_b, n) <- zip bits [offsetStart, offsetStart - width .. ]]
  where
    width = finiteBitSize @a undefined
    offsetStart = width * (length bits - 1)
