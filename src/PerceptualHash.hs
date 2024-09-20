{-# LANGUAGE DerivingStrategies #-}
module PerceptualHash where
import Codec.Picture
import Codec.Picture.Resize
import Codec.Picture.Colorspace
import Codec.Picture.DCT
import Data.PerceptualHash
import Data.Word
import Data.Bits
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Text.Printf
import Data.Char

loadRGB :: FilePath -> IO (Either String (Image PixelRGB8))
loadRGB = fmap (convertRGB8 <$>) . readImage

phash :: Image PixelRGB8 -> PerceptualHash
phash rgbImage =
  PerceptualHash $ vecBitsBy (> mean dctCoefficients) dctCoefficients
  where
    vecBitsBy :: (Double -> Bool) -> Vector Double -> Word64
    vecBitsBy valueToBit =
      Vec.ifoldr (\idx v outBits -> if valueToBit v then outBits `setBit` idx else outBits) 0
    mean = (/ (fromIntegral $ Vec.length dctCoefficients)) . Vec.sum . Vec.tail
    dctCoefficients =
      dctImage . resizeImage imgSize . desaturateLuminosity $ rgbImage
    imgSize = ImageSize 32 32

perceptualHash :: IO ()
perceptualHash = putStrLn "perceptualHash placeholder"
