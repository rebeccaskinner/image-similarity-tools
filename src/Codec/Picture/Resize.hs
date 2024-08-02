{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Codec.Picture.Resize where
import Control.Monad.ST
import Data.Ratio
import Data.Foldable
import Data.Word
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Data.Vector.MatrixTools
import Data.Vector.Storable qualified as StorableVec
import Text.Printf
import Codec.Picture

newtype ScaleFactor = ScaleFactor { getScaleFactor :: Ratio Int }
data ImageSize = ImageSize { imageWidth :: Int, imageHeight :: Int }

scaleBy :: Floating a => ScaleFactor -> a -> a
scaleBy (ScaleFactor f) n = n / (a / b)
  where a = fromIntegral $ numerator f
        b = fromIntegral $ denominator f

sinc :: Double -> Double
sinc 0 = 1
sinc x = sin (pi * x) / (pi * x)

lanczos :: Int -> Double -> Double
lanczos a x
  | abs x >= a' = 0
  | otherwise = sinc x * sinc (x / a')
  where a' = fromIntegral a

lanczosKernel :: Int -> ScaleFactor -> Vector Double
lanczosKernel a scaleFactor = Vec.generate kernelSize $ \n ->
  let x = scaleBy scaleFactor (fromIntegral $ n - a)
  in lanczos a x
  where kernelSize = 2 * a + 1

resample :: Int -> Int -> Vector Word8 -> Vector Word8
resample inputSize outputSize input =
  Vec.generate outputSize weightedValueAt
  where
    window = Vec.fromList [-2 .. 3]
    weightedValueAt outPos =
      let
        inPos = outPos * posScale
        inputWindow = Vec.map (+ inPos) window
        inputsPosWithScale = Vec.zip kernel inputWindow
        addTuple (kernelWeight,i) (totalWeight,totalPixelVal)
          | i >= 0 && i < inputSize =
              let pixelVal = input Vec.! i
                  weightedPixel = fromIntegral pixelVal * kernelWeight
              in (kernelWeight + totalWeight, weightedPixel + totalPixelVal)
          | otherwise = (totalWeight, totalPixelVal)
        (w, e) = Vec.foldr addTuple (0,0) inputsPosWithScale
      in if e == 0 then 0 else round (e / w)
    posScale = inputSize `div` outputSize
    kernel = lanczosKernel 3 $ ScaleFactor (inputSize % outputSize)

resampleRows :: Int -> Int -> Vector Word8 -> Vector Word8
resampleRows inputRowSize outputRowSize inputData
  | Vec.null inputData = Vec.empty
  | otherwise =
      let (h,t) = Vec.splitAt inputRowSize inputData
      in resample inputRowSize outputRowSize h <> resampleRows inputRowSize outputRowSize t

resizeImageData :: ImageSize -> ImageSize -> Vector Word8 -> Vector Word8
resizeImageData (ImageSize inX inY) (ImageSize outX outY) =
  transpose outY . resampleRows inY outY . transpose outX . resampleRows inX outX

resizeImage :: ImageSize -> Image Pixel8 -> Image Pixel8
resizeImage outSize@(ImageSize outX outY) (Image inX inY inputImg) =
  let inSize = ImageSize inX inY
      inputData = StorableVec.convert inputImg
  in Image outX outY (StorableVec.convert $ resizeImageData inSize outSize inputData)

printKernel :: Vector Double -> IO ()
printKernel = Vec.mapM_ (putStrLn . printf "%.6f")
