{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Codec.Picture.DCT where
import Data.Vector.MatrixTools
import Codec.Picture
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Data.Vector.Storable qualified as StorableVec
import Data.Word

dct :: Vector Double -> Vector Double
dct input = Vec.generate inputLen dctVal
  where
    inputLen = Vec.length input
    p = pi / fromIntegral inputLen
    componentAt n k xn = xn * cos (p * (n + 0.5) * k)
    dctVal k = Vec.sum $ Vec.imap (\n xn -> componentAt (fromIntegral n) (fromIntegral k) xn) input

dct2D :: Int -> Int -> Vector Double -> Vector Double
dct2D x y = applySeparable dct (x, x) (y, y)

dctImage :: Image Pixel8 -> Vector Double
dctImage (Image x y inputVals) =
  dct2D x y . Vec.map fromIntegral . StorableVec.convert $ inputVals

idct :: Vector Double -> Vector Double
idct input = Vec.generate inputLen dctVal
  where
    scaleFactor = 2 / fromIntegral inputLen
    inputLen = Vec.length input
    p = pi / fromIntegral inputLen
    componentAt 0 k xn = 0.5 * xn
    componentAt n k xn = xn * cos(p * (k + 0.5) * n)
    componentAt' n k =
      componentAt (fromIntegral n) (fromIntegral k)
    dctVal k = (* scaleFactor) . Vec.sum . Vec.imap (`componentAt'` k) $ input

idct2D :: Int -> Int -> Vector Double -> Vector Double
idct2D x y = applySeparable idct (x, x) (y, y)

idctImage :: Image Pixel8 -> Vector Double
idctImage (Image x y inputVals) =
  idct2D x y . Vec.map fromIntegral . StorableVec.convert $ inputVals
