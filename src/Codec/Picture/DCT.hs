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

dct :: Vector Word8 -> Vector Word8
dct input = Vec.generate inputLen dctVal
  where
    inputLen = Vec.length input
    scale = pi / fromIntegral inputLen
    components = Vec.imap (\i v -> fromIntegral i * fromIntegral v) input
    dctVal i =
      let val = fromIntegral $ input Vec.! i
          j = 0.5 + fromIntegral i
      in round . Vec.sum $ Vec.map (\x -> val * cos (j * x)) components

idct :: Vector Word8 -> Vector Word8
idct input = Vec.generate inputLen idctVal
  where
    firstElem = Vec.singleton $ (input Vec.! 0) `div` 2
    inputLen = Vec.length input
    scale = pi / fromIntegral inputLen
    components = Vec.imap (\i v -> fromIntegral i * fromIntegral v) input
    idctVal i =
      let val = fromIntegral $ input Vec.! i
          j = 0.5 + fromIntegral i
      in round . Vec.sum $ Vec.map (\x -> val * cos (j * x)) components

dct2D :: Int -> Int -> Vector Word8 -> Vector Word8
dct2D x y = applySeparable dct (x, x) (y, y)

idct2D :: Int -> Int -> Vector Word8 -> Vector Word8
idct2D x y = applySeparable idct (x, x) (y, y)

dctImage :: Image Pixel8 -> Image Pixel8
dctImage (Image x y inputVals) =
  Image x y (StorableVec.convert . dct2D x y . StorableVec.convert $ inputVals)

idctImage :: Image Pixel8 -> Image Pixel8
idctImage (Image x y inputVals) =
  Image x y (StorableVec.convert . idct2D x y . StorableVec.convert $ inputVals)
