{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoStarIsType #-}
module Codec.Picture.ImageConversions where

import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Data.Vector.Sized (SizedVector2D)
import Data.Vector.Sized qualified as SizedVec
import GHC.TypeLits
import Codec.Picture
import Data.Vector.Storable qualified as StorableVec
import Foreign.Storable

pixelData :: Image Pixel8 -> Vector Pixel8
pixelData (Image _ _ pixels) = StorableVec.convert pixels

imageFromPixels :: Int -> Int -> Vector Pixel8 -> Image Pixel8
imageFromPixels x y = Image x y . StorableVec.convert

pixelToDouble :: Pixel8 -> Double
pixelToDouble = fromIntegral

doubleToPixel :: Double -> Pixel8
doubleToPixel = floor

imageDataDouble :: Image Pixel8 -> Vector Double
imageDataDouble = Vec.map pixelToDouble . pixelData

imageFromPixelData :: Int -> Int -> Vector Double -> Image Pixel8
imageFromPixelData x y = imageFromPixels x y . Vec.map doubleToPixel

imageToSizedVector ::
  forall x y a.
  (KnownNat x, KnownNat y, KnownNat (x * y), Storable (PixelBaseComponent a)) =>
  Image a ->
  Maybe (SizedVector2D x y (PixelBaseComponent a))
imageToSizedVector (Image imgX imgY pixels)
  | imgX == SizedVec.natInt @x && imgY == SizedVec.natInt @y = SizedVec.sizedVector2D @x @y $ StorableVec.convert pixels
  | otherwise = Nothing

sizedVectorToImage ::
  forall x y a.
  (KnownNat x, KnownNat y, Storable (PixelBaseComponent a)) =>
  SizedVector2D x y (PixelBaseComponent a) ->
  Image a
sizedVectorToImage vec =
  Image x y pixels
  where
    x = SizedVec.natInt @x
    y = SizedVec.natInt @y
    pixels = StorableVec.convert $ SizedVec.fromSizedVector2D vec
