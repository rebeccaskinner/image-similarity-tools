{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE NoStarIsType #-}
module Codec.Picture.DCT where
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Data.Vector.Sized (SizedVector, SizedVector2D)
import Data.Vector.Sized qualified as SizedVec
import Data.Word
import GHC.TypeLits

dct ::
  forall size.
  KnownNat size =>
  SizedVector size Double ->
  SizedVector size Double
dct input = SizedVec.generate @size dctVal
  where
    inputLen = SizedVec.natInt @size
    p = pi / fromIntegral inputLen
    componentAt n k xn = xn * cos (p * (n + 0.5) * k)
    dctVal k =
      Vec.sum . SizedVec.fromSizedVector $ SizedVec.imap (\n xn -> componentAt (fromIntegral n) (fromIntegral k) xn) input

dct2D ::
  forall x y.
  (KnownNat x, KnownNat y, KnownNat (x * y), KnownNat (y * x)) =>
  SizedVector2D x y Double ->
  SizedVector2D x y Double
dct2D = SizedVec.applySeparableKernel @KnownNat dct


idct ::
  forall size.
  KnownNat size =>
  SizedVector size Double ->
  SizedVector size Double
idct input = SizedVec.generate  dctVal
  where
    scaleFactor = 2 / fromIntegral inputLen
    inputLen = SizedVec.natInt @size
    p = pi / fromIntegral inputLen
    componentAt 0 k xn = 0.5 * xn
    componentAt n k xn = xn * cos(p * (k + 0.5) * n)
    componentAt' n k =
      componentAt (fromIntegral n) (fromIntegral k)
    dctVal k = (* scaleFactor) . Vec.sum . SizedVec.fromSizedVector . SizedVec.imap (`componentAt'` k) $ input

idct2D ::
  forall x y.
  (KnownNat x, KnownNat y, KnownNat (x * y), KnownNat (y * x)) =>
  SizedVector2D x y Double ->
  SizedVector2D x y Double
idct2D = SizedVec.applySeparableKernel @KnownNat idct
