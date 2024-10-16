{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
module Data.Vector.Sized where
import Prelude hiding (take, map)
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Data.Kind
import GHC.TypeLits
import Data.Proxy

type DivisibleBy a b = (a `Mod` b ~ 0)

newtype SizedVector (size :: Natural) (elem :: Type) =
  SizedVector { fromSizedVector :: Vector elem }
  deriving (Eq, Show) via (Vector elem)
  deriving Functor via Vector

newtype SizedVector2D (sizeX :: Natural) (sizeY :: Natural) (elem :: Type) =
  SizedVector2D { fromSizedVector2D :: Vector elem }
  deriving (Eq, Show) via (Vector elem)
  deriving Functor via Vector

index ::
  forall ix elem size.
  (KnownNat ix, ix + 1 <= size) =>
  SizedVector size elem -> elem
index (SizedVector vec) = vec Vec.! natInt @ix

unsafeIndex :: forall size elem. KnownNat size => SizedVector size elem -> Int -> elem
unsafeIndex (SizedVector vec) idx
  | idx >= natInt @size = error "unsafeIndex: out of bounds"
  | otherwise = vec Vec.! idx

index2D ::
  forall ix iy elem sizeX sizeY .
  (KnownNat ix, KnownNat iy, KnownNat sizeX,
   ix + 1 <= sizeX, iy + 1 <= sizeY) =>
  SizedVector2D sizeX sizeY elem ->
  elem
index2D (SizedVector2D vec) =
  vec Vec.! offset
  where
    offset = offY * stride + offX
    offY = natInt @iy
    offX = natInt @ix
    stride = natInt @sizeX

unsafeIndex2D :: forall x y elem. (KnownNat x, KnownNat y) => SizedVector2D x y elem -> Int -> Int -> elem
unsafeIndex2D (SizedVector2D vec) ix iy
  | ix >= natInt @x || iy >= natInt @y = error "unsafeIndex2D: out of bounds"
  | otherwise = vec Vec.! ix'
  where
    stride = natInt @x
    ix' = iy * stride + ix

imap :: (Int -> a -> b) -> SizedVector size a -> SizedVector size b
imap f (SizedVector vec) = SizedVector (Vec.imap f vec)

sizedToVec :: SizedVector size elem -> Vector elem
sizedToVec = fromSizedVector

sized2DtoVec :: SizedVector2D x y elem -> Vector elem
sized2DtoVec = fromSizedVector2D

natInt :: forall size. KnownNat size => Int
natInt = fromIntegral . natVal $ Proxy @size

natDouble :: forall size. KnownNat size => Double
natDouble = fromIntegral . natVal $ Proxy @size

sizedVector ::
  forall size elem. KnownNat size =>
  Vector elem ->
  Maybe (SizedVector size elem)
sizedVector vec
  | Vec.length vec == natInt @size = Just (SizedVector vec)
  | otherwise = Nothing

generate ::
  forall size elem.
  KnownNat size =>
  (Int -> elem) ->
  SizedVector size elem
generate f = SizedVector $ Vec.generate (natInt @size) f

generate2D ::
  forall sizeX sizeY elem.
  (KnownNat sizeX, KnownNat sizeY) =>
  (Int -> Int -> elem) ->
  SizedVector2D sizeX sizeY elem
generate2D f = SizedVector2D $ Vec.generate totalSize f'
  where
    totalSize = natInt @sizeX * natInt @sizeY
    stride = natInt @sizeX
    f' flatIdx =
      let (y, x) = flatIdx `divMod` stride
      in f x y

sizedVector2D ::
  forall sizeX sizeY size elem. (size ~ sizeX * sizeY, KnownNat size) =>
  Vector elem ->
  Maybe (SizedVector2D sizeX sizeY elem)
sizedVector2D vec
  | Vec.length vec == natInt @size = Just (SizedVector2D vec)
  | otherwise = Nothing

sizedVector2DFromRows ::
  forall sizeX sizeY size elem. (size ~ sizeX * sizeY, KnownNat size) =>
  SizedVector sizeY (SizedVector sizeX elem) ->
  SizedVector2D sizeX sizeY elem
sizedVector2DFromRows (SizedVector rows) =
  SizedVector2D joinedRows
  where
    joinedRows = Vec.foldr (<>) Vec.empty $ Vec.map fromSizedVector rows

emptySizedVector :: SizedVector 0 elem
emptySizedVector = SizedVector Vec.empty

cons ::
  forall a b elem. (KnownNat a, KnownNat b, b ~ a + 1) =>
  elem ->
  SizedVector a elem ->
  SizedVector b elem
cons val (SizedVector vec) = SizedVector (Vec.cons val vec)

uncons ::
  forall a b elem.
  (KnownNat a, KnownNat b, b ~ a + 1) =>
  SizedVector b elem ->
  (elem, SizedVector a elem)
uncons (SizedVector vec) =
  case Vec.uncons vec of
    Nothing -> error "impossible"
    Just (val, vec') -> (val, SizedVector vec')

sizedVectorRows ::
  forall sizeX sizeY elem.
  (KnownNat sizeX, KnownNat sizeY) =>
  SizedVector2D sizeX sizeY elem ->
  SizedVector sizeY (SizedVector sizeX elem)
sizedVectorRows (SizedVector2D elems) = SizedVector rows
  where
    stride = natInt @sizeX
    rowCount = natInt @sizeY
    lastRowStart = stride * rowCount - 1
    rows = Vec.fromList [SizedVector $ Vec.slice offset stride elems | offset <- [0,stride..lastRowStart]]

consRow ::
  forall sizeX sizeY sizeY' elem.
  (KnownNat sizeX, KnownNat sizeY, KnownNat sizeY', sizeY' ~ sizeY + 1) =>
  SizedVector sizeX elem ->
  SizedVector2D sizeX sizeY elem ->
  SizedVector2D sizeX sizeY' elem
consRow (SizedVector newRow) (SizedVector2D rows) =
  SizedVector2D (newRow <> rows)

consCol ::
  forall sizeX sizeX' sizeY size size' elem.
  (KnownNat sizeX, KnownNat sizeX', KnownNat sizeY, KnownNat size, KnownNat size'
  , sizeX' ~ sizeX + 1, size ~ sizeX * sizeY, size' ~ sizeX' * sizeY) =>
  SizedVector sizeY elem ->
  SizedVector2D sizeX sizeY elem ->
  SizedVector2D sizeX' sizeY elem
consCol (SizedVector col) vec2d =
  sizedVector2DFromRows rows'
  where
    rows' = SizedVector $ Vec.map SizedVector $ Vec.zipWith Vec.cons col rawRows
    rawRows = Vec.map fromSizedVector . fromSizedVector . sizedVectorRows $ vec2d

take ::
  forall size' size elem.
  (KnownNat size', size' <= size) =>
  SizedVector size elem ->
  SizedVector size' elem
take (SizedVector v) = SizedVector $ Vec.take amt v
  where amt = natInt @size'

take2D ::
  forall sizeX' sizeY' sizeX sizeY size size' elem.
  (KnownNat sizeX', KnownNat sizeY', KnownNat sizeX, KnownNat sizeY, KnownNat size, KnownNat size'
  , size ~ sizeX * sizeY, size' ~ sizeX' * sizeY'
  , sizeX' <= sizeX, sizeY' <= sizeY) =>
  SizedVector2D sizeX sizeY elem ->
  SizedVector2D sizeX' sizeY' elem
take2D vec2d = sizedVector2DFromRows resizedRows
  where
    rows = sizedVectorRows vec2d
    resizedRows = take @sizeY' $ take @sizeX' <$> rows

subsample2D ::
  forall blockX blockY sizeX sizeY sizeX' sizeY' elem.
  ( KnownNat blockX, KnownNat blockY, KnownNat sizeX, KnownNat sizeY, KnownNat sizeX', KnownNat sizeY'
  , sizeX' ~ sizeX `Div` blockX
  , sizeY' ~ sizeY `Div` blockY
  , DivisibleBy sizeX blockX
  , DivisibleBy sizeY blockY
  ) =>
  SizedVector2D sizeX sizeY elem ->
  SizedVector2D sizeX' sizeY' (SizedVector2D blockX blockY elem)
subsample2D (SizedVector2D vec) = SizedVector2D (Vec.fromList subsampledBlocks)
  where
    blockCountX = natInt @sizeX'
    blockCountY = natInt @sizeY'
    blockSizeX = natInt @blockX
    blockSizeY = natInt @blockY
    vecSizeX = natInt @sizeX

    subsampledBlocks :: [SizedVector2D blockX blockY elem]
    subsampledBlocks =
      [ blockCoords (x * blockSizeX) (y * blockSizeY)
      | y <- [0..blockCountY - 1]
      , x <- [0..blockCountX - 1]
      ]

    mkSubsampledBlock :: [Int] -> SizedVector2D blockX blockY elem
    mkSubsampledBlock = SizedVector2D . Vec.fromList . fmap (vec Vec.!)

    blockCoords :: Int -> Int -> SizedVector2D blockX blockY elem
    blockCoords blockIx blockIy =
      mkSubsampledBlock coords
      where
        coords = [ y * vecSizeX + x
                 | y <- [blockIy .. blockIy + blockSizeY - 1]
                 , x <- [blockIx .. blockIx + blockSizeX - 1]
                 ]

transpose ::
  forall sizeX sizeY elem.
  (KnownNat sizeX, KnownNat sizeY) =>
  SizedVector2D sizeX sizeY elem ->
  SizedVector2D sizeY sizeX elem
transpose (SizedVector2D values) =
  let
    strideIn = natInt @sizeX
    strideOut = natInt @sizeY
    values' = Vec.generate (Vec.length values) $ \n ->
      let (row, col) = n `divMod` strideOut
          offset = col * strideIn + row
      in values Vec.! offset
  in SizedVector2D values'

mapRows ::
 forall sizeX sizeX' sizeY elemIn elemOut.
 (KnownNat sizeX, KnownNat sizeX', KnownNat sizeY, KnownNat (sizeX' * sizeY)) =>
 (SizedVector sizeX elemIn -> SizedVector sizeX' elemOut) ->
 SizedVector2D sizeX sizeY elemIn ->
 SizedVector2D sizeX' sizeY elemOut
mapRows f vec2d =
  sizedVector2DFromRows $ f <$> sizedVectorRows vec2d

applySeparableKernel ::
  forall c sizeX sizeY elem.
  (KnownNat sizeX, KnownNat sizeY, KnownNat (sizeX * sizeY), KnownNat (sizeY * sizeX), c sizeX, c sizeY) =>
  (forall size. c size => SizedVector size elem -> SizedVector size elem) ->
  SizedVector2D sizeX sizeY elem ->
  SizedVector2D sizeX sizeY elem
applySeparableKernel f = transpose . mapRows f . transpose . mapRows f
