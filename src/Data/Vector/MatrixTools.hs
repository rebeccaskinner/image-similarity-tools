module Data.Vector.MatrixTools where
import Data.Vector (Vector)
import Data.Vector qualified as Vec

transpose :: Int -> Vector a -> Vector a
transpose strideIn values =
  let strideOut = Vec.length values `div` strideIn
  in Vec.generate (Vec.length values) $ \n ->
    let (row, col) = n `divMod` strideOut
        offset = col * strideIn + row
    in values Vec.! offset

applyToRows :: (Vector a -> Vector b) -> Int -> Vector a -> Vector b
applyToRows f stride values
  | Vec.null values = Vec.empty
  | otherwise =
      let (h, t) = Vec.splitAt stride values
      in f h <> applyToRows f stride t

applySeparable :: (Vector a -> Vector a) -> (Int,Int) -> (Int,Int) -> Vector a -> Vector a
applySeparable f (inX, outX) (inY, outY) =
  transpose outY . applyToRows f inY . transpose outX . applyToRows f inX
