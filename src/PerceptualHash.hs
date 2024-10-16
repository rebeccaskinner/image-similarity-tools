{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE DataKinds #-}
module PerceptualHash where
import Codec.Picture
import Codec.Picture.Resize
import Codec.Picture.Colorspace
import Data.PerceptualHash
import Data.Word
import Data.Vector.Sized (SizedVector2D)
import Codec.Picture.ImageConversions
import Codec.Picture.DCT

loadRGB :: FilePath -> IO (Image PixelRGB8)
loadRGB fname = do
  dynImage <- readImage fname
  case dynImage of
    Left err -> ioError $ userError err
    Right imgData -> pure $ convertRGB8 imgData

preprocessImage :: Image PixelRGB8 -> SizedVector2D 64 64 Word8
preprocessImage = resizeDataBilinear' . desaturateLuminosity

discreteFourierTransform :: SizedVector2D 64 64 Word8 -> SizedVector2D 64 64 Double
discreteFourierTransform = dct2D . fmap pixelToDouble

roundtripImage :: Image PixelRGB8 -> Image Pixel8
roundtripImage =
  sizedVectorToImage . fmap doubleToPixel . idct2D . discreteFourierTransform . preprocessImage

roundtripImageFile :: FilePath -> FilePath -> IO ()
roundtripImageFile fnameIn fnameOut =
  loadRGB fnameIn >>= writePng fnameOut . roundtripImage

perceptualHashImage :: Image PixelRGB8 -> PerceptualHash
perceptualHashImage =
  perceptualHash . discreteFourierTransform . preprocessImage

perceptualHashFile :: FilePath -> IO PerceptualHash
perceptualHashFile fname =
  perceptualHashImage <$> loadRGB fname
