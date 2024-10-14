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

loadRGB :: FilePath -> IO (Either String (Image PixelRGB8))
loadRGB = fmap (convertRGB8 <$>) . readImage

preprocessImage :: Image PixelRGB8 -> Either String (SizedVector2D 64 64 Word8)
preprocessImage = resizeFixed . desaturateLuminosity
  where
    resizeFixed img =
      case imageToSizedVector $ resizeImage (ImageSize 64 64) img of
        Nothing -> Left "Could not resize image"
        Just vec -> Right vec

discreteFourierTransform :: SizedVector2D 64 64 Word8 -> SizedVector2D 64 64 Double
discreteFourierTransform = dct2D . fmap pixelToDouble

roundtripImage :: Image PixelRGB8 -> Either String (Image Pixel8)
roundtripImage img =
  sizedVectorToImage . fmap doubleToPixel . idct2D . discreteFourierTransform <$> preprocessImage img

roundtripImageFile :: FilePath -> FilePath -> IO ()
roundtripImageFile fnameIn fnameOut = do
  imgData <- loadRGB fnameIn
  case imgData >>= roundtripImage of
    Left err -> ioError $ userError err
    Right preprocessedImageData -> writePng fnameOut preprocessedImageData

perceptualHashImage :: Image PixelRGB8 -> Either String PerceptualHash
perceptualHashImage image =
  perceptualHash . discreteFourierTransform <$> preprocessImage image

perceptualHashFile :: FilePath -> IO (Either String PerceptualHash)
perceptualHashFile fname = (>>= perceptualHashImage) <$> loadRGB fname
