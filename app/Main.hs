module Main where
import Data.Vector (Vector)
import Data.Word
import Codec.Picture
import Codec.Picture.Resize
import Codec.Picture.Colorspace
import Codec.Picture.DCT
import System.Environment (getArgs)

loadRGB :: FilePath -> IO (Image PixelRGB8)
loadRGB inputPath = do
  result <- readImage inputPath -- fmap (convertRGB8 <$>) . readImage
  case result of
    Left err -> ioError $ userError err
    Right val -> pure $ convertRGB8 val

targetSize :: ImageSize
targetSize = ImageSize 64 64

fixImageData :: Image PixelRGB8 -> Image Pixel8
fixImageData = idctImage . dctImage . resizeImage targetSize . desaturateLuminosity

main :: IO ()
main = do
  [inFname, outFname] <- getArgs
  imgData <- loadRGB inFname
  let outData = fixImageData imgData
  writePng outFname outData
