module PerceptualHash where
import Codec.Picture

loadRGB :: FilePath -> IO (Either String (Image PixelRGB8))
loadRGB = fmap (convertRGB8 <$>) . readImage

perceptualHash :: IO ()
perceptualHash = print "perceptualHash placeholder"
