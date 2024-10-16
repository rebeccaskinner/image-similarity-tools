module Codec.Picture.Colorspace where
import Codec.Picture
import Data.Vector.Sized (SizedVector2D)

desaturatePixelData :: SizedVector2D sizeX sizeY PixelRGB8 -> SizedVector2D sizeX sizeY Pixel8
desaturatePixelData = fmap luminosity
  where
    luminosity (PixelRGB8 r g b) =
      round @Double $ (0.299 * fromIntegral r) + (0.587 * fromIntegral g) + (0.114 * fromIntegral b)

desaturateLuminosity :: Image PixelRGB8 -> Image Pixel8
desaturateLuminosity = pixelMap luminosity
  where
    luminosity (PixelRGB8 r g b) =
      round @Double $ (0.299 * fromIntegral r) + (0.587 * fromIntegral g) + (0.114 * fromIntegral b)
