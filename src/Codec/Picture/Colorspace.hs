module Codec.Picture.Colorspace where
import Codec.Picture

desaturateLuminosity :: Image PixelRGB8 -> Image Pixel8
desaturateLuminosity = pixelMap luminosity
  where
    luminosity (PixelRGB8 r g b) =
      round @Double $ (0.299 * fromIntegral r) + (0.587 * fromIntegral g) + (0.114 * fromIntegral b)
