module Mandala.Types.Color where

import Mandala.Types.Annotations
import Mandala.Types.Math

import Data.Word


type Color = V3 Double RGB

-- Applies gamma encoding, must remove gamma encoding when we write the buffer
rgb :: Double -> Double -> Double -> Color
rgb r g b = V3 (f r) (f g) (f b)
  where f x = (x / 255) ** 2.2

hexColor :: Word8 -> Word8 -> Word8 -> Color
hexColor x y z = rgb (fromIntegral x) (fromIntegral y) (fromIntegral z)
