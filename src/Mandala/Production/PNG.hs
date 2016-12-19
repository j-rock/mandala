module Mandala.Production.PNG where

import Mandala.Types

import qualified Codec.Picture as Pic
import qualified Data.Vector as Vector
import Data.Vector (Vector)

stdColorXform :: V3 Double RGB -> Pic.PixelRGB8
stdColorXform (V3 r g b) = Pic.PixelRGB8 (f r) (f g) (f b)
  where f = round . clamp 0 255 . \x -> 255 * (x ** (1/2.2))
        clamp lo hi x| x < lo    = lo
                     | otherwise = min x hi

writePng :: (Pic.PngSavable px, Pic.Pixel px) =>
            FilePath
         -> (color -> px)
         -> Vector color
         -> IO ()
writePng fp xform input =
  let lenF :: Double
      lenF = sqrt . fromIntegral . Vector.length $ input
      len = truncate lenF
      indexer x y = Vector.unsafeIndex input $ y * len + x
      genFunc x y = xform $ indexer x y
  in Pic.writePng fp $ Pic.generateImage genFunc len len


writeStdPng :: FilePath
            -> Vector (V3 Double RGB)
            -> IO ()
writeStdPng fp = writePng fp stdColorXform
