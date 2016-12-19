module Mandala.Production.Antialias where

import Mandala.Types

import Control.Arrow ((&&&))

type DrawFunc = V2 Double Pixel -> Color
type AntialiasFunc = DrawFunc -> DrawFunc

data StdDev
data GridRadius
data SampleStep


data SquareGridOptions = SquareGridOptions { gridRadius :: Double
                                           , numSteps   :: Int
                                           }
squareGrid :: SquareGridOptions
           -> V2 Double Pixel
           -> [V2 Double Pixel]
squareGrid (SquareGridOptions rad steps) (V2 px py) =
    let f x = (2*rad / fromIntegral steps) * fromIntegral x - rad
        range = [0..steps-1]
    in [V2 (px + f i) (py + f j)
       | i <- range
       , j <- range]

kernel :: (V2 Double Pixel -> [V2 Double Pixel]) -- generate a grid
       -> (V2 Double Pixel -> Double)      -- generate a weight from point
       -> AntialiasFunc
kernel gridF weightF drawF vPixel =
    let (colors, weights) = unzip . map (drawF &&& weightF) $ gridF vPixel
        total = sum $ zipWith scale3 colors weights
    in scale3 total (1.0 / sum weights)

data RadialBlurOptions = RadialBlurOptions { stdDev :: Double
                                           }

radialBlur :: SquareGridOptions
           -> RadialBlurOptions
           -> AntialiasFunc
radialBlur gridOpts (RadialBlurOptions std) drawF vp@(V2 px py) =
    let gauss (V2 x y) =
            let x' = px - x
                y' = py - y
                twoSigma = 2 * std * std
                expo = negate (x'*x' + y'*y') / twoSigma
            in exp expo

        gridF = squareGrid gridOpts
  
    in kernel gridF gauss drawF vp
