{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Mandala.Math.Coordinates where

import Mandala.Types

pixelToCartesian :: ScreenInfo -> V2 Double Pixel -> V2 Double Cartesian
pixelToCartesian ScreenInfo{..} (V2 px py) =
    let len = fromIntegral $ screenLength - borderLength
        x = px / len - 0.5
        y = 0.5 - py / len
    in V2 x y

cartesianToPolar :: V2 Double Cartesian -> V2 Double Polar
cartesianToPolar (V2 x y) =
    let r = sqrt (x*x + y*y) / 0.5 -- have to account for cartesian range [-0.5, 0.5]
        rawRadian = atan2 y x
        coterminalRadian = if rawRadian < 0 then rawRadian + (2*pi) else rawRadian
        theta = 180.0 * coterminalRadian / pi
    in V2 theta r

pixelToPolar :: ScreenInfo -> V2 Double Pixel -> V2 Double Polar
pixelToPolar si = cartesianToPolar . pixelToCartesian si
