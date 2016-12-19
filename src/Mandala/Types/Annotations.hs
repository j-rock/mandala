{-# LANGUAGE TypeOperators #-}
module Mandala.Types.Annotations where

type (:::) rep ann = rep

data Pixel
data Cartesian -- x,y:[-0.5,0.5]
data UV        -- u,v:[0,1]
data Polar     -- theta:[0,360], r:[0,+inf)
data Degree    -- theta:[0,360]
data RGB
data Radius
