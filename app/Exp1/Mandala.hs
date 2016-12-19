{-# LANGUAGE RecordWildCards #-}
module Exp1.Mandala where

import Mandala

import Data.Vector (Vector)

draw :: IO (Vector (V3 Double RGB))
draw =
    let antialiaser =
          radialBlur SquareGridOptions{ gridRadius = 0.3
                                      , numSteps   = 5
                                      }
                     RadialBlurOptions { stdDev = 1.5
                                       }
        drawFunc = antialiaser $ reifyGraph screenInfo graph
    in writeToBuffer screenInfo drawFunc

screenInfo :: ScreenInfo
screenInfo = ScreenInfo { screenLength = 512
                        , borderLength = 1
                        , backgroundColor = const $ hexColor 0xb1 0xbc 0xce
                        }

graph :: Graph
graph = weightedSeq $
             [ ((SectorInfo 16  True, blueBlack   ), 1)
             , ((SectorInfo 1  False, coffeeRing 6), 3)
             , ((SectorInfo 128 True, blueBlack   ), 1)
             , ((SectorInfo 1  False, coffeeRing 7), 1)
             , ((SectorInfo 300 True, blueBlack   ), 1)
             ]

blueBlack :: FunctionSequence
blueBlack =
    let q color = Process $ \i -> Just $ scale3 color $ theta i
          where theta = _v2x . uvCoord
        blue  = q $ hexColor 0xeb 0xcf 0xb9
        black = q $ hexColor 0x54 0x35 0x32
    in weightedSeq [ (blue, 40.0)
                   , (black, 1.0)
                   ]

coffeeRing :: Int -> FunctionSequence
coffeeRing n = uniformSeq $ take n $ cycle $
                   [ colorF $ hexColor 0x26 0x12 0x12
                   , colorF $ hexColor 0x54 0x35 0x32
                   , colorF $ hexColor 0xeb 0xcf 0xb9
                   , colorF $ hexColor 0xee 0xf0 0xef
                   , colorF $ hexColor 0xb1 0xbc 0xce
                   ]
