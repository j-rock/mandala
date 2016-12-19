{-# LANGUAGE RecordWildCards #-}
module Exp2.Mandala where

import Mandala

import Data.Semigroup
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
                        , backgroundColor = const c3
                        }

graph :: Graph
graph = genSeq $ [ ((SectorInfo 1  True, whiteCircle), SequenceInfo 0.1 Under 0.2)

                 , ((SectorInfo 7  True, quadratic c1 c2),   SequenceInfo 2 Over 0.9)
                 , ((SectorInfo 28 True, downQ     c3 c4),   SequenceInfo 1 Over 0.6)
                 , ((SectorInfo 14 True, downQ'    c5 c1),   SequenceInfo 1 Over 0.4)

                 , ((SectorInfo 7  True, quadratic c2 c3),   SequenceInfo 2 Over 0.9)
                 , ((SectorInfo 28 True, downQ     c4 c5),   SequenceInfo 1 Over 0.6)
                 , ((SectorInfo 14 True, downQ'    c1 c2),   SequenceInfo 1 Over 0.4)

                 , ((SectorInfo 7  True, quadratic c3 c4),   SequenceInfo 2 Over 0.9)
                 , ((SectorInfo 28 True, downQ     c5 c1),   SequenceInfo 1 Over 0.6)
                 , ((SectorInfo 14 True, downQ'    c2 c3),   SequenceInfo 1 Over 0.4)

                 , ((SectorInfo 7  True, quadratic c4 c5),   SequenceInfo 2 Over 0.9)
                 , ((SectorInfo 28 True, downQ     c1 c2),   SequenceInfo 1 Over 0.6)
                 , ((SectorInfo 14 True, downQ'    c3 c4),   SequenceInfo 1 Over 0.4)

                 , ((SectorInfo 7  True, quadratic c5 c1),   SequenceInfo 2 Over 0.9)
                 , ((SectorInfo 28 True, downQ     c2 c3),   SequenceInfo 1 Over 0.6)
                 , ((SectorInfo 14 True, downQ'    c4 c5),   SequenceInfo 1 Over 0.4)

                 , ((SectorInfo 1 True, whiteCircle),  SequenceInfo 0.1 Over 0)
                 ]

c1,c2,c3,c4,c5 :: Color
c1 = hexColor 0xd6 0xf4 0xa6
c2 = hexColor 0x22 0xe8 0xde
c3 = hexColor 0x00 0x87 0x91
c4 = hexColor 0xff 0xbf 0xdf
c5 = hexColor 0x6d 0x32 0x6d

whiteCircle :: FunctionSequence
whiteCircle = singletonProcess $ colorF $ rgb 255 255 255

quadratic :: Color -> Color -> FunctionSequence
quadratic a b = fOfX a b 0.2 $
               (\(V2 u v) -> v <= (u-0.5)**2)

downQ :: Color -> Color -> FunctionSequence
downQ a b = fOfX a b 0.3 $
              (\(V2 u v) -> v <= 0.7 - (u**2))

downQ' :: Color -> Color -> FunctionSequence
downQ' a b = fOfX a b 0.1 $
               (\(V2 u v) -> v <= 0.7 - ((u-0.5)**2))

type Function = V2 Double UV -> Bool

fOfX :: Color -> Color -> Double -> Function -> FunctionSequence
fOfX inClr outClr edgeWidth func =
    let wrapFunction i = Process $ \(Input uvCoord) ->
                             if func uvCoord
                             then Just i
                             else Nothing

        shift (Input (V2 u v)) = Input $ V2 u (v - edgeWidth)

        lower = wrapFunction outClr
        upper = enhance shift $ wrapFunction inClr

    in singletonProcess $ lower <> upper
