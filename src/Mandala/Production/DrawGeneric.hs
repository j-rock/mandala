{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Mandala.Production.DrawGeneric
  ( reifyGraph
  ) where

import Control.Arrow (second)
import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Mandala.Math
import Mandala.Types


reifyGraph :: ScreenInfo -> Graph -> V2 Double Pixel -> Color
reifyGraph si@ScreenInfo{..} graph = \vPixel ->
    let vPolar = pixelToPolar si vPixel

        colorFromPolarCoord :: V2 Double Polar -> Maybe Color
        colorFromPolarCoord = reifyScreenAgnostic graph

    in Maybe.fromMaybe (backgroundColor vPixel) $ colorFromPolarCoord vPolar


reifyScreenAgnostic :: Graph -> V2 Double Polar -> Maybe Color
reifyScreenAgnostic graph (V2 theta r) =
    let sectorNodes :: [Interval (SectorInfo, FunctionSequence)]
        sectorNodes = unwrapGraphNode <$> unwrapGraph graph

        applySearchInternal :: FunctionSequence -> Search (Process Input)
        applySearchInternal = searchInterval . map unwrapFNode . unwrapFSeq

        appliedInternalSearch = (fmap . fmap) (second applySearchInternal) $ sectorNodes

        largeSearch :: Search (SectorInfo, Search (Process Input))
        largeSearch = searchInterval appliedInternalSearch

        matches :: [Maybe Color]
        matches = do (relativeGraphNodeRadius, (sectorInfo, pSearch)) <- largeSearch r
                     (relativeProcessRadius, process) <- pSearch relativeGraphNodeRadius
                     return $ apply process $ buildInput sectorInfo (V2 theta relativeProcessRadius)

    in Monad.join $ List.find Maybe.isJust matches

buildInput :: SectorInfo -> V2 Double Polar -> Input
buildInput SectorInfo{..} (V2 t r) =
    let perSectorAngularWidth = 360.0 / fromIntegral numSectors
        bucketIdx = truncate $ (t / perSectorAngularWidth) :: Int

        reflected = reflection && mod bucketIdx 2 == 1
        theta|reflected = (fromIntegral bucketIdx + 1) - t / perSectorAngularWidth
             |otherwise = t / perSectorAngularWidth - fromIntegral bucketIdx

    in Input { uvCoord = V2 theta r
             }
