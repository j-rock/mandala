{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Mandala.Math.Interval where

import qualified Data.List as List
import Mandala.Types

-- Meant to be curried. Pass in a an interval list and get a function that takes
-- a uv-radius and possibly returns the interval data with a relative uv-radius.
searchInterval :: [Interval a] -> Search a
searchInterval is r = searchFlattened (flattenIntervals is) r


data IntervalMetadata = IntervalMetadata { minVal  :: Double
                                         , maxVal  :: Double
                                         , trueMin :: Double
                                         , trueMax :: Double
                                         } deriving (Show)

searchFlattened :: [(IntervalMetadata, a)] -> Search a
searchFlattened is rad =
    let withinBounds (IntervalMetadata{..}, _) = trueMin <= rad && rad <= trueMax
        withinRealBounds (IntervalMetadata{..}, _) = minVal <= rad && rad <= maxVal

        preserveTop _ j| withinRealBounds j = GT
                       | otherwise          = LT

        matches = filter withinBounds is
        orderedMatches = List.sortBy preserveTop matches
  
        relative (IntervalMetadata{..}, a) = ((rad - trueMin) / (trueMax - trueMin), a)

    in map relative orderedMatches

flattenIntervals :: [Interval a] -> [(IntervalMetadata, a)]
flattenIntervals is =
    let weightSum = computeIntervalLength is
        scaled    = scaleIntervalLengths weightSum is
    in collapseScaledIntervals scaled

computeIntervalLength :: [Interval a] -> Double
computeIntervalLength =
    sum . map (\Interval{..} -> intervalLength * (1 - tailOverlapRatio))

scaleIntervalLengths :: Double -> [Interval a] -> [Interval a]
scaleIntervalLengths w = map $ \i -> i{intervalLength = intervalLength i / w}

collapseScaledIntervals :: [Interval a] -> [(IntervalMetadata, a)]
collapseScaledIntervals is =
    let go  _     _     []                = []
        go !start !skip (Interval{..}:js) =
            let isOver = overOrUnderNext == Over

                maxValue|isOver    = trueMax imeta
                        |otherwise = nextStart

                imeta = IntervalMetadata { minVal  = start + skip
                                         , maxVal  = maxValue
                                         , trueMin = start
                                         , trueMax = start + intervalLength
                                         }

                nextStart = start + intervalLength * (1 - tailOverlapRatio)

                nextSkip|isOver    = intervalLength * tailOverlapRatio
                        |otherwise = 0

            in (imeta, idata) : go nextStart nextSkip js
    in go 0 0 is

debugIntervals :: Graph -> [(IntervalMetadata, ())]
debugIntervals = flattenIntervals . map (fmap (const ())) . map unwrapGraphNode . unwrapGraph
