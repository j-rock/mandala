{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
module Mandala.Types.Graph where

import Control.Applicative ((<|>))
import Data.Semigroup
import Mandala.Types.Annotations
import Mandala.Types.Color
import Mandala.Types.Math


newtype Process input = Process { apply :: input -> Maybe Color
                                }

instance Show (Process input) where
  show _ = "Process"

instance Semigroup (Process input) where
  Process f <> Process g = Process $ \i -> f i <|> g i

data ScreenInfo = ScreenInfo { screenLength    :: Int ::: Pixel
                             , borderLength    :: Int ::: Pixel
                             , backgroundColor :: V2 Double Pixel -> Color
                             }

data SectorInfo = SectorInfo { numSectors :: Int
                             , reflection :: Bool
                             }

data OverUnder = Over | Under deriving (Eq, Show)

type Search a = Double ::: UV -> [(Double ::: UV, a)]

data SequenceInfo = SequenceInfo { seqLength       :: Double
                                 , overUnder       :: OverUnder
                                 , overlapFraction :: Double
                                 }

data Interval a = Interval { intervalLength   :: Double
                           , overOrUnderNext  :: OverUnder
                           , tailOverlapRatio :: Double
                           , idata            :: a
                           } deriving (Show, Functor)

newtype FunctionNode input = FNode { unwrapFNode :: Interval (Process input)
                                   } deriving (Show)

data Input = Input { uvCoord :: V2 Double UV
                   } deriving (Show)

newtype FunctionSequence = FSeq { unwrapFSeq :: [FunctionNode Input]
                                } deriving (Show)

instance Semigroup FunctionSequence where
  FSeq fs <> FSeq gs = FSeq $ fs <> gs

newtype GraphNode = GNode { unwrapGraphNode :: Interval (SectorInfo, FunctionSequence)
                          }

newtype Graph = Graph { unwrapGraph :: [GraphNode]
                      }

instance Semigroup Graph where
  Graph fs <> Graph gs = Graph $ fs <> gs
