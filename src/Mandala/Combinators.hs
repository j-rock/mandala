{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Mandala.Combinators where

import Data.Semigroup
import Mandala.Types

class Wrappable a where
  type Wrapped a :: *
  wrap :: a -> SequenceInfo -> Wrapped a
  unseq :: a -> SequenceInfo -> Interval a
  unseq a SequenceInfo{..} = Interval seqLength overUnder overlapFraction a

instance Wrappable (Process Input) where
  type Wrapped  (Process Input) = FunctionSequence
  wrap :: Process Input -> SequenceInfo -> FunctionSequence
  wrap p si = FSeq [FNode $ unseq p si]

instance Wrappable (SectorInfo, FunctionSequence) where
  type Wrapped (SectorInfo, FunctionSequence) = Graph
  wrap :: (SectorInfo, FunctionSequence) -> SequenceInfo -> Graph
  wrap a si = Graph [GNode $ unseq a si]

genSeq :: (Wrappable a, Semigroup (Wrapped a)) => [(a, SequenceInfo)] -> Wrapped a
genSeq = foldr1 (<>) . map (uncurry wrap)

weightedSeq :: (Wrappable a, Semigroup (Wrapped a)) => [(a, Double)] -> Wrapped a
weightedSeq fs =
    let wrapped (a, weight) = wrap a seqInfo
          where seqInfo = SequenceInfo weight Over 0
    in foldr1 (<>) $ wrapped <$> fs

uniformSeq :: (Wrappable a, Semigroup (Wrapped a)) => [a] -> Wrapped a
uniformSeq = weightedSeq . fmap (\a -> (a, 1))

colorF :: Color -> Process Input
colorF = Process . const . Just

singletonProcess :: Process Input -> FunctionSequence
singletonProcess p = uniformSeq [p]

enhance :: (new -> old) -> Process old -> Process new
enhance f (Process p) = Process $ \new -> p (f new)

