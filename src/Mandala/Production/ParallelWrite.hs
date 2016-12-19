{-# LANGUAGE TypeOperators #-}

module Mandala.Production.ParallelWrite
  ( writeToBuffer
  ) where

import Mandala.Types

import Data.Tuple (swap)

import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as MVector

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.MVar as MVar
import Control.Concurrent.MVar (MVar)
import qualified Control.Monad as Monad


data BufferIndex

writeToBuffer :: ScreenInfo -> (V2 Double Pixel -> color) -> IO (Vector color)
writeToBuffer screenInfo colorF =
    let numPixels = screenLength screenInfo
        --borderLen = fromIntegral $ borderLength screenInfo
        indexToPixelCoord :: Int ::: BufferIndex -> V2 Double Pixel
        indexToPixelCoord i =
            let V2 ix iy = uncurry V2 . swap $ i `divMod` numPixels
            in V2 (fromIntegral ix) (fromIntegral iy)

        bufferLen = numPixels * numPixels

        workerF = colorF . indexToPixelCoord

        forkWorker v (s, e) = Concurrent.forkIO . worker v workerF s e

    in do numWorkers <- Concurrent.getNumCapabilities
          vec <- MVector.new bufferLen

          let bounds = genBounds numWorkers bufferLen
              ((s1, e1), remBounds) = (head bounds, tail bounds)
              -- want the main thread to do the first interval

          signals <- mapM (const MVar.newEmptyMVar) bounds
          Monad.zipWithM_ (forkWorker vec) remBounds (tail signals)
          worker vec workerF s1 e1 (head signals)
          Monad.forM_ signals MVar.takeMVar

          Vector.unsafeFreeze vec

genBounds :: Int -> Int -> [(Int ::: BufferIndex, Int ::: BufferIndex)]
genBounds numWorkers len =
    let (base, remainder) = len `divMod` numWorkers

        go 1 s _ = [(s, len - 1)]
        go i s r = (s, end) : go (i-1) (end+1) (r-1)
          where end = s + base + (if r > 0 then 0 else (-1))

    in go numWorkers 0 remainder

worker :: IOVector color -- output buffer
       -> (Int ::: BufferIndex -> color) -- bufferindex -> color func
       -> Int ::: BufferIndex -- start index
       -> Int ::: BufferIndex -- end index
       -> MVar () -- synchronization primitive
       -> IO ()
worker vec f start end signal =
    let write :: Int ::: BufferIndex -> IO ()
        write i = MVector.unsafeWrite vec i $! f i

        go i|i > end   = MVar.putMVar signal ()
        go i|otherwise = write i >> go (i+1)

    in go start
