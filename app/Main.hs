{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Mandala
import qualified Exp2.Mandala as Exp

import Options.Generic

data Args = Args { outfile :: FilePath }
            deriving (Generic)

instance ParseRecord Args


suffixed :: String -> String -> IO ()
say, waiting, exclaim :: String -> IO ()

suffixed suffix str = putStrLn $ str ++ suffix
say     = suffixed "."
waiting = suffixed "..."
exclaim = suffixed "!"

main :: IO ()
main = do args <- getRecord "Mandala PNG Generator"

          waiting "Generating image"
          buffer <- Exp.draw

          waiting "Writing image"
          Mandala.writeStdPng (outfile args) buffer

          exclaim "Success"
