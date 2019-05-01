{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Import
import Bot

import System.Random

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  let app = App {}
  g <- getStdGen
  runRIO app (startBot g 1)
