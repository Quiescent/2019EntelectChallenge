{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Import
import Bot

import Control.Exception as E
import System.Random

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  let app = App {}
  g <- getStdGen
  E.catch (runRIO app $ startBot g) exceptionHandler
  where
    exceptionHandler :: SomeException -> IO ()
    exceptionHandler e = do
      logStdErr $ "Worker died with (" ++ show e ++ ")... restarting."
      let app = App {}
      g <- getStdGen
      runRIO app $ startBot g
