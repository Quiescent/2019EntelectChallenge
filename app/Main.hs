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
  g <- getStdGen
  E.catch (startBot g) exceptionHandler
  where
    exceptionHandler :: SomeException -> IO ()
    exceptionHandler e = do
      logStdErr $ "Worker died with (" ++ show e ++ ")... restarting."
      main
