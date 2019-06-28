{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Import

import RIO.List.Partial

import System.Environment

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  let app = App {}
  args   <- getArgs
  when (length args == 1) $ do
    let logDirectory = head args
    runRIO app $ runTest logDirectory

runTest :: String -> RIO App ()
runTest _ = undefined
