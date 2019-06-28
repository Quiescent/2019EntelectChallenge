{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Import

import Bot

import RIO.List.Partial
import RIO.Directory

import qualified Data.List as L

import System.Environment

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  let app = App {}
  args   <- getArgs
  when (length args == 1) $ do
    let logDirectory = head args
    runRIO app $ runTest logDirectory

runTest :: FilePath -> RIO App ()
runTest matchLogDirectory = do
  roundsDirectories <- fmap (filter (L.isPrefixOf "Round")) $ listDirectory matchLogDirectory
  -- TODO sort the rounds here...
  simulateAndCheckRounds roundsDirectories
  
simulateAndCheckRounds :: [FilePath] -> RIO App ()
simulateAndCheckRounds (directory:directories) = do
  initialState <- loadStateForRound directory
  iter initialState directories
  where
    iter :: State -> [FilePath] -> RIO App ()
    iter _ _ = undefined

loadStateForRound :: FilePath -> RIO App State
loadStateForRound _ = undefined

loadThisPlayersCommand :: FilePath -> RIO App Move
loadThisPlayersCommand _ = undefined

loadThatPlayersCommand :: FilePath -> RIO App Move
loadThatPlayersCommand _ = undefined

tickState :: Move -> Move -> State -> RIO App State
tickState _ _ _ = undefined

