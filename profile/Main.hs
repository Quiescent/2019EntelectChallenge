{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Main
  where

import Import
import Simulate
import Bot

import RIO.List
import RIO.List.Partial
import Control.Concurrent
import System.Random
import System.Environment
import Data.Maybe
import qualified System.IO as IO

import Prelude (read, putStrLn)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  let app = App {}
  args   <- getArgs
  guard (length args >= 1)
  let dataSetDirectory = head args
  let startFrom        = headMaybe $ tail args
  let repeatTimes      = (tailMaybe $ tail args) >>= headMaybe
  when (isJust startFrom && (not $ isJust repeatTimes)) $
    liftIO $ putStrLn ("> Starting search from: " ++ show (fromJust startFrom))
  when (isJust startFrom && isJust repeatTimes) $
    liftIO $ putStrLn ("> Repeating search on: " ++
                       show (fromJust startFrom) ++
                       " " ++
                       show (fromJust repeatTimes) ++
                       " times")
  runRIO app $ runDataSet dataSetDirectory startFrom repeatTimes

runDataSet :: FilePath -> Maybe String -> Maybe String -> RIO App ()
runDataSet matchLogsDirectory startFrom repeatTimes = do
  withRoundsDirectories matchLogsDirectory (runSearchForEachRound startFrom repeatTimes)
  liftIO $ IO.putStrLn ("Ran search for each round in: " ++ matchLogsDirectory)

runSearchForEachRound :: Maybe String -> Maybe String -> [FilePath] -> RIO App ()
runSearchForEachRound _         _           []           = error "No round directories to profile over."
runSearchForEachRound startFrom repeatTimes directories  =
  let directories' =
        if isJust startFrom
        then dropWhile (not . isInfixOf (fromJust startFrom)) directories
        else directories
      repeatMode  = isJust repeatTimes
      repeatCount = read $ fromJust repeatTimes
  in if repeatMode
     then do
       let directory = head directories'
       state         <- fmap fromJust $ loadStateForRound directory
       gen           <- liftIO getStdGen
       treeChannel   <- liftIO newComms
       stateChannel  <- liftIO newComms
       _             <- liftIO $ forkIO (iterativelyImproveSearch gen state SearchFront stateChannel treeChannel)
       forM_ [(1::Int)..repeatCount] $ \ i -> do
         liftIO $ logStdErr $ "Round " ++ show i ++ "/" ++ show repeatCount
         _      <- liftIO $ searchForAlottedTime state treeChannel
         liftIO $ writeComms stateChannel ((fromMoves doNothing doNothing), state)
     else if (length directories' < 1)
          then (liftIO (putStrLn ("User error: specified directory has less than 1 rounds after starting from: " ++
                             case startFrom of
                               Just x -> x
                               _      -> "the beginning.")))
          else do
    let directory     = head directories'
    let directories'' = tail directories'
    state         <- fmap fromJust $ loadStateForRound directory
    gen           <- liftIO getStdGen
    treeChannel   <- liftIO newComms
    stateChannel  <- liftIO newComms
    _             <- liftIO $ forkIO (iterativelyImproveSearch gen state SearchFront stateChannel treeChannel)
    mapM_ (\ directory' -> do
              liftIO $ logStdErr $ "Profiling directory: " ++ directory'
              state' <- fmap fromJust $ loadStateForRound directory'
              _      <- liftIO $ searchForAlottedTime state' treeChannel
              -- Simulate the worst case scenario.  Both players do
              -- nothing and we don't have it in the tree.
              liftIO $ writeComms stateChannel ((fromMoves doNothing doNothing), state'))
      directories''
