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

import System.Clock
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
  args   <- getArgs
  guard (length args >= 1)
  let dataSetDirectory = head args
  let startFrom        = headMaybe $ tail args
  let repeatTimes      = (tailMaybe $ tail args) >>= headMaybe
  when (isJust startFrom && (not $ isJust repeatTimes)) $
    putStrLn ("> Starting search from: " ++ show (fromJust startFrom))
  when (isJust startFrom && isJust repeatTimes) $
    putStrLn ("> Repeating search on: " ++
              show (fromJust startFrom) ++
              " " ++
              show (fromJust repeatTimes) ++
              " times")
  runDataSet dataSetDirectory startFrom repeatTimes

runDataSet :: FilePath -> Maybe String -> Maybe String -> IO ()
runDataSet matchLogsDirectory startFrom repeatTimes = do
  withRoundsDirectories matchLogsDirectory (runSearchForEachRound startFrom repeatTimes)
  IO.putStrLn ("Ran search for each round in: " ++ matchLogsDirectory)

runSearchForEachRound :: Maybe String -> Maybe String -> [FilePath] -> IO ()
runSearchForEachRound _         _           []           = error "No round directories to profile over."
runSearchForEachRound startFrom repeatTimes directories  =
  let directories' =
        if isJust startFrom
        then dropWhile (not . isInfixOf (fromJust startFrom)) directories
        else directories
      repeatMode  = isJust repeatTimes
      repeatCount = read $ fromJust repeatTimes
      clock       = Realtime
  in if repeatMode
     then do
       let directory = head directories'
       state         <- fmap fromJust $ loadStateForRound directory
       gen           <- getStdGen
       treeVariable  <- newVariable SearchFront
       stateChannel  <- newComms
       _             <- forkIO (iterativelyImproveSearch gen state SearchFront stateChannel treeVariable)
       forM_ [(1::Int)..repeatCount] $ \ i -> do
         logStdErr $ "Round " ++ show i ++ "/" ++ show repeatCount
         startingTime <- fmap toNanoSecs $ getTime clock
         _            <- searchForAlottedTime state clock startingTime treeVariable
         writeComms stateChannel ((fromMoves doNothing doNothing), False, state)
     else if (length directories' < 1)
          then ((putStrLn ("User error: specified directory has less than 1 rounds after starting from: " ++
                          case startFrom of
                            Just x -> x
                            _      -> "the beginning.")))
          else do
    let directory     = head directories'
    let directories'' = tail directories'
    state         <- fmap fromJust $ loadStateForRound directory
    gen           <- getStdGen
    treeVariable  <- newVariable SearchFront
    stateChannel  <- newComms
    _             <- forkIO (iterativelyImproveSearch gen state SearchFront stateChannel treeVariable)
    mapM_ (\ directory' -> do
              logStdErr $ "Profiling directory: " ++ directory'
              startingTime <- fmap toNanoSecs $ getTime clock
              state'       <- fmap fromJust $ loadStateForRound directory'
              _            <- searchForAlottedTime state' clock startingTime treeVariable
              -- Simulate the worst case scenario.  Both players do
              -- nothing and we don't have it in the tree.
              writeComms stateChannel ((fromMoves doNothing doNothing), False, state'))
      directories''
