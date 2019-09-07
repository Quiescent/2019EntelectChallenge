module Main
  where

import Import
import Simulate
import Bot

import System.Random
import System.Environment
import Control.Concurrent
import qualified System.IO as IO
import Data.Maybe

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args   <- getArgs
  guard (length args == 1)
  let dataSetDirectory = head args
  runDataSet dataSetDirectory

type Message = String

data BenchmarkResult = GamesPlayedPerRound [Int]
                     | Failed Message

joinString :: Show a => String -> [a] -> String
joinString joiner strings =
  let withExtra = concat $ map ( \ x -> show x ++ joiner) strings
  in take ((length withExtra) - (length joiner)) withExtra

runDataSet :: FilePath -> IO ()
runDataSet matchLogsDirectory = do
  result <- withRoundsDirectories matchLogsDirectory runSearchForEachRound
  case result of
    (GamesPlayedPerRound gamesPlayedPerRound) ->
      IO.putStrLn (joinString "," gamesPlayedPerRound)
    (Failed message) -> IO.putStrLn message

runSearchForEachRound :: [FilePath] -> IO BenchmarkResult
runSearchForEachRound []                      = error "No directories to benchmark over."
runSearchForEachRound (directory:directories) = do
  state         <- fmap fromJust $ loadStateForRound directory
  gen           <- getStdGen
  treeChannel   <- newVariable SearchFront
  stateChannel  <- newComms
  _             <- forkIO (iterativelyImproveSearch gen state SearchFront stateChannel treeChannel)
  go directories [] treeChannel stateChannel
  where
    go []         results _           _            = return $ GamesPlayedPerRound (reverse results)
    go (dir:dirs) results treeChannel stateChannel = do
      state <- loadStateForRound dir
      if not $ isJust state
      then return (Failed $ "Couldn't load state from: " ++ show dir)
      else do
        logStdErr $ "Benchmarking directory: " ++ dir
        tree <- treeAfterAlottedTime (fromJust state) treeChannel
        let count' = countGames tree
        logStdErr $ "Games played for state: " ++ show count'
        writeComms stateChannel (fromMoves doNothing doNothing, False, (fromJust state))
        go dirs (count' : results) treeChannel stateChannel
