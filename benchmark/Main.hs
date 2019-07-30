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
  let app = App {}
  args   <- getArgs
  guard (length args == 1)
  let dataSetDirectory = head args
  runRIO app $ runDataSet dataSetDirectory

type Message = String
type GamesPlayed = Int

data BenchmarkResult = GamesPlayedPerRound [GamesPlayed]
                     | Failed Message

joinString :: Show a => String -> [a] -> String
joinString joiner strings =
  let withExtra = concat $ map ( \ x -> show x ++ joiner) strings
  in take ((length withExtra) - (length joiner)) withExtra

runDataSet :: FilePath -> RIO App ()
runDataSet matchLogsDirectory = do
  result <- withRoundsDirectories matchLogsDirectory runSearchForEachRound
  case result of
    (GamesPlayedPerRound gamesPlayedPerRound) ->
      liftIO $ IO.putStrLn (joinString "," gamesPlayedPerRound)
    (Failed message) -> liftIO $ IO.putStrLn message

runSearchForEachRound :: [FilePath] -> RIO App BenchmarkResult
runSearchForEachRound []                      = error "No directories to benchmark over."
runSearchForEachRound (directory:directories) = do
  state         <- fmap fromJust $ loadStateForRound directory
  gen           <- liftIO getStdGen
  treeChannel   <- liftIO newComms
  stateChannel  <- liftIO newComms
  _             <- liftIO $ forkIO (iterativelyImproveSearch gen state SearchFront stateChannel treeChannel)
  go directories [] treeChannel stateChannel
  where
    go []         results _           _            = return $ GamesPlayedPerRound (reverse results)
    go (dir:dirs) results treeChannel stateChannel = do
      state <- loadStateForRound dir
      if not $ isJust state
      then return (Failed $ "Couldn't load state from: " ++ show dir)
      else do
        liftIO $ logStdErr $ "Benchmarking directory: " ++ dir
        tree <- liftIO $ treeAfterAlottedTime treeChannel
        go dirs (countGames tree : results) treeChannel stateChannel
