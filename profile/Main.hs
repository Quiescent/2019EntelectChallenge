module Main
  where

import Import
import Simulate
import Bot

import Control.Concurrent
import System.Random
import System.Environment
import Data.Maybe
import qualified System.IO as IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  let app = App {}
  args   <- getArgs
  guard (length args == 1)
  let dataSetDirectory = head args
  runRIO app $ runDataSet dataSetDirectory

runDataSet :: FilePath -> RIO App ()
runDataSet matchLogsDirectory = do
  withRoundsDirectories matchLogsDirectory runSearchForEachRound
  liftIO $ IO.putStrLn ("Ran search for each round in: " ++ matchLogsDirectory)

-- This is in microseconds.
oneSecond :: Int
oneSecond = 1000000

runSearchForEachRound :: [FilePath] -> RIO App ()
runSearchForEachRound []                      = error "No round directories to profile over."
runSearchForEachRound (directory:directories) = do
  state         <- fmap fromJust $ loadStateForRound directory
  gen           <- liftIO getStdGen
  treeChannel   <- liftIO newComms
  stateChannel  <- liftIO newComms
  _             <- liftIO $ forkIO (iterativelyImproveSearch gen state SearchFront stateChannel treeChannel)
  mapM_ (\ directory' -> do
            state' <- fmap fromJust $ loadStateForRound directory'
            _      <- liftIO $ searchForAlottedTime treeChannel
            -- Simulate the worst case scenario.  Both players do
            -- nothing and we don't have it in the tree.
            liftIO $ writeComms stateChannel ((fromMoves doNothing doNothing), state'))
        directories
