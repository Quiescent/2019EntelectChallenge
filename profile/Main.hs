module Main
  where

import Import
import Simulate
import Bot

import RIO.List
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
  guard (length args >= 1)
  let dataSetDirectory = head args
  let startFrom = headMaybe $ tail args
  runRIO app $ runDataSet dataSetDirectory startFrom

runDataSet :: FilePath -> Maybe String -> RIO App ()
runDataSet matchLogsDirectory startFrom = do
  withRoundsDirectories matchLogsDirectory (runSearchForEachRound startFrom)
  liftIO $ IO.putStrLn ("Ran search for each round in: " ++ matchLogsDirectory)

runSearchForEachRound :: Maybe String -> [FilePath] -> RIO App ()
runSearchForEachRound _         []           = error "No round directories to profile over."
runSearchForEachRound startFrom directories  =
  let directories' =
        if isJust startFrom
        then dropWhile (not . isInfixOf (fromJust startFrom)) directories
        else directories
  in if (length directories' > 1)
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
              _      <- liftIO $ searchForAlottedTime state treeChannel
              -- Simulate the worst case scenario.  Both players do
              -- nothing and we don't have it in the tree.
              liftIO $ writeComms stateChannel ((fromMoves doNothing doNothing), state'))
      directories''
