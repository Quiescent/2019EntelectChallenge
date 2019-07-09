module Main
  where

import Import
import Simulate
import Bot

import System.Environment
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
runSearchForEachRound directories =
  go directories []
  where
    go []         results = return $ GamesPlayedPerRound (reverse results)
    go (dir:dirs) results = do
      state <- loadStateForRound dir
      if not $ isJust state
      then return (Failed $ "Couldn't load state from: " ++ show dir)
      else liftIO (treeAfterHalfSecond (fromJust state)) >>=
           ( \ tree -> go dirs (countGames tree : results))

countGames :: SearchTree -> Int
countGames = sum . map ( (\ (Played x) -> x) . played) . myMovesFromTree
