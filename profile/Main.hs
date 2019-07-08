module Main
  where

import Import
import Simulate
import System.Environment
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
  result <- withRoundsDirectories matchLogsDirectory runSearchForEachRound
  case result of
    Success           -> liftIO $ IO.putStrLn ("Ran search for each round in: " ++ matchLogsDirectory)
    (Failure message) -> liftIO $ IO.putStrLn message

runSearchForEachRound :: [FilePath] -> RIO App Result
runSearchForEachRound = undefined
