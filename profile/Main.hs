module Main
  where

import Import
import System.Environment

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  let app = App {}
  args   <- getArgs
  guard (length args == 1)
  let dataSetDirectory = head args
  runRIO app $ runDataSet dataSetDirectory

runDataSet :: FilePath -> RIO App ()
runDataSet = undefined
