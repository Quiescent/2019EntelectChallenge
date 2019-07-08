module Simulate
  where

import qualified Data.List as L

import RIO.Directory

import Import

data Result = Success
            | Failure String

withRoundsDirectories :: FilePath -> ([FilePath] -> RIO App Result) -> RIO App Result
withRoundsDirectories matchLogsDirectory f = do
  (fmap (map ((++) (matchLogsDirectory ++ "/")) . L.sort . filter (L.isPrefixOf "Round")) $
    listDirectory matchLogsDirectory) >>= f
