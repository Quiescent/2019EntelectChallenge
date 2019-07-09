module Simulate
  where

import Bot
import Import

import qualified Data.List as L
import qualified RIO.ByteString.Lazy as B
import RIO.List
import RIO.Directory
import Data.Maybe
import Data.Aeson (decode)

data Result = Success
            | Failure String

withRoundsDirectories :: FilePath -> ([FilePath] -> RIO App a) -> RIO App a
withRoundsDirectories matchLogsDirectory f = do
  (fmap (map ((++) (matchLogsDirectory ++ "/")) . L.sort . filter (L.isPrefixOf "Round")) $
    listDirectory matchLogsDirectory) >>= f

loadStateForRound :: FilePath -> RIO App (Maybe State)
loadStateForRound path = do
  playerPaths     <- listDirectory path
  let aPlayersPath = headMaybe $ L.sort playerPaths
  if isJust aPlayersPath
  then fmap decode $ B.readFile (path ++ "/" ++ (fromJust aPlayersPath) ++ "/JsonMap.json")
  else return Nothing
