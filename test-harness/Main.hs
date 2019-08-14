{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Import

import Bot
import Simulate

import RIO.List.Partial
import RIO.List
import RIO.Directory
import Data.Maybe
import qualified System.IO           as IO
import qualified System.Process      as Process

import qualified Data.List as L

import System.Environment

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  let app = App {}
  args   <- getArgs
  guard (length args == 1)
  let logDirectory = head args
  runRIO app $ runTest logDirectory

runTest :: FilePath -> RIO App ()
runTest matchLogDirectory = do
  result <- withRoundsDirectories matchLogDirectory simulateAndCheckRounds
  case result of
     Success           -> liftIO $ IO.putStrLn ("Successfully simulated: " ++ matchLogDirectory)
     (Failure message) -> liftIO $ IO.putStrLn message

simulateAndCheckRounds :: [FilePath] -> RIO App Result
simulateAndCheckRounds []                      = return $ Failure "There are no rounds in the given directory."
simulateAndCheckRounds dirs@(directory:_) = do
  initialState <- loadStateForRound directory
  if not $ isJust initialState
  then return (Failure $ "Couldn't load initial state from: " ++ show directory)
  else iter (fromJust initialState) dirs
  where
    iter :: State -> [FilePath] -> RIO App Result
    iter currentState (path:nextPath:paths) = do
      let thisWormsCoord'    = thisWormsCoord currentState
      let thatWormsCoord'    = thatWormsCoord currentState
      -- Assume: that there are valid initial worm positions
      thisMove              <- loadThisPlayersCommand currentState thisWormsCoord' path
      thatMove              <- loadThatPlayersCommand currentState thatWormsCoord' path
      let movesAreValid      = isJust thisMove && isJust thatMove
      if not movesAreValid
      then return $ (Failure $ "Couldn't load the players moves for: " ++ show directory)
      else do
        nextState             <- loadStateForRound nextPath
        let simulatedNextState = tickState (fromJust thisMove) (fromJust thatMove) currentState
        if any (simulatedNextState /=) nextState
        then do
               _         <- liftIO $ IO.putStrLn ("ERROR: Failed on round: " ++ path)
               stateDiff <- diff (show nextState) (show simulatedNextState)
               return (Failure ("Failed for: " ++ path ++ "\nDiff:\n" ++ stateDiff ++ "\nExpected:\n" ++ show nextState ++ "\nBut got:\n" ++ show simulatedNextState))
        else iter simulatedNextState (nextPath:paths)
    iter _            _                     = return Success

diff :: String -> String -> RIO App String
diff this that = do
  liftIO $ IO.writeFile "diff_1" this
  liftIO $ IO.writeFile "diff_2" that
  liftIO $ Process.callCommand "diff diff_1 diff_2 || echo '' > diff_output"
  liftIO $ IO.readFile "diff_output"

loadCommandFromSubfolder :: (State -> Coord -> String -> Maybe Move) -> ([FilePath] -> Maybe FilePath) -> State -> Coord -> FilePath -> RIO App (Maybe Move)
loadCommandFromSubfolder readMove choosePath state coord directoryPath = do
  playerPaths     <- listDirectory directoryPath
  let aPlayersPath = choosePath playerPaths
  if isJust aPlayersPath
  then liftIO $
       fmap (withoutCommandWord >=> readMove state coord) $
       IO.readFile (directoryPath ++ "/" ++ (fromJust aPlayersPath) ++ "/PlayerCommand.txt")
  else return Nothing

loadThisCommandFromSubfolder :: ([FilePath] -> Maybe FilePath) -> State -> Coord -> FilePath -> RIO App (Maybe Move)
loadThisCommandFromSubfolder = loadCommandFromSubfolder readThisMove

readThisMove :: State -> Coord -> String -> Maybe Move
readThisMove state coord moveString =
  msum [
    matchThisSelectMove   state moveString,
    matchMoveCommand      coord moveString,
    matchDirectionCommand moveString,
    matchDigCommand       coord moveString,
    matchBananaMove       coord moveString,
    Just doNothing]

matchThisSelectMove :: State -> String -> Maybe Move
matchThisSelectMove = matchSelectMove readThisWorm

loadThatCommandFromSubfolder :: ([FilePath] -> Maybe FilePath) -> State -> Coord -> FilePath -> RIO App (Maybe Move)
loadThatCommandFromSubfolder = loadCommandFromSubfolder readThatMove

loadThisPlayersCommand :: State -> Coord -> FilePath -> RIO App (Maybe Move)
loadThisPlayersCommand = loadThisCommandFromSubfolder (headMaybe . L.sort)

loadThatPlayersCommand :: State -> Coord -> FilePath -> RIO App (Maybe Move)
loadThatPlayersCommand = loadThatCommandFromSubfolder ((tailMaybe >=> headMaybe) . L.sort)

tickState :: Move -> Move -> State -> State
tickState thisMove thatMove state =
  -- TODO do we swap?! /shrug
  makeMove True (fromMoves thisMove thatMove) state

