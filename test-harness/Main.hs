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

import Prelude (read)

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

hasAnyBananaMove :: Move -> Bool
hasAnyBananaMove = isABananaMove . removeSelectionFromMove

hasAnySnowballMove :: Move -> Bool
hasAnySnowballMove = isASnowballMove . removeSelectionFromMove

invalidMove :: Maybe String
invalidMove = Just "invalid"

zeroToMinusOne :: Int -> Int
zeroToMinusOne 0 = (-1)
zeroToMinusOne x = x

decrementIfBananaMove :: (State -> Bool) -> Maybe Move -> State -> Int
decrementIfBananaMove hasBananasLeft move state =
  if any hasAnyBananaMove move && hasBananasLeft state
  then (-1)
  else 0

decrementIfSnowballMove :: (State -> Bool) -> Maybe Move -> State -> Int
decrementIfSnowballMove hasSnowballsLeft move state =
  if any hasAnySnowballMove move && hasSnowballsLeft state
  then (-1)
  else 0

simulateAndCheckRounds :: [FilePath] -> RIO App Result
simulateAndCheckRounds []                      = return $ Failure "There are no rounds in the given directory."
simulateAndCheckRounds dirs@(directory:_) = do
  initialState <- loadStateForRound directory
  if not $ isJust initialState
  then return (Failure $ "Couldn't load initial state from: " ++ show directory)
  else iter (fromJust initialState) dirs 3 3 3 3
  where
    iter :: State -> [FilePath] -> Int -> Int -> Int -> Int -> RIO App Result
    iter currentState (path:nextPath:paths) myBananas opponentBananas mySnowballs opponentSnowballs = do
      let thisWormsCoord'    = thisWormsCoord currentState
      let thatWormsCoord'    = thatWormsCoord currentState
      -- Assume: that there are valid initial worm positions
      thisMove              <- loadThisPlayersCommand currentState thisWormsCoord' path
      thatMove              <- loadThatPlayersCommand currentState thatWormsCoord' path
      let thatIdAfterSelect  = thatPlayersCurrentWormId $ (if any hasASelection thatMove
                                                           then makeOpponentsSelection (fromJust thatMove)
                                                           else id) currentState
      let thatMove'          = if (\ (State { frozenDurations = frozenDurations' }) ->
                                     aListContainsId thatIdAfterSelect frozenDurations') currentState
                               then Just doNothing
                               else thatMove
      let movesAreValid      = isJust thisMove && isJust thatMove
      if not movesAreValid
      then return $ (Failure $ "Couldn't load the players moves for: " ++ show directory)
      else do
        nextState             <- loadStateForRound nextPath
        let thisBananaWormDied = not $ aListContainsId (WormId 2) (wormHealths (fromJust nextState))
        let thatBananaWormDied = not $ aListContainsId (WormId 8) (wormHealths (fromJust nextState))
        let myBananas'         = if thisBananaWormDied
                                 then (-1)
                                 else zeroToMinusOne $
                                      myBananas         + decrementIfBananaMove thisWormHasBananasLeft thisMove currentState
        let opponentBananas'   = if thatBananaWormDied
                                 then (-1)
                                 else zeroToMinusOne $
                                      opponentBananas   + decrementIfBananaMove thatWormHasBananasLeft thatMove currentState
        let thisSnowyWormDied  = not $ aListContainsId (WormId 3) (wormHealths (fromJust nextState))
        let thatSnowyWormDied  = not $ aListContainsId (WormId 12) (wormHealths (fromJust nextState))
        let mySnowballs'       = if thisSnowyWormDied
                                 then (-1)
                                 else zeroToMinusOne $
                                      mySnowballs       + decrementIfSnowballMove thisWormHasSnowballsLeft thisMove currentState
        let opponentSnowballs' = if thatSnowyWormDied
                                 then (-1)
                                 else zeroToMinusOne $
                                      opponentSnowballs + decrementIfSnowballMove thatWormHasSnowballsLeft thatMove currentState
        let nextState'         = fmap (setOpponentsLastMove currentState (fromJust thatMove') .
                                       withWormBananas (always $
                                         aListFromList [(2, myBananas'),   (8,  opponentBananas')]) .
                                       withWormSnowballs (always $
                                         aListFromList [(3, mySnowballs'), (12, opponentSnowballs')])) nextState
        let simulatedNextState  = tickState (fromJust thisMove) (fromJust thatMove) currentState
        let simulatedNextState' = if (any (\ (State { opponentsLastCommand = opponentsLastCommand' }) -> opponentsLastCommand' == invalidMove) nextState')
                                  then setOpponentsLastMove' invalidMove simulatedNextState
                                  else simulatedNextState
        if any (simulatedNextState' /=) nextState'
        then do
               _         <- liftIO $ IO.putStrLn ("ERROR: Failed on round: " ++ path)
               stateDiff <- diff (show nextState') (show simulatedNextState')
               return (Failure ("Failed for: " ++
                                path ++
                                "\nDiff:\n" ++
                                stateDiff ++
                                "\nExpected:\n" ++
                                show nextState' ++
                                "\nBut got:\n" ++
                                show simulatedNextState' ++
                                "\nReadable input state:\n" ++
                                readableShow currentState ++
                                "\nReadable expected state:\n" ++
                                readableShow (fromJust nextState') ++
                                "\nNon-pretty moves made: (" ++
                                show thisMove ++
                                ", " ++
                                show thatMove ++
                                ")" ++
                                "\nMoves made: (" ++
                                prettyPrintThisMove currentState (fromJust thisMove) ++
                                ", " ++
                                prettyPrintThatMove currentState (fromJust thatMove) ++
                                ")"))
        else iter simulatedNextState' (nextPath:paths) myBananas' opponentBananas' mySnowballs' opponentSnowballs'
    iter _ _ _ _ _ _ = return Success

setOpponentsLastMove' :: Maybe String -> ModifyState
setOpponentsLastMove' x state = state { opponentsLastCommand = x }

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
    matchSnowballMove     coord moveString,
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

readThisWorm :: String -> WormId
readThisWorm = WormId . read

withoutCommandWord :: String -> Maybe String
withoutCommandWord = tailMaybe . dropWhile (/= ' ')
