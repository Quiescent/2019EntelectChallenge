{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Import

import Bot

import RIO.List.Partial
import RIO.List
import RIO.Directory
import Data.Maybe
import qualified RIO.ByteString.Lazy as B
import qualified System.IO           as IO
import qualified System.Process      as Process
import Data.Aeson (decode)
import Prelude (read)

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
  roundsDirectories <- fmap (map ((++) (matchLogDirectory ++ "/")) . L.sort . filter (L.isPrefixOf "Round")) $
                       listDirectory matchLogDirectory
  result            <- simulateAndCheckRounds roundsDirectories
  case result of
     Success           -> liftIO $ IO.putStrLn ("Successfully simulated: " ++ matchLogDirectory)
     (Failure message) -> liftIO $ IO.putStrLn message

data Result = Success
            | Failure String

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
      thisMove              <- loadThisPlayersCommand thisWormsCoord' path
      thatMove              <- loadThatPlayersCommand thatWormsCoord' path
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

loadStateForRound :: FilePath -> RIO App (Maybe State)
loadStateForRound path = do
  playerPaths     <- listDirectory path
  let aPlayersPath = headMaybe $ L.sort playerPaths
  if isJust aPlayersPath
  then fmap decode $ B.readFile (path ++ "/" ++ (fromJust aPlayersPath) ++ "/JsonMap.json")
  else return Nothing

loadCommandFromSubfolder :: ([FilePath] -> Maybe FilePath) -> Coord -> FilePath -> RIO App (Maybe Move)
loadCommandFromSubfolder choosePath coord directoryPath = do
  playerPaths     <- listDirectory directoryPath
  let aPlayersPath = choosePath playerPaths
  if isJust aPlayersPath
  then liftIO $
       fmap (readMove coord) $
       IO.readFile (directoryPath ++ "/" ++ (fromJust aPlayersPath) ++ "/PlayerCommand.txt")
  else return Nothing

loadThisPlayersCommand :: Coord -> FilePath -> RIO App (Maybe Move)
loadThisPlayersCommand = loadCommandFromSubfolder (headMaybe . L.sort)

loadThatPlayersCommand :: Coord -> FilePath -> RIO App (Maybe Move)
loadThatPlayersCommand = loadCommandFromSubfolder ((tailMaybe >=> headMaybe) . L.sort)

-- Must be a bit (or some bits) which are out of range for the bitmask
doNothingMove :: Maybe Move
doNothingMove = Just (Move (32))

readMove :: Coord -> String -> Maybe Move
readMove coord moveString =
  msum [
    matchMoveCommand      coord moveString,
    matchDirectionCommand moveString,
    matchDigCommand       coord moveString,
    doNothingMove]

matchDirectionCommand :: String -> Maybe Move
matchDirectionCommand original = do
  tokens     <- tailMaybe $ words original
  firstToken <- headMaybe tokens
  guard (firstToken == "shoot")
  direction <- tailMaybe tokens >>= headMaybe
  return $ case direction of
               "N"  -> Move 0
               "NE" -> Move 1
               "E"  -> Move 2
               "SE" -> Move 3
               "S"  -> Move 4
               "SW" -> Move 5
               "W"  -> Move 6
               "NW" -> Move 7
               _    -> error $ "matchDirectionCommand: " ++ show direction

toInt :: String -> Int
toInt x' = read x'

matchMoveCommand :: Coord -> String -> Maybe Move
matchMoveCommand origCoord original = do
  tokens       <- tailMaybe $ words original
  firstToken   <- headMaybe tokens
  guard (firstToken == "move")
  coords       <- tailMaybe tokens
  xValue       <- fmap toInt $ headMaybe coords
  yValue       <- fmap toInt $ tailMaybe coords >>= headMaybe
  let destCoord = toCoord xValue yValue
  return $ moveFrom origCoord destCoord

matchDigCommand :: Coord -> String -> Maybe Move
matchDigCommand origCoord original = do
  tokens       <- tailMaybe $ words original
  firstToken   <- headMaybe tokens
  guard (firstToken == "dig")
  coords       <- tailMaybe tokens
  xValue       <- fmap toInt $ headMaybe coords
  yValue       <- fmap toInt $ tailMaybe coords >>= headMaybe
  let destCoord = toCoord xValue yValue
  return $ digFrom origCoord destCoord

data Ternary = NegOne
             | Zero
             | One

compareToTernary :: Int -> Int -> Ternary
compareToTernary x' y' =
  if x' - y' < 0
  then NegOne
  else if x' - y' > 0
       then One
       else Zero

digFrom :: Coord -> Coord -> Move
digFrom from to' =
  let (x',  y')  = fromCoord from
      (x'', y'') = fromCoord to'
  in case (compareToTernary x'' x', compareToTernary y'' y') of
    -- Start from N and move anti clockwise
    (Zero,   NegOne) -> Move 16
    (One,    NegOne) -> Move 17
    (One,    Zero)   -> Move 18
    (One,    One)    -> Move 19
    (Zero,   One)    -> Move 20
    (NegOne, One)    -> Move 21
    (NegOne, Zero)   -> Move 22
    (NegOne, NegOne) -> Move 23
    (Zero,   Zero)   -> Move 32

moveFrom :: Coord -> Coord -> Move
moveFrom from to' =
  let (x',  y')  = fromCoord from
      (x'', y'') = fromCoord to'
  in case (compareToTernary x'' x', compareToTernary y'' y') of
    -- Start from N and move anti clockwise
    (Zero,   NegOne) -> Move 8
    (One,    NegOne) -> Move 9
    (One,    Zero)   -> Move 10
    (One,    One)    -> Move 11
    (Zero,   One)    -> Move 12
    (NegOne, One)    -> Move 13
    (NegOne, Zero)   -> Move 14
    (NegOne, NegOne) -> Move 15
    (Zero,   Zero)   -> Move 32

tickState :: Move -> Move -> State -> State
tickState thisMove thatMove state =
  -- TODO do we swap?! /shrug
  makeMove True (fromMoves thisMove thatMove) state

