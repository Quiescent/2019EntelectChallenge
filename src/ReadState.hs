{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ReadState
  where

import qualified RIO.Vector.Boxed as V
import qualified RIO.ByteString.Lazy as B
import Data.Aeson (decode, withObject, (.:), (.:?), FromJSON, parseJSON)
import Data.Bits
import RIO.List
import RIO.List.Partial
import Data.Maybe

import MapArithmetic
import AList
import Ternary
import Import
import MakeMove

-- TODO: think long and hard about this...
import Prelude (read)

-- TODO: If there is no opponent then I'll bail out here :/
toState :: ScratchPlayer -> V.Vector Opponent -> V.Vector (V.Vector Cell) -> Int -> State
toState myPlayer' opponents' gameMap' currentRound' =
  let state = do
        opponent' <- opponents' V.!? 0
        let (healths',
             positions',
             bananas',
             snowballs',
             frozenDurations') = factsFromMyWorms myPlayer'
        let (healths'',
             positions'',
             bananas'',
             snowballs'',
             frozenDurations'') = factsFromOpponentsWorms opponent'
        let wormHealths'         = aListAddMineAndHis healths'         healths''
        let wormPositions'       = aListAddMineAndHis positions'       positions''
        let wormBananas'         = aListAddMineAndHis bananas'         bananas''
        let wormSnowballs'       = aListAddMineAndHis snowballs'       snowballs''
        let wormFrozenDurations' = aListAddMineAndHis frozenDurations' frozenDurations''
        return (opponent',
                wormHealths',
                wormPositions',
                wormBananas',
                wormSnowballs',
                wormFrozenDurations')
  in case state of
    Just (opponent',
          wormHealths',
          wormPositions',
          wormBananas',
          wormSnowballs',
          wormFrozenDurations') ->
      State (lastCommand opponent')
            currentRound'
            wormHealths'
            wormPositions'
            wormBananas'
            wormSnowballs'
            wormFrozenDurations'
            (removeHealthPoints aListSumThisPlayersValues wormHealths' $ toPlayer myPlayer')
            (removeHealthPoints aListSumThatPlayersValues wormHealths' $ opponentToPlayer opponent')
            (vectorGameMapToGameMap $ V.concat $ V.toList gameMap')
    Nothing -> error "There was no opponent to play against..."

wormCount :: Int
wormCount = 3

healthPoints :: Int -> Int
healthPoints totalHealth = totalHealth `div` wormCount

removeHealthPoints :: (AList -> Int) -> WormHealths -> Player -> Player
removeHealthPoints summingFunction wormHealths' (Player score' wormId' selections') =
  let totalHealth = summingFunction wormHealths'
  in Player (score' - healthPoints totalHealth) wormId' selections'

vectorToAList :: V.Vector (Int, Int) -> AList
vectorToAList = aListFromList . V.toList

vectorMaybesToAList :: V.Vector (Maybe (Int, Int)) -> AList
vectorMaybesToAList = aListFromList . catMaybes . V.toList

instance FromJSON State where
  parseJSON = withObject "State" $ \ v ->
    toState <$> v .: "myPlayer"
            <*> v .: "opponents"
            <*> v .: "map"
            <*> v .: "currentRound"

instance FromJSON Cell where
  parseJSON = withObject "Cell" $ \ v ->
    toCell <$> v .:  "type"
           <*> v .:? "powerup"

-- TODO: repitition!!!  (What differs is the type of worm :/)
factsFromMyWorms :: ScratchPlayer -> (WormHealths, WormPositions, WormBananas, WormSnowballs, WormFrozenDurations)
factsFromMyWorms (ScratchPlayer _ _ worms' _) =
  let deadIds          = V.toList $
                         V.map fst $
                         V.filter ((<= 0) . snd) $
                         V.map (\ (ScratchWorm { wormId     = wormId',
                                                 wormHealth = wormHealth' }) -> (wormId', wormHealth'))
                         worms'
      notDead          = \ (ScratchWorm { wormId = wormId' }) -> not $ elem wormId' deadIds
      liveWorms        = V.filter notDead worms'
      healths          = vectorToAList $
                         V.map (\ (ScratchWorm { wormId     = wormId',
                                                 wormHealth = wormHealth' }) -> (wormId', wormHealth'))
                         liveWorms
      positions        = vectorToAList $
                         V.map (\ (ScratchWorm { wormId   = wormId',
                                                 position = position' }) -> (wormId', coordToInt position'))
                         liveWorms
      bananas          = vectorMaybesToAList $
                         V.map (\ (ScratchWorm { wormId       = wormId',
                                                  bananaBombs = bananas' }) ->
                                   fmap (\ (BananaBomb count') -> (wormId', if count' == 0
                                                                            then (-1)
                                                                            else count') )
                                   bananas')
                         liveWorms
      snowballs'       = vectorMaybesToAList $
                         V.map (\ (ScratchWorm { wormId    = wormId',
                                                 snowballs = snowballs'' }) ->
                                   fmap (\ (Snowball (count')) -> (wormId', if count' == 0
                                                                            then (-1)
                                                                            else count'))
                                   snowballs'')
                         liveWorms
      frozenDurations' = vectorToAList $
                         V.map (\ (ScratchWorm { wormId              = wormId',
                                                 roundsUntilUnfrozen = roundsUntilUnfrozen' }) ->
                                   (wormId', (if roundsUntilUnfrozen' == 0
                                              then -1
                                              else roundsUntilUnfrozen')))
                         liveWorms
  in (healths, positions, bananas, snowballs', frozenDurations')

toThatWormId :: Int -> Int
toThatWormId wormId' = shiftL wormId' 2

factsFromOpponentsWorms :: Opponent -> (WormHealths, WormPositions, WormBananas, WormSnowballs, WormFrozenDurations)
factsFromOpponentsWorms (Opponent _ _ _ worms' _) =
  let deadIds          = V.toList $
                         V.map fst $
                         V.filter ((<= 0) . snd) $
                         V.map (\ (OpponentWorm { opWormId     = wormId',
                                                  opWormHealth = wormHealth' }) ->
                                   (wormId', wormHealth'))
                         worms'
      notDead          = \ (OpponentWorm { opWormId = wormId' }) -> not $ elem wormId' deadIds
      liveWorms        = V.filter notDead worms'
      healths          = vectorToAList $
                         V.map (\ (OpponentWorm { opWormId     = wormId',
                                                  opWormHealth = wormHealth' }) ->
                                   (toThatWormId wormId', wormHealth'))
                         liveWorms
      positions        = vectorToAList $
                         V.map (\ (OpponentWorm { opWormId   = wormId',
                                                  opPosition = position' }) ->
                                   (toThatWormId wormId', coordToInt position'))
                         liveWorms
      frozenDurations' = vectorToAList $
                         V.map (\ (OpponentWorm { opWormId              = wormId',
                                                  opRoundsUntilUnfrozen = opRoundsUntilUnfrozen' }) ->
                                   (toThatWormId wormId', if opRoundsUntilUnfrozen' == 0
                                                          then -1
                                                          else opRoundsUntilUnfrozen'))
                         liveWorms
      -- TODO: These two will parse the wrong count of bananas left.
      -- If I lose the state on the first round.
      bananas          = vectorMaybesToAList $
                         V.map (\ (OpponentWorm { opWormId = wormId', profession = profession' }) ->
                                   if profession' == "Agent"
                                   then Just (((toThatWormId wormId'), 3))
                                   else Nothing)
                         liveWorms
      snowballs'       = vectorMaybesToAList $
                         V.map (\ (OpponentWorm { opWormId   = wormId',
                                                  profession = profession' }) ->
                                   if profession' == "Technologist"
                                   then Just (((toThatWormId wormId'), 3))
                                   else Nothing)
                         liveWorms
  in (healths, positions, bananas, snowballs', frozenDurations')

opponentToPlayer :: Opponent -> Player
opponentToPlayer (Opponent _ score' currentWormId' _ selections') =
  Player score' (WormId (shift currentWormId' 2)) (Selections selections')

toPlayer :: ScratchPlayer -> Player
toPlayer (ScratchPlayer score' currentWormId' _ selections') =
  Player score' (WormId currentWormId') (Selections selections')

data ScratchPlayer = ScratchPlayer { score                   :: Int,
                                     currentWormId           :: Int,
                                     worms                   :: V.Vector ScratchWorm,
                                     remainingWormSelections :: Int }
                   deriving (Show, Generic, Eq)

instance FromJSON ScratchPlayer

data Opponent = Opponent { lastCommand               :: Maybe String,
                           opponentsScore            :: Int,
                           opponentCurrentWormId     :: Int,
                           opponentsWorms            :: V.Vector OpponentWorm,
                           opRemainingWormSelections :: Int }
              deriving (Show, Generic, Eq)

instance FromJSON Opponent where
  parseJSON = withObject "Opponent" $ \ v ->
    toOpponent <$> v .:? "previousCommand"
               <*> v .:  "score"
               <*> v .:  "currentWormId"
               <*> v .:  "worms"
               <*> v .:  "remainingWormSelections"

-- TODO add parsing here
toOpponent :: Maybe String -> Int -> Int -> V.Vector OpponentWorm -> Int -> Opponent
toOpponent opponentsLastCommand' score' currentWormId' worms' remainingWormSelections' =
  Opponent opponentsLastCommand'
           score'
           currentWormId'
           worms'
           remainingWormSelections'

readThatMove :: State -> Coord -> String -> Maybe Move
readThatMove state coord moveString =
  msum [
    matchThatSelectMove   state moveString,
    matchMoveCommand      coord moveString,
    matchDirectionCommand moveString,
    matchDigCommand       coord moveString,
    matchBananaMove       coord moveString,
    matchSnowballMove     coord moveString,
    Just doNothing]

readThatWorm :: String -> WormId
readThatWorm = WormId . (flip shiftL) 2 . read

matchThatSelectMove :: State -> String -> Maybe Move
matchThatSelectMove = matchSelectMove readThatWorm

matchSelectMove :: (String -> WormId) -> State -> String -> Maybe Move
matchSelectMove readWorm state move' = do
  guard (any (== ';') move')
  let moves        = span (/= ';') move'
  let selectTokens = words (fst moves)
  firstToken      <- headMaybe selectTokens
  guard (firstToken == "select")
  let selectedWorm = readWorm $ last selectTokens
  otherTokens     <- tailMaybe $ snd moves
  let coord''      = coordForWorm selectedWorm $ wormPositions state
  otherMove       <- readThatMove state coord'' otherTokens
  return $ withSelection selectedWorm otherMove

withSelection :: WormId -> Move -> Move
withSelection  (WormId id') (Move x) =
  Move $ x .|. (shiftL id' selectEncodingRange)

matchDirectionCommand :: String -> Maybe Move
matchDirectionCommand original = do
  let tokens  = words original
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
  let tokens    = words original
  firstToken   <- headMaybe tokens
  guard (firstToken == "move")
  coords       <- tailMaybe tokens
  xValue       <- fmap toInt $ headMaybe coords
  yValue       <- fmap toInt $ tailMaybe coords >>= headMaybe
  let destCoord = toCoord xValue yValue
  return $ moveFrom origCoord destCoord

matchDigCommand :: Coord -> String -> Maybe Move
matchDigCommand origCoord original = do
  let tokens    = words original
  firstToken   <- headMaybe tokens
  guard (firstToken == "dig")
  coords       <- tailMaybe tokens
  xValue       <- fmap toInt $ headMaybe coords
  yValue       <- fmap toInt $ tailMaybe coords >>= headMaybe
  let destCoord = toCoord xValue yValue
  return $ digFrom origCoord destCoord

matchBananaMove :: Coord -> String -> Maybe Move
matchBananaMove origCoord original = do
  let tokens    = words original
  firstToken   <- headMaybe tokens
  guard (firstToken == "banana")
  coords       <- tailMaybe tokens
  xValue       <- fmap toInt $ headMaybe coords
  yValue       <- fmap toInt $ tailMaybe coords >>= headMaybe
  let destCoord = toCoord xValue yValue
  return $ bananaFrom origCoord destCoord

bananaFrom :: Coord -> Coord -> Move
bananaFrom from to' =
  let (x',  y')  = fromCoord from
      (x'', y'') = fromCoord to'
      xDiff      = x'' - x'
      yDiff      = y'' - y'
  -- TODO: fromJust?
  in Move $
     (+ 24) $
     fromJust $
     findIndex (\ (deltaX, deltaY) -> deltaX == xDiff && deltaY == yDiff) $
     zip bananaXDisplacements bananaYDisplacements

matchSnowballMove :: Coord -> String -> Maybe Move
matchSnowballMove origCoord original = do
  let tokens    = words original
  firstToken   <- headMaybe tokens
  guard (firstToken == "snowball")
  coords       <- tailMaybe tokens
  xValue       <- fmap toInt $ headMaybe coords
  yValue       <- fmap toInt $ tailMaybe coords >>= headMaybe
  let destCoord = toCoord xValue yValue
  return $ snowballFrom origCoord destCoord

snowballFrom :: Coord -> Coord -> Move
snowballFrom from to' =
  let (x',  y')  = fromCoord from
      (x'', y'') = fromCoord to'
      xDiff      = x'' - x'
      yDiff      = y'' - y'
  -- TODO: fromJust?
  in Move $
     (+ 105) $
     fromJust $
     findIndex (\ (deltaX, deltaY) -> deltaX == xDiff && deltaY == yDiff) $
     zip bananaXDisplacements bananaYDisplacements

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

data ScratchWorm = ScratchWorm { wormId              :: Int,
                                 wormHealth          :: Int,
                                 position            :: JSONCoord,
                                 weapon              :: Weapon,
                                 diggingRange        :: Int,
                                 movementRange       :: Int,
                                 roundsUntilUnfrozen :: Int,
                                 bananaBombs         :: Maybe BananaBomb,
                                 snowballs           :: Maybe Snowball }
            deriving (Show, Generic, Eq)

instance FromJSON ScratchWorm where
  parseJSON = withObject "Worm" $ \ v ->
    ScratchWorm <$> v .:  "id"
                <*> v .:  "health"
                <*> v .:  "position"
                <*> v .:  "weapon"
                <*> v .:  "diggingRange"
                <*> v .:  "movementRange"
                <*> v .:  "roundsUntilUnfrozen"
                <*> v .:? "bananaBombs"
                <*> v .:? "snowballs"



data Snowball = Snowball { snowballCount :: Int }
  deriving (Show, Generic, Eq)

instance FromJSON Snowball where
  parseJSON = withObject "Snowball" $ \ v ->
    Snowball <$> v .: "count"

data BananaBomb = BananaBomb { count :: Int }
                deriving (Show, Generic, Eq)

instance FromJSON BananaBomb

data OpponentWorm = OpponentWorm { opWormId              :: Int,
                                   opWormHealth          :: Int,
                                   opPosition            :: JSONCoord,
                                   opDiggingRange        :: Int,
                                   opMovementRange       :: Int,
                                   opRoundsUntilUnfrozen :: Int,
                                   profession            :: String }
            deriving (Show, Generic, Eq)

instance FromJSON OpponentWorm where
  parseJSON = withObject "OpponentWorm" $ \ v ->
    OpponentWorm <$> v .: "id"
                 <*> v .: "health"
                 <*> v .: "position"
                 <*> v .: "diggingRange"
                 <*> v .: "movementRange"
                 <*> v .: "roundsUntilUnfrozen"
                 <*> v .: "profession"

data JSONCoord = JSONCoord Int
  deriving (Generic, Eq)

instance Show JSONCoord where
  show (JSONCoord xy) = case fromCoord xy of
    (x', y') -> show x' ++ " " ++ show y'

toJSONCoord :: Int -> Int -> JSONCoord
toJSONCoord x y = JSONCoord $ toCoord x y

coordToInt :: JSONCoord -> Int
coordToInt (JSONCoord xy) = xy

instance FromJSON JSONCoord where
  parseJSON = withObject "Coord" $ \ v ->
    toJSONCoord <$> v .: "x"
                <*> v .: "y"

data Weapon = Weapon { damage :: Int,
                       range :: Int }
              deriving (Show, Generic, Eq)

instance FromJSON Weapon

data Powerup = Powerup { powerupType :: String,
                         value       :: Int }
               deriving (Show, Generic, Eq)

instance FromJSON Powerup where
  parseJSON = withObject "Powerup" $ \ v ->
    Powerup <$> v .: "type"
            <*> v .: "value"


toCell :: String -> Maybe Powerup -> Cell
toCell "DIRT"       _              = DIRT
toCell "DEEP_SPACE" _              = DEEP_SPACE
-- We've hard coded the lava states so don't bother parsing them
toCell "LAVA"       _              = AIR
toCell "AIR"        Nothing        = AIR
toCell "AIR"        (Just powerup) =
  if (powerupType powerup == "HEALTH_PACK")
  then MEDIPACK
  else AIR
toCell cellType     _       = error $ "Can't create a cell with type: " ++ cellType

readGameState :: Int -> IO (Maybe State)
readGameState r = do
  stateString <- B.readFile $ "./rounds/" ++ show r ++ "/state.json"
  return $ decode stateString
