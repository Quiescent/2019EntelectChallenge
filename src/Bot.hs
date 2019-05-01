{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Bot
  where

import Import hiding (round)

import qualified RIO.Vector.Boxed as V
import GHC.Generics (Generic)
import qualified RIO.ByteString.Lazy as B
import System.IO
import System.Random
import Data.Aeson (decode, withObject, (.:), FromJSON, ToJSON, parseJSON)

type GameMap = V.Vector Cell

data State = State { currentRound :: Int,
                     maxRounds :: Int,
                     currentWormId :: Int,
                     consecutiveDoNothingCount :: Int,
                     myPlayer :: Player,
                     opponents :: V.Vector Opponent,
                     gameMap :: GameMap }
             deriving (Show, Generic, Eq)

instance ToJSON   State
instance FromJSON State where
  parseJSON = withObject "State" $ \ v ->
    toState <$> v .: "currentRound"
            <*> v .: "maxRounds"
            <*> v .: "currentWormId"
            <*> v .: "consecutiveDoNothingCount"
            <*> v .: "myPlayer"
            <*> v .: "opponents"
            <*> v .: "map"

toState :: Int -> Int -> Int -> Int -> Player -> V.Vector Opponent -> V.Vector (V.Vector Cell) -> State
toState currentRound' maxRounds' currentWormId' consecutiveDoNothingCount' myPlayer' opponents' gameMap' =
  State currentRound'
        maxRounds'
        currentWormId'
        consecutiveDoNothingCount'
        myPlayer'
        opponents'
        (V.concat $ V.toList gameMap')

data Player = Player { id :: Int,
                       score :: Int,
                       health :: Int,
                       worms :: V.Vector Worm }
              deriving (Show, Generic, Eq)

instance FromJSON Player
instance ToJSON   Player

data Opponent = Opponent { opponentsId :: Int,
                           opponentsScore :: Int,
                           opponentsWorms :: V.Vector OpponentWorm }
              deriving (Show, Generic, Eq)

instance ToJSON   Opponent
instance FromJSON Opponent where
  parseJSON = withObject "Opponent" $ \ v ->
    Opponent <$> v .: "id"
             <*> v .: "score"
             <*> v .: "worms"

data Worm = Worm { wormId :: Int,
                   wormHealth :: Int,
                   position :: Coord,
                   weapon :: Weapon,
                   diggingRange :: Int,
                   movementRange :: Int }
            deriving (Show, Generic, Eq)

instance ToJSON   Worm
instance FromJSON Worm where
  parseJSON = withObject "Worm" $ \ v ->
    Worm <$> v .: "id"
         <*> v .: "health"
         <*> v .: "position"
         <*> v .: "weapon"
         <*> v .: "diggingRange"
         <*> v .: "movementRange"

data OpponentWorm = OpponentWorm { opWormId :: Int,
                                   opWormHealth :: Int,
                                   opPosition :: Coord,
                                   opDiggingRange :: Int,
                                   opMovementRange :: Int }
            deriving (Show, Generic, Eq)

instance ToJSON   OpponentWorm
instance FromJSON OpponentWorm where
  parseJSON = withObject "OpponentWorm" $ \ v ->
    OpponentWorm <$> v .: "id"
                 <*> v .: "health"
                 <*> v .: "position"
                 <*> v .: "diggingRange"
                 <*> v .: "movementRange"

data Coord = Coord Int
  deriving (Generic, Eq)

instance ToJSON   Coord
instance FromJSON Coord where
  parseJSON = withObject "Coord" $ \ v ->
    toCoord <$> v .: "x"
            <*> v .: "y"

mapDim :: Int
mapDim = 33

toCoord :: Int -> Int -> Coord
toCoord xCoord yCoord =
  Coord $ mapDim * yCoord + xCoord

-- Consider whether to use applicative here and get rid of the tuple
fromCoord :: Coord -> (Int, Int)
fromCoord (Coord xy) =
  case (divMod xy mapDim) of
    (y', x') -> (x', y')

instance Show Coord where
  show xy = case fromCoord xy of
    (x', y') -> show x' ++ " " ++ show y'

data Weapon = Weapon { damage :: Int,
                       range :: Int }
              deriving (Show, Generic, Eq)

instance FromJSON Weapon
instance ToJSON   Weapon

data Cell = AIR
          | DIRT
          | DEEP_SPACE
          deriving (Show, Generic, Eq)

instance ToJSON   Cell
instance FromJSON Cell where
  parseJSON = withObject "Cell" $ \ v ->
    toCell <$> v .: "type"

toCell :: String -> Cell
toCell "AIR"        = AIR
toCell "DIRT"       = DIRT
toCell "DEEP_SPACE" = DEEP_SPACE
toCell cellType     = error $ "Can't create a cell with type: " ++ cellType

readGameState :: Int -> RIO App (Maybe State)
readGameState r = do
  stateString <- B.readFile $ "./rounds/" ++ show r ++ "/state.json"
  return $ decode stateString

data Move = Move Int
  deriving (Show)

moves :: V.Vector Move
moves = fmap Move $ V.fromList [0..17]

formatMove :: Move -> Coord -> GameMap -> String
-- Shoot
formatMove (Move 0) _ _ = "shoot N"
formatMove (Move 1) _ _ = "shoot NE"
formatMove (Move 2) _ _ = "shoot E"
formatMove (Move 3) _ _ = "shoot SE"
formatMove (Move 4) _ _ = "shoot S"
formatMove (Move 5) _ _ = "shoot SW"
formatMove (Move 6) _ _ = "shoot W"
formatMove (Move 7) _ _ = "shoot NW"
-- Move or Dig
formatMove dir xy xs =
  let (Coord xy') = displaceCoordByMove xy dir
  in case xs V.!? xy' of
       Just AIR        -> "move " ++ show xy'
       Just DIRT       -> "dig "  ++ show xy'
       Just DEEP_SPACE -> "nothing"
       Nothing         -> "nothing"

displaceCoordByMove :: Coord -> Move -> Coord
displaceCoordByMove (Coord xy) moveDir@(Move dir) =
  case dir of
    -- N
    8 -> Coord $ xy - mapDim
    -- NE
    9 -> Coord $ xy - mapDim + 1
    -- E
    10 -> Coord $ xy + 1
    -- SE
    11 -> Coord $ xy + mapDim + 1
    -- S
    12 -> Coord $ xy + mapDim
    -- SW
    13 -> Coord $ xy + mapDim - 1
    -- W
    14 -> Coord $ xy - 1
    -- NW
    15 -> Coord $ xy - mapDim - 1
    -- Invalid Move
    _ -> error $ "Attempted to move in invalid direction with " ++ show moveDir

readRound :: RIO App Int
readRound = liftIO readLn

startBot :: StdGen -> Int -> RIO App ()
startBot g roundNumber = do
  round <- readRound
  state <- readGameState round
  liftIO $ putStrLn $ show state
  liftIO $ putStrLn $ "C;" ++ show roundNumber ++ ";nothing\n"
  startBot g (roundNumber + 1)
