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
                     map :: GameMap }
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
toState currentRound' maxRounds' currentWormId' consecutiveDoNothingCount' myPlayer' opponents' map' =
  State currentRound'
        maxRounds'
        currentWormId'
        consecutiveDoNothingCount'
        myPlayer'
        opponents'
        (V.concat $ V.toList map')

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
  deriving (Show, Generic, Eq)

instance ToJSON   Coord
instance FromJSON Coord where
  parseJSON = withObject "Coord" $ \ v ->
    toCoord <$> v .: "x"
            <*> v .: "y"

mapSize :: Int
mapSize = 33

toCoord :: Int -> Int -> Coord
toCoord xCoord yCoord =
  Coord $ mapSize * yCoord + xCoord

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

readRound :: RIO App Int
readRound = liftIO readLn

startBot :: StdGen -> Int -> RIO App ()
startBot g roundNumber = do
  round <- readRound
  state <- readGameState round
  liftIO $ putStrLn $ show state
  liftIO $ putStrLn $ "C;" ++ show roundNumber ++ ";nothing\n"
  startBot g (roundNumber + 1)
