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
import Data.Bits
import System.IO
import System.Random
import Data.Aeson (decode, withObject, (.:), FromJSON, parseJSON)

type GameMap = V.Vector Cell

data State = State { currentWormId :: Int,
                     weaponRange   :: Int,
                     weaponDamage  :: Int,
                     digRange      :: Int,
                     moveRange     :: Int,
                     myPlayer      :: Player,
                     opponent      :: Player,
                     gameMap       :: GameMap }
             deriving (Show, Generic, Eq)

instance FromJSON State where
  parseJSON = withObject "State" $ \ v ->
    toState <$> v .: "currentWormId"
            <*> v .: "myPlayer"
            <*> v .: "opponents"
            <*> v .: "map"

data Player = Player Int (V.Vector Worm)
  deriving (Show, Generic, Eq)

-- TODO: If there is no opponent then I'll bail out here :/
toState :: Int -> ScratchPlayer -> V.Vector Opponent -> V.Vector (V.Vector Cell) -> State
toState currentWormId' myPlayer' opponents' gameMap' =
  let state = do
        opponent'        <- opponents'       V.!? 0
        exampleWorm      <- (worms myPlayer') V.!? 0
        let weapon'       = weapon        exampleWorm
        let weaponRange'  = range         weapon'
        let weaponDamage' = damage        weapon'
        let moveRange'    = movementRange exampleWorm
        let digRange'     = diggingRange  exampleWorm
        return (opponent', weaponRange', weaponDamage', moveRange', digRange')
  in case state of
    Just (opponent', weaponRange', weaponDamage', moveRange', digRange') ->
      State currentWormId'
            weaponRange'
            weaponDamage'
            moveRange'
            digRange'
            (toPlayer myPlayer')
            (opponentToPlayer opponent')
            (V.concat $ V.toList gameMap')
    Nothing -> error "There was no opponent to play against..."

opponentToPlayer :: Opponent -> Player
opponentToPlayer (Opponent score' worms') = Player score' $ fmap fromOpponentWorm worms'

fromOpponentWorm :: OpponentWorm -> Worm
fromOpponentWorm (OpponentWorm id' health' position' _ _) = Worm id' health' position'

toPlayer :: ScratchPlayer -> Player
toPlayer (ScratchPlayer score' _ worms') = Player score' $ fmap fromScratchWorm worms'

fromScratchWorm :: ScratchWorm -> Worm
fromScratchWorm (ScratchWorm id' health' position' _ _ _) = Worm id' health' position'

data Worm = Worm Int Int Coord
  deriving (Show, Generic, Eq)

data ScratchPlayer = ScratchPlayer { score  :: Int,
                                     health :: Int,
                                     worms  :: V.Vector ScratchWorm }
                   deriving (Show, Generic, Eq)

instance FromJSON ScratchPlayer

data Opponent = Opponent { opponentsScore :: Int,
                           opponentsWorms :: V.Vector OpponentWorm }
              deriving (Show, Generic, Eq)

instance FromJSON Opponent where
  parseJSON = withObject "Opponent" $ \ v ->
    Opponent <$> v .: "score"
             <*> v .: "worms"

data ScratchWorm = ScratchWorm { wormId        :: Int,
                                 wormHealth    :: Int,
                                 position      :: Coord,
                                 weapon        :: Weapon,
                                 diggingRange  :: Int,
                                 movementRange :: Int }
            deriving (Show, Generic, Eq)

instance FromJSON ScratchWorm where
  parseJSON = withObject "Worm" $ \ v ->
    ScratchWorm <$> v .: "id"
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

instance FromJSON OpponentWorm where
  parseJSON = withObject "OpponentWorm" $ \ v ->
    OpponentWorm <$> v .: "id"
                 <*> v .: "health"
                 <*> v .: "position"
                 <*> v .: "diggingRange"
                 <*> v .: "movementRange"

data Coord = Coord Int
  deriving (Generic, Eq)

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

data Cell = AIR
          | DIRT
          | DEEP_SPACE
          deriving (Show, Generic, Eq)

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
  deriving (Show, Eq)

allMoves :: V.Vector Move
allMoves = fmap Move $ V.fromList [0..17]

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
    8  -> Coord $ xy - mapDim
    -- NE
    9  -> Coord $ xy - mapDim + 1
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
    _  -> error $ "Attempted to move in invalid direction with " ++ show moveDir

data CombinedMove = CombinedMove Int

fromMoves :: Move -> Move -> CombinedMove
fromMoves (Move myMove) (Move opponentsMove) =
  CombinedMove $ myMove .|. (opponentsMove `shiftL` 4)

toMoves :: CombinedMove -> (Move, Move)
toMoves (CombinedMove moves) =
  (Move $ moves .&. 15, Move $ (moves .&. (15 `shiftL` 4)) `shiftR` 4)

makeMove :: CombinedMove -> State -> State
makeMove moves =
  let (myMove, opponentsMove) = toMoves moves
  in makeShootMoves myMove opponentsMove . makeDigMoves myMove opponentsMove . makeMoveMoves myMove opponentsMove

makeMoveMoves :: Move -> Move -> State -> State
makeMoveMoves this' other' state' = state'

makeDigMoves :: Move -> Move -> State -> State
makeDigMoves this' other' state' = state'

makeShootMoves :: Move -> Move -> State -> State
makeShootMoves this' other' state' = state'

makeOposingMove :: Move -> Int -> Int -> Int -> Int -> Int -> Player -> Player -> GameMap -> State
makeOposingMove move currentWormId' weaponRange' weaponDamage' digRange' moveRange' this'@(Player score' worms') other =
  case V.find (\ (Worm id' _ _) -> id' == currentWormId') worms' of
    Nothing -> State currentWormId' weaponRange' weaponDamage' digRange' moveRange' this' other
    Just (Worm _ health' position') ->
      undefined

readRound :: RIO App Int
readRound = liftIO readLn

startBot :: StdGen -> Int -> RIO App ()
startBot g roundNumber = do
  round <- readRound
  state <- readGameState round
  liftIO $ putStrLn $ show state
  liftIO $ putStrLn $ "C;" ++ show roundNumber ++ ";nothing\n"
  startBot g (roundNumber + 1)
