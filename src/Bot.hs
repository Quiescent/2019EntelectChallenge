{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Bot
  where

import Import hiding (round)

import qualified RIO.Vector.Boxed as V
import qualified RIO.HashMap as M
import GHC.Generics (Generic)
import qualified RIO.ByteString.Lazy as B
import Data.Bits
import Data.Maybe
import System.IO
import System.Random
import Data.Aeson (decode, withObject, (.:), FromJSON, parseJSON)

-- DEBUG
import RIO.List

data GameMap = GameMap (V.Vector Cell)
  deriving (Generic, Eq)

instance Show GameMap where
  show = showRows . splitGameMap

showRows :: [V.Vector Cell] -> String
showRows xs =
  "|" ++ (foldr (++) "" $ take mapDim $ repeat "-") ++ "|\n" ++
  (foldr (\ nextRow gameMap' -> gameMap' ++ "|" ++ (foldr (++) "" $ fmap show nextRow) ++ "|\n") "" xs) ++
  "|" ++ (foldr (++) "" $ take mapDim $ repeat "-") ++ "|"

splitGameMap :: GameMap -> [V.Vector Cell]
splitGameMap (GameMap xs) =
  iter xs
  where
    iter xs'
      | V.null xs' = []
      | otherwise = V.take mapDim xs' : (iter $ V.drop mapDim xs')

data State = State { currentWormId :: Int,
                     weaponRange   :: Int,
                     weaponDamage  :: Int,
                     digRange      :: Int,
                     moveRange     :: Int,
                     myPlayer      :: Player,
                     opponent      :: Player,
                     gameMap       :: GameMap }
             deriving (Generic, Eq)

instance Show State where
  show (State currentWormId'
              weaponRange'
              weaponDamage'
              digRange'
              moveRange'
              myPlayer'
              opponent'
              gameMap') =
    "State {\n" ++
    "  currentWormId' = " ++ show currentWormId' ++ "\n" ++
    "  weaponRange'   = " ++ show weaponRange'   ++ "\n" ++
    "  weaponDamage'  = " ++ show weaponDamage'  ++ "\n" ++
    "  digRange'      = " ++ show digRange'      ++ "\n" ++
    "  moveRange'     = " ++ show moveRange'     ++ "\n" ++
    "  myPlayer'      = " ++ show myPlayer'      ++ "\n" ++
    "  opponent'      = " ++ show opponent'      ++ "\n" ++
    "  gameMap':\n" ++
    show gameMap' ++
    "}"

instance FromJSON State where
  parseJSON = withObject "State" $ \ v ->
    toState <$> v .: "currentWormId"
            <*> v .: "myPlayer"
            <*> v .: "opponents"
            <*> v .: "map"

data Player = Player Int Worms
  deriving (Show, Generic, Eq)

type Worms = M.HashMap Int Worm

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
            (GameMap $ V.concat $ V.toList gameMap')
    Nothing -> error "There was no opponent to play against..."

opponentToPlayer :: Opponent -> Player
opponentToPlayer (Opponent score' worms') =
  Player score' $
  wormsToMap $
  fmap fromOpponentWorm worms'

wormsToMap :: V.Vector Worm -> Worms
wormsToMap = V.foldl' (\ acc worm@(Worm id' _ _) -> M.insert id' worm acc) M.empty

fromOpponentWorm :: OpponentWorm -> Worm
fromOpponentWorm (OpponentWorm id' health' position' _ _) = Worm id' health' position'

toPlayer :: ScratchPlayer -> Player
toPlayer (ScratchPlayer score' _ worms') =
  Player score' $
  wormsToMap $
  fmap fromScratchWorm worms'

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
          | MEDIPACK
          deriving (Generic, Eq)

instance Show Cell where
  show AIR        = "_"
  show DIRT       = "#"
  show DEEP_SPACE = " "
  show MEDIPACK   = "+"

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
-- Nothing
formatMove (Move 16) _ _ = "nothing"
-- Move or Dig
formatMove dir xy (GameMap xs) =
  let (Coord xy') = displaceCoordByMove xy dir
  in case xs V.!? xy' of
       Just AIR        -> "move " ++ show xy'
       Just DIRT       -> "dig "  ++ show xy'
       Just DEEP_SPACE -> "nothing"
       Just MEDIPACK   -> "nothing"
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
  CombinedMove $ myMove .|. (opponentsMove `shiftL` 5)

toMoves :: CombinedMove -> (Move, Move)
toMoves (CombinedMove moves) =
  (Move $ moves .&. 31, Move $ (moves .&. (31 `shiftL` 5)) `shiftR` 5)

makeMove :: Bool -> CombinedMove -> State -> State
makeMove thisMoveWins moves =
  let (myMove, opponentsMove) = toMoves moves
  in makeShootMoves              myMove opponentsMove .
     makeDigMoves                myMove opponentsMove .
     makeMoveMoves  thisMoveWins myMove opponentsMove

thisCurrentWorm :: State -> Maybe Worm
thisCurrentWorm state =
  let currentWormId' = currentWormId state
  in M.lookup currentWormId' $ thisPlayersWorms state

thisPlayersWorms :: State -> Worms
thisPlayersWorms = (\ (Player _ worms') -> worms') . myPlayer

thatCurrentWorm :: State -> Maybe Worm
thatCurrentWorm state =
  let currentWormId' = currentWormId state
  in M.lookup currentWormId' $ thatPlayersWorms state

thatPlayersWorms :: State -> Worms
thatPlayersWorms = (\ (Player _ worms') -> worms') . opponent

makeMoveMoves :: Bool -> Move -> Move -> State -> State
makeMoveMoves thisMoveWins this that state =
  let thisMoveMove         = if isAMoveMove this then Just this else Nothing
      thatMoveMove         = if isAMoveMove that then Just that else Nothing
      thisTarget           = thisMoveMove >>= ((flip targetOfThisMove) state)
      thatTarget           = thatMoveMove >>= ((flip targetOfThatMove) state)
      thisTargetIsValid    = (thisTarget >>= mapAtCoord state) == Just AIR && (fmap (containsAnyWorm state) thisTarget) == Just False
      thatTargetIsValid    = (thatTarget >>= mapAtCoord state) == Just AIR && (fmap (containsAnyWorm state) thatTarget) == Just False
      -- fromJust is valid because we test whether it's Just on the above two lines
      validThisTarget      = fromJust thisTarget
      validThatTarget      = fromJust thatTarget
      thisWormMovedIfValid = if thisTargetIsValid
                             then moveThisWorm validThisTarget state
                             else state
      thatWormMovedIfValid = if thatTargetIsValid
                             then moveThatWorm validThatTarget thisWormMovedIfValid
                             else thisWormMovedIfValid
  in if thisTargetIsValid && thatTargetIsValid && thisTarget == thatTarget
     then knockBackDamage $ if thisMoveWins
                            then moveThisWorm validThisTarget state
                            else moveThatWorm validThatTarget state
     else thatWormMovedIfValid

containsAnyWorm :: State -> Coord -> Bool
containsAnyWorm State { opponent = (Player _ opponentWorms),
                        myPlayer = (Player _ myWorms)} coord' =
  (M.foldl' matchesCoord False $ M.map coordOf opponentWorms) ||
  (M.foldl' matchesCoord False $ M.map coordOf myWorms)
  where
    coordOf (Worm _ _ position') = position'
    matchesCoord True  _         = True
    matchesCoord False wormCoord = coord' == wormCoord

isAMoveMove :: Move -> Bool
isAMoveMove (Move x)
  | x >= 8 && x < 16 = True
  | otherwise        = False

mapAtCoord :: State -> Coord -> Maybe Cell
mapAtCoord State { gameMap = gameMap' } (Coord target) = (\(GameMap xs) -> xs V.!? target) gameMap'

-- TODO: get actual amount of damage
knockBackDamageAmount :: Int
knockBackDamageAmount = 1

knockBackDamage :: State -> State
knockBackDamage =
  mapWorm withThisWorm knockBackDamage' . mapWorm withThatWorm knockBackDamage'
  where
    knockBackDamage' (Worm id' health' position') = (Worm id' (health' - knockBackDamageAmount) position')

moveThisWorm :: Coord -> State -> State
moveThisWorm newCoord' = mapWorm withThisWorm (moveWorm newCoord')

moveWorm :: Coord -> Worm -> Worm
moveWorm position' (Worm id' health' _) = Worm id' health' position'

mapWorm :: (State -> (Worm -> Worm) -> State) -> (Worm -> Worm) -> State -> State
mapWorm withWorm f = (flip withWorm) f

withThisWorm :: State -> (Worm -> Worm) -> State
withThisWorm state@(State { currentWormId = currentWormId', myPlayer = myPlayer' }) f =
  state { myPlayer = mapWorms myPlayer' (withCurrentWorm currentWormId' f) }

withCurrentWorm :: Int -> (Worm -> Worm) -> Worms -> Worms
withCurrentWorm i f = M.adjust f i

mapWorms :: Player -> (Worms -> Worms) -> Player
mapWorms (Player health' worms') f =
  Player health' $ f worms'

moveThatWorm :: Coord -> State -> State
moveThatWorm newCoord' = mapWorm withThatWorm (moveWorm newCoord')

withThatWorm :: State -> (Worm -> Worm) -> State
withThatWorm state@(State { currentWormId = currentWormId', opponent = opponent' }) f =
  state { opponent = mapWorms opponent' (withCurrentWorm currentWormId' f) }

targetOfThisMove :: Move -> State -> Maybe Coord
targetOfThisMove dir =
  fmap (targetOfMove dir) . thisCurrentWorm

targetOfThatMove :: Move -> State -> Maybe Coord
targetOfThatMove dir =
  fmap (targetOfMove dir) . thatCurrentWorm

targetOfMove :: Move -> Worm -> Coord
targetOfMove = flip go
  where
    go :: Worm -> Move -> Coord
    go (Worm _ _ xy) = displaceCoordByMove xy

makeDigMoves :: Move -> Move -> State -> State
makeDigMoves this other state = state

makeShootMoves :: Move -> Move -> State -> State
makeShootMoves this other state = state

makeOposingMove :: Move -> Int -> Int -> Int -> Int -> Int -> Player -> Player -> GameMap -> State
makeOposingMove move currentWormId' weaponRange' weaponDamage' digRange' moveRange' this'@(Player score' worms') other =
  case M.lookup currentWormId' worms' of
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
