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
import RIO.List
import Data.Bits
import Data.Maybe
import System.IO
import System.Random
import Data.Aeson (decode, withObject, (.:), FromJSON, parseJSON)

data GameMap = GameMap (M.HashMap Int Cell)
  deriving (Generic, Eq)

instance Show GameMap where
  show = showRows . splitGameMap

showRows :: [[Cell]] -> String
showRows xs =
  "|" ++ (foldr (++) "" $ take mapDim $ repeat "-") ++ "|\n" ++
  (foldr (\ nextRow gameMap' -> gameMap' ++ "|" ++ (foldr (++) "" $ fmap show nextRow) ++ "|\n") "" xs) ++
  "|" ++ (foldr (++) "" $ take mapDim $ repeat "-") ++ "|"

splitGameMap :: GameMap -> [[Cell]]
splitGameMap (GameMap xs) =
  reverse $ iter $ map snd $ sortOn fst $ M.toList xs
  where
    iter []  = []
    iter xs' = take mapDim xs' : (iter $ drop mapDim xs')

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
            (vectorGameMapToHashGameMap $ V.concat $ V.toList gameMap')
    Nothing -> error "There was no opponent to play against..."

vectorGameMapToHashGameMap :: V.Vector Cell -> GameMap
vectorGameMapToHashGameMap = GameMap . M.fromList . V.toList . V.zip (V.fromList [0..])

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

healthPackHealth :: Int
healthPackHealth = 10

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
formatMove dir xy gameMap' = moveFromMaybe $ do
  newCoord     <- displaceCoordByMove xy dir
  targetSquare <- lookupCoord newCoord gameMap'
  return $ case targetSquare of
    AIR        -> "move " ++ show newCoord
    DIRT       -> "dig "  ++ show newCoord
    DEEP_SPACE -> "nothing"
    MEDIPACK   -> "nothing"
  where
    moveFromMaybe (Just move) = move
    moveFromMaybe Nothing     = "nothing"

lookupCoord :: Coord -> GameMap -> Maybe Cell
lookupCoord (Coord xy') (GameMap xs) =
  M.lookup xy' xs

displaceCoordByMove :: Coord -> Move -> Maybe Coord
displaceCoordByMove xy moveDir@(Move dir) =
  fmap (uncurry toCoord) $ isOOB $
  let (x', y') = fromCoord xy
  in case dir of
    -- N
    8  -> (x', y' - 1)
    -- NE
    9  -> (x' + 1, y' - 1)
    -- E
    10 -> (x' + 1, y')
    -- SE
    11 -> (x' + 1, y' + 1)
    -- S
    12 -> (x', y' + 1)
    -- SW
    13 -> (x' - 1, y' + 1)
    -- W
    14 -> (x' - 1, y')
    -- NW
    15 -> (x' - 1, y' - 1)
    -- Invalid Move
    _  -> error $ "Attempted to move in invalid direction with " ++ show moveDir

isOOB :: (Int, Int) -> Maybe (Int, Int)
isOOB (x', y')
  | x' >= 0 && x' < mapDim && y' >= 0 && y' < mapDim = Just (x', y')
  | otherwise                                        = Nothing

data CombinedMove = CombinedMove Int

fromMoves :: Move -> Move -> CombinedMove
fromMoves (Move myMove) (Move opponentsMove) =
  CombinedMove $ myMove .|. (opponentsMove `shiftL` 5)

toMoves :: CombinedMove -> (Move, Move)
toMoves (CombinedMove moves) =
  (Move $ moves .&. 31, Move $ (moves .&. (31 `shiftL` 5)) `shiftR` 5)

makeMove :: Bool -> CombinedMove -> State -> State
makeMove swapping moves =
  let (myMove, opponentsMove) = toMoves moves
  in makeShootMoves          myMove opponentsMove .
     makeDigMoves            myMove opponentsMove .
     makeMoveMoves  swapping myMove opponentsMove

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

wormPosition :: Worm -> Coord
wormPosition (Worm _ _ position') = position'

makeMoveMoves :: Bool -> Move -> Move -> State -> State
makeMoveMoves swapping this that state =
  let thisMoveMove          = if isAMoveMove this && (not $ targetOfThisMoveIsDirt this state) then Just this else Nothing
      thatMoveMove          = if isAMoveMove that && (not $ targetOfThatMoveIsDirt that state) then Just that else Nothing
      thisTarget            = thisMoveMove >>= ((flip targetOfThisMove) state)
      thatTarget            = thatMoveMove >>= ((flip targetOfThatMove) state)
      -- ASSUME: that a worm won't be on an invalid square
      thisWormsPosition     = thisWormsCoord state
      thatWormsPosition     = thatWormsCoord state
      thisTargetIsValid     = ((thisTarget >>= mapAtCoord state) == Just AIR || (thisTarget >>= mapAtCoord state) == Just MEDIPACK) && (fmap (containsAnyWorm state) thisTarget) == Just False
      thisTargetIsAMedipack = (thisTarget >>= mapAtCoord state) == Just MEDIPACK
      thatTargetIsValid     = ((thatTarget >>= mapAtCoord state) == Just AIR || (thatTarget >>= mapAtCoord state) == Just MEDIPACK) && (fmap (containsAnyWorm state) thatTarget) == Just False
      thatTargetIsAMedipack = (thatTarget >>= mapAtCoord state) == Just MEDIPACK
      -- fromJust is valid because we test whether it's Just on the above two lines
      validThisTarget       = fromJust thisTarget
      validThatTarget       = fromJust thatTarget
      medipackThisWorm      = if thisTargetIsAMedipack then giveMedipackToThisWorm . removeMedipack validThisTarget else id
      medipackThatWorm      = if thatTargetIsAMedipack then giveMedipackToThatWorm . removeMedipack validThatTarget else id
      penaliseThisMove      = if (thisTargetIsValid || thisMoveMove == Nothing) then id else penaliseThisPlayerForAnInvalidCommand
      penaliseThatMove      = if (thatTargetIsValid || thatMoveMove == Nothing) then id else penaliseThatPlayerForAnInvalidCommand
      applyPenalties        = penaliseThisMove . penaliseThatMove
      awardPointsToThisMove = if thisTargetIsValid then awardPointsToThisPlayerForMovingToAir else id
      awardPointsToThatMove = if thatTargetIsValid then awardPointsToThatPlayerForMovingToAir else id
      awardPoints           = awardPointsToThatMove . awardPointsToThisMove
      moveThisWormToTarget  = if thisTargetIsValid then moveThisWorm validThisTarget else id
      moveThatWormToTarget  = if thatTargetIsValid then moveThatWorm validThatTarget else id
      moveThisWormIfValid   = medipackThisWorm . moveThisWormToTarget
      moveThatWormIfValid   = medipackThatWorm . moveThatWormToTarget
      moveWorms             = moveThisWormIfValid . moveThatWormIfValid
      swapWorms             = moveThisWorm thatWormsPosition . moveThatWorm thisWormsPosition
      swapIfSwapping        = knockBackDamage . if swapping then swapWorms else id
      collideWorms          = if thisTargetIsValid && thatTargetIsValid && thisTarget == thatTarget then swapIfSwapping else moveWorms
      move                  = awardPoints . applyPenalties . collideWorms
  in move state

targetOfThisMoveIsDirt :: Move -> State -> Bool
targetOfThisMoveIsDirt move state =
  (targetOfThisMove move state >>= mapAtCoord state) == Just DIRT

targetOfThatMoveIsDirt :: Move -> State -> Bool
targetOfThatMoveIsDirt move state =
  (targetOfThatMove move state >>= mapAtCoord state) == Just DIRT

giveMedipackToThatWorm :: State -> State
giveMedipackToThatWorm = (flip mapThatWorm) increaseHealth

removeMedipack :: Coord -> State -> State
removeMedipack position' =
  (flip mapGameMap) (cellTo position' AIR)

always :: a -> b -> a
always x _ = x

cellTo :: Coord -> Cell -> GameMap -> GameMap
cellTo (Coord position') newCell (GameMap xs) =
  GameMap $ M.adjust (always newCell) position' xs

mapGameMap :: State -> (GameMap -> GameMap) -> State
mapGameMap state@(State { gameMap = gameMap' }) f =
  state { gameMap = f gameMap' }

removeDirtAt :: Coord -> GameMap -> GameMap
removeDirtAt = (flip mapSquareAt) (always AIR)

removeDirtFromMapAt :: Coord -> State -> State
removeDirtFromMapAt coord = (flip mapGameMap) (removeDirtAt coord)

mapSquareAt :: Coord -> (Cell -> Cell) -> GameMap -> GameMap
mapSquareAt (Coord coord) f (GameMap xs) =
  GameMap $ M.adjust f coord xs

increaseHealth :: Worm -> Worm
increaseHealth (Worm id' health' position') =
  Worm id' (health' + healthPackHealth) position'

giveMedipackToThisWorm :: State -> State
giveMedipackToThisWorm = (flip mapThisWorm) increaseHealth

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
mapAtCoord State { gameMap = gameMap' } (Coord target) = (\(GameMap xs) -> M.lookup target xs) gameMap'

-- TODO: get actual amount of damage
knockBackDamageAmount :: Int
knockBackDamageAmount = 1

knockBackDamage :: State -> State
knockBackDamage =
  mapWorm mapThisWorm knockBackDamage' . mapWorm mapThatWorm knockBackDamage'
  where
    knockBackDamage' (Worm id' health' position') = (Worm id' (health' - knockBackDamageAmount) position')

moveThisWorm :: Coord -> State -> State
moveThisWorm newCoord' = mapWorm mapThisWorm (moveWorm newCoord')

moveWorm :: Coord -> Worm -> Worm
moveWorm position' (Worm id' health' _) = Worm id' health' position'

mapWorm :: (State -> (Worm -> Worm) -> State) -> (Worm -> Worm) -> State -> State
mapWorm mapWorm' f = (flip mapWorm') f

mapThisWorm :: State -> (Worm -> Worm) -> State
mapThisWorm state@(State { currentWormId = currentWormId', myPlayer = myPlayer' }) f =
  state { myPlayer = mapWorms myPlayer' (mapCurrentWorm currentWormId' f) }

mapCurrentWorm :: Int -> (Worm -> Worm) -> Worms -> Worms
mapCurrentWorm i f = M.adjust f i

mapWorms :: Player -> (Worms -> Worms) -> Player
mapWorms (Player health' worms') f =
  Player health' $ f worms'

moveThatWorm :: Coord -> State -> State
moveThatWorm newCoord' = mapWorm mapThatWorm (moveWorm newCoord')

mapThatWorm :: State -> (Worm -> Worm) -> State
mapThatWorm state@(State { currentWormId = currentWormId', opponent = opponent' }) f =
  state { opponent = mapWorms opponent' (mapCurrentWorm currentWormId' f) }

targetOfThisMove :: Move -> State -> Maybe Coord
targetOfThisMove dir state =
  thisCurrentWorm state >>= targetOfMove dir

targetOfThatMove :: Move -> State -> Maybe Coord
targetOfThatMove dir state =
  thatCurrentWorm state >>= targetOfMove dir

targetOfMove :: Move -> Worm -> Maybe Coord
targetOfMove = flip go
  where
    go :: Worm -> Move -> Maybe Coord
    go (Worm _ _ xy) = displaceCoordByMove xy

mapThisPlayer :: (Player -> Player) -> State -> State
mapThisPlayer f state@(State { myPlayer = player' }) =
  state { myPlayer = f player' }

mapThatPlayer :: (Player -> Player) -> State -> State
mapThatPlayer f state@(State { opponent = opponent' }) =
  state { opponent = f opponent' }

modifyScore :: Int -> Player -> Player
modifyScore delta (Player score' worms') =
  Player (score' + delta) worms'

penaliseForInvalidCommand :: Player -> Player
penaliseForInvalidCommand = modifyScore (-4)

penaliseThisPlayerForAnInvalidCommand :: State -> State
penaliseThisPlayerForAnInvalidCommand = mapThisPlayer penaliseForInvalidCommand

penaliseThatPlayerForAnInvalidCommand :: State -> State
penaliseThatPlayerForAnInvalidCommand = mapThatPlayer penaliseForInvalidCommand

awardPointsForMovingToAir :: Player -> Player
awardPointsForMovingToAir = modifyScore 5

awardPointsToThisPlayerForMovingToAir :: State -> State
awardPointsToThisPlayerForMovingToAir = mapThisPlayer awardPointsForMovingToAir

awardPointsToThatPlayerForMovingToAir :: State -> State
awardPointsToThatPlayerForMovingToAir = mapThatPlayer awardPointsForMovingToAir

makeDigMoves :: Move -> Move -> State -> State
makeDigMoves this that state =
  let thisMoveMove          = if isAMoveMove this then Just this else Nothing
      thatMoveMove          = if isAMoveMove that then Just that else Nothing
      thisTarget            = thisMoveMove >>= ((flip targetOfThisMove) state)
      thatTarget            = thatMoveMove >>= ((flip targetOfThatMove) state)
      thisTargetIsValid     = (thisTarget >>= mapAtCoord state) == Just DIRT
      thatTargetIsValid     = (thatTarget >>= mapAtCoord state) == Just DIRT
      -- fromJust is valid because we test whether it's Just on the above two lines
      validThisTarget       = fromJust thisTarget
      validThatTarget       = fromJust thatTarget
      digOutThisTarget      = if thisTargetIsValid then removeDirtFromMapAt validThisTarget else id
      digOutThatTarget      = if thatTargetIsValid then removeDirtFromMapAt validThatTarget else id
      awardPointsToThisMove = if thisTargetIsValid then awardPointsToThisPlayerForDigging else id
      awardPointsToThatMove = if thatTargetIsValid then awardPointsToThatPlayerForDigging else id
      awardPoints           = awardPointsToThatMove . awardPointsToThisMove
      dig                   = awardPoints . digOutThatTarget . digOutThisTarget
  in dig state

awardPointsForDigging :: Player -> Player
awardPointsForDigging = modifyScore 7

awardPointsToThisPlayerForDigging :: State -> State
awardPointsToThisPlayerForDigging = mapThisPlayer awardPointsForDigging

awardPointsToThatPlayerForDigging :: State -> State
awardPointsToThatPlayerForDigging = mapThatPlayer awardPointsForDigging

makeShootMoves :: Move -> Move -> State -> State
makeShootMoves this that state =
  let thisShootMove              = if isAShootMove this then Just this else Nothing
      thatShootMove              = if isAShootMove that then Just that else Nothing
      gameMap'                   = gameMap state
      -- ASSUME: that a worm won't be on an invalid square
      thisWormsPosition          = thisWormsCoord state
      thatWormsPosition          = thatWormsCoord state
      thisShotsDir               = thisShootMove >>= directionOfShot
      thatShotsDir               = thatShootMove >>= directionOfShot
      thatWormHitFromThisShot    = thisShotsDir >>= ((flip (hitsWorm thisWormsPosition gameMap')) (thatPlayersWorms state))
      thisWormHitFromThatShot    = thatShotsDir >>= ((flip (hitsWorm thatWormsPosition gameMap')) (thisPlayersWorms state))
      thatWormWasHitFromThisShot = isJust thatWormHitFromThisShot
      thisWormWasHitFromThatShot = isJust thisWormHitFromThatShot
      thatTargetFromThis         = fromJust thatWormHitFromThisShot
      thisTargetFromThat         = fromJust thisWormHitFromThatShot
      thisWormHitFromThisShot    = thisShotsDir >>= ((flip (hitsWorm thisWormsPosition gameMap')) (thisPlayersWorms state))
      thatWormHitFromThatShot    = thatShotsDir >>= ((flip (hitsWorm thatWormsPosition gameMap')) (thatPlayersWorms state))
      thisWormWasHitFromThisShot = isJust thisWormHitFromThisShot
      thatWormWasHitFromThatShot = isJust thatWormHitFromThatShot
      thisTargetFromThis         = fromJust thisWormHitFromThisShot
      thatTargetFromThat         = fromJust thatWormHitFromThatShot
      harmThisWormWithThisShot   = if thisWormWasHitFromThisShot then harmThisWormByWormWithRocket thisTargetFromThis else id
      harmThatWormWithThisShot   = if thatWormWasHitFromThisShot then harmThatWormByWormWithRocket thatTargetFromThis else id
      harmThisWormWithThatShot   = if thisWormWasHitFromThatShot then harmThisWormByWormWithRocket thisTargetFromThat else id
      harmThatWormWithThatShot   = if thatWormWasHitFromThatShot then harmThatWormByWormWithRocket thatTargetFromThat else id
      harmWorms                  = harmThisWormWithThatShot .
                                   harmThisWormWithThisShot .
                                   harmThatWormWithThisShot .
                                   harmThatWormWithThatShot
  in harmWorms state

mapThatWormByWorm :: Worm -> (Worm -> Worm) -> State -> State
mapThatWormByWorm (Worm id' _ _) f state@(State { opponent = opponent' }) =
  state { opponent = mapWorms opponent' (mapCurrentWorm id' f) }

mapThisWormByWorm :: Worm -> (Worm -> Worm) -> State -> State
mapThisWormByWorm (Worm id' _ _) f state@(State { myPlayer = myPlayer' }) =
  state { myPlayer = mapWorms myPlayer' (mapCurrentWorm id' f) }

harmThatWormByWormWithRocket :: Worm -> State -> State
harmThatWormByWormWithRocket worm = mapThatWormByWorm worm harmWormWithRocket

harmThisWormByWormWithRocket :: Worm -> State -> State
harmThisWormByWormWithRocket worm = mapThisWormByWorm worm harmWormWithRocket

harmWormWithRocket :: Worm -> Worm
harmWormWithRocket (Worm id' health' position') =
  Worm id' (health' - rocketDamage) position'

rocketDamage :: Int
rocketDamage = 10

data Hit = HitWorm Worm
         | HitObstacle
         | HitNothing
  deriving (Eq, Show)

hitsWorm :: Coord -> GameMap -> Direction -> Worms -> Maybe Worm
hitsWorm origin gameMap' direction worms' =
  case (foldl' (firstWormHit gameMap' worms') HitNothing $
        possibleHitCoordinates origin direction) of
    HitWorm worm -> Just worm
    _            -> Nothing

firstWormHit :: GameMap -> Worms -> Hit -> Coord -> Hit
firstWormHit _        _      hit@(HitWorm _) _      = hit
firstWormHit _        _      HitObstacle     _      = HitObstacle
firstWormHit gameMap' worms' HitNothing      coord' =
  if obstacleAt coord' gameMap'
  then HitObstacle
  else isAPositionOfAWorm coord' worms'

isAPositionOfAWorm :: Coord -> Worms -> Hit
isAPositionOfAWorm coord' worms' =
  case find (\ (Worm _ _ position') -> coord' == position') worms' of
    Just worm -> HitWorm worm
    Nothing   -> HitNothing

obstacleAt :: Coord -> GameMap -> Bool
obstacleAt coord' =
   any (\ square -> square == DIRT || square == DEEP_SPACE) .
   lookupCoord coord'

possibleHitCoordinates :: Coord -> Direction -> [Coord]
possibleHitCoordinates coord W  = iterateHorizontally coord (-)
possibleHitCoordinates coord E  = iterateHorizontally coord (+)
possibleHitCoordinates coord N  = iterateVertically   coord (-)
possibleHitCoordinates coord S  = iterateVertically   coord (+)
possibleHitCoordinates coord SE = iterateDiagonally   coord (+) (+)
possibleHitCoordinates coord NW = iterateDiagonally   coord (-) (-)
possibleHitCoordinates coord SW = iterateDiagonally   coord (-) (+)
possibleHitCoordinates coord NE = iterateDiagonally   coord (+) (-)

type Operator = Int -> Int -> Int

idOperator :: Operator
idOperator x _ = x

iterateDiagonally :: Coord -> Operator -> Operator -> [Coord]
iterateDiagonally coord fX fY = iterateCoordinate coord 2 fX fY

iterateVertically :: Coord -> Operator -> [Coord]
iterateVertically coord fY = iterateCoordinate coord 3 idOperator fY

iterateHorizontally :: Coord -> Operator -> [Coord]
iterateHorizontally coord fX = iterateCoordinate coord 3 fX idOperator

iterateCoordinate :: Coord -> Int -> Operator -> Operator -> [Coord]
iterateCoordinate coord depth fX fY =
  let (x', y') = fromCoord coord
  in zipWith
     toCoord
     (zipWith fX (repeat x') (take depth [1..]))
     (zipWith fY (repeat y') (take depth [1..]))

thisWormsCoord :: State -> Coord
thisWormsCoord = fromJust . fmap wormPosition . thisCurrentWorm

thatWormsCoord :: State -> Coord
thatWormsCoord = fromJust . fmap wormPosition . thatCurrentWorm

data Direction = N
               | NE
               | E
               | SE
               | S
               | SW
               | W
               | NW

directionOfShot :: Move -> Maybe Direction
directionOfShot (Move 0) = Just N
directionOfShot (Move 1) = Just NE
directionOfShot (Move 2) = Just E
directionOfShot (Move 3) = Just SE
directionOfShot (Move 4) = Just S
directionOfShot (Move 5) = Just SW
directionOfShot (Move 6) = Just W
directionOfShot (Move 7) = Just NW
directionOfShot _        = Nothing

isAShootMove :: Move -> Bool
isAShootMove (Move x)
  | x < 8 && x >= 0 = True
  | otherwise       = False

readRound :: RIO App Int
readRound = liftIO readLn

startBot :: StdGen -> Int -> RIO App ()
startBot g roundNumber = do
  round <- readRound
  state <- readGameState round
  liftIO $ putStrLn $ show state
  liftIO $ putStrLn $ "C;" ++ show roundNumber ++ ";nothing\n"
  startBot g (roundNumber + 1)
