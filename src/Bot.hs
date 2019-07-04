{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Bot
  where

import Import

import qualified RIO.Vector.Boxed as V
import qualified RIO.HashMap as M
import GHC.Generics (Generic)
import qualified RIO.ByteString.Lazy as B
import RIO.List
import RIO.List.Partial
import Data.Bits
import Data.Maybe
import System.IO
import System.Random
import Data.Aeson (decode, withObject, (.:), (.:?), FromJSON, parseJSON)

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

data State = State { weaponRange   :: Int,
                     weaponDamage  :: Int,
                     digRange      :: Int,
                     moveRange     :: Int,
                     wormHealths   :: WormHealths,
                     wormPositions :: WormPositions,
                     myPlayer      :: Player,
                     opponent      :: Player,
                     gameMap       :: GameMap }
             deriving (Generic, Eq)

type WormHealths = AList WormHealth

type WormPositions = AList Coord

data AList a = AList [AListEntry a]
  deriving (Eq)

instance (Show a) => Show (AList a) where
  show (AList xs) =
    "[\n" ++
    (foldr (++) "" $ map (\ x -> "    " ++ show x ++ ",\n") xs) ++
    "]"

data AListEntry a = AListEntry WormId a
  deriving (Eq)

instance (Show a) => Show (AListEntry a) where
  show (AListEntry (WormId wormId') value') =
    show wormId' ++ " -> " ++ show value'

data WormHealth = WormHealth Int
  deriving (Eq, Show)

data WormId = WormId Int
  deriving (Eq, Show, Ord)

instance Show State where
  show (State weaponRange'
              weaponDamage'
              digRange'
              moveRange'
              wormsHealth'
              wormPositions'
              myPlayer'
              opponent'
              gameMap') =
    "State {\n" ++
    "  weaponRange'   = " ++ show weaponRange'   ++ "\n" ++
    "  weaponDamage'  = " ++ show weaponDamage'  ++ "\n" ++
    "  digRange'      = " ++ show digRange'      ++ "\n" ++
    "  moveRange'     = " ++ show moveRange'     ++ "\n" ++
    "  wormHealths'   = " ++ show wormsHealth'   ++ "\n" ++
    "  wormPositions' = " ++ show wormPositions' ++ "\n" ++
    "  myPlayer'      = " ++ show myPlayer'      ++ "\n" ++
    "  opponent'      = " ++ show opponent'      ++ "\n" ++
    "  gameMap':\n" ++
    show gameMap' ++
    "}"

instance FromJSON State where
  parseJSON = withObject "State" $ \ v ->
    toState <$> v .: "myPlayer"
            <*> v .: "opponents"
            <*> v .: "map"

-- TODO: Change Int to PlayerScore for stronger types
data Player = Player Int WormId
  deriving (Show, Generic, Eq)

-- TODO: If there is no opponent then I'll bail out here :/
toState :: ScratchPlayer -> V.Vector Opponent -> V.Vector (V.Vector Cell) -> State
toState myPlayer' opponents' gameMap' =
  let state = do
        opponent'                    <- opponents'        V.!? 0
        exampleWorm                  <- (worms myPlayer') V.!? 0
        let (healths',  positions')   = factsFromMyWorms myPlayer'
        let (healths'', positions'')  = factsFromOpponentsWorms opponent'
        let wormHealths'              = aListConcat healths' healths''
        let wormPositions'            = aListConcat positions' positions''
        let weapon'                   = weapon        exampleWorm
        let weaponRange'              = range         weapon'
        let weaponDamage'             = damage        weapon'
        let moveRange'                = movementRange exampleWorm
        let digRange'                 = diggingRange  exampleWorm
        return (opponent', weaponRange', weaponDamage', moveRange', digRange', wormHealths', wormPositions')
  in case state of
    Just (opponent', weaponRange', weaponDamage', moveRange', digRange', wormHealths', wormPositions') ->
      State weaponRange'
            weaponDamage'
            moveRange'
            digRange'
            wormHealths'
            wormPositions'
            (toPlayer myPlayer')
            (opponentToPlayer opponent')
            (vectorGameMapToHashGameMap $ V.concat $ V.toList gameMap')
    Nothing -> error "There was no opponent to play against..."

aListConcat :: AList a -> AList a -> AList a
aListConcat (AList xs) (AList ys) = AList $ xs ++ ys

factsFromMyWorms :: ScratchPlayer -> (WormHealths, WormPositions)
factsFromMyWorms (ScratchPlayer _ _ worms') =
  let healths   = AList $
                  V.toList $
                  V.map (\ (ScratchWorm { wormId = wormId',
                                          wormHealth = wormHealth' }) -> AListEntry (WormId wormId')
                                                                                    (WormHealth wormHealth'))
                  worms'
      deadIds   = map idSlot $
                  filter ((<= 0) . deconstructHealth . dataSlot) $
                  aListFoldl' (flip (:)) [] healths
      notDead   = \ (AListEntry wormId' _) -> not $ elem wormId' deadIds
      positions = AList $
                  V.toList $
                  V.map (\ (ScratchWorm { wormId = wormId',
                                          position = position' }) -> AListEntry (WormId wormId')
                                                                                position')
                  worms'
  in (aListFilter notDead healths, aListFilter notDead positions)

factsFromOpponentsWorms :: Opponent -> (WormHealths, WormPositions)
factsFromOpponentsWorms (Opponent _ _ worms') =
  let healths   = AList $
                  V.toList $
                  V.map (\ (OpponentWorm { opWormId = wormId',
                                           opWormHealth = wormHealth' }) -> AListEntry (WormId (shift wormId' 2))
                                                                                       (WormHealth wormHealth'))
                  worms'
      deadIds   = map idSlot $
                  filter ((<= 0) . deconstructHealth . dataSlot) $
                  aListFoldl' (flip (:)) [] healths
      notDead   = \ (AListEntry wormId' _) -> not $ elem wormId' deadIds
      positions = AList $
                  V.toList $
                  V.map (\ (OpponentWorm { opWormId = wormId',
                                           opPosition = position' }) -> AListEntry (WormId (shift wormId' 2))
                                                                                   position')
                  worms'
  in (aListFilter notDead healths, aListFilter notDead positions)

vectorGameMapToHashGameMap :: V.Vector Cell -> GameMap
vectorGameMapToHashGameMap = GameMap . M.fromList . zip [0..] . V.toList

opponentToPlayer :: Opponent -> Player
opponentToPlayer (Opponent score' currentWormId' _) =
  Player score' (WormId (shift currentWormId' 2))

toPlayer :: ScratchPlayer -> Player
toPlayer (ScratchPlayer score' currentWormId' _) =
  Player score' (WormId currentWormId')

data ScratchPlayer = ScratchPlayer { score  :: Int,
                                     currentWormId :: Int,
                                     worms  :: V.Vector ScratchWorm }
                   deriving (Show, Generic, Eq)

instance FromJSON ScratchPlayer

data Opponent = Opponent { opponentsScore :: Int,
                           opponentCurrentWormId :: Int,
                           opponentsWorms :: V.Vector OpponentWorm }
              deriving (Show, Generic, Eq)

instance FromJSON Opponent where
  parseJSON = withObject "Opponent" $ \ v ->
    Opponent <$> v .: "score"
             <*> v .: "currentWormId"
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

data Powerup = Powerup { powerupType :: String,
                         value       :: Int }
               deriving (Show, Generic, Eq)

instance FromJSON Powerup where
  parseJSON = withObject "Powerup" $ \ v ->
    Powerup <$> v .: "type"
            <*> v .: "value"

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
    toCell <$> v .:  "type"
           <*> v .:? "powerup"

toCell :: String -> Maybe Powerup -> Cell
toCell "DIRT"       _              = DIRT
toCell "DEEP_SPACE" _              = DEEP_SPACE
toCell "AIR"        Nothing        = AIR
toCell "AIR"        (Just powerup) =
  if (powerupType powerup == "HEALTH_PACK")
  then MEDIPACK
  else AIR
toCell cellType     _       = error $ "Can't create a cell with type: " ++ cellType

readGameState :: Int -> RIO App (Maybe State)
readGameState r = do
  stateString <- B.readFile $ "./rounds/" ++ show r ++ "/state.json"
  return $ decode stateString

data Move = Move Int
  deriving (Show, Eq)

allMoves :: V.Vector Move
allMoves = fmap Move $ V.fromList [0..17]

formatMove :: Move -> Coord -> String
-- Shoot
formatMove (Move 0) _ = "shoot N"
formatMove (Move 1) _ = "shoot NE"
formatMove (Move 2) _ = "shoot E"
formatMove (Move 3) _ = "shoot SE"
formatMove (Move 4) _ = "shoot S"
formatMove (Move 5) _ = "shoot SW"
formatMove (Move 6) _ = "shoot W"
formatMove (Move 7) _ = "shoot NW"
formatMove dir@(Move x) xy
  -- Move
  | x < 16 = moveFromMaybe $ fmap (\ newCoord -> "move " ++ show newCoord) $ displaceCoordByMove xy dir
  -- Dig
  | x < 24 = moveFromMaybe $ fmap (\ newCoord -> "dig "  ++ show newCoord) $ displaceCoordByMove xy (Move (x - 8))
-- Nothing
formatMove _ _ = "nothing"

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

playersMoveBits :: Int
playersMoveBits = 5

playersMoveShift :: Int
playersMoveShift = playersMoveBits + 1

playerMoveMask :: Int
playerMoveMask = (2 ^ playersMoveShift) - 1

fromMoves :: Move -> Move -> CombinedMove
fromMoves (Move myMove) (Move opponentsMove) =
  CombinedMove $ myMove .|. (opponentsMove `shiftL` playersMoveShift)

toMoves :: CombinedMove -> (Move, Move)
toMoves (CombinedMove moves) =
  (Move $ moves .&. playerMoveMask,
   Move $ (moves .&. (playerMoveMask `shiftL` playersMoveShift)) `shiftR` playersMoveShift)

makeMove :: Bool -> CombinedMove -> ModifyState
makeMove swapping moves =
  let (myMove, opponentsMove) = toMoves moves
  in advanceWormSelections .
     makeShootMoves          myMove opponentsMove .
     makeDigMoves            myMove opponentsMove .
     makeMoveMoves  swapping myMove opponentsMove

advanceWormSelections :: ModifyState
advanceWormSelections =
  advanceThisWormSelection .
  advanceThatWormSelection

advanceThisWormSelection :: ModifyState
advanceThisWormSelection =
  advanceWormSelectionByWorms isMyWorm thisPlayersCurrentWormId mapThisPlayer

advanceThatWormSelection :: ModifyState
advanceThatWormSelection =
  advanceWormSelectionByWorms isOpponentWorm thatPlayersCurrentWormId mapThatPlayer

withCurrentWormId :: WormId -> Player -> Player
withCurrentWormId wormId' (Player score' _) = (Player score' wormId')

-- Assume that there are worm ids to search through
nextWormId :: WormId -> [WormId] -> WormId
nextWormId wormId' wormIds =
  iter wormIds
  where
    iter []     = head wormIds
    iter (x:xs) = if x == wormId'
                  then if xs == []
                       then head wormIds
                       else head xs
                  else iter xs

advanceWormSelectionByWorms :: (WormId -> Bool) -> (State -> WormId) -> (ModifyPlayer -> ModifyState) -> ModifyState
advanceWormSelectionByWorms idPredicate playersWormId mapPlayer state@(State { wormHealths = wormHealths' }) =
  let myWormIds      = sort $ filter idPredicate $ aListFoldl' (flip ((:) . idSlot)) [] wormHealths'
      currentWormId' = playersWormId state
  in mapPlayer (withCurrentWormId (nextWormId currentWormId' myWormIds)) state

playerCurrentWormId :: Player -> WormId
playerCurrentWormId (Player _ wormId') = wormId'

thisPlayersCurrentWormId :: State -> WormId
thisPlayersCurrentWormId = playerCurrentWormId . myPlayer

thatPlayersCurrentWormId :: State -> WormId
thatPlayersCurrentWormId = playerCurrentWormId . opponent

makeMoveMoves :: Bool -> Move -> Move -> ModifyState
makeMoveMoves swapping this that state =
  let thisMoveMove          = if isAMoveMove this && (not $ targetOfThisMoveIsDirt this state) then Just this else Nothing
      thatMoveMove          = if isAMoveMove that && (not $ targetOfThatMoveIsDirt that state) then Just that else Nothing
      thisTarget            = thisMoveMove >>= ((flip targetOfThisMove) state)
      thatTarget            = thatMoveMove >>= ((flip targetOfThatMove) state)
      -- ASSUME: that a worm won't be on an invalid square
      thisWormsPosition     = thisWormsCoord state
      thatWormsPosition     = thatWormsCoord state
      thisWormId            = thisPlayersCurrentWormId state
      thisTargetIsValid     = ((thisTarget >>= mapAtCoord state) == Just AIR || (thisTarget >>= mapAtCoord state) == Just MEDIPACK) && (fmap (containsAnyWormExcept state thisWormId) thisTarget) == Just False
      thisTargetIsAMedipack = (thisTarget >>= mapAtCoord state) == Just MEDIPACK
      thatWormId            = thatPlayersCurrentWormId state
      thatTargetIsValid     = ((thatTarget >>= mapAtCoord state) == Just AIR || (thatTarget >>= mapAtCoord state) == Just MEDIPACK) && (fmap (containsAnyWormExcept state thatWormId) thatTarget) == Just False
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

giveMedipackToThisWorm :: ModifyState
giveMedipackToThisWorm state =
  let thisWormId = thisPlayersCurrentWormId state
  in awardPointsToThisPlayerForCollectingAMedipack $
     withWormHealths (mapWormById thisWormId (mapDataSlot increaseHealth)) state

giveMedipackToThatWorm :: ModifyState
giveMedipackToThatWorm state =
  let thatWormId = thatPlayersCurrentWormId state
  in awardPointsToThatPlayerForCollectingAMedipack $
     withWormHealths (mapWormById thatWormId (mapDataSlot increaseHealth)) state

wormCount :: Int
wormCount = 3

awardPointsToThisPlayerForCollectingAMedipack :: ModifyState
awardPointsToThisPlayerForCollectingAMedipack state =
  mapThisPlayer (modifyScore (healthPackHealth `div` wormCount)) state

awardPointsToThatPlayerForCollectingAMedipack :: ModifyState
awardPointsToThatPlayerForCollectingAMedipack state =
  mapThatPlayer (modifyScore (healthPackHealth `div` wormCount)) state

increaseHealth :: WormHealth -> WormHealth
increaseHealth = mapHealth (+ healthPackHealth)

removeMedipack :: Coord -> ModifyState
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

removeDirtFromMapAt :: Coord -> ModifyState
removeDirtFromMapAt coord = (flip mapGameMap) (removeDirtAt coord)

mapSquareAt :: Coord -> (Cell -> Cell) -> GameMap -> GameMap
mapSquareAt (Coord coord) f (GameMap xs) =
  GameMap $ M.adjust f coord xs

-- TODO test
allWormFacts :: (AListEntry a -> Bool) -> AList a -> Bool
allWormFacts f = aListFoldl' ( \ acc worm -> acc && f worm) True

-- TODO test
anyWormFacts :: (AListEntry a -> Bool) -> AList a -> Bool
anyWormFacts f = aListFoldl' ( \ acc worm -> acc || f worm) False

-- TODO test
aListFoldl' :: (b -> AListEntry a -> b) -> b -> AList a -> b
aListFoldl' f x (AList ys) = foldl' f x ys

containsAnyWormExcept :: State -> WormId -> Coord -> Bool
containsAnyWormExcept State { wormPositions = wormPositions' } wormId' coord' =
  anyWormFacts (\ (AListEntry wormId'' coord'') -> coord' == coord'' && wormId' /= wormId'') wormPositions'

isAMoveMove :: Move -> Bool
isAMoveMove (Move x)
  | x >= 8 && x < 16 = True
  | otherwise        = False

mapAtCoord :: State -> Coord -> Maybe Cell
mapAtCoord State { gameMap = gameMap' } (Coord target) = (\(GameMap xs) -> M.lookup target xs) gameMap'

-- TODO: get actual amount of damage
knockBackDamageAmount :: Int
knockBackDamageAmount = 20

mapDataSlot :: (a -> b) -> AListEntry a -> AListEntry b
mapDataSlot f (AListEntry id' x) = AListEntry id' $ f x

mapHealth :: (Int -> Int) -> WormHealth -> WormHealth
mapHealth f (WormHealth x) = WormHealth $ f x

knockBackDamage :: ModifyState
knockBackDamage state =
  let thisWormId       = thisPlayersCurrentWormId state
      thatWormId       = thatPlayersCurrentWormId state
      thisWormHealth'  = fromJust $
                         fmap dataSlot $
                         aListFind ((== thisWormId) . idSlot) $
                         wormHealths state
      thatWormHealth'  = fromJust $
                         fmap dataSlot $
                         aListFind ((== thatWormId) . idSlot) $
                         wormHealths state
      thisWormDied     = knockBackDamageAmount >= deconstructHealth thisWormHealth'
      thatWormDied     = knockBackDamageAmount >= deconstructHealth thatWormHealth'
      knockBackDamage' = mapDataSlot (mapHealth (+ (-knockBackDamageAmount)))
      cleanUpThis      = withWormHealths (removeWormById thisWormId) .
                         withWormPositions (removeWormById thisWormId)
      cleanUpThat      = withWormHealths (removeWormById thatWormId) .
                         withWormPositions (removeWormById thatWormId)
      harmThisWorm     = if thisWormDied
                         then cleanUpThis
                         else withWormHealths $ mapWormById thisWormId knockBackDamage'
      harmThatWorm     = if thatWormDied
                         then cleanUpThat
                         else withWormHealths $ mapWormById thatWormId knockBackDamage'
  in harmThisWorm $ harmThatWorm state

moveThisWorm :: Coord -> ModifyState
moveThisWorm newCoord' state =
  let thisWormId = thisPlayersCurrentWormId state
  in withWormPositions (mapWormById thisWormId (moveWorm newCoord')) state

idSlot :: AListEntry a -> WormId
idSlot (AListEntry id' _) = id'

dataSlot :: AListEntry a -> a
dataSlot (AListEntry _ data') = data'

moveWorm :: Coord -> AListEntry Coord -> AListEntry Coord
moveWorm position' = mapDataSlot (always position')

-- TODO test
mapWormById :: WormId -> (AListEntry a -> AListEntry a) -> AList a -> AList a
mapWormById wormId' f (AList xs) =
  AList $ map (\ x -> if idSlot x == wormId' then f x else x) xs

-- TODO test
moveThatWorm :: Coord -> ModifyState
moveThatWorm newCoord' state =
  let thatWormId = thatPlayersCurrentWormId state
  in withWormPositions (mapWormById thatWormId (moveWorm newCoord')) state

targetOfThisMove :: Move -> State -> Maybe Coord
targetOfThisMove dir state =
  let thisWormId = thisPlayersCurrentWormId state
      position'  = fmap dataSlot $ aListFind ((== thisWormId) . idSlot) $ wormPositions state
  in position' >>= targetOfMove dir

targetOfThatMove :: Move -> State -> Maybe Coord
targetOfThatMove dir state =
  let thatWormId = thatPlayersCurrentWormId state
      position'   = fmap dataSlot $ aListFind ((== thatWormId) . idSlot) $ wormPositions state
  in position' >>= targetOfMove dir

targetOfMove :: Move -> Coord -> Maybe Coord
targetOfMove = flip displaceCoordByMove

mapThisPlayer :: ModifyPlayer -> ModifyState
mapThisPlayer f state@(State { myPlayer = player' }) =
  state { myPlayer = f player' }

mapThatPlayer :: ModifyPlayer -> ModifyState
mapThatPlayer f state@(State { opponent = opponent' }) =
  state { opponent = f opponent' }

modifyScore :: Int -> Player -> Player
modifyScore delta (Player score' currentWorm) =
  Player (score' + delta) currentWorm

penaliseForInvalidCommand :: Player -> Player
penaliseForInvalidCommand = modifyScore (-4)

penaliseThisPlayerForAnInvalidCommand :: ModifyState
penaliseThisPlayerForAnInvalidCommand = mapThisPlayer penaliseForInvalidCommand

penaliseThatPlayerForAnInvalidCommand :: ModifyState
penaliseThatPlayerForAnInvalidCommand = mapThatPlayer penaliseForInvalidCommand

awardPointsForMovingToAir :: Player -> Player
awardPointsForMovingToAir = modifyScore 5

awardPointsToThisPlayerForMovingToAir :: ModifyState
awardPointsToThisPlayerForMovingToAir = mapThisPlayer awardPointsForMovingToAir

awardPointsToThatPlayerForMovingToAir :: ModifyState
awardPointsToThatPlayerForMovingToAir = mapThatPlayer awardPointsForMovingToAir

isADigMove :: Move -> Bool
isADigMove (Move x)
  | x >= 16 && x < 24 = True
  | otherwise         = False

-- Assumes that the move is a dig move
shiftDigToMoveRange :: Move -> Move
shiftDigToMoveRange (Move x) = Move $ x - 8

makeDigMoves :: Move -> Move -> ModifyState
makeDigMoves this that state =
  (makeDigMove this targetOfThisMove awardPointsToThisPlayerForDigging) $
  (makeDigMove that targetOfThatMove awardPointsToThatPlayerForDigging) state
  where
    makeDigMove move targetOfMove' awardPointsForDigging' =
      -- Target of move works with moves and not digs
      let moveMove      = if isADigMove move then Just (shiftDigToMoveRange move) else Nothing
          target        = moveMove >>= ((flip targetOfMove') state)
          targetIsValid = (target >>= mapAtCoord state) == Just DIRT
          -- fromJust is valid because we test whether it's Just on the above two lines
          validTarget   = fromJust target
          digOutTarget  = if targetIsValid then removeDirtFromMapAt validTarget else id
          awardPoints   = if targetIsValid then awardPointsForDigging' else id
      in awardPoints . digOutTarget

awardPointsForDigging :: Player -> Player
awardPointsForDigging = modifyScore 7

awardPointsToThisPlayerForDigging :: ModifyState
awardPointsToThisPlayerForDigging = mapThisPlayer awardPointsForDigging

awardPointsToThatPlayerForDigging :: ModifyState
awardPointsToThatPlayerForDigging = mapThatPlayer awardPointsForDigging

aListFilter :: (AListEntry a -> Bool) -> AList a -> AList a
aListFilter p (AList xs) = AList $ filter p xs

isMyWorm :: WormId -> Bool
isMyWorm (WormId 1) = True
isMyWorm (WormId 2) = True
isMyWorm (WormId 3) = True
isMyWorm _          = False

isOpponentWorm :: WormId -> Bool
isOpponentWorm (WormId 4)  = True
isOpponentWorm (WormId 8)  = True
isOpponentWorm (WormId 12) = True
isOpponentWorm _           = False

makeShootMoves :: Move -> Move -> ModifyState
makeShootMoves this that state =
  (harmWormForMove thisWormsCoord
                   (thisPlayersCurrentWormId state)
                   this
                   penaliseThisPlayerForHittingHisFriendlyWorm
                   awardPointsToThisPlayerForHittingAnEnemy
                   awardPointsToThisPlayerForKillingAnEnemy
                   awardPointsToThisPlayerForMissing) $
  (harmWormForMove thatWormsCoord
                   (thatPlayersCurrentWormId state)
                   that
                   penaliseThatPlayerForHittingHisFriendlyWorm
                   awardPointsToThatPlayerForHittingAnEnemy
                   awardPointsToThatPlayerForKillingAnEnemy
                   awardPointsToThatPlayerForMissing) state
  where
    harmWormForMove wormsCoord
                    wormId'
                    move
                    penalise
                    awardPlayer
                    awardPlayerForKill
                    awardPointsForMiss =
      let isShootMove     = isAShootMove move
          shootMove       = if isShootMove then Just move else Nothing
          gameMap'        = gameMap state
          -- ASSUME: that a worm won't be on an invalid square
          wormsPosition   = wormsCoord state
          shotsDir        = shootMove >>= directionOfShot
          coord           = shotsDir >>= ((flip (hitsWorm wormsPosition gameMap'))
                                          (wormPositions state))
          isHit           = isJust coord
          coord'          = fromJust coord
          awardMissPoints = if isShootMove then awardPointsForMiss else id
      in if isHit
         then harmWormWithRocket wormId'
                                 state
                                 penalise
                                 awardPlayer
                                 awardPlayerForKill
                                 coord'
         else awardMissPoints

awardPointsForMissing :: Player -> Player
awardPointsForMissing = modifyScore 2

awardPointsToThisPlayerForMissing :: ModifyState
awardPointsToThisPlayerForMissing = mapThisPlayer awardPointsForMissing

awardPointsToThatPlayerForMissing :: ModifyState
awardPointsToThatPlayerForMissing = mapThatPlayer awardPointsForMissing

awardPointsForHittingAnEnemy :: Player -> Player
awardPointsForHittingAnEnemy = modifyScore 2 -- (2 * rocketDamage)

awardPointsToThisPlayerForHittingAnEnemy :: ModifyState
awardPointsToThisPlayerForHittingAnEnemy = mapThisPlayer awardPointsForHittingAnEnemy

awardPointsToThatPlayerForHittingAnEnemy :: ModifyState
awardPointsToThatPlayerForHittingAnEnemy = mapThatPlayer awardPointsForHittingAnEnemy

awardPointsToThisPlayerForKillingAnEnemy :: ModifyState
awardPointsToThisPlayerForKillingAnEnemy =
  mapThisPlayer awardPointsForKillingAnEnemy

awardPointsToThatPlayerForKillingAnEnemy :: ModifyState
awardPointsToThatPlayerForKillingAnEnemy =
  mapThatPlayer awardPointsForKillingAnEnemy

awardPointsForKillingAnEnemy :: Player -> Player
awardPointsForKillingAnEnemy = modifyScore 40

deconstructHealth :: WormHealth -> Int
deconstructHealth (WormHealth x) = x

harmWormWithRocket :: WormId -> State -> ModifyState -> ModifyState -> ModifyState -> Coord -> ModifyState
harmWormWithRocket wormId'
                   originalState
                   penalisePlayer
                   awardPlayer
                   awardPlayerForKill =
  harmWorm wormId'
           originalState
           rocketDamage
           penalisePlayer
           awardPlayer
           awardPlayerForKill

harmWormById :: Int -> WormId -> WormHealths -> WormHealths
harmWormById damage' wormId' = mapWormById wormId' (mapDataSlot (mapWormHealth (+ (-damage'))))

-- TODO Test
removeWormById :: WormId -> AList a -> AList a
removeWormById wormId' = aListFilter ((/= wormId') . idSlot)

-- Assume: that the given coord maps to a worm
harmWorm :: WormId -> State -> Int -> ModifyState -> ModifyState -> ModifyState -> Coord -> ModifyState
harmWorm shootingWormId'
         originalState
         damage'
         penalisePlayer
         awardPlayer
         awardPlayerForKill
         coord =
  let wormId'       = fromJust $
                      fmap idSlot $
                      aListFind ((== coord) . dataSlot) $
                      wormPositions originalState
      samePlayer    = wormsBelongToSamePlayer wormId' shootingWormId'
      wormHealth'   = fromJust $
                      fmap dataSlot $
                      aListFind ((== wormId') . idSlot) $
                      wormHealths originalState
      wormDied      = (deconstructHealth wormHealth') <= damage'
      awardPoints   = if wormDied then awardPlayerForKill else awardPlayer
      dishOutPoints = if samePlayer
                      then penalisePlayer
                      else awardPoints
      cleanUp       = withWormHealths (removeWormById wormId') .
                      withWormPositions (removeWormById wormId')
      harm          = withWormHealths (harmWormById damage' wormId')
      go            = dishOutPoints . if wormDied then cleanUp else harm
  in go

penaliseThisPlayerForHittingHisFriendlyWorm :: ModifyState
penaliseThisPlayerForHittingHisFriendlyWorm = mapThisPlayer penaliseForHittingFriendlyWorm

penaliseThatPlayerForHittingHisFriendlyWorm :: ModifyState
penaliseThatPlayerForHittingHisFriendlyWorm = mapThatPlayer penaliseForHittingFriendlyWorm

penaliseForHittingFriendlyWorm :: Player -> Player
penaliseForHittingFriendlyWorm = modifyScore (-2 * rocketDamage)

wormsBelongToSamePlayer :: WormId -> WormId -> Bool
wormsBelongToSamePlayer thisWormId thatWormId =
  isMyWorm thisWormId == isMyWorm thatWormId

type ModifyState = State -> State

type ModifyPlayer = Player -> Player

type GetPlayer = State -> Player

type ModifyFacts a = AList a -> AList a

type WithWormFacts a = ModifyFacts a -> ModifyState

withWormHealths :: WithWormFacts WormHealth
withWormHealths f state@(State { wormHealths = wormHealths' }) =
  state { wormHealths = f wormHealths' }

withWormPositions :: WithWormFacts Coord
withWormPositions f state@(State { wormPositions = wormPositions' }) =
  state { wormPositions = f wormPositions' }

mapWormHealth :: (Int -> Int) -> WormHealth -> WormHealth
mapWormHealth f (WormHealth x) = WormHealth $ f x

rocketDamage :: Int
rocketDamage = 8

data Hit = HitWorm Coord
         | HitObstacle
         | HitNothing
  deriving (Eq, Show)

hitsWorm :: Coord -> GameMap -> Direction -> WormPositions -> Maybe Coord
hitsWorm origin gameMap' direction worms' =
  case (foldl' (firstWormHit gameMap' worms') HitNothing $
        possibleHitCoordinates origin direction) of
    HitWorm worm -> Just worm
    _            -> Nothing

firstWormHit :: GameMap -> WormPositions -> Hit -> Coord -> Hit
firstWormHit _        _      hit@(HitWorm _) _      = hit
firstWormHit _        _      HitObstacle     _      = HitObstacle
firstWormHit gameMap' worms' HitNothing      coord' =
  if obstacleAt coord' gameMap'
  then HitObstacle
  else isAPositionOfAWorm coord' worms'

aListFind :: (AListEntry a -> Bool) -> AList a -> Maybe (AListEntry a)
aListFind p (AList xs) = find p xs

isAPositionOfAWorm :: Coord -> WormPositions -> Hit
isAPositionOfAWorm coord' wormPositions' =
  case aListFind (\ (AListEntry _ position') -> coord' == position') wormPositions' of
    Just (AListEntry _ position') -> HitWorm position'
    Nothing                       -> HitNothing

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

diagonalRocketRange :: Int
diagonalRocketRange = 3

iterateDiagonally :: Coord -> Operator -> Operator -> [Coord]
iterateDiagonally coord fX fY = iterateCoordinate coord diagonalRocketRange fX fY

horizontalRocketRange :: Int
horizontalRocketRange = 4

iterateVertically :: Coord -> Operator -> [Coord]
iterateVertically coord fY = iterateCoordinate coord horizontalRocketRange idOperator fY

iterateHorizontally :: Coord -> Operator -> [Coord]
iterateHorizontally coord fX = iterateCoordinate coord horizontalRocketRange fX idOperator

iterateCoordinate :: Coord -> Int -> Operator -> Operator -> [Coord]
iterateCoordinate coord depth fX fY =
  let (x', y') = fromCoord coord
  in zipWith
     toCoord
     (zipWith fX (repeat x') (take depth [1..]))
     (zipWith fY (repeat y') (take depth [1..]))

-- Assume that this worm is never at an invalid position.
thisWormsCoord :: State -> Coord
thisWormsCoord state =
  let thisWormId = thisPlayersCurrentWormId state
  in  dataSlot $
      fromJust $
      aListFind ((== thisWormId) . idSlot) $
      wormPositions state

-- Assume that that worm is never at an invalid position.
thatWormsCoord :: State -> Coord
thatWormsCoord state =
  let thatWormId = thatPlayersCurrentWormId state
  in  dataSlot $
      fromJust $
      aListFind ((== thatWormId) . idSlot) $
      wormPositions state

data Direction = N
               | NE
               | E
               | SE
               | S
               | SW
               | W
               | NW
  deriving (Show)

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
  round' <- readRound
  state  <- readGameState round'
  liftIO $ putStrLn $ show state
  liftIO $ putStrLn $ "C;" ++ show roundNumber ++ ";nothing\n"
  startBot g (roundNumber + 1)
