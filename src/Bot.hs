{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Bot
  where

import Import

import qualified RIO.Vector.Boxed as V
import GHC.Generics (Generic)
import qualified RIO.ByteString.Lazy as B
import RIO.List
import RIO.List.Partial
import Data.Bits
import Data.Maybe
import System.IO
import System.Random
import Data.Aeson (decode, withObject, (.:), (.:?), FromJSON, parseJSON)
import System.Clock
import qualified Control.Concurrent.MVar as MVar
import Control.Concurrent

-- TODO: think long and hard about this...
import Prelude (read)

data State = State { opponentsLastCommand :: Maybe String,
                     currentRound         :: Int,
                     wormHealths          :: WormHealths,
                     wormPositions        :: WormPositions,
                     wormBananas          :: WormBananas,
                     wormSnowballs        :: WormSnowballs,
                     frozenDurations      :: WormFrozenDurations,
                     myPlayer             :: Player,
                     opponent             :: Player,
                     gameMap              :: GameMap }
             deriving (Generic, Eq)

type WormFrozenDurations = AList

type WormSnowballs = AList

type WormHealths = AList

type WormPositions = AList

type WormBananas = AList

type ModifyFacts = AList -> AList

type Bananas = Int

type WormHealth = Int

data AList = AList Int Int Int Int Int Int
  deriving (Eq)

wormIds :: [WormId]
wormIds = [WormId 1, WormId 2, WormId 3, WormId 4, WormId 8, WormId 12]

instance Show AList where
  show aList =
    let xs = aListToList aList
    in
    "[\n" ++
    (foldr (++) "" $ map (\ (wormId', value') -> "    " ++ show wormId' ++ " -> " ++ show value' ++ ",\n") xs) ++
    "]"

data WormId = WormId Int
  deriving (Eq, Show, Ord)

aListFindById :: WormId -> AList -> Maybe Int
aListFindById (WormId 1)  (AList (-1)    _    _     _    _     _) = Nothing
aListFindById (WormId 1)  (AList  x      _    _     _    _     _) = Just x
aListFindById (WormId 2)  (AList  _   (-1)    _     _    _     _) = Nothing
aListFindById (WormId 2)  (AList  _      x    _     _    _     _) = Just x
aListFindById (WormId 3)  (AList  _      _ (-1)     _    _     _) = Nothing
aListFindById (WormId 3)  (AList  _      _    x     _    _     _) = Just x
aListFindById (WormId 4)  (AList  _      _    _  (-1)    _     _) = Nothing
aListFindById (WormId 4)  (AList  _      _    _     x    _     _) = Just x
aListFindById (WormId 8)  (AList  _      _    _     _ (-1)     _) = Nothing
aListFindById (WormId 8)  (AList  _      _    _     _    x     _) = Just x
aListFindById (WormId 12) (AList  _      _    _     _    _  (-1)) = Nothing
aListFindById (WormId 12) (AList  _      _    _     _    _     x) = Just x
aListFindById wormId'     _                                       = error $ "aListFindById: " ++ show wormId'

aListFindDataById :: WormId -> AList -> Maybe Int
aListFindDataById = aListFindById

aListFindDataByData :: Int -> AList -> Maybe Int
aListFindDataByData x (AList a b c d e f) =
  case (x == a, x == b, x == c, x == d, x == e, x == f) of
    (True,    _,    _,    _,    _,    _) -> Just x
    (   _, True,    _,    _,    _,    _) -> Just x
    (   _,    _, True,    _,    _,    _) -> Just x
    (   _,    _,    _, True,    _,    _) -> Just x
    (   _,    _,    _,    _, True,    _) -> Just x
    (   _,    _,    _,    _,    _, True) -> Just x
    _                                    -> Nothing

aListFindIdByData :: Int -> AList -> Maybe WormId
aListFindIdByData x (AList a b c d e f) =
  case (x == a, x == b, x == c, x == d, x == e, x == f) of
    (True,    _,    _,    _,    _,    _) -> Just $ WormId 1
    (   _, True,    _,    _,    _,    _) -> Just $ WormId 2
    (   _,    _, True,    _,    _,    _) -> Just $ WormId 3
    (   _,    _,    _, True,    _,    _) -> Just $ WormId 4
    (   _,    _,    _,    _, True,    _) -> Just $ WormId 8
    (   _,    _,    _,    _,    _, True) -> Just $ WormId 12
    _                                    -> Nothing

minusOneWhenFailed :: (Int -> Bool) -> Int -> Int
minusOneWhenFailed _ (-1) = -1
minusOneWhenFailed p x
  | p x       = x
  | otherwise = -1

aListFilterByData :: (Int -> Bool) -> AList -> AList
aListFilterByData p (AList a b c d e f) =
  AList (minusOneWhenFailed p a)
        (minusOneWhenFailed p b)
        (minusOneWhenFailed p c)
        (minusOneWhenFailed p d)
        (minusOneWhenFailed p e)
        (minusOneWhenFailed p f)

aListSumThisPlayersValues :: AList -> Int
aListSumThisPlayersValues (AList a b c _ _ _) = a + b + c

aListSumThatPlayersValues :: AList -> Int
aListSumThatPlayersValues (AList _ _ _ d e f) = d + e + f

-- TODO: use bit twiddling hacks to supercharge this
aListCountMyEntries :: AList -> Int
aListCountMyEntries (AList a b c _ _ _) =
  if a /= -1 then 1 else 0 +
  if b /= -1 then 1 else 0 +
  if c /= -1 then 1 else 0

aListCountOpponentsEntries :: AList -> Int
aListCountOpponentsEntries (AList _ _ _ d e f) =
  if d /= -1 then 1 else 0 +
  if e /= -1 then 1 else 0 +
  if f /= -1 then 1 else 0

aListSumMyEntries :: AList -> Int
aListSumMyEntries (AList a b c _ _ _) =
  if a /= -1 then a else 0 +
  if b /= -1 then b else 0 +
  if c /= -1 then c else 0

aListSumOpponentsEntries :: AList -> Int
aListSumOpponentsEntries (AList _ _ _ d e f) =
  if d /= -1 then d else 0 +
  if e /= -1 then e else 0 +
  if f /= -1 then f else 0

aListMyIds :: AList -> [WormId]
aListMyIds (AList a b c _ _ _) =
  catMaybes [
  if a /= -1 then Just (WormId 1) else Nothing,
  if b /= -1 then Just (WormId 2) else Nothing,
  if c /= -1 then Just (WormId 3) else Nothing]

aListOpponentIds :: AList -> [WormId]
aListOpponentIds (AList _ _ _ d e f) =
  catMaybes [
  if d /= -1 then Just (WormId 4)  else Nothing,
  if e /= -1 then Just (WormId 8)  else Nothing,
  if f /= -1 then Just (WormId 12) else Nothing]

aListMyData :: AList -> [Int]
aListMyData (AList a b c _ _ _) =
  catMaybes [
  if a /= -1 then Just a else Nothing,
  if b /= -1 then Just b else Nothing,
  if c /= -1 then Just c else Nothing]

aListOpponentData :: AList -> [Int]
aListOpponentData (AList _ _ _ d e f) =
  catMaybes [
  if d /= -1 then Just d else Nothing,
  if e /= -1 then Just e else Nothing,
  if f /= -1 then Just f else Nothing]

anyWormFacts :: (WormId -> Int -> Bool) -> AList -> Bool
anyWormFacts p (AList a b c d e f) =
  p (WormId 1)  a ||
  p (WormId 4)  d ||
  p (WormId 2)  b ||
  p (WormId 8)  e ||
  p (WormId 3)  c ||
  p (WormId 12) f

anyWormData :: (Int -> Bool) -> AList -> Bool
anyWormData p (AList a b c d e f) =
  p a ||
  p d ||
  p b ||
  p e ||
  p c ||
  p f

aListAddMineAndHis :: AList -> AList -> AList
aListAddMineAndHis (AList a b c _ _ _) (AList _ _ _ d e f) =
  AList a b c d e f

aListFromList :: [(Int, Int)] -> AList
aListFromList xs =
  go xs (AList (-1) (-1) (-1) (-1) (-1) (-1))
  where
    go []               aList               = aList
    go ((1,   data'):xt) (AList _ b c d e f) = go xt (AList data'     b     c     d     e     f)
    go ((2,   data'):xt) (AList a _ c d e f) = go xt (AList     a data'     c     d     e     f)
    go ((3,   data'):xt) (AList a b _ d e f) = go xt (AList     a     b data'     d     e     f)
    go ((4,   data'):xt) (AList a b c _ e f) = go xt (AList     a     b     c data'     e     f)
    go ((8,   data'):xt) (AList a b c d _ f) = go xt (AList     a     b     c     d data'     f)
    go ((12,  data'):xt) (AList a b c d e _) = go xt (AList     a     b     c     d     e data')
    go ((id',     _):_)  _                   = error $ "aListFromList with wormId: " ++ show id'

aListToList :: AList -> [(WormId, Int)]
aListToList (AList a b c d e f) =
  catMaybes [if a /= -1 then Just  (WormId 1, a) else Nothing,
             if b /= -1 then Just  (WormId 2, b) else Nothing,
             if c /= -1 then Just  (WormId 3, c) else Nothing,
             if d /= -1 then Just  (WormId 4, d) else Nothing,
             if e /= -1 then Just  (WormId 8, e) else Nothing,
             if f /= -1 then Just (WormId 12, f) else Nothing]

-- TODO test
aListMapWormById :: WormId -> (Int -> Int) -> AList -> AList
aListMapWormById (WormId 1)  f' (AList a b c d e f) = AList (f' a)      b      c      d      e      f
aListMapWormById (WormId 2)  f' (AList a b c d e f) = AList      a (f' b)      c      d      e      f
aListMapWormById (WormId 3)  f' (AList a b c d e f) = AList      a      b (f' c)      d      e      f
aListMapWormById (WormId 4)  f' (AList a b c d e f) = AList      a      b      c (f' d)      e      f
aListMapWormById (WormId 8)  f' (AList a b c d e f) = AList      a      b      c      d (f' e)      f
aListMapWormById (WormId 12) f' (AList a b c d e f) = AList      a      b      c      d      e (f' f)
aListMapWormById wormId'     _  _                   = error $ "aListMapWormById with wormId: " ++ show wormId'

-- TODO Test
aListRemoveWormById :: WormId -> AList -> AList
aListRemoveWormById (WormId 1)  (AList _ b c d e f) = AList (-1)    b    c    d    e    f
aListRemoveWormById (WormId 2)  (AList a _ c d e f) = AList    a (-1)    c    d    e    f
aListRemoveWormById (WormId 3)  (AList a b _ d e f) = AList    a    b (-1)    d    e    f
aListRemoveWormById (WormId 4)  (AList a b c _ e f) = AList    a    b    c (-1)    e    f
aListRemoveWormById (WormId 8)  (AList a b c d _ f) = AList    a    b    c    d (-1)    f
aListRemoveWormById (WormId 12) (AList a b c d e _) = AList    a    b    c    d    e (-1)
aListRemoveWormById wormId'     _                   = error $ "Can't remove worm with id: " ++ show wormId'

showPositions :: AList -> String
showPositions aList =
    let xs = aListToList aList
    in
    "[\n" ++
    (foldr (++) "" $ map (\ (wormId', coord') -> "    " ++ show wormId' ++ " -> " ++ showCoord coord' ++ ",\n") xs) ++
    "]"

emptyAList :: AList
emptyAList = AList (-1) (-1) (-1) (-1) (-1) (-1)

instance Show State where
  show (State opponentsLastCommand'
              currentRound'
              wormsHealth'
              wormPositions'
              wormBananas'
              wormSnowballs'
              wormFrozenDurations'
              myPlayer'
              opponent'
              gameMap') =
    "State {\n" ++
    "  opponentsLastCommand = " ++ show opponentsLastCommand'   ++ "\n" ++
    "  currentRound         = " ++ show currentRound'           ++ "\n" ++
    "  wormHealths          = " ++ show wormsHealth'            ++ "\n" ++
    "  wormPositions        = " ++ showPositions wormPositions' ++ "\n" ++
    "  wormBananas          = " ++ show wormBananas'            ++ "\n" ++
    "  wormSnowballs        = " ++ show wormSnowballs'          ++ "\n" ++
    "  wormFrozenDurations' = " ++ show wormFrozenDurations'    ++ "\n" ++
    "  myPlayer             = " ++ show myPlayer'               ++ "\n" ++
    "  opponent             = " ++ show opponent'               ++ "\n" ++
    "  gameMap:\n" ++
    show gameMap' ++
    "}"

data GameMap = GameMap Integer Integer Integer Integer
  deriving (Generic, Eq)

instance Show GameMap where
  show = showRows . splitGameMap

showRows :: [[Cell]] -> String
showRows xs =
  "|" ++ (foldr (++) "" $ take mapDim $ repeat "-") ++ "|\n" ++
  (foldr (\ nextRow gameMap' -> gameMap' ++ "|" ++ (foldr (++) "" $ fmap show nextRow) ++ "|\n") "" xs) ++
  "|" ++ (foldr (++) "" $ take mapDim $ repeat "-") ++ "|"

splitGameMap :: GameMap -> [[Cell]]
splitGameMap gameMap' =
  let cells = zip [(1::Int)..]
                  (foldl' (\ acc coord' -> mapAt coord' gameMap' : acc) []
                   [0..((mapDim * mapDim) - 1)])
      iter []  = []
      iter xs' = (reverse $ take mapDim xs') : (iter $ drop mapDim xs')
  in iter $ map snd $ sortOn fst $ cells

mapAt :: Int -> GameMap -> Cell
mapAt coord' map'@(GameMap air dirt space medipacks) =
  if testBit air coord'
  then AIR
  else if testBit dirt coord'
       then DIRT
       else if testBit space coord'
            then DEEP_SPACE
            else if testBit medipacks coord'
                 then MEDIPACK
                 else error $ "Invalid game map for coord (" ++ show (fromCoord coord') ++ "):\n" ++ show map'

clearCellAt :: Coord -> Cell -> GameMap -> GameMap
clearCellAt coord' AIR        (GameMap air dirt space medipacks) =
  GameMap (clearBit air coord') dirt space medipacks
clearCellAt coord' DIRT       (GameMap air dirt space medipacks) =
  GameMap air (clearBit dirt coord') space medipacks
clearCellAt coord' DEEP_SPACE (GameMap air dirt space medipacks) =
  GameMap air dirt (clearBit space coord') medipacks
clearCellAt coord' MEDIPACK   (GameMap air dirt space medipacks) =
  GameMap air dirt space (clearBit medipacks coord')

setCellAt :: Coord -> Cell -> GameMap -> GameMap
setCellAt coord' AIR        (GameMap air dirt space medipacks) =
  GameMap (setBit air coord') dirt space medipacks
setCellAt coord' DIRT       (GameMap air dirt space medipacks) =
  GameMap air (setBit dirt coord') space medipacks
setCellAt coord' DEEP_SPACE (GameMap air dirt space medipacks) =
  GameMap air dirt (setBit space coord') medipacks
setCellAt coord' MEDIPACK   (GameMap air dirt space medipacks) =
  GameMap air dirt space (setBit medipacks coord')

modifyMapCellAt :: Int -> (Cell -> Cell) -> GameMap -> GameMap
modifyMapCellAt coord' f gameMap' =
  let cell               = mapAt coord' gameMap'
      gameMapWithoutCell = clearCellAt coord' cell gameMap'
      cell'              = f cell
  in setCellAt coord' cell' gameMapWithoutCell

emptyGameMap :: GameMap
emptyGameMap = GameMap 0 0 0 0

vectorGameMapToGameMap :: V.Vector Cell -> GameMap
vectorGameMapToGameMap =
  V.foldl' (\ gameMap' (coord', cell) -> setCellAt coord' cell gameMap') emptyGameMap .
  V.zip (V.fromList [0..(mapDim * mapDim) - 1])

mapAtCoord :: State -> Coord -> Cell
mapAtCoord State { gameMap = gameMap' } target = mapAt target gameMap'

mapDim :: Int
mapDim = 33

cellTo :: Coord -> Cell -> GameMap -> GameMap
cellTo position' newCell gameMap' =
  modifyMapCellAt position' (always newCell) gameMap'

removeDirtAt :: Coord -> GameMap -> GameMap
removeDirtAt = (flip cellTo) AIR

removeDirtFromMapAt :: Coord -> ModifyState
removeDirtFromMapAt coord = (flip mapGameMap) (removeDirtAt coord)

blockTypeAt :: Cell -> Coord -> GameMap -> Bool
blockTypeAt cell coord' = (== cell) . mapAt coord'

deepSpaceAt ::  Coord -> GameMap -> Bool
deepSpaceAt = blockTypeAt DEEP_SPACE

dirtAt :: Coord -> GameMap -> Bool
dirtAt = blockTypeAt DIRT

medipackAt :: Coord -> GameMap -> Bool
medipackAt = blockTypeAt MEDIPACK

obstacleAt :: Coord -> GameMap -> Bool
obstacleAt coord' =
   (\ square -> square == DIRT || square == DEEP_SPACE) . mapAt coord'

instance FromJSON State where
  parseJSON = withObject "State" $ \ v ->
    toState <$> v .: "myPlayer"
            <*> v .: "opponents"
            <*> v .: "map"
            <*> v .: "currentRound"

data Selections = Selections Int
  deriving (Eq, Show)

-- TODO: Change Int to PlayerScore for stronger types
data Player = Player Int WormId Selections
  deriving (Show, Generic, Eq)

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

removeHealthPoints :: (AList -> Int) -> WormHealths -> Player -> Player
removeHealthPoints summingFunction wormHealths' (Player score' wormId' selections') =
  let totalHealth = summingFunction wormHealths'
  in Player (score' - (totalHealth `div` wormCount)) wormId' selections'

-- TODO: repitition!!!  (What differs is the type of worm :/)
factsFromMyWorms :: ScratchPlayer -> (WormHealths, WormPositions, WormBananas, WormSnowballs, WormFrozenDurations)
factsFromMyWorms (ScratchPlayer _ _ worms' _) =
  let deadIds   = V.toList $
                  V.map fst $
                  V.filter ((<= 0) . snd) $
                  V.map (\ (ScratchWorm { wormId = wormId',
                                          wormHealth = wormHealth' }) -> (wormId', wormHealth'))
                  worms'
      notDead   = \ wormId' -> not $ elem wormId' deadIds
      healths   = aListFromList $
                  V.toList $
                  V.filter (notDead . fst) $
                  V.map (\ (ScratchWorm { wormId = wormId',
                                          wormHealth = wormHealth' }) -> (wormId', wormHealth'))
                  worms'
      positions = aListFromList $
                  V.toList $
                  V.filter (notDead . fst) $
                  V.map (\ (ScratchWorm { wormId = wormId',
                                          position = position' }) -> (wormId', coordToInt position'))
                  worms'
      bananas   = aListFromList $
                  filter (notDead . fst) $
                  catMaybes $
                  V.toList $
                  V.map ( \ (ScratchWorm { wormId      = wormId',
                                           bananaBombs = bananas' }) ->
                            fmap ( \ (BananaBomb count') -> (wormId', count') )
                            bananas')
                  worms'
  in (healths, positions, bananas, emptyAList, emptyAList)

factsFromOpponentsWorms :: Opponent -> (WormHealths, WormPositions, WormBananas, WormSnowballs, WormFrozenDurations)
factsFromOpponentsWorms (Opponent _ _ _ worms' _) =
  let deadIds   = V.toList $
                  V.map fst $
                  V.filter ((<= 0) . snd) $
                  V.map (\ (OpponentWorm { opWormId = wormId',
                                           opWormHealth = wormHealth' }) -> (shift wormId' 2, wormHealth'))
                  worms'
      notDead   = \ wormId' -> not $ elem wormId' deadIds
      healths   = aListFromList $
                  V.toList $
                  V.filter (notDead . fst) $
                  V.map (\ (OpponentWorm { opWormId = wormId',
                                           opWormHealth = wormHealth' }) -> ((shift wormId' 2), wormHealth'))
                  worms'
      positions = aListFromList $
                  V.toList $
                  V.filter (notDead . fst) $
                  V.map (\ (OpponentWorm { opWormId = wormId',
                                           opPosition = position' }) -> ((shift wormId' 2), coordToInt position'))
                  worms'
      -- TODO: This will parse the wrong count of bananas left.  If I
      -- lose the state on the first round.
      bananas   = aListFromList $
                  filter (notDead . fst) $
                  catMaybes $
                  V.toList $
                  V.map ( \ (OpponentWorm { opWormId = wormId', profession = profession' }) ->
                            if profession' == "Agent"
                            then Just (((shift wormId' 2), 3))
                            else Nothing)
                  worms'
  in (healths, positions, bananas, emptyAList, emptyAList)

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
    Just doNothing]

readThatWorm :: String -> WormId
readThatWorm = WormId . (flip shiftL) 2 . read

readThisWorm :: String -> WormId
readThisWorm = WormId . read

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
  -- TODO fromJust?
  let coord''      = fromJust $ coordForWorm selectedWorm $ wormPositions state
  otherMove       <- readThatMove state coord'' otherTokens
  return $ withSelection selectedWorm otherMove

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

data ScratchWorm = ScratchWorm { wormId        :: Int,
                                 wormHealth    :: Int,
                                 position      :: JSONCoord,
                                 weapon        :: Weapon,
                                 diggingRange  :: Int,
                                 movementRange :: Int,
                                 bananaBombs   :: Maybe BananaBomb,
                                 snowballs     :: Maybe Snowball }
            deriving (Show, Generic, Eq)

instance FromJSON ScratchWorm where
  parseJSON = withObject "Worm" $ \ v ->
    ScratchWorm <$> v .:  "id"
                <*> v .:  "health"
                <*> v .:  "position"
                <*> v .:  "weapon"
                <*> v .:  "diggingRange"
                <*> v .:  "movementRange"
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

data OpponentWorm = OpponentWorm { opWormId        :: Int,
                                   opWormHealth    :: Int,
                                   opPosition      :: JSONCoord,
                                   opDiggingRange  :: Int,
                                   opMovementRange :: Int,
                                   profession      :: String }
            deriving (Show, Generic, Eq)

instance FromJSON OpponentWorm where
  parseJSON = withObject "OpponentWorm" $ \ v ->
    OpponentWorm <$> v .: "id"
                 <*> v .: "health"
                 <*> v .: "position"
                 <*> v .: "diggingRange"
                 <*> v .: "movementRange"
                 <*> v .: "profession"

type Coord = Int

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

healthPackHealth :: Int
healthPackHealth = 10

toCoord :: Int -> Int -> Coord
toCoord xCoord yCoord =
  mapDim * yCoord + xCoord

-- Consider whether to use applicative here and get rid of the tuple
fromCoord :: Coord -> (Int, Int)
fromCoord xy =
  case (divMod xy mapDim) of
    (y', x') -> (x', y')

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

readGameState :: Int -> IO (Maybe State)
readGameState r = do
  stateString <- B.readFile $ "./rounds/" ++ show r ++ "/state.json"
  return $ decode stateString

-- ==========Move Encoding==========
-- Integer encoded move representation
-- 0  -> 8
-- 9  -> 16
-- 17 -> 24
-- 81 -> 106

-- Bit packing of move:
-- 0000000 0000 000000000000000000000
--    ^     ^
--    |     |
-- Moves Selects (mine then his because his worm id's are always left
-- shifted by 3)
-- Range of moves: 0 -> 127

-- Process to extract:
-- 1) check for a select;
-- 2) mask out the select;
-- 3) shift the select;
-- 4) check the range of the remaining number;
-- 5) extract and shift according to the type of move;
data Move = Move Int
  deriving (Show, Eq)

showCoord :: Coord -> String
showCoord xy = case fromCoord xy of
    (x', y') -> show x' ++ " " ++ show y'


formatMove :: (State -> Maybe Coord) -> (Move -> ModifyState) -> Move -> Coord -> State -> String
-- Shoot
formatMove wormsCoord makeSelections' dir@(Move x) xy state
  -- Select: Calls back into this function without the select
  | hasASelection dir = formatSelect wormsCoord makeSelections' dir state
  -- Shoot
  | isAShootMove  dir = formatShootMove dir
  -- Move
  | isAMoveMove   dir = moveFromMaybe $
                        fmap (\ newCoord -> "move "   ++ showCoord newCoord) $
                        displaceCoordByMove xy dir
  -- Dig
  | isADigMove    dir = moveFromMaybe $
                        fmap (\ newCoord -> "dig "    ++ showCoord newCoord) $
                        displaceCoordByMove xy (Move (x - 8))
  -- Throwing the bomb
  | isABananaMove dir = moveFromMaybe $
                        fmap (\ newCoord -> "banana " ++ showCoord newCoord) $
                        displaceToBananaDestination dir xy
-- Nothing
formatMove _ _ _ _ _ = "nothing"

-- For reference.  Here's how to visualise the encoding of banana bomb
-- destinations:
-- [                              24,
--                 25,  26,  27,  28,  29,  30,  31,
--            32,  33,  34,  35,  36,  37,  38,  39,  40,
--            41,  42,  43,  44,  45,  46,  47,  48,  49,
--            50,  51,  52,  53,  54,  55,  56,  57,  58,
--       59,  60,  61,  62,  63,  64,  65,  66,  67,  68,  69,
--            70,  71,  72,  73,  74,  75,  76,  77,  78,
--            79,  80,  81,  82,  83,  84,  85,  86,  87,
--            88,  89,  90,  91,  92,  93,  94,  95,  96,
--                 97,  98,  99, 100, 101, 102, 103,
--                               104]
-- TODO: Might want to consider never throwning the bomb at myself.
-- TODO: Might want to consider never hurting myself too?
coordDeltasInRange :: [(Coord -> Maybe Coord)]
coordDeltasInRange =
  zipWith ( \ dx dy ->
              \ xy ->
                fmap (uncurry toCoord) $
                isOOB $
                let (x', y') = fromCoord xy
                in (x' + dx, y' + dy))
  bananaXDisplacements
  bananaYDisplacements

bananaXDisplacements :: [Int]
bananaXDisplacements =
  [                    0,
           -3, -2, -1, 0, 1, 2, 3,
       -4, -3, -2, -1, 0, 1, 2, 3, 4,
       -4, -3, -2, -1, 0, 1, 2, 3, 4,
       -4, -3, -2, -1, 0, 1, 2, 3, 4,
   -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5,
       -4, -3, -2, -1, 0, 1, 2, 3, 4,
       -4, -3, -2, -1, 0, 1, 2, 3, 4,
       -4, -3, -2, -1, 0, 1, 2, 3, 4,
           -3, -2, -1, 0, 1, 2, 3,
                       0]

bananaYDisplacements :: [Int]
bananaYDisplacements =
  [                    -5,
           -4, -4, -4, -4, -4, -4, -4,
       -3, -3, -3, -3, -3, -3, -3, -3, -3,
       -2, -2, -2, -2, -2, -2, -2, -2, -2,
       -1, -1, -1, -1, -1, -1, -1, -1, -1,
     0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
        1,  1,  1,  1,  1,  1,  1,  1,  1,
        2,  2,  2,  2,  2,  2,  2,  2,  2,
        3,  3,  3,  3,  3,  3,  3,  3,  3,
            4,  4,  4,  4,  4,  4,  4,
                        5]

displaceToBananaDestination :: Move -> Coord -> Maybe Coord
displaceToBananaDestination (Move dir) coord' =
  (coordDeltasInRange !! (dir - 24)) coord'

selectEncodingRange :: Int
selectEncodingRange = 7

selectMoveMask :: Int
selectMoveMask = shiftL 15 selectEncodingRange

decodeSelection :: Move -> Int
decodeSelection (Move x) =
  shiftR (x .&. selectMoveMask) selectEncodingRange

decodeSelectionForFormatting :: Move -> Int
decodeSelectionForFormatting move =
  let shifted = decodeSelection move
  in if shifted > 3
     then shiftR shifted 2
     else shifted

moveMask :: Int
moveMask = complement selectMoveMask

removeSelectionFromMove :: Move -> Move
removeSelectionFromMove (Move x) =
  Move $ x .&. moveMask

formatSelect :: (State -> Maybe Coord) -> (Move -> ModifyState) -> Move -> State -> String
formatSelect wormsCoord makeSelections' move state =
  let selection = decodeSelectionForFormatting move
      move'     = removeSelectionFromMove move
  in "select " ++
     show selection ++
     ";" ++
     formatMove wormsCoord
                makeSelections'
                move'
                -- ASSUME: that we're selecting a worm at a valid coord
                (fromJust $ wormsCoord $ makeSelections' move state)
                state

makeThisSelection :: Move -> ModifyState
makeThisSelection move = makeSelections move doNothing

makeThatSelection :: Move -> ModifyState
makeThatSelection move = makeSelections doNothing move

formatShootMove :: Move -> String
formatShootMove (Move 0) = "shoot N"
formatShootMove (Move 1) = "shoot NE"
formatShootMove (Move 2) = "shoot E"
formatShootMove (Move 3) = "shoot SE"
formatShootMove (Move 4) = "shoot S"
formatShootMove (Move 5) = "shoot SW"
formatShootMove (Move 6) = "shoot W"
formatShootMove (Move 7) = "shoot NW"
formatShootMove x        = error $ "formatShootMove: " ++ show x

moveFromMaybe :: IsString p => Maybe p -> p
moveFromMaybe (Just move) = move
moveFromMaybe Nothing     = "nothing"

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
  deriving (Eq, Show)

playersMoveBits :: Int
playersMoveBits = 11

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
makeMove swapping moves state =
  let (myMove,  opponentsMove)  = toMoves moves
      (myMove', opponentsMove') = (removeSelectionFromMove myMove,
                                   removeSelectionFromMove opponentsMove)
  in -- assertValidState state  myMove  opponentsMove  $
     setOpponentsLastMove    state   opponentsMove  $
     advanceWormSelections                          $
     makeShootMoves          myMove' opponentsMove' $
     makeBananaMoves         myMove' opponentsMove' $
     makeDigMoves            myMove' opponentsMove' $
     makeMoveMoves  swapping myMove' opponentsMove' $
     makeSelections          myMove  opponentsMove state

-- DEBUG: This is for debugging.  I should comment out the line above
-- when I'm done using it...
assertValidState :: State -> Move -> Move -> ModifyState
assertValidState previousState myMove opponentsMove state =
  let wormHealths'       = map snd $ aListToList $ wormHealths   state
      wormPositions'     = map snd $ aListToList $ wormPositions state
      lengthMismatch     = length wormHealths' /= length wormPositions'
      invalidWormHealths = any (< (-1)) wormHealths'
      coordinatesOOB     = any (not . isJust . isOOB) $ map fromCoord wormPositions'
  in if lengthMismatch || invalidWormHealths || coordinatesOOB
     then error ("\nMy move:        " ++ prettyPrintThisMove previousState myMove ++ "(" ++ show myMove ++ ")"++ "\n" ++
                 "Opponents Move: " ++ prettyPrintThatMove previousState opponentsMove ++ "(" ++ show opponentsMove ++ ")" ++ "\n" ++
                 "Led to a bad state transition: " ++
                 "lengthMismatch: " ++ show lengthMismatch ++
                 ", invalidWormHealths: " ++ show invalidWormHealths ++
                 ", coordinatesOOB: " ++ show coordinatesOOB ++ ".\n" ++
                 "Moving from:\n" ++ show previousState ++ "\n" ++
                 "To:\n" ++ show state)
     else state

-- TODO I shouldn't even be doing this at all.
setOpponentsLastMove :: State -> Move -> State -> State
setOpponentsLastMove stateWhenMoveWasMade move' state =
  state { opponentsLastCommand = Just $ prettyPrintThatMove stateWhenMoveWasMade move' }

decrementSelections :: Selections -> Selections
decrementSelections (Selections x) = Selections $ x - 1

decrementPlayerSelections :: ModifyPlayer
decrementPlayerSelections (Player points' wormId' selections') =
  Player points' wormId' $ decrementSelections selections'

decrementThisPlayersSelections :: ModifyState
decrementThisPlayersSelections = mapThisPlayer decrementPlayerSelections

decrementThatPlayersSelections :: ModifyState
decrementThatPlayersSelections = mapThatPlayer decrementPlayerSelections

hasSelectionsLeft :: Player -> Bool
hasSelectionsLeft (Player _ _ (Selections x)) = x > 0

thisPlayerHasSelectionsLeft :: State -> Bool
thisPlayerHasSelectionsLeft = hasSelectionsLeft . myPlayer

thatPlayerHasSelectionsLeft :: State -> Bool
thatPlayerHasSelectionsLeft = hasSelectionsLeft . opponent

hasASelection :: Move -> Bool
hasASelection (Move x) = x >= 128

makeSelections :: Move -> Move -> ModifyState
makeSelections this that state =
  (makeSelection this
                 thisPlayerHasSelectionsLeft
                 decrementThisPlayersSelections
                 mapThisPlayer) $
  (makeSelection that
                 thatPlayerHasSelectionsLeft
                 decrementThatPlayersSelections
                 mapThatPlayer) state
  where
    makeSelection :: Move -> (State -> Bool) -> ModifyState -> (ModifyPlayer -> ModifyState) -> ModifyState
    makeSelection move' playerHasSelectionsLeft' decrementPlayersSelections' mapPlayer =
      let selection      = if hasASelection move'
                           then Just (WormId $ decodeSelection move')
                           else Nothing
          validSelection = selection >>= (\ selection' -> if wormExists selection' state &&
                                                             playerHasSelectionsLeft' state
                                                          then Just selection'
                                                          else Nothing)
          isValid        = isJust validSelection
          wormId'        = fromJust validSelection
      in if isValid
         then decrementPlayersSelections' . mapPlayer (withCurrentWormId wormId')
         else id

-- It's fine to use `findById' here because we don't care whether it's
-- an AListEntry or w/e.
wormExists :: WormId -> State -> Bool
wormExists wormId' = isJust . aListFindById wormId' . wormPositions

isABananaMove :: Move -> Bool
isABananaMove (Move x) =
  x < 105 && x >= 24

hasBananas :: Bananas -> Bool
hasBananas x = x > 0

thisWormHasBananasLeft :: State -> Bool
thisWormHasBananasLeft = wormHasBananasLeft thisPlayersCurrentWormId

thatWormHasBananasLeft :: State -> Bool
thatWormHasBananasLeft = wormHasBananasLeft thatPlayersCurrentWormId

wormHasBananasLeft :: (State -> WormId) -> State -> Bool
wormHasBananasLeft wormsId state =
  let wormId' = wormsId state
  in any hasBananas $ aListFindDataById wormId' $ wormBananas state

decrementBananas :: Bananas -> Bananas
decrementBananas (-1) = (-1)
decrementBananas 1    = (-1)
decrementBananas x    = x - 1

decrementWormsBananas :: (State -> WormId) -> ModifyState
decrementWormsBananas wormsId state =
  let wormId' = wormsId state
  in withWormBananas (aListMapWormById wormId' decrementBananas) state

decrementThisWormsBananas :: ModifyState
decrementThisWormsBananas = decrementWormsBananas thisPlayersCurrentWormId

decrementThatWormsBananas :: ModifyState
decrementThatWormsBananas = decrementWormsBananas thatPlayersCurrentWormId

withWormBananas :: WithWormFacts
withWormBananas f state@(State { wormBananas = wormBananas' }) =
  state { wormBananas = f wormBananas' }

makeBananaMoves :: Move -> Move -> ModifyState
makeBananaMoves this that state =
  (throwBanana this
               thisPlayersCurrentWormId
               thisWormsCoord
               thisWormHasBananasLeft
               decrementThisWormsBananas
               awardPointsToThisPlayerForDigging
               awardPointsToThisPlayerForDamage
               penaliseThisPlayerForDamage
               awardPointsToThisPlayerForMissing
               awardPointsToThisPlayerForKillingAnEnemy) $
  (throwBanana that
               thatPlayersCurrentWormId
               thatWormsCoord
               thatWormHasBananasLeft
               decrementThatWormsBananas
               awardPointsToThatPlayerForDigging
               awardPointsToThatPlayerForDamage
               penaliseThatPlayerForDamage
               awardPointsToThatPlayerForMissing
               awardPointsToThatPlayerForKillingAnEnemy) state
  where
    throwBanana :: Move -> (State -> WormId) -> (State -> Maybe Coord) -> (State -> Bool) -> ModifyState -> ModifyState -> (Int -> ModifyState) -> (Int -> ModifyState) -> ModifyState -> ModifyState -> ModifyState
    throwBanana move'
                playersCurrentWormId'
                wormsCoord'
                wormHasBananasLeft'
                decrementWormsBananas'
                awardPointsToPlayerForDigging'
                awardPointsToPlayerForDamage'
                penalisePlayerForDamage'
                awardPointsToPlayerForMissing'
                awardPointsToPlayerForKillingAnEnemy' =
      let bananaMove       = if isABananaMove move' then Just move' else Nothing
          wormsId'         = playersCurrentWormId' state
          destinationBlock = bananaMove >>=
            ( \ bananaMove' -> bananaMoveDestination wormHasBananasLeft' wormsCoord' bananaMove' state)
          -- TODO: looks very similar to L:2286
          isValid          = isJust destinationBlock
          target           = fromJust destinationBlock
      in if isValid
         then decrementWormsBananas' .
              bananaBlast wormsId'
                          awardPointsToPlayerForDigging'
                          awardPointsToPlayerForDamage'
                          penalisePlayerForDamage'
                          awardPointsToPlayerForMissing'
                          awardPointsToPlayerForKillingAnEnemy'
                          state
                          target
         else id

bananaMoveDestination :: (State -> Bool) -> (State -> Maybe Coord) -> Move -> State -> Maybe Coord
bananaMoveDestination currentWormHasBananasLeft
                      currentWormCoord
                      move
                      state =
  let move'     = if currentWormHasBananasLeft state then Just move else Nothing
      wormCoord = currentWormCoord state
  in wormCoord >>= (\ coord' -> move' >>= (flip displaceToBananaDestination) coord')

awardPointsToThisPlayerForDamage :: Int -> ModifyState
awardPointsToThisPlayerForDamage damage' = mapThisPlayer (awardPointsForDamage damage')

awardPointsToThatPlayerForDamage :: Int -> ModifyState
awardPointsToThatPlayerForDamage damage' = mapThatPlayer (awardPointsForDamage damage')

penaliseThisPlayerForDamage :: Int -> ModifyState
penaliseThisPlayerForDamage  damage' = mapThisPlayer (awardPointsForDamage (-damage'))

penaliseThatPlayerForDamage :: Int -> ModifyState
penaliseThatPlayerForDamage  damage' = mapThatPlayer (awardPointsForDamage (-damage'))

blastCoordDeltasInRange :: [(Coord -> Maybe (Int, Coord))]
blastCoordDeltasInRange =
  zipWith ( \ (damage', dx) dy ->
              \ xy ->
                fmap ( \ x -> (damage', x)) $
                fmap (uncurry toCoord) $
                isOOB $
                let (x', y') = fromCoord xy
                in (x' + dx, y' + dy))
  (zip damageTemplate
   [        0,
        -1, 0, 1,
    -2, -1, 0, 1, 2,
        -1, 0, 1,
            0])
   [       -2,
       -1, -1, -1,
    0,  0,  0,  0,  0,
        1,  1,  1,
            2]

containsAnyWorm :: Coord -> State -> Bool
containsAnyWorm coord' = anyWormData (== coord') . wormPositions

bananaBlastRadius :: Int
bananaBlastRadius = 2

damageTemplate :: [Int]
damageTemplate =
  zipWith ( \ dx dy ->
            let blastRadius' = bananaBlastRadius + 1
            in round $
               fromIntegral bananaCentreDamage *
               (((fromIntegral blastRadius') - (sqrt $ squareAbsFloating dx + squareAbsFloating dy)) /
                 fromIntegral blastRadius'))
  [        0,
       -1, 0, 1,
   -2, -1, 0, 1, 2,
       -1, 0, 1,
           0]
  [       -2,
      -1, -1, -1,
   0,  0,  0,  0,  0,
       1,  1,  1,
           2]
  where
    squareAbsFloating :: Int -> Double
    squareAbsFloating x = fromIntegral $ abs x * abs x

bananaBlast :: WormId -> ModifyState -> (Int -> ModifyState) -> (Int -> ModifyState) -> ModifyState -> ModifyState -> State -> Coord -> ModifyState
bananaBlast wormId'
            awardPointsForDigging'
            awardPointsForDamage'
            penaliseForDamage'
            awardPointsForMissing'
            rewardKill
            originalState
            targetCoord
            state =
  let gameMap'          = gameMap originalState
      targetIsDeepSpace = deepSpaceAt targetCoord gameMap'
      potentialHits     = catMaybes $ map ($ targetCoord) blastCoordDeltasInRange
      -- Compute the things to hit off of the original state
      wormHits          = filter ((flip containsAnyWorm) originalState . snd) potentialHits
      dirtHits          = filter ((flip dirtAt)          gameMap'      . snd) potentialHits
      packHits          = filter ((flip medipackAt)      gameMap'      . snd) potentialHits
      -- Effect the current state (could have changed as a result of
      -- the other worm blasting too)
      withWormsDamaged  = foldl' (\ state' (damage', nextWormHit) ->
                                    harmWorm wormId'
                                             originalState
                                             damage'
                                             (penaliseForDamage'    damage')
                                             (awardPointsForDamage' damage')
                                             rewardKill
                                             nextWormHit
                                             state')
                          state
                          wormHits
      withDirtRemoved   = foldl' (\ state' (_, dirtHit) ->
                                    awardPointsForDigging' $ removeDirtFromMapAt dirtHit state')
                          withWormsDamaged dirtHits
  in if targetIsDeepSpace then awardPointsForMissing' state
     else foldl' (\ state' (_, packHit) -> removeMedipack packHit state')
          withDirtRemoved packHits

bananaCentreDamage :: Int
bananaCentreDamage = 20

advanceWormSelections :: ModifyState
advanceWormSelections =
  advanceThisWormSelection .
  advanceThatWormSelection

advanceThisWormSelection :: ModifyState
advanceThisWormSelection =
  advanceWormSelectionByWorms aListMyIds thisPlayersCurrentWormId mapThisPlayer

advanceThatWormSelection :: ModifyState
advanceThatWormSelection =
  advanceWormSelectionByWorms aListOpponentIds thatPlayersCurrentWormId mapThatPlayer

withCurrentWormId :: WormId -> Player -> Player
withCurrentWormId wormId' (Player score' _ selections') = (Player score' wormId' selections')

-- ASSUME: that there are worm ids to search through
nextWormId :: WormId -> [WormId] -> WormId
nextWormId wormId' wormIds' =
  iter wormIds'
  where
    iter []     = head wormIds'
    iter (x:xs) = if x == wormId'
                  then if xs == []
                       then head wormIds'
                       else head xs
                  else iter xs

advanceWormSelectionByWorms :: (AList -> [WormId]) -> (State -> WormId) -> (ModifyPlayer -> ModifyState) -> ModifyState
advanceWormSelectionByWorms playersWormIds playersWormId mapPlayer state@(State { wormHealths = wormHealths' }) =
  let myWormIds      = sort $ playersWormIds wormHealths'
      currentWormId' = playersWormId state
  in if myWormIds == []
     then state
     else mapPlayer (withCurrentWormId (nextWormId currentWormId' myWormIds)) state

playerCurrentWormId :: Player -> WormId
playerCurrentWormId (Player _ wormId' _) = wormId'

thisPlayersCurrentWormId :: State -> WormId
thisPlayersCurrentWormId = playerCurrentWormId . myPlayer

thatPlayersCurrentWormId :: State -> WormId
thatPlayersCurrentWormId = playerCurrentWormId . opponent

-- TODO refactor with is valid and friends
makeMoveMoves :: Bool -> Move -> Move -> ModifyState
makeMoveMoves swapping this that state =
  -- This check is old!!!  Digging is encoded differently now.
  let thisMoveMove          = if isAMoveMove this && (not $ targetOfThisMoveIsDirt this state) then Just this else Nothing
      thatMoveMove          = if isAMoveMove that && (not $ targetOfThatMoveIsDirt that state) then Just that else Nothing
      thisTarget            = thisMoveMove >>= ((flip targetOfThisMove) state)
      thatTarget            = thatMoveMove >>= ((flip targetOfThatMove) state)
      -- ASSUME: move moves always come first so the worm will be there
      thisWormsPosition     = fromJust $ thisWormsCoord state
      thatWormsPosition     = fromJust $ thatWormsCoord state
      thisWormId            = thisPlayersCurrentWormId state
      thisTargetIsValid     = ((fmap (mapAtCoord state) thisTarget) == Just AIR || (fmap (mapAtCoord state) thisTarget) == Just MEDIPACK) && (fmap (containsAnyWormExcept state thisWormId) thisTarget) == Just False
      thisTargetIsAMedipack = (fmap (mapAtCoord state) thisTarget) == Just MEDIPACK
      thatWormId            = thatPlayersCurrentWormId state
      thatTargetIsValid     = ((fmap (mapAtCoord state) thatTarget) == Just AIR || (fmap (mapAtCoord state) thatTarget) == Just MEDIPACK) && (fmap (containsAnyWormExcept state thatWormId) thatTarget) == Just False
      thatTargetIsAMedipack = (fmap (mapAtCoord state) thatTarget) == Just MEDIPACK
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
  (fmap (mapAtCoord state) $ targetOfThisMove move state) == Just DIRT

targetOfThatMoveIsDirt :: Move -> State -> Bool
targetOfThatMoveIsDirt move state =
  (fmap (mapAtCoord state) $ targetOfThatMove move state) == Just DIRT

giveMedipackToThisWorm :: ModifyState
giveMedipackToThisWorm state =
  let thisWormId = thisPlayersCurrentWormId state
  in withWormHealths (aListMapWormById thisWormId increaseHealth) state

giveMedipackToThatWorm :: ModifyState
giveMedipackToThatWorm state =
  let thatWormId = thatPlayersCurrentWormId state
  in withWormHealths (aListMapWormById thatWormId increaseHealth) state

increaseHealth :: WormHealth -> WormHealth
increaseHealth = (+) healthPackHealth

removeMedipack :: Coord -> ModifyState
removeMedipack position' =
  (flip mapGameMap) (cellTo position' AIR)

always :: a -> b -> a
always x _ = x

mapGameMap :: State -> (GameMap -> GameMap) -> State
mapGameMap state@(State { gameMap = gameMap' }) f =
  state { gameMap = f gameMap' }

containsAnyWormExcept :: State -> WormId -> Coord -> Bool
containsAnyWormExcept State { wormPositions = wormPositions' } wormId' coord' =
  anyWormFacts (\ wormId'' coord'' -> coord' == coord'' && wormId' /= wormId'') wormPositions'

isAMoveMove :: Move -> Bool
isAMoveMove (Move x) = x >= 8 && x < 16

-- TODO: get actual amount of damage
knockBackDamageAmount :: Int
knockBackDamageAmount = 20

knockBackDamage :: ModifyState
knockBackDamage state =
  knockBackDamageToOne thisPlayersCurrentWormId $
  knockBackDamageToOne thatPlayersCurrentWormId state
  where
    knockBackDamage' = (+) (-knockBackDamageAmount)
    -- ASSUME: that there is a worm to apply knockback damage to
    -- because both worms must have moved for this to happen
    knockBackDamageToOne wormsId =
      let wormId'      = wormsId state
          wormsHealth' = fromJust $
                         aListFindDataById wormId' $
                         wormHealths state
          wormDied     = knockBackDamageAmount >= wormsHealth'
          cleanUp      = cleanUpDeadWorm wormId'
      in if wormDied
         then cleanUp
         else withWormHealths $ aListMapWormById wormId' knockBackDamage'

cleanUpDeadWorm :: WormId -> ModifyState
cleanUpDeadWorm wormId' =
  withWormHealths   (aListRemoveWormById wormId') .
  withWormPositions (aListRemoveWormById wormId') .
  withWormBananas   (aListRemoveWormById wormId')

moveWorm :: (State -> WormId) -> Coord -> ModifyState
moveWorm wormsId newCoord' state =
  let thisWormId = wormsId state
  in withWormPositions (aListMapWormById thisWormId (always newCoord')) state

moveThisWorm :: Coord -> ModifyState
moveThisWorm = moveWorm thisPlayersCurrentWormId

-- TODO test
moveThatWorm :: Coord -> ModifyState
moveThatWorm = moveWorm thatPlayersCurrentWormId

targetOfMove :: (State -> WormId) -> Move -> State -> Maybe Coord
targetOfMove wormsId dir state =
  let wormId'   = wormsId state
      position' = aListFindDataById wormId' $ wormPositions state
  in position' >>= (flip displaceCoordByMove) dir

targetOfThisMove :: Move -> State -> Maybe Coord
targetOfThisMove = targetOfMove thisPlayersCurrentWormId

targetOfThatMove :: Move -> State -> Maybe Coord
targetOfThatMove = targetOfMove thatPlayersCurrentWormId

mapThisPlayer :: ModifyPlayer -> ModifyState
mapThisPlayer f state@(State { myPlayer = player' }) =
  state { myPlayer = f player' }

mapThatPlayer :: ModifyPlayer -> ModifyState
mapThatPlayer f state@(State { opponent = opponent' }) =
  state { opponent = f opponent' }

modifyScore :: Int -> Player -> Player
modifyScore delta (Player score' currentWorm selections') =
  Player (score' + delta) currentWorm selections'

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
isADigMove (Move x) = x >= 16 && x < 24

-- ASSUME: that the move is a dig move
shiftDigToMoveRange :: Move -> Move
shiftDigToMoveRange (Move x) = Move $ x - 8

makeDigMoves :: Move -> Move -> ModifyState
makeDigMoves this that state =
  (makeDigMove this targetOfThisMove awardPointsToThisPlayerForDigging penaliseThisPlayerForAnInvalidCommand) $
  (makeDigMove that targetOfThatMove awardPointsToThatPlayerForDigging penaliseThatPlayerForAnInvalidCommand) state
  where
    makeDigMove move targetOfMove' awardPointsForDigging' penalise =
      -- Target of move works with moves and not digs
      let isADigMove'    = isADigMove move
          target         = targetOfDigMove targetOfMove' state move
          targetIsValid  = isValidDigMove  targetOfMove' state move
          -- fromJust is valid because we test whether it's Just on the above two lines
          validTarget    = fromJust target
          digOutTarget   = if targetIsValid then removeDirtFromMapAt validTarget else id
          penaliseBadDig = if isADigMove' && (not targetIsValid) then penalise else id
          awardPoints    = if targetIsValid then awardPointsForDigging' else penaliseBadDig
      in awardPoints . digOutTarget

awardPointsForDigging :: Player -> Player
awardPointsForDigging = modifyScore 7

awardPointsToThisPlayerForDigging :: ModifyState
awardPointsToThisPlayerForDigging = mapThisPlayer awardPointsForDigging

awardPointsToThatPlayerForDigging :: ModifyState
awardPointsToThatPlayerForDigging = mapThatPlayer awardPointsForDigging

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
          wormsPosition   = wormsCoord state
          shotsDir        = shootMove >>= directionOfShot
          coord           = wormsPosition >>=
            ( \ position' -> shotsDir >>= ((flip (hitsWorm position' gameMap')))
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

awardPointsForDamage :: Int -> Player -> Player
awardPointsForDamage damage' = modifyScore (2 * damage')

awardPointsForHittingAnEnemy :: Player -> Player
awardPointsForHittingAnEnemy = awardPointsForDamage rocketDamage

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
harmWormById damage' wormId' = aListMapWormById wormId' (+ (-damage'))

-- DEBUG: This is for debugging.  I should comment out the lines
-- bellow when I'm done using it...
errorWithMessageIfJust :: String -> Maybe a -> Maybe a
errorWithMessageIfJust message Nothing = error message
errorWithMessageIfJust _       x       = x

-- ASSUME: that the given coord maps to a worm
harmWorm :: WormId -> State -> Int -> ModifyState -> ModifyState -> ModifyState -> Coord -> ModifyState
harmWorm shootingWormId'
         originalState
         damage'
         penalisePlayer
         awardPlayer
         awardPlayerForKill
         coord
         state =
  let wormId'       = fromJust $
                      -- errorWithMessageIfJust ("Couldn't find worm with position: " ++ showCoord coord ++ "\nState: " ++ show originalState) $
                      aListFindIdByData coord $
                      wormPositions originalState
      samePlayer    = wormsBelongToSamePlayer wormId' shootingWormId'
      wormHealth'   = aListFindDataById wormId' $
                      wormHealths state
      wormDied      = (not $ isJust wormHealth') || (fromJust wormHealth') <= damage'
      awardPoints   = if wormDied then (awardPlayer . awardPlayerForKill) else awardPlayer
      dishOutPoints = if samePlayer
                      then penalisePlayer
                      else awardPoints
      cleanUp       = cleanUpDeadWorm wormId'
      harm          = withWormHealths (harmWormById damage' wormId')
      go            = dishOutPoints . if wormDied then cleanUp else harm
  in go state

penaliseThisPlayerForHittingHisFriendlyWorm :: ModifyState
penaliseThisPlayerForHittingHisFriendlyWorm = mapThisPlayer penaliseForHittingFriendlyWorm

penaliseThatPlayerForHittingHisFriendlyWorm :: ModifyState
penaliseThatPlayerForHittingHisFriendlyWorm = mapThatPlayer penaliseForHittingFriendlyWorm

penaliseForHittingFriendlyWorm :: Player -> Player
penaliseForHittingFriendlyWorm = awardPointsForDamage (-rocketDamage)

wormsBelongToSamePlayer :: WormId -> WormId -> Bool
wormsBelongToSamePlayer thisWormId thatWormId =
  isMyWorm thisWormId == isMyWorm thatWormId

type ModifyState = State -> State

type ModifyPlayer = Player -> Player

type GetPlayer = State -> Player

type WithWormFacts = ModifyFacts -> ModifyState

withWormHealths :: WithWormFacts
withWormHealths f state@(State { wormHealths = wormHealths' }) =
  state { wormHealths = f wormHealths' }

withWormPositions :: WithWormFacts
withWormPositions f state@(State { wormPositions = wormPositions' }) =
  state { wormPositions = f wormPositions' }

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

isAPositionOfAWorm :: Coord -> WormPositions -> Hit
isAPositionOfAWorm coord' wormPositions' =
  case aListFindDataByData coord' wormPositions' of
    Just position' -> HitWorm position'
    Nothing        -> HitNothing

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
  let (x', y')     = fromCoord coord
      curriedIsOOB = curry isOOB
  in catMaybes $
     zipWith
     -- I don't know why I can't simplify this expression but W/E
     (\ x'' y'' -> fmap (uncurry toCoord) $ curriedIsOOB x'' y'')
     (zipWith fX (repeat x') (take depth [1..]))
     (zipWith fY (repeat y') (take depth [1..]))

-- ASSUME: that this worm is never at an invalid position.
--
-- This assumption is wrong because a worm could die before we get to
-- later stages where we use it.
thisWormsCoord :: State -> Maybe Coord
thisWormsCoord state =
  let thisWormId = thisPlayersCurrentWormId state
  in  coordForWorm thisWormId $ wormPositions state

coordForWorm :: WormId -> WormPositions -> Maybe Coord
coordForWorm = aListFindDataById

-- ASSUME: that that worm is never at an invalid position.
--
-- This assumption is wrong because a worm could die before we get to
-- later stages where we use it.
thatWormsCoord :: State -> Maybe Coord
thatWormsCoord state =
  let thatWormId = thatPlayersCurrentWormId state
  in  coordForWorm thatWormId $ wormPositions state

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
isAShootMove (Move x) = x < 8 && x >= 0

readRound :: IO Int
readRound = readLn

myMovesFromTree :: SearchTree -> SuccessRecords
myMovesFromTree (SearchedLevel   (MyMoves myMoves) _ _) = myMoves
myMovesFromTree (UnSearchedLevel (MyMoves myMoves) _)   = myMoves
myMovesFromTree SearchFront                             =
  error $ "myMovesFromTree of SearchFront"

iterationsBeforeComms :: Int
iterationsBeforeComms = 10

type CommsChannel = MVar.MVar

pollComms :: CommsChannel a -> IO (Maybe a)
pollComms = MVar.tryTakeMVar

readComms :: CommsChannel a -> IO a
readComms = MVar.takeMVar

writeComms :: CommsChannel a -> a -> IO ()
writeComms = MVar.putMVar

newComms :: IO (CommsChannel a)
newComms = MVar.newEmptyMVar

logStdErr :: String -> IO ()
logStdErr = hPutStrLn stderr

-- First iteration I think that I'll suspend the thread until a new
-- state comes along.
-- TODO: don't suspend the thread when the new state comes along.
iterativelyImproveSearch :: StdGen -> State -> SearchTree -> CommsChannel (CombinedMove, State) -> CommsChannel SearchTree -> IO ()
iterativelyImproveSearch gen initialState tree stateChannel treeChannel = do
  go gen iterationsBeforeComms tree
  where
    go :: StdGen -> Int -> SearchTree-> IO ()
    go gen' 0      searchTree = do
      searchTree'    <- evaluate searchTree
      writeComms treeChannel searchTree'
      newRoundsState <- pollComms stateChannel
      case newRoundsState of
        Just (move', state') -> do
          -- This isn't good enough.  I need to have a mode of searching in
          -- between, when the runner hasn't yet told me to move because it's
          -- 900ms from that point that I communicate back.
          let tree'' = makeMoveInTree move' searchTree
          let (myMove', opponentsMove') = (toMoves move')
          when (tree'' == SearchFront) $
            logStdErr $
            "Not in search tree: " ++
            "\n\tCombined: " ++ show move' ++
            "\n\tMy move: " ++ prettyPrintThisMove initialState myMove' ++
            "\n\tOpponents move: " ++ prettyPrintThatMove initialState opponentsMove'
          iterativelyImproveSearch gen' state' tree'' stateChannel treeChannel
        Nothing -> go gen' iterationsBeforeComms searchTree'
    go gen' count' searchTree =
      let (result, gen'') = search gen' 0 initialState searchTree [] []
          newTree         = updateTree initialState result searchTree
      in go gen'' (count' - 1) newTree

makeMoveInTree :: CombinedMove -> SearchTree -> SearchTree
makeMoveInTree move' (SearchedLevel   _ _ transitions) = findSubTree move' transitions
makeMoveInTree _     (UnSearchedLevel _ _)             = SearchFront
makeMoveInTree _     SearchFront                       = SearchFront

-- In nanoseconds
maxSearchTime :: Integer
maxSearchTime = 900000000

-- In microseconds
pollInterval :: Int
pollInterval = 5000

joinWith :: (a -> String) -> String -> [a] -> String
joinWith toString joinString strings =
  let withExtra = concat $ map ( \ x -> toString x ++ "\n\t") strings
  in take ((length withExtra) - (length joinString)) withExtra

prettyPrintMove :: (State -> Maybe Coord) -> (Move -> ModifyState) -> State -> Move -> String
prettyPrintMove wormsCoord makeSelections' state move =
  let coord' = (fromJust $ wormsCoord state)
  in formatMove wormsCoord makeSelections' move coord' state

prettyPrintThisMove :: State -> Move -> String
prettyPrintThisMove = prettyPrintMove thisWormsCoord makeThisSelection

prettyPrintThatMove :: State -> Move -> String
prettyPrintThatMove = prettyPrintMove thatWormsCoord makeThatSelection

prettyPrintSuccessRecord :: (State -> Move -> String) -> State -> SuccessRecord -> String
prettyPrintSuccessRecord printMove state (SuccessRecord (Wins wins') (Played played') move') =
    printMove state move' ++ ": " ++ show wins' ++ "/" ++ show played'

prettyPrintThisSuccessRecord :: State -> SuccessRecord -> String
prettyPrintThisSuccessRecord = prettyPrintSuccessRecord prettyPrintThisMove

prettyPrintThatSuccessRecord :: State -> SuccessRecord -> String
prettyPrintThatSuccessRecord = prettyPrintSuccessRecord prettyPrintThatMove

prettyPrintSearchTree :: State -> SearchTree -> String
prettyPrintSearchTree state (SearchedLevel (MyMoves myMoves) (OpponentsMoves opponentsMoves) transitions') =
    "Searched:\n" ++
    "My moves:\n\t" ++ (joinWith (prettyPrintThisSuccessRecord state) "\n\t" myMoves) ++ "\n" ++
    "Opponents moves:\n\t" ++ (joinWith (prettyPrintThatSuccessRecord state) "\n\t" opponentsMoves) ++ "\n" ++
    "Transitions:\n\t" ++ (join' ", " $ map (\ (StateTransition move' _) -> move') transitions')
prettyPrintSearchTree state (UnSearchedLevel (MyMoves myMoves) (OpponentsMoves opponentsMoves)) =
    "UnSearched:\n" ++
    "My moves:\n\t" ++ (joinWith (prettyPrintThisSuccessRecord state) "\n\t" myMoves) ++ "\n" ++
    "Opponents moves:\n\t" ++ (joinWith (prettyPrintThatSuccessRecord state) "\n\t" opponentsMoves)
prettyPrintSearchTree _     SearchFront =
    "SearchFront"

treeAfterAlottedTime :: State -> CommsChannel SearchTree -> IO SearchTree
treeAfterAlottedTime _ treeChannel = do
  startingTime <- fmap toNanoSecs $ getTime clock
  searchTree   <- go SearchFront startingTime
  return searchTree
  where
    clock = Realtime
    go searchTree startingTime =
      (getTime clock) >>=
      \ timeNow ->
        if ((toNanoSecs timeNow) - startingTime) > maxSearchTime
        then return searchTree -- (logStdErr $ prettyPrintSearchTree state searchTree) >> return searchTree
        else do
          pollResult <- pollComms treeChannel
          let searchTree' = case pollResult of
                              Just    x -> x
                              Nothing   -> searchTree
          Control.Concurrent.threadDelay pollInterval
          go searchTree' startingTime

searchForAlottedTime :: State -> CommsChannel SearchTree -> IO Move
searchForAlottedTime state =
  fmap (successRecordMove . chooseBestMove . myMovesFromTree) . (treeAfterAlottedTime state)

runRound :: Int -> State -> CommsChannel (CombinedMove, State) -> CommsChannel SearchTree -> IO ()
runRound roundNumber previousState stateChannel treeChannel = do
  move                 <- liftIO $ searchForAlottedTime previousState treeChannel
  liftIO $
    putStrLn $
    -- ASSUME: that the worm is on a valid square to begin with
    "C;" ++
    show roundNumber ++
    ";" ++
    formatMove thisWormsCoord makeThisSelection move (fromJust $ thisWormsCoord previousState) previousState ++ "\n"
  roundNumber'         <- readRound
  state                <- readGameState roundNumber'
  -- TODO fromJust?
  state'               <- evaluate $ fromJust state
  opponentsLastMove    <- evaluate $ parseLastCommand previousState $ opponentsLastCommand state'
  -- TODO!!!!!  I shouldn't be reading this state in the searcher.
  -- All I care about is the opponents move...
  writeComms stateChannel $ (fromMoves move opponentsLastMove, state')
  runRound roundNumber' state' stateChannel treeChannel

parseLastCommand :: State -> Maybe String -> Move
parseLastCommand _             Nothing             = doNothing
parseLastCommand previousState (Just lastCommand') =
  let -- TODO fromJust?
      coord'  = fromJust $ thatWormsCoord previousState
  in fromJust $ readThatMove previousState coord' lastCommand'

withoutCommandWord :: String -> Maybe String
withoutCommandWord = tailMaybe . dropWhile (/= ' ')

startBot :: StdGen -> RIO App ()
startBot g = do
  treeChannel   <- liftIO newComms
  stateChannel  <- liftIO newComms
  -- This is where I seed it with a search front
  initialRound' <- liftIO $ readRound
  initialState  <- liftIO $ fmap fromJust $ readGameState initialRound'
  _             <- liftIO $ forkIO (iterativelyImproveSearch g initialState SearchFront stateChannel treeChannel)
  liftIO $ runRound initialRound' initialState stateChannel treeChannel

data Wins = Wins Int
  deriving (Eq)

data Played = Played Int
  deriving (Eq)

data SuccessRecord = SuccessRecord Wins Played Move
  deriving (Eq)

instance Show SuccessRecord where
  show (SuccessRecord (Wins wins') (Played played') move') =
    show move' ++ ": " ++ show wins' ++ "/" ++ show played'

type SuccessRecords = [SuccessRecord]

successRecordMove :: SuccessRecord -> Move
successRecordMove (SuccessRecord _ _ move) = move

wins :: SuccessRecord -> Wins
wins (SuccessRecord wins' _ _) = wins'

played :: SuccessRecord -> Played
played (SuccessRecord _ played' _) = played'

data MyMoves = MyMoves SuccessRecords
  deriving (Eq)

data OpponentsMoves = OpponentsMoves SuccessRecords
  deriving (Eq)

data StateTransition = StateTransition CombinedMove SearchTree
  deriving (Eq, Show)

hasMove :: CombinedMove -> StateTransition -> Bool
hasMove move' (StateTransition move'' _) = move' == move''

subTree :: StateTransition -> SearchTree
subTree (StateTransition _ tree) = tree

type StateTransitions = [StateTransition]

data SearchTree = SearchedLevel   MyMoves OpponentsMoves StateTransitions
                | UnSearchedLevel MyMoves OpponentsMoves
                | SearchFront
                deriving (Eq)

join' :: Show a => String -> [a] -> String
join' joinString strings =
  let withExtra = concat $ map ( \ x -> show x ++ "\n\t") strings
  in take ((length withExtra) - (length joinString)) withExtra

instance Show SearchTree where
  show (SearchedLevel (MyMoves myMoves) (OpponentsMoves opponentsMoves) _) =
    "Searched: " ++ "\n" ++
    "My moves:\n\t" ++ (join' "\n\t" myMoves) ++ "\n" ++
    "Opponents moves:\n\t" ++ (join' "\n\t" opponentsMoves) -- ++ "\n" ++
    -- "Transitions: " ++ show transitions
  show (UnSearchedLevel (MyMoves myMoves) (OpponentsMoves opponentsMoves)) =
    "UnSearched: " ++ "\n" ++
    "My moves:\n\t" ++ (join' "\n\t" myMoves) ++ "\n" ++
    "Opponents moves:\n\t" ++ (join' "\n\t" opponentsMoves)
  show SearchFront =
    "SearchFront"

data MyReward = MyReward Int

data OpponentsReward = OpponentsReward Int

data Reward = Reward MyReward OpponentsReward

type Rewards = [Reward]

data SearchResult = SearchResult Payoff Moves
                  deriving (Show)

inc :: Int -> Int -> Int
inc x = (+x)

dec :: Int -> Int -> Int
dec x y = y - x

incInc :: Int -> SuccessRecord -> SuccessRecord
incInc x (SuccessRecord (Wins wins') (Played played') playerMove') =
  SuccessRecord (Wins $ (inc x) wins') (Played $ (inc maxScore) played') playerMove'

gamesPlayedForRecords :: [SuccessRecord] -> Int
gamesPlayedForRecords = sum . map ( (\ (Played x) -> x) . played)

countGames :: SearchTree -> Int
countGames = (`div` maxScore) . gamesPlayedForRecords . myMovesFromTree

-- TODO: doesn't go deep
updateTree :: State -> SearchResult -> SearchTree -> SearchTree
updateTree state result SearchFront =
  updateTree state result $
  UnSearchedLevel
  (MyMoves        $ map (SuccessRecord (Wins 0) (Played 0)) $ myMovesFrom        state)
  (OpponentsMoves $ map (SuccessRecord (Wins 0) (Played 0)) $ opponentsMovesFrom state)
updateTree _ result level@(UnSearchedLevel (MyMoves myMoves) (OpponentsMoves opponentsMoves)) =
  case result of
    (SearchResult  (Payoff (MyPayoff myPayoff) (OpponentsPayoff opponentsPayoff)) (move':_)) ->
      let (thisMove, thatMove) = toMoves move'
          myMoves'             = MyMoves        $ updateCount (incInc myPayoff)        myMoves        thisMove
          opponentsMoves'      = OpponentsMoves $ updateCount (incInc opponentsPayoff) opponentsMoves thatMove
      in (transitionLevelType myMoves' opponentsMoves') myMoves' opponentsMoves'
    _                           -> level
updateTree state result level@(SearchedLevel (MyMoves myMoves) (OpponentsMoves opponentsMoves) stateTransitions) =
  case result of
    (SearchResult  (Payoff (MyPayoff myPayoff) (OpponentsPayoff opponentsPayoff)) (move':_)) ->
      let (thisMove, thatMove) = toMoves move'
          myMoves'             = MyMoves        $ updateCount (incInc myPayoff)        myMoves        thisMove
          opponentsMoves'      = OpponentsMoves $ updateCount (incInc opponentsPayoff) opponentsMoves thatMove
      in SearchedLevel myMoves' opponentsMoves' $ updateSubTree state result stateTransitions
    _                           -> level

-- TODO: consider whether I should be treating the rewards like I do moves when transitioning down a tree..?
updateSubTree :: State -> SearchResult -> StateTransitions -> StateTransitions
updateSubTree state (SearchResult payoff (move':moves')) [] =
  [StateTransition move' $ updateTree state (SearchResult payoff moves') SearchFront]
updateSubTree _     (SearchResult _ [])                   transitions = transitions
updateSubTree state
              result@(SearchResult payoff (move':moves'))
              (transition@(StateTransition transitionMove' subTree'):transitions)
  | move' == transitionMove' = (StateTransition transitionMove' $
                                updateTree (makeMove False transitionMove' state)
                                           (SearchResult payoff moves') subTree') : transitions
  | otherwise                = transition : updateSubTree state result transitions

transitionLevelType :: MyMoves -> OpponentsMoves -> (MyMoves -> OpponentsMoves -> SearchTree)
transitionLevelType myMoves opponentsMoves =
    if allGamesPlayed myMoves opponentsMoves
    then \ myMoves' opponentsMoves' -> SearchedLevel   myMoves' opponentsMoves' []
    else \ myMoves' opponentsMoves' -> UnSearchedLevel myMoves' opponentsMoves'

allGamesPlayed :: MyMoves -> OpponentsMoves -> Bool
allGamesPlayed (MyMoves myMoves) (OpponentsMoves opponentsMoves) =
  all hasBeenPlayed myMoves && all hasBeenPlayed opponentsMoves
  where
    hasBeenPlayed (SuccessRecord _ (Played played') _) = played' /= 0

updateCount :: (SuccessRecord -> SuccessRecord) -> SuccessRecords -> Move -> SuccessRecords
updateCount _           []            _ = []
updateCount changeCount (record:rest) move'
  | successRecordMove record == move' = (changeCount record):rest
  | otherwise                         = record:(updateCount changeCount rest move')

reward :: State -> State -> Reward
reward previousState nextState =
  Reward (MyReward        $ computeMyScore        nextState - computeMyScore        previousState)
         (OpponentsReward $ computeOpponentsScore nextState - computeOpponentsScore previousState)

search :: StdGen -> Int -> State -> SearchTree -> Moves -> Rewards -> (SearchResult, StdGen)
-- The first iteration of play randomly is here because we need to use
-- that move when we write the first entry in an unsearched level.
search g round' state SearchFront                moves rewards =
  case gameOver state round' rewards of
    GameOver payoff -> (SearchResult payoff (reverse moves), g)
    NoResult        ->
      let availableMoves = filteredMovesFrom state
          (move, g')     = pickOneAtRandom g availableMoves
          state'         = makeMove False move state
          reward'        = reward state state'
      in playRandomly g' (round' + 1) state' (move:moves) (reward':rewards)
search g round' state tree@(SearchedLevel _ _ _) moves rewards =
  case gameOver state round' rewards of
    GameOver payoff -> (SearchResult payoff (reverse moves), g)
    NoResult        -> searchSearchedLevel g round' state tree moves rewards
search g
       round'
       state
       (UnSearchedLevel (MyMoves myMoves) (OpponentsMoves opponentsMoves))
       moves
       rewards =
  case gameOver state round' rewards of
    GameOver payoff -> (SearchResult payoff (reverse moves), g)
    NoResult        ->
      let (myRecord,        g')  = pickOneAtRandom g  myMoves
          (opponentsRecord, g'') = pickOneAtRandom g' opponentsMoves
          myMove                 = successRecordMove myRecord
          opponentsMove          = successRecordMove opponentsRecord
          combinedMove           = fromMoves myMove opponentsMove
          state'                 = makeMove False combinedMove state
          reward'                = reward state state'
      in search g''
                (round' + 1)
                state'
                SearchFront
                (combinedMove:moves)
                (reward':rewards)

findSubTree :: CombinedMove -> StateTransitions -> SearchTree
findSubTree combinedMove stateTransitions =
  case (find (hasMove combinedMove) stateTransitions) of
        Just transition -> subTree transition
        Nothing         -> SearchFront

pickOneAtRandom :: StdGen -> [a] -> (a, StdGen)
pickOneAtRandom g xs =
  let (i, g') = next g
      index   = i `mod` (length xs)
  in (xs !! index, g')

type Moves = [CombinedMove]

searchSearchedLevel :: StdGen -> Int -> State -> SearchTree -> Moves -> Rewards -> (SearchResult, StdGen)
searchSearchedLevel _ _ _ SearchFront                   _ _ = error "searchSearchedLevel: SearchFront"
searchSearchedLevel _ _ _ level@(UnSearchedLevel _ _ )  _ _ = error $ "searchSearchedLevel: " ++ show level
searchSearchedLevel g
                    round'
                    state
                    (SearchedLevel (MyMoves myMoves) (OpponentsMoves opponentsMoves) transitions)
                    moves
                    rewards =
  let myBestMove        = successRecordMove $ chooseBestMove myMoves
      opponentsBestMove = successRecordMove $ chooseBestMove opponentsMoves
      combinedMove      = fromMoves myBestMove opponentsBestMove
      state'            = makeMove True combinedMove state
      reward'           = reward state state'
  in search g
            (round' + 1)
            state'
            (findSubTree combinedMove transitions)
            (combinedMove:moves)
            (reward':rewards)

-- Number is the points I get.  The opponent gets ten less that
-- number.
data GameOver = GameOver Payoff
              | NoResult

playRandomly :: StdGen -> Int -> State -> Moves -> Rewards -> (SearchResult, StdGen)
playRandomly g round' state moves rewards =
  case gameOver state round' rewards of
    GameOver payoff -> (SearchResult payoff (reverse moves), g)
    NoResult        ->
      let availableMoves  = filteredMovesFrom state
          (move, g')      = if availableMoves == []
                            then (fromMoves doNothing doNothing, g)
                            else pickOneAtRandom g availableMoves
          state'          = makeMove False move state
          reward'         = reward state state'
      in playRandomly g' (round' + 1) state' moves (reward':rewards)

maxRound :: Int
maxRound = 4

playerScore :: Player -> Int
playerScore (Player score' _ _) = score'

-- REALLY IMPORTANT IDEA: Never ever make a move if it would lead to
-- defeat in the next turn!!  i.e. become super conservative on 6
-- health, just run away and don't run into the line of fire.

-- IMPROVEMENT ON REALLY IMPORTANT IDEA: Actually the end of a game is
-- very simple.  I should be playing from a play book during it.  Modes:
--  - If I'm ahead in points then I should run such that I'm never in
--    the line of fire.
--  - If I'm ahead in health and the other worm is engaging me then
--    kill him.

-- IDEA: When winning or losing, instead of getting one point and the
-- number of rounds increasing by one get points as a ratio of 500
-- points ahead or behind hte opponent.  i.e. 10 points for being 500
-- or more points ahead and -10 for being as much behind (although the
-- numbers here will always be positive.)

maxScore :: Int
maxScore = length diffMaxScale

computeScore :: (State -> Int) -> (State -> Player) -> State -> Int
computeScore totalWormHealth' player state =
  let averageHealth :: Double
      averageHealth = (fromIntegral $ totalWormHealth' state) / fromIntegral wormCount
  in (playerScore $ player state) + round averageHealth

computeMyScore :: State -> Int
computeMyScore = computeScore myTotalWormHealth myPlayer

computeOpponentsScore :: State -> Int
computeOpponentsScore = computeScore opponentsTotalWormHealth opponent

-- TODO simplified score calculation to save time here...
gameOver :: State -> Int -> Rewards -> GameOver
gameOver state round' rewards =
  let myWormCount            = aListCountMyEntries $ wormHealths state
      myAverageHealth :: Double
      myAverageHealth        = (fromIntegral $ myTotalWormHealth state) / fromIntegral wormCount
      myScore'               = (playerScore $ myPlayer state) + round myAverageHealth
      opponentWormCount      = aListCountOpponentsEntries $ wormHealths state
      opponentsAverageHealth :: Double
      opponentsAverageHealth = (fromIntegral $ opponentsTotalWormHealth state) / fromIntegral wormCount
      opponentsScore'        = (playerScore $ opponent state) + round opponentsAverageHealth
      myScoreIsHigher        = myScore' > opponentsScore'
  in if myWormCount == 0
     then if opponentWormCount == 0
          -- We died on the same round
          then if myScoreIsHigher
               -- I won because of points when both players are dead
               then GameOver $ Payoff (MyPayoff maxScore) (OpponentsPayoff 0)
               -- I lost because of points when both players are dead
               else GameOver $ Payoff (MyPayoff 0) (OpponentsPayoff maxScore)
          -- The opponent killed all my worms and I didn't kill his
          else GameOver $ Payoff (MyPayoff 0) (OpponentsPayoff maxScore)
     else if opponentWormCount == 0
          -- I Killed his worms and he didn't kill mine
          then GameOver $ Payoff (MyPayoff maxScore) (OpponentsPayoff 0)
          else if round' >= maxRound
               -- Simulation was terminated early.  Decide based on how valuable the moves were
               then GameOver $ diffMax rewards
               -- Simulation isn't over yet
               else NoResult

myTotalWormHealth :: State -> Int
myTotalWormHealth = aListSumMyEntries . wormHealths

opponentsTotalWormHealth :: State -> Int
opponentsTotalWormHealth = aListSumOpponentsEntries . wormHealths

diffMaxScale :: [Int]
diffMaxScale = [-1, 3, 3, 3, 3, 7, 20, 20, 40, maxBound::Int]

-- Normalisation factor is calculated as the relative worth of each
-- round summed together.
normalisationFactor :: Double
normalisationFactor =
  sum $ map (1.0 /) $ take maxRound [1..]

data OpponentsPayoff = OpponentsPayoff Int
  deriving (Eq, Show)

data MyPayoff = MyPayoff Int
  deriving (Eq, Show)

data Payoff = Payoff MyPayoff OpponentsPayoff
  deriving (Eq, Show)

-- This is a sliding scale so low values have high resolution and it
-- begins to tail off with larger values.  Linear isn't a good idea
-- because the bot loses incentive to get ahead.
--
-- This function expects the rewards to be reversed because that's
-- convenient for the search function (which conses them onto an
-- accumulator).
diffMax :: Rewards -> Payoff
diffMax rewards =
  -- This function linearly decreases the value of points gained by
  -- making a move.  The rewards are in reverse order here
  let (myScore', opponentsScore') = (\ (accX', accY') -> (round $ accX' / normalisationFactor,
                                                          round $ accY' / normalisationFactor)) accumulated
      accumulated :: (Double, Double)
      accumulated                 = foldl' (\ (accX', accY') (x', y') -> (x' + accX', y' + accY')) (0, 0) $
                                    withDecreasingReward
      withDecreasingReward :: [(Double, Double)]
      withDecreasingReward        = zipWith ( \ (Reward (MyReward x') (OpponentsReward y')) i -> (fromIntegral x' / i, fromIntegral y' / i))
                                    (reverse rewards) [1..]
      myIndex                     = fromJust $ findIndex (>= myScore')        diffMaxScale
      opponentsIndex              = fromJust $ findIndex (>= opponentsScore') diffMaxScale
  in Payoff (MyPayoff myIndex) (OpponentsPayoff opponentsIndex)

chooseBestMove :: [SuccessRecord] -> SuccessRecord
chooseBestMove successRecords =
  let totalGames = gamesPlayedForRecords successRecords
      computeConfidence (SuccessRecord (Wins wins')  (Played played') _) =
        confidence totalGames wins' played'
  in maximumBy ( \ oneTree otherTree -> compare (computeConfidence oneTree) (computeConfidence otherTree)) successRecords

confidence :: Int -> Int -> Int -> Float
confidence totalCount wins' played' =
  (w_i / n_i) +
  c * sqrt ((log count_i) / n_i)
  where
    count_i = fromIntegral totalCount
    n_i     = fromIntegral played'
    w_i     = fromIntegral wins'
    c       = sqrt 2

movesFrom :: State -> [CombinedMove]
movesFrom state = do
  myMove        <- myMovesFrom        state
  opponentsMove <- opponentsMovesFrom state
  return $ fromMoves myMove opponentsMove

filteredMovesFrom :: State -> [CombinedMove]
filteredMovesFrom state = do
  myMove        <- myFilteredMovesFrom        state
  opponentsMove <- opponentsFilteredMovesFrom state
  return $ fromMoves myMove opponentsMove

myMovesFrom :: State -> [Move]
myMovesFrom state = do
  let moves  = map Move [0..104]
  let moves' = addThisPlayersSelects state moves
  myMove <- moves'
  guard (isThisMoveValid state myMove)
  return myMove

myFilteredMovesFrom :: State -> [Move]
myFilteredMovesFrom state =
  let moves' = myMovesFrom state
  in filter (shouldMakeThisMove state moves') moves'

addPlayersSelects :: (State -> Bool) -> (AList -> [WormId]) -> State -> [Move] -> [Move]
addPlayersSelects playerHasSelectionsLeft playersWormIds state moves =
  if not $ playerHasSelectionsLeft state
  then moves
  else moves ++ do
    selection  <- playersWormIds $ wormPositions state
    move       <- moves
    return $ withSelection selection move

addThisPlayersSelects :: State -> [Move] -> [Move]
addThisPlayersSelects = addPlayersSelects thisPlayerHasSelectionsLeft aListMyIds

addThatPlayersSelects :: State -> [Move] -> [Move]
addThatPlayersSelects = addPlayersSelects thatPlayerHasSelectionsLeft aListOpponentIds

-- TODO: Lots of repitition
withSelection :: WormId -> Move -> Move
withSelection  (WormId id') (Move x) =
  Move $ x .|. (shiftL id' selectEncodingRange)

opponentsMovesFrom :: State -> [Move]
opponentsMovesFrom state = do
  let moves  = map Move [0..104]
  let moves' = addThatPlayersSelects state moves
  opponentsMove <- moves'
  guard (isThatMoveValid state opponentsMove)
  return $ opponentsMove

opponentsFilteredMovesFrom :: State -> [Move]
opponentsFilteredMovesFrom state =
  let moves' = opponentsMovesFrom state
  in filter (shouldMakeThatMove state moves') moves'

doNothing :: Move
doNothing = Move 106

isThisMoveValid :: State -> Move -> Bool
isThisMoveValid state move
  | isADigMove move    = isValidDigMove targetOfThisMove state move
  | isAMoveMove move   = isValidMoveMove targetOfThisMove thisPlayersCurrentWormId state move
  | isAShootMove move  = True
  | isABananaMove move = shouldMakeBananaMove
                         (bananaMoveDestination thisWormHasBananasLeft thisWormsCoord)
                         state
                         move
  | hasASelection move = thisPlayerHasSelectionsLeft state &&
                         (isThisMoveValid (makeSelections move doNothing state) $
                          removeSelectionFromMove move)
  | otherwise          = True

shouldMakeThisMove :: State -> [Move] -> Move -> Bool
shouldMakeThisMove state moves move
  | isADigMove move    = isValidDigMove targetOfThisMove state move
  | isAMoveMove move   = shouldThisPlayerMakeMoveMove moves state move
  | isAShootMove move  = any (elem move) $ theseHits state
  | isABananaMove move = shouldMakeBananaMove
                         (bananaMoveDestination thisWormHasBananasLeft thisWormsCoord)
                         state
                         move
  | hasASelection move = thisPlayerHasSelectionsLeft state &&
                         (shouldMakeThisMove (makeSelections move doNothing state) moves $
                          removeSelectionFromMove move)
  | otherwise          = True

shouldThisPlayerMakeMoveMove :: [Move] -> State -> Move -> Bool
shouldThisPlayerMakeMoveMove =
  shouldMakeMoveMove targetOfThisMove thisPlayersCurrentWormId thoseHitCoords

shouldMakeMoveMove :: (Move -> State -> Maybe Coord) -> (State -> WormId) -> (State -> Maybe [Coord]) -> [Move] -> State -> Move -> Bool
shouldMakeMoveMove targetOfMove' currentWormId' hitFunction moves state move =
  let wormId' = currentWormId' state
      coord'  = fromJust $ aListFindDataById wormId' $ wormPositions state
  in isValidMoveMove targetOfMove' currentWormId' state move &&
     ((not $ any (isValidDigMove targetOfMove' state) moves) ||
      (any (elem coord') $ hitFunction state))

hitCoords :: (AList -> [Int]) -> (State -> Maybe Coord) -> State -> Maybe [Coord]
hitCoords wormData wormsCoord state =
  fmap ( \ wormPosition -> wormData $
                           aListFilterByData (hits' wormPosition) $
                           wormPositions state) $
  wormsCoord state
  where
    hits' :: Coord -> Coord -> Bool
    hits' wormPosition opPosition' =
      aligns  wormPosition opPosition' &&
      inRange wormPosition opPosition' weaponRange

thoseHitCoords :: State -> Maybe [Coord]
thoseHitCoords = hitCoords aListMyData thatWormsCoord

theseHitCoords :: State -> Maybe [Coord]
theseHitCoords = hitCoords aListOpponentData thisWormsCoord

shouldMakeBananaMove :: (Move -> State -> Maybe Coord) -> State -> Move -> Bool
shouldMakeBananaMove destination state move =
  let destination' = destination move state
  in isJust destination' && (not $ any ((flip deepSpaceAt) $ gameMap state) destination')

-- TODO bad repitition!
isThatMoveValid :: State -> Move -> Bool
isThatMoveValid state move
  | isADigMove    move = isValidDigMove targetOfThatMove state move
  | isAMoveMove   move = isValidMoveMove targetOfThatMove thatPlayersCurrentWormId state move
  | isAShootMove  move = True
  | isABananaMove move = shouldMakeBananaMove
                         (bananaMoveDestination thatWormHasBananasLeft thatWormsCoord)
                         state
                         move
  | hasASelection move = thatPlayerHasSelectionsLeft state &&
                         (isThatMoveValid (makeSelections doNothing move state) $
                          removeSelectionFromMove move)
  | otherwise          = True

shouldMakeThatMove :: State -> [Move] -> Move -> Bool
shouldMakeThatMove state moves move
  | isADigMove    move = isValidDigMove targetOfThatMove state move
  | isAMoveMove   move = shouldThatPlayerMakeMoveMove moves state move
  | isAShootMove  move = any (elem move) $ thoseHits state
  | isABananaMove move = shouldMakeBananaMove
                         (bananaMoveDestination thatWormHasBananasLeft thatWormsCoord)
                         state
                         move
  | hasASelection move = thatPlayerHasSelectionsLeft state &&
                         (shouldMakeThatMove (makeSelections doNothing move state) moves $
                          removeSelectionFromMove move)
  | otherwise          = True

shouldThatPlayerMakeMoveMove :: [Move] -> State -> Move -> Bool
shouldThatPlayerMakeMoveMove =
  shouldMakeMoveMove targetOfThatMove thatPlayersCurrentWormId theseHitCoords

targetOfMoveMove :: (Move -> State -> Maybe Coord) -> State -> Move -> Maybe Coord
targetOfMoveMove targetOfMove' state move =
  let moveMove = if isAMoveMove move then Just move else Nothing
  in moveMove >>= ((flip targetOfMove') state)

isValidMoveMove :: (Move -> State -> Maybe Coord) -> (State -> WormId) -> State -> Move -> Bool
isValidMoveMove targetOfMove' currentWormId' state move =
  let targetCoord = targetOfMoveMove targetOfMove' state move
      target      = (fmap (mapAtCoord state) $ targetCoord)
      wormId'     = currentWormId' state
  in (target == Just AIR || target == Just MEDIPACK) &&
     (fmap (containsAnyWormExcept state wormId') targetCoord) == Just False

targetOfDigMove :: (Move -> State -> Maybe Coord) -> State -> Move -> Maybe Coord
targetOfDigMove targetOfMove' state move =
  let digMove = if isADigMove move then Just (shiftDigToMoveRange move) else Nothing
  in digMove >>= ((flip targetOfMove') state)

isValidDigMove :: (Move -> State -> Maybe Coord) -> State -> Move -> Bool
isValidDigMove targetOfMove' state move =
  (fmap (mapAtCoord state) $ targetOfDigMove targetOfMove' state move) == Just DIRT

hits :: (AList -> [Int]) -> (State -> Maybe Coord) -> State -> Maybe [Move]
hits wormData wormsCoord state = do
  hitCoords'   <- hitCoords wormData wormsCoord state
  wormPosition <- wormsCoord state
  return $ map (directionFrom wormPosition) hitCoords'

theseHits :: State -> Maybe [Move]
theseHits = hits aListOpponentData thisWormsCoord

thoseHits :: State -> Maybe [Move]
thoseHits = hits aListMyData thatWormsCoord

weaponRange :: Int
weaponRange = 4

directionFrom :: Coord -> Coord -> Move
directionFrom xy' xy'' =
  let (x', y')   = fromCoord xy'
      (x'', y'') = fromCoord xy''
  in case (y' == y'', x' == x'', x' > x'', y' > y'') of
       (True,  False, True,  False) -> Move 6
       (True,  False, False, False) -> Move 2
       (False, True,  False, True)  -> Move 0
       (False, True,  False, False) -> Move 4
       (False, False, True,  True)  -> Move 7
       (False, False, False, True)  -> Move 1
       (False, False, True,  False) -> Move 5
       (False, False, False, False) -> Move 3
       -- TODO
       _                            -> error $ "Implement! " ++ show (y' == y'', x' == x'', x' > x'', y' > y'')

aligns :: Coord -> Coord -> Bool
aligns xy' xy'' =
  let (x', y')   = fromCoord xy'
      (x'', y'') = fromCoord xy''
  in x' == x'' ||
     y' == y'' ||
     (abs (x' - x'')) == (abs (y' -y''))

inRange :: Coord -> Coord -> Int -> Bool
inRange xy' xy'' range' =
  let (x', y')   = fromCoord xy'
      (x'', y'') = fromCoord xy''
      dx         = (fromIntegral (x' - x''))
      dy         = (fromIntegral (y' - y''))
  in sqrt (((dx::Double) ** 2) + (dy ** 2)) <= (fromIntegral range')
