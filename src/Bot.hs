{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Bot
  where

import Import
import Lava

import qualified RIO.Vector.Boxed as V
import qualified RIO.Vector.Boxed.Partial as PV
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
import Control.Exception as E
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Control.Concurrent.STM.TMVar as TMVar
import qualified Control.Monad.STM as STM
import Control.Concurrent
import Control.DeepSeq

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

instance NFData State where
  rnf (State opponentsLastCommand'
             currentRound'
             wormHealths'
             wormPositions'
             wormBananas'
             wormSnowballs'
             frozenDurations'
             myPlayer'
             opponent'
             gameMap') =
    opponentsLastCommand' `deepseq`
    currentRound' `deepseq`
    wormHealths' `deepseq`
    wormPositions' `deepseq`
    wormBananas' `deepseq`
    wormSnowballs' `deepseq`
    frozenDurations' `deepseq`
    myPlayer' `deepseq`
    opponent' `deepseq`
    gameMap' `deepseq`
    ()

type WormFrozenDurations = AList

type WormSnowballs = AList

type WormHealths = AList

type WormPositions = AList

type WormBananas = AList

type ModifyFacts = AList -> AList

type Bananas = Int

type Snowballs = Int

type WormHealth = Int

data AList = AList Int Int Int Int Int Int
  deriving (Eq)

instance NFData AList where
  rnf (AList a b c d e f) =
    a `deepseq` b `deepseq` c `deepseq` d `deepseq` e `deepseq` f `deepseq` ()

aListReadableShow :: AList -> String
aListReadableShow (AList a b c d e f) =
  "(AList (" ++
  show a ++ ") (" ++
  show b ++ ") (" ++
  show c ++ ") (" ++
  show d ++ ") (" ++
  show e ++ ") (" ++
  show f ++ "))"

instance Show AList where
  show aList =
    let xs = aListToList aList
    in
    "[\n" ++
    (foldr (++) "" $ map (\ (wormId', value') -> "    " ++ show wormId' ++ " -> " ++ show value' ++ ",\n") xs) ++
    "]"

data WormId = WormId Int
  deriving (Eq, Show, Ord)

instance NFData WormId where
  rnf (WormId wormId') = wormId' `deepseq` ()

aListFindById :: WormId -> AList -> Int
aListFindById (WormId 1)  (AList (-1)    _    _     _    _     _) = error $ "aListFindById doesn't contain id:" ++ show (1::Int)
aListFindById (WormId 1)  (AList  x      _    _     _    _     _) = x
aListFindById (WormId 2)  (AList  _   (-1)    _     _    _     _) = error $ "aListFindById doesn't contain id:" ++ show (2::Int)
aListFindById (WormId 2)  (AList  _      x    _     _    _     _) = x
aListFindById (WormId 3)  (AList  _      _ (-1)     _    _     _) = error $ "aListFindById doesn't contain id:" ++ show (3::Int)
aListFindById (WormId 3)  (AList  _      _    x     _    _     _) = x
aListFindById (WormId 4)  (AList  _      _    _  (-1)    _     _) = error $ "aListFindById doesn't contain id:" ++ show (4::Int)
aListFindById (WormId 4)  (AList  _      _    _     x    _     _) = x
aListFindById (WormId 8)  (AList  _      _    _     _ (-1)     _) = error $ "aListFindById doesn't contain id:" ++ show (8::Int)
aListFindById (WormId 8)  (AList  _      _    _     _    x     _) = x
aListFindById (WormId 12) (AList  _      _    _     _    _  (-1)) = error $ "aListFindById doesn't contain id:" ++ show (12::Int)
aListFindById (WormId 12) (AList  _      _    _     _    _     x) = x
aListFindById wormId'     _                                       = error $ "aListFindById: " ++ show wormId'

aListContainsId :: WormId -> AList -> Bool
aListContainsId (WormId 1)  (AList (-1)    _    _     _    _     _) = False
aListContainsId (WormId 1)  _                                       = True
aListContainsId (WormId 2)  (AList  _   (-1)    _     _    _     _) = False
aListContainsId (WormId 2)  _                                       = True
aListContainsId (WormId 3)  (AList  _      _ (-1)     _    _     _) = False
aListContainsId (WormId 3)  _                                       = True
aListContainsId (WormId 4)  (AList  _      _    _  (-1)    _     _) = False
aListContainsId (WormId 4)  _                                       = True
aListContainsId (WormId 8)  (AList  _      _    _     _ (-1)     _) = False
aListContainsId (WormId 8)  _                                       = True
aListContainsId (WormId 12) (AList  _      _    _     _    _  (-1)) = False
aListContainsId (WormId 12) _                                       = True
aListContainsId wormId'     _                                       = error $ "aListContainsId: " ++ show wormId'

aListFindDataById :: WormId -> AList -> Int
aListFindDataById = aListFindById

aListFindDataByData :: Int -> AList -> Int
aListFindDataByData x (AList a b c d e f) =
  case (x == a, x == b, x == c, x == d, x == e, x == f) of
    (True,    _,    _,    _,    _,    _) -> x
    (   _, True,    _,    _,    _,    _) -> x
    (   _,    _, True,    _,    _,    _) -> x
    (   _,    _,    _, True,    _,    _) -> x
    (   _,    _,    _,    _, True,    _) -> x
    (   _,    _,    _,    _,    _, True) -> x
    _                                    -> error $ "aListFindDataByData doesn't contain data by value: " ++ show x

aListContainsData :: Int -> AList -> Bool
aListContainsData x (AList a b c d e f) =
  x == a || x == b || x == c || x == d || x == e || x == f

aListFindIdByData :: Int -> AList -> WormId
aListFindIdByData x (AList a b c d e f) =
  case (x == a, x == b, x == c, x == d, x == e, x == f) of
    (True,    _,    _,    _,    _,    _) -> WormId 1
    (   _, True,    _,    _,    _,    _) -> WormId 2
    (   _,    _, True,    _,    _,    _) -> WormId 3
    (   _,    _,    _, True,    _,    _) -> WormId 4
    (   _,    _,    _,    _, True,    _) -> WormId 8
    (   _,    _,    _,    _,    _, True) -> WormId 12
    _                                    -> error $ "aListFindIdByData doesn't contain data: " ++ show x

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
  (if a /= -1 then 1 else 0) +
  (if b /= -1 then 1 else 0) +
  (if c /= -1 then 1 else 0)

aListCountOpponentsEntries :: AList -> Int
aListCountOpponentsEntries (AList _ _ _ d e f) =
  (if d /= -1 then 1 else 0) +
  (if e /= -1 then 1 else 0) +
  (if f /= -1 then 1 else 0)

aListSumMyEntries :: AList -> Int
aListSumMyEntries (AList a b c _ _ _) =
  (if a /= -1 then a else 0) +
  (if b /= -1 then b else 0) +
  (if c /= -1 then c else 0)

aListSumOpponentsEntries :: AList -> Int
aListSumOpponentsEntries (AList _ _ _ d e f) =
  (if d /= -1 then d else 0) +
  (if e /= -1 then e else 0) +
  (if f /= -1 then f else 0)

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

anyWormData :: (Int -> Bool) -> AList -> Bool
anyWormData p (AList a b c d e f) =
  p a ||
  p d ||
  p b ||
  p e ||
  p c ||
  p f

aListAnyOpponentData :: (Int -> Bool) -> AList -> Bool
aListAnyOpponentData p (AList _ _ _ d e f) =
  p d ||
  p e ||
  p f

aListAnyOfMyData :: (Int -> Bool) -> AList -> Bool
aListAnyOfMyData p (AList a b c _ _ _) =
  p a ||
  p b ||
  p c

aListIsEmpty :: AList -> Bool
aListIsEmpty (AList (-1) (-1) (-1) (-1) (-1) (-1)) = True
aListIsEmpty _                                     = False

aListKeepWormsDefinedInFirst :: AList -> AList -> AList
aListKeepWormsDefinedInFirst (AList a b c d e f) (AList a' b' c' d' e' f') =
  (AList (if a /= (-1) then a' else (-1))
         (if b /= (-1) then b' else (-1))
         (if c /= (-1) then c' else (-1))
         (if d /= (-1) then d' else (-1))
         (if e /= (-1) then e' else (-1))
         (if f /= (-1) then f' else (-1)))

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

aListMap :: (Int -> Int) -> AList -> AList
aListMap f' (AList a b c d e f) = AList (f' a) (f' b) (f' c) (f' d) (f' e) (f' f)

-- TODO Test
aListRemoveWormById :: WormId -> AList -> AList
aListRemoveWormById (WormId 1)  (AList _ b c d e f) = AList (-1)    b    c    d    e    f
aListRemoveWormById (WormId 2)  (AList a _ c d e f) = AList    a (-1)    c    d    e    f
aListRemoveWormById (WormId 3)  (AList a b _ d e f) = AList    a    b (-1)    d    e    f
aListRemoveWormById (WormId 4)  (AList a b c _ e f) = AList    a    b    c (-1)    e    f
aListRemoveWormById (WormId 8)  (AList a b c d _ f) = AList    a    b    c    d (-1)    f
aListRemoveWormById (WormId 12) (AList a b c d e _) = AList    a    b    c    d    e (-1)
aListRemoveWormById wormId'     _                   = error $ "Can't remove worm with id: " ++ show wormId'

-- Produce minus one if there are no two entries which are the same
--
-- NOTE: One's own worms are never compared with each other because
-- they can't collide with eachother.
aListContainsDuplicatedOpponentEntry :: AList -> Bool
aListContainsDuplicatedOpponentEntry (AList a b c d e f)
  | a /= -1 && a == d = True
  | a /= -1 && a == e = True
  | a /= -1 && a == f = True
  | b /= -1 && b == d = True
  | b /= -1 && b == e = True
  | b /= -1 && b == f = True
  | c /= -1 && c == d = True
  | c /= -1 && c == e = True
  | c /= -1 && c == f = True
  | otherwise         = False

showPositions :: AList -> String
showPositions aList =
    let xs = aListToList aList
    in
    "[\n" ++
    (foldr (++) "" $ map (\ (wormId', coord') -> "    " ++ show wormId' ++ " -> " ++ showCoord coord' ++ ",\n") xs) ++
    "]"

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
    "  wormFrozenDurations  = " ++ show wormFrozenDurations'    ++ "\n" ++
    "  myPlayer             = " ++ show myPlayer'               ++ "\n" ++
    "  opponent             = " ++ show opponent'               ++ "\n" ++
    "  gameMap:\n" ++
    show gameMap' ++
    "}"

data GameMap = GameMap Integer Integer Integer Integer
  deriving (Generic, Eq)

instance NFData GameMap where
  rnf (GameMap air dirt space medipacks) =
    air `deepseq` dirt `deepseq` space `deepseq` medipacks `deepseq` ()

instance Show GameMap where
  show = showRows . splitGameMap

readableShow :: State -> String
readableShow (State opponentsLastCommand'
                    currentRound'
                    wormHealths'
                    wormPositions'
                    wormBananas'
                    wormSnowballs'
                    frozenDurations'
                    myPlayer'
                    opponent'
                    gameMap') =
  "(State\n" ++
  " (" ++ show opponentsLastCommand' ++ ")\n" ++
  " " ++ show currentRound' ++ "\n" ++
  " " ++ aListReadableShow wormHealths' ++ "\n" ++
  " " ++ aListReadableShow wormPositions' ++ "\n" ++
  " " ++ aListReadableShow wormBananas' ++ "\n" ++
  " " ++ aListReadableShow wormSnowballs' ++ "\n" ++
  " " ++ aListReadableShow frozenDurations' ++ "\n" ++
  " (" ++ show myPlayer' ++ ")\n" ++
  " (" ++ show opponent' ++ ")\n" ++
  " (" ++ rawMapString gameMap' ++ "))\n"



rawMapString :: GameMap -> String
rawMapString (GameMap air dirt space medipacks) =
  "GameMap\n\t" ++ show air ++ "\n\t" ++ show dirt ++ "\n\t" ++ show space ++ "\n\t" ++ show medipacks

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

mapAt :: Coord -> GameMap -> Cell
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

-- Deprecated
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

instance NFData Selections where
  rnf (Selections selections) = selections `deepseq` ()

-- TODO: Change Int to PlayerScore for stronger types
data Player = Player Int WormId Selections
  deriving (Show, Generic, Eq)

instance NFData Player where
  rnf (Player score' wormId' selections') =
    score' `deepseq` wormId' `deepseq` selections' `deepseq` ()

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

vectorToAList :: V.Vector (Int, Int) -> AList
vectorToAList = aListFromList . V.toList

vectorMaybesToAList :: V.Vector (Maybe (Int, Int)) -> AList
vectorMaybesToAList = aListFromList . catMaybes . V.toList

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
                                   fmap (\ (BananaBomb count') -> (wormId', count') )
                                   bananas')
                         liveWorms
      snowballs'       = vectorMaybesToAList $
                         V.map (\ (ScratchWorm { wormId    = wormId',
                                                 snowballs = snowballs'' }) ->
                                   fmap (\ (Snowball (count')) -> (wormId', count'))
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

-- ==========Move Encoding==========
-- Integer encoded move representation
-- 0   -> 7
-- 8   -> 15
-- 16  -> 23
-- 24  -> 104
-- 105 -> 185

-- Bit packing of move:
-- 00000000 0000 000000000000000000000
--    ^      ^
--    |      |
-- Moves Selects.  Mine then his because his worm id's are always left
-- shifted by 3
-- Range of moves: 0 -> 127

-- Process to extract:
-- 1. check for a select;
-- 2. mask out the select;
-- 3. shift the select;
-- 4. check the range of the remaining number;
-- 5. extract and shift according to the type of move;
data Move = Move !Int
  deriving (Show, Eq)

instance NFData Move where
  rnf (Move move) = move `deepseq` ()

showCoord :: Coord -> String
showCoord xy = case fromCoord xy of
    (x', y') -> show x' ++ " " ++ show y'

formatMove :: (State -> Coord) -> (Move -> ModifyState) -> Move -> Coord -> State -> String
-- Shoot
formatMove wormsCoord makeSelections' dir@(Move x) xy state
  -- Select: Calls back into this function without the select
  | hasASelection dir   = formatSelect wormsCoord makeSelections' dir state
  -- Shoot
  | isAShootMove  dir   = formatShootMove dir
  -- Move
  | isAMoveMove   dir   = "move " ++ (showCoord $ displaceCoordByMove xy dir)
  -- Dig
  | isADigMove    dir   = "dig "  ++ (showCoord $ displaceCoordByMove xy (Move (x - 8)))
  -- Throwing the bomb
  | isABananaMove dir   = moveFromMaybe $
                          fmap (\ newCoord -> "banana " ++ showCoord newCoord) $
                          displaceToBananaDestination dir xy
  -- Throwing a snowball
  | isASnowballMove dir = moveFromMaybe $
                          fmap (\ newCoord -> "snowball " ++ showCoord newCoord) $
                          displaceToBananaDestination (snowballMoveToBananaRange dir) xy
-- Nothing
formatMove _ _ _ _ _ = "nothing"

snowballMoveToBananaRange :: Move -> Move
snowballMoveToBananaRange (Move x) = (Move $ x - 81)

isASnowballMove :: Move -> Bool
isASnowballMove (Move x') =
  x' >= 105 && x' < 186

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
  zipWith (\ dx dy ->
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
selectEncodingRange = 8

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

formatSelect :: (State -> Coord) -> (Move -> ModifyState) -> Move -> State -> String
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
                (wormsCoord $ makeSelections' move state)
                state

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

displaceCoordByMove :: Coord -> Move -> Coord
displaceCoordByMove xy moveDir@(Move dir) =
  case dir of
    -- N
    8  -> xy - mapDim
    -- NE
    9  -> xy + 1 - mapDim
    -- E
    10 -> xy + 1
    -- SE
    11 -> xy + mapDim + 1
    -- S
    12 -> xy + mapDim
    -- SW
    13 -> xy + mapDim - 1
    -- W
    14 -> xy - 1
    -- NW
    15 -> xy - mapDim - 1
    -- Invalid Move
    _  -> error $ "Attempted to move in invalid direction with direction: " ++ show moveDir

moveWouldGoOOB :: Coord -> Move -> Bool
moveWouldGoOOB xy moveDir@(Move dir) =
  case dir of
    -- N
    8  -> isOnNorthernBorder xy
    -- NE
    9  -> isOnNorthernBorder xy || isOnEasternBorder xy
    -- E
    10 -> isOnEasternBorder xy
    -- SE
    11 -> isOnSouthernBorder xy || isOnEasternBorder xy
    -- S
    12 -> isOnSouthernBorder xy
    -- SW
    13 -> isOnSouthernBorder xy || isOnWesternBorder xy
    -- W
    14 -> isOnWesternBorder xy
    -- NW
    15 -> isOnNorthernBorder xy || isOnWesternBorder xy
    -- Invalid Move
    _  -> error $ "Attempted to check a boundary condition in invalid direction with direction: " ++ show moveDir

isOOB :: (Int, Int) -> Maybe (Int, Int)
isOOB (x', y')
  | x' >= 0 && x' < mapDim && y' >= 0 && y' < mapDim = Just (x', y')
  | otherwise                                        = Nothing

data CombinedMove = CombinedMove Int
  deriving (Eq, Show)

instance NFData CombinedMove where
  rnf (CombinedMove combinedMove) = combinedMove `deepseq` ()

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

data MoveType = MOVE
              | DIG
              | SHOOT
              | THROW_BANANA
              | THROW_SNOWBALL
              | SELECT
              | NOTHING
  deriving (Show, Eq)

decodeMoveType :: Move -> MoveType
decodeMoveType move
  | isAMoveMove     move = MOVE
  | isADigMove      move = DIG
  | isAShootMove    move = SHOOT
  | isABananaMove   move = THROW_BANANA
  | isASnowballMove move = THROW_SNOWBALL
  | hasASelection   move = SELECT
  | otherwise            = NOTHING

-- Move ordering:
--
-- 1. Movement
-- 2. Digging
-- 3. Banana
-- 4. Shooting
-- 5. Snowball
makeMove :: Bool -> CombinedMove -> ModifyState
makeMove _ moves state =
  let (myMove,  opponentsMove)  = freezeActions state $ toMoves moves
  in -- assertValidState state  myMove  opponentsMove  $

     -- Post movement actions
     incrementRound                                  $
     setOpponentsLastMove      state   opponentsMove $
     advanceWormSelections                           $
     cleanUpDeadWorms                                $

     -- Make moves
     go (decodeMoveType myMove) (decodeMoveType opponentsMove) myMove opponentsMove $

     -- Pre move actions
     tickFreezeDurations $
     dealLavaDamage state
  where
    go !myMoveType !opponentsMoveType !myMove !opponentsMove state' =
      makeMove' myMoveType opponentsMoveType state'
      where
        wormHealths'              = wormHealths state'
        wormPositions'            = wormPositions state'
        gameMap'                  = gameMap state'
        thisWormsCoord'           = thisWormsCoord state'
        thatWormsCoord'           = thatWormsCoord state'
        thisWormsId               = thisPlayersCurrentWormId state'
        thatWormsId               = thatPlayersCurrentWormId state'
        thisWormHasBananasLeft'   = thisWormHasBananasLeft state'
        thatWormHasBananasLeft'   = thisWormHasBananasLeft state'
        thisWormHasSnowballsLeft' = thisWormHasSnowballsLeft state'
        thatWormHasSnowballsLeft' = thatWormHasSnowballsLeft state'
        makeMove' SELECT          SELECT =
          let myMove'            = removeSelectionFromMove myMove
              opponentsMove'     = removeSelectionFromMove opponentsMove
              myMoveType'        = decodeMoveType myMove'
              opponentsMoveType' = decodeMoveType opponentsMove'
          in go myMoveType' opponentsMoveType' myMove' opponentsMove' .
             makeMySelection           myMove .
             makeOpponentsSelection    opponentsMove
        makeMove' _               SELECT =
          let opponentsMove'     = removeSelectionFromMove opponentsMove
              opponentsMoveType' = decodeMoveType opponentsMove'
          in go myMoveType opponentsMoveType' myMove opponentsMove' .
             makeOpponentsSelection opponentsMove
        makeMove' SELECT          _ =
          let myMove'     = removeSelectionFromMove myMove
              myMoveType' = decodeMoveType myMove'
          in go myMoveType' opponentsMoveType myMove' opponentsMove .
             makeMySelection           myMove
        makeMove' MOVE            MOVE =
          collideWorms wormHealths' wormPositions' gameMap' .
          makeMyMoveMove        wormPositions' thisWormsCoord' thisWormsId gameMap' myMove .
          makeOpponentsMoveMove wormPositions' thatWormsCoord' thatWormsId gameMap' opponentsMove
        makeMove' MOVE            DIG =
          makeOpponentsDigMove thatWormsCoord' gameMap' opponentsMove .
          makeMyMoveMove       wormPositions' thisWormsCoord' thisWormsId gameMap' myMove
        makeMove' MOVE            NOTHING =
          makeMyMoveMove wormPositions' thisWormsCoord' thisWormsId gameMap' myMove
        makeMove' MOVE            _ =
          go NOTHING opponentsMoveType doNothing opponentsMove .
          makeMyMoveMove wormPositions' thisWormsCoord' thisWormsId gameMap' myMove
        makeMove' DIG             DIG =
          makeMyDigMove        thisWormsCoord' gameMap' myMove .
          makeOpponentsDigMove thatWormsCoord' gameMap' opponentsMove
        makeMove' DIG             THROW_SNOWBALL =
          makeOpponentsSnowballMove thatWormsId
                                    thatWormsCoord'
                                    thatWormHasSnowballsLeft'
                                    gameMap'
                                    wormPositions'
                                    opponentsMove .
          makeMyDigMove thisWormsCoord' gameMap' myMove
        makeMove' DIG             NOTHING =
          makeMyDigMove thisWormsCoord' gameMap' myMove
        makeMove' DIG             MOVE =
          makeMyDigMove thisWormsCoord' gameMap' myMove .
          makeOpponentsMoveMove wormPositions' thatWormsCoord' thatWormsId gameMap' opponentsMove
        makeMove' DIG             _ =
          go NOTHING opponentsMoveType doNothing opponentsMove .
          makeMyDigMove thisWormsCoord' gameMap' myMove
        makeMove' SHOOT           SHOOT =
          makeMyShootMove thisWormsCoord'
                          thisWormsId
                          gameMap'
                          wormPositions'
                          myMove .
          makeOpponentsShootMove thatWormsCoord'
                                 thatWormsId
                                 gameMap'
                                 wormPositions'
                                 opponentsMove
        makeMove' SHOOT           THROW_BANANA =
          makeMyShootMove thisWormsCoord'
                          thisWormsId
                          gameMap'
                          wormPositions'
                          myMove .
          makeOpponentsBananaMove thatWormsId
                                  thatWormsCoord'
                                  thatWormHasBananasLeft'
                                  gameMap'
                                  wormPositions'
                                  opponentsMove
        makeMove' SHOOT           THROW_SNOWBALL =
          makeOpponentsSnowballMove thatWormsId
                                    thatWormsCoord'
                                    thatWormHasSnowballsLeft'
                                    gameMap'
                                    wormPositions'
                                    opponentsMove .
          makeMyShootMove thisWormsCoord'
                          thisWormsId
                          gameMap'
                          wormPositions'
                          myMove
        makeMove' SHOOT           NOTHING =
          makeMyShootMove thisWormsCoord'
                          thisWormsId
                          gameMap'
                          wormPositions'
                          myMove
        makeMove' THROW_BANANA    THROW_BANANA =
          makeMyBananaMove thisWormsId
                           thisWormsCoord'
                           thisWormHasBananasLeft'
                           gameMap'
                           wormPositions'
                           myMove .
          makeOpponentsBananaMove thatWormsId
                                  thatWormsCoord'
                                  thatWormHasBananasLeft'
                                  gameMap'
                                  wormPositions'
                                  opponentsMove
        makeMove' THROW_BANANA    THROW_SNOWBALL =
          makeOpponentsSnowballMove thatWormsId
                                    thatWormsCoord'
                                    thatWormHasSnowballsLeft'
                                    gameMap'
                                    wormPositions'
                                    opponentsMove .
          makeMyBananaMove thisWormsId
                           thisWormsCoord'
                           thisWormHasBananasLeft'
                           gameMap'
                           wormPositions'
                           myMove
        makeMove' THROW_BANANA    NOTHING =
          makeMyBananaMove thisWormsId
                           thisWormsCoord'
                           thisWormHasBananasLeft'
                           gameMap'
                           wormPositions'
                           myMove
        makeMove' THROW_SNOWBALL  THROW_SNOWBALL =
          makeMySnowballMove thisWormsId
                             thisWormsCoord'
                             thisWormHasSnowballsLeft'
                             gameMap'
                             wormPositions'
                             myMove .
          makeOpponentsSnowballMove thatWormsId
                                    thatWormsCoord'
                                    thatWormHasSnowballsLeft'
                                    gameMap'
                                    wormPositions'
                                    opponentsMove
        makeMove' THROW_SNOWBALL  NOTHING =
          makeMySnowballMove thisWormsId
                             thisWormsCoord'
                             thisWormHasSnowballsLeft'
                             gameMap'
                             wormPositions'
                             myMove
        makeMove' NOTHING         MOVE =
          makeOpponentsMoveMove wormPositions' thatWormsCoord' thatWormsId gameMap' opponentsMove
        makeMove' _           MOVE =
          go myMoveType NOTHING myMove doNothing .
          makeOpponentsMoveMove wormPositions' thatWormsCoord' thatWormsId gameMap' opponentsMove
        makeMove' THROW_SNOWBALL  DIG =
          makeMySnowballMove thisWormsId
                             thisWormsCoord'
                             thisWormHasSnowballsLeft'
                             gameMap'
                             wormPositions'
                             myMove .
          makeOpponentsDigMove thatWormsCoord' gameMap' opponentsMove
        makeMove' NOTHING         DIG =
          makeOpponentsDigMove thatWormsCoord' gameMap' opponentsMove
        makeMove' _           DIG =
          go myMoveType NOTHING myMove doNothing .
          makeOpponentsDigMove thatWormsCoord' gameMap' opponentsMove
        makeMove' THROW_BANANA    SHOOT =
          makeOpponentsShootMove thatWormsCoord'
                                 thatWormsId
                                 gameMap'
                                 wormPositions'
                                 opponentsMove .
          makeMyBananaMove thisWormsId
                           thisWormsCoord'
                           thisWormHasBananasLeft'
                           gameMap'
                           wormPositions'
                           myMove
        makeMove' THROW_SNOWBALL  SHOOT =
          makeMySnowballMove thisWormsId
                             thisWormsCoord'
                             thisWormHasSnowballsLeft'
                             gameMap'
                             wormPositions'
                             myMove .
          makeOpponentsShootMove thatWormsCoord'
                                 thatWormsId
                                 gameMap'
                                 wormPositions'
                                 opponentsMove
        makeMove' NOTHING         SHOOT =
          makeOpponentsShootMove thatWormsCoord'
                                 thatWormsId
                                 gameMap'
                                 wormPositions'
                                 opponentsMove
        makeMove' THROW_SNOWBALL  THROW_BANANA =
          makeMySnowballMove thisWormsId
                             thisWormsCoord'
                             thisWormHasSnowballsLeft'
                             gameMap'
                             wormPositions'
                             myMove .
          makeOpponentsBananaMove thatWormsId
                                  thatWormsCoord'
                                  thatWormHasBananasLeft'
                                  gameMap'
                                  wormPositions'
                                  opponentsMove
        makeMove' NOTHING         THROW_BANANA =
          makeOpponentsBananaMove thatWormsId
                                  thatWormsCoord'
                                  thatWormHasBananasLeft'
                                  gameMap'
                                  wormPositions'
                                  opponentsMove
        makeMove' NOTHING         THROW_SNOWBALL =
          makeOpponentsSnowballMove thatWormsId
                                    thatWormsCoord'
                                    thatWormHasSnowballsLeft'
                                    gameMap'
                                    wormPositions'
                                    opponentsMove
        makeMove' NOTHING         NOTHING = id

cleanUpDeadWorms :: ModifyState
cleanUpDeadWorms state =
  go (wormHealths state) state
  where go wormHealths' state'
          | aListContainsData 0 wormHealths' =
            let state'' = cleanUpDeadWorm (aListFindIdByData 0 wormHealths') state'
            in go (wormHealths state'') state''
          | otherwise                        = state'

-- We never swap here because I'm not sure how worth while it is to
-- simulate that.
collideWorms :: WormHealths -> WormPositions -> GameMap-> ModifyState
collideWorms !originalWormHealths !originalWormPositions !originalGameMap state =
  if aListContainsDuplicatedOpponentEntry (wormPositions state)
  then knockBackDamage $
       withWormPositions (always originalWormPositions) $
       withWormHealths (always originalWormHealths) $
       -- This one is valid because we can't also have had the dirt on
       -- the map destroyed and lava doesn't form part of the map.
       state { gameMap = originalGameMap }
  else state

isOnLavaForRound :: Int -> Coord -> Bool
isOnLavaForRound currentRound' coord' =
  testBit (lava PV.! currentRound') coord'

lavaDamage :: Int
lavaDamage = 3

-- TODO: come up with a clever way of not checking for lava on every
-- worm every time.
dealLavaDamage :: ModifyState
dealLavaDamage state =
  let currentRound' = currentRound state
      hits'         = aListFilterByData (isOnLavaForRound currentRound') $ wormPositions state
  in if not $ aListIsEmpty hits'
     then foldl' (\ state' (wormId', _) ->
                    let wormHealth' = aListFindDataById wormId' $ wormHealths state'
                        cleanUp     = if wormHealth' <= lavaDamage
                                      then flagWormForCleaning wormId'
                                      else id
                    in cleanUp $ withWormHealths (harmWormById lavaDamage wormId') state') state $
          aListToList hits'
     else state

-- TODO: You can still select whne you're frozen...
freezeActions :: State -> (Move, Move) -> (Move, Move)
freezeActions state (myMove, opponentsMove) =
  let frozenDurations' = frozenDurations state
      freezeThisWorm   = aListContainsId (thisPlayersCurrentWormId state) frozenDurations'
      freezeThatWorm   = aListContainsId (thatPlayersCurrentWormId state) frozenDurations'
  in case (freezeThisWorm, freezeThatWorm) of
    (True,  True)  -> (doNothing, doNothing)
    (True,  False) -> (doNothing, opponentsMove)
    (False, True)  -> (myMove,    doNothing)
    (False, False) -> (myMove,    opponentsMove)

tickFreezeDurations :: ModifyState
tickFreezeDurations = withFrozenDurations (aListMap (\ x -> if x > 0 then x - 1 else x))

withFrozenDurations :: WithWormFacts
withFrozenDurations f state@(State { frozenDurations = frozenDurations' }) =
  state { frozenDurations = f frozenDurations' }

incrementRound :: ModifyState
incrementRound state =
  state { currentRound = currentRound state + 1 }

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
                 "To:\n" ++ show state ++ "\n" ++
                 "Readable input state:\n" ++ readableShow previousState)
     else state

-- TODO I shouldn't even be doing this at all.
setOpponentsLastMove :: State -> Move -> State -> State
setOpponentsLastMove stateWhenMoveWasMade move' state =
  state { opponentsLastCommand =
          Just $ if move' == doNothing
                 then "nothing \"Player chose to do nothing\""
                 else prettyPrintThatMove stateWhenMoveWasMade move' }

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

playerSelections :: Player -> Int
playerSelections (Player _ _ (Selections x)) = x

thisPlayerHasSelectionsLeft :: State -> Bool
thisPlayerHasSelectionsLeft = hasSelectionsLeft . myPlayer

thatPlayerHasSelectionsLeft :: State -> Bool
thatPlayerHasSelectionsLeft = hasSelectionsLeft . opponent

hasASelection :: Move -> Bool
hasASelection (Move x) = x >= 256

makeMySelection :: Move -> ModifyState
makeMySelection =
  makeSelection thisPlayerHasSelectionsLeft
                decrementThisPlayersSelections
                mapThisPlayer

makeOpponentsSelection :: Move -> ModifyState
makeOpponentsSelection =
  makeSelection thatPlayerHasSelectionsLeft
                decrementThatPlayersSelections
                mapThatPlayer

makeSelection :: (State -> Bool) -> ModifyState -> (ModifyPlayer -> ModifyState) -> Move-> ModifyState
makeSelection playerHasSelectionsLeft' decrementPlayersSelections' mapPlayer !move' state =
  let selection      = if hasASelection move'
                       then Just (WormId $ decodeSelection move')
                       else Nothing
      validSelection = selection >>= (\ selection' -> if wormExists selection' state &&
                                                         playerHasSelectionsLeft' state
                                                      then Just selection'
                                                      else Nothing)
      isValid        = isJust validSelection
      wormId'        = fromJust validSelection
  in (if isValid
      then decrementPlayersSelections' . mapPlayer (withCurrentWormId wormId')
      else id) state

-- It's fine to use `findById' here because we don't care whether it's
-- an AListEntry or w/e.
wormExists :: WormId -> State -> Bool
wormExists wormId' = aListContainsId wormId' . wormPositions

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
  let wormId'  = wormsId state
      bananas' = wormBananas state
  in aListContainsId wormId' bananas' && (hasBananas $ aListFindDataById wormId' bananas')

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

makeMyBananaMove :: WormId -> Coord -> Bool -> GameMap -> WormPositions -> Move -> ModifyState
makeMyBananaMove =
  throwBanana decrementThisWormsBananas
              awardPointsToThisPlayerForDigging
              awardPointsToThisPlayerForDamage
              penaliseThisPlayerForDamage
              awardPointsToThisPlayerForMissing
              awardPointsToThisPlayerForKillingAnEnemy

makeOpponentsBananaMove :: WormId -> Coord -> Bool -> GameMap -> WormPositions -> Move -> ModifyState
makeOpponentsBananaMove =
  throwBanana decrementThatWormsBananas
              awardPointsToThatPlayerForDigging
              awardPointsToThatPlayerForDamage
              penaliseThatPlayerForDamage
              awardPointsToThatPlayerForMissing
              awardPointsToThatPlayerForKillingAnEnemy

throwBanana :: ModifyState -> ModifyState -> (Int -> ModifyState) -> (Int -> ModifyState) -> ModifyState -> ModifyState -> WormId -> Coord -> Bool -> GameMap -> WormPositions -> Move -> ModifyState
throwBanana decrementWormsBananas'
            awardPointsToPlayerForDigging'
            awardPointsToPlayerForDamage'
            penalisePlayerForDamage'
            awardPointsToPlayerForMissing'
            awardPointsToPlayerForKillingAnEnemy'
            !wormId'
            !coord'
            !hasBananasLeft
            !gameMapPriorToBlast
            !wormPositionsPriorToBlast
            !move' =
  let destinationBlock = displaceToBananaDestination move' coord'
      -- TODO: looks very similar to L:2286
      isValid          = hasBananasLeft && isJust destinationBlock
      target           = fromJust destinationBlock
  in if isValid
     then decrementWormsBananas' .
          bananaBlast wormId'
                      gameMapPriorToBlast
                      wormPositionsPriorToBlast
                      awardPointsToPlayerForDigging'
                      awardPointsToPlayerForDamage'
                      penalisePlayerForDamage'
                      awardPointsToPlayerForMissing'
                      awardPointsToPlayerForKillingAnEnemy'
                      target
     else id

makeMySnowballMove ::  WormId -> Coord -> Bool -> GameMap -> WormPositions -> Move -> ModifyState
makeMySnowballMove =
  throwSnowball decrementThisWormsSnowballs
                awardThisPlayerForFreezingAWorm
                penaliseThisPlayerForFreezingAWorm
                awardPointsToThisPlayerForMissing

makeOpponentsSnowballMove :: WormId -> Coord -> Bool -> GameMap -> WormPositions -> Move -> ModifyState
makeOpponentsSnowballMove =
  throwSnowball decrementThatWormsSnowballs
                awardThatPlayerForFreezingAWorm
                penaliseThatPlayerForFreezingAWorm
                awardPointsToThatPlayerForMissing

throwSnowball :: ModifyState -> ModifyState -> ModifyState -> ModifyState -> WormId -> Coord -> Bool -> GameMap -> WormPositions -> Move ->ModifyState
throwSnowball decrementWormsSnowballs'
              awardPlayerForFreezingAWorm'
              penalisePlayerForFreezingAWorm'
              awardPointsToPlayerForMissing'
              !wormId'
              !coord'
              !hasSnowballsLeft
              !gameMap'
              !wormPositions'
              !move' =
  let destinationBlock = displaceToBananaDestination (snowballMoveToBananaRange move') coord'
      -- TODO: looks very similar to L:2286
      isValid          = hasSnowballsLeft && isJust destinationBlock
      target           = fromJust destinationBlock
  in if isValid
     then decrementWormsSnowballs' .
          snowballBlast wormId'
                        gameMap'
                        wormPositions'
                        awardPlayerForFreezingAWorm'
                        penalisePlayerForFreezingAWorm'
                        awardPointsToPlayerForMissing'
                        target
     else id

snowballBlast :: WormId -> GameMap -> WormPositions -> ModifyState -> ModifyState -> ModifyState -> Coord -> ModifyState
snowballBlast wormId'
              gameMap'
              wormPositions'
              awardPlayerForFreezingAWorm'
              penalisePlayerForFreezingAWorm'
              awardPointsToPlayerForMissing'
              targetCoord
              state =
  -- TODO: removed this (wormHits == [])
  if deepSpaceAt targetCoord gameMap'
  then awardPointsToPlayerForMissing' state
  else foldOverSnowballBlastCoordsInRange
         targetCoord
         (\ !state' !nextWormCoord ->
            if containsAnyWorm nextWormCoord wormPositions'
            then freezeWorm wormId'
                            awardPlayerForFreezingAWorm'
                            penalisePlayerForFreezingAWorm'
                            nextWormCoord
                            state'
            else state')
         state

-- ASSUME: that the worm being frozen exists
freezeWorm :: WormId -> ModifyState -> ModifyState -> Coord -> ModifyState
freezeWorm throwingWormId'
           awardPlayerForFreezingAWorm'
           penalisePlayerForFreezingAWorm'
           targetCoord
           state' =
  let wormId'       = aListFindIdByData targetCoord $
                      wormPositions state'
      samePlayer    = wormsBelongToSamePlayer wormId' throwingWormId'
      dishOutPoints = if samePlayer then penalisePlayerForFreezingAWorm' else awardPlayerForFreezingAWorm'
      freezeWorm'   = withWormFrozenDurations $ freezeWormById wormId'
  in freezeWorm' $ dishOutPoints state'

freezeWormById :: WormId -> WormFrozenDurations -> WormFrozenDurations
freezeWormById wormId' = aListMapWormById wormId' (always 5)

withWormFrozenDurations :: WithWormFacts
withWormFrozenDurations f state@(State { frozenDurations = frozenDurations' }) =
  state { frozenDurations = f frozenDurations' }

foldOverSnowballBlastCoordsInRange :: (Show b) => Coord -> (b -> Coord -> b) -> b -> b
foldOverSnowballBlastCoordsInRange epicentre f accumulator =
  go (0::Int) accumulator
  where
    go 0 !acc = let acc' = f acc epicentre
                in if isOnWesternBorder epicentre
                   then if isOnNorthernBorder epicentre
                        then go 5 acc'
                        else go 3 acc'
                   else go 1 acc'
    go 1 !acc = let coord' = epicentre - 1
                    acc'   = f acc coord'
                in if isOnNorthernBorder coord'
                      then if isOnEasternBorder epicentre
                           then go 7 acc'
                           else go 5 acc'
                   else go 2 acc'
    go 2 !acc = let coord' = epicentre - mapDim - 1
                    acc'   = f acc coord'
                in go 3 acc'
    go 3 !acc = let coord' = epicentre - mapDim
                    acc'   = f acc coord'
                in if isOnEasternBorder coord'
                   then if isOnSouthernBorder epicentre
                        then acc'
                        else go 7 acc'
                   else go 4 acc'
    go 4 !acc = let coord' = epicentre - mapDim + 1
                    acc'   = f acc coord'
                in go 5 acc'
    go 5 !acc = let coord' = epicentre + 1
                    acc'   = f acc coord'
                in if isOnSouthernBorder coord'
                   then acc'
                   else go 6 acc'
    go 6 !acc = let coord' = epicentre + mapDim + 1
                    acc'   = f acc coord'
                in go 7 acc'
    go 7 !acc = let coord' = epicentre + mapDim
                    acc'   = f acc coord'
                in if isOnWesternBorder coord'
                   then acc'
                   else go 8 acc'
    go 8 !acc = let coord' = epicentre + mapDim - 1
                    acc'   = f acc coord'
                in acc'
    go x acc  = error $ "folding over snowball hits ran into " ++ show x ++ ", with accumulator at " ++ show acc

-- Pattern for iteration:
-- [  2,   3,  4,
--    1,   0,  5,
--    8,   7,  6]

awardPointsToThisPlayerForDamage :: Int -> ModifyState
awardPointsToThisPlayerForDamage damage' = mapThisPlayer (awardPointsForDamage damage')

awardPointsToThatPlayerForDamage :: Int -> ModifyState
awardPointsToThatPlayerForDamage damage' = mapThatPlayer (awardPointsForDamage damage')

freezeScore :: Int
freezeScore = 17

awardPointsForFreezing :: ModifyPlayer
awardPointsForFreezing = modifyScore freezeScore

penaliseForFreezing :: ModifyPlayer
penaliseForFreezing = modifyScore (-freezeScore)

awardThisPlayerForFreezingAWorm :: ModifyState
awardThisPlayerForFreezingAWorm = mapThisPlayer awardPointsForFreezing

penaliseThisPlayerForFreezingAWorm :: ModifyState
penaliseThisPlayerForFreezingAWorm = mapThisPlayer penaliseForFreezing

awardThatPlayerForFreezingAWorm :: ModifyState
awardThatPlayerForFreezingAWorm = mapThatPlayer awardPointsForFreezing

penaliseThatPlayerForFreezingAWorm :: ModifyState
penaliseThatPlayerForFreezingAWorm = mapThatPlayer penaliseForFreezing

hasSnowballs :: Snowballs -> Bool
hasSnowballs x = x > 0

thisWormHasSnowballsLeft :: State -> Bool
thisWormHasSnowballsLeft = wormHasSnowballsLeft thisPlayersCurrentWormId

thatWormHasSnowballsLeft :: State -> Bool
thatWormHasSnowballsLeft = wormHasSnowballsLeft thatPlayersCurrentWormId

wormHasSnowballsLeft :: (State -> WormId) -> State -> Bool
wormHasSnowballsLeft wormsId state =
  let wormId'    = wormsId state
      snowballs' = wormSnowballs state
  in aListContainsId wormId' snowballs' && (hasSnowballs $ aListFindDataById wormId' snowballs')

decrementSnowballs :: Snowballs -> Snowballs
decrementSnowballs (-1) = (-1)
decrementSnowballs 1    = (-1)
decrementSnowballs x    = x - 1

decrementWormsSnowballs :: (State -> WormId) -> ModifyState
decrementWormsSnowballs wormsId state =
  let wormId' = wormsId state
  in withWormSnowballs (aListMapWormById wormId' decrementSnowballs) state

decrementThisWormsSnowballs :: ModifyState
decrementThisWormsSnowballs = decrementWormsSnowballs thisPlayersCurrentWormId

decrementThatWormsSnowballs :: ModifyState
decrementThatWormsSnowballs = decrementWormsSnowballs thatPlayersCurrentWormId

withWormSnowballs :: WithWormFacts
withWormSnowballs f state@(State { wormSnowballs = wormSnowballs' }) =
  state { wormSnowballs = f wormSnowballs' }

penaliseThisPlayerForDamage :: Int -> ModifyState
penaliseThisPlayerForDamage  damage' = mapThisPlayer (awardPointsForDamage (-damage'))

penaliseThatPlayerForDamage :: Int -> ModifyState
penaliseThatPlayerForDamage  damage' = mapThatPlayer (awardPointsForDamage (-damage'))

bananaDamageAt :: Coord -> Coord -> Int
bananaDamageAt centre coord' =
  let (xCentre, yCentre) = fromCoord centre
      (xCoord,  yCoord)  = fromCoord coord'
  in switchDamage (xCentre - xCoord) (yCentre - yCoord)
     where
       -- Going in the order of the blastCoordDeltasInRange template
       switchDamage 0    (-2) = 7
       switchDamage (-1) (-1) = 11
       switchDamage 0    (-1) = 13
       switchDamage 1    (-1) = 11
       switchDamage (-2) 0    = 7
       switchDamage (-1) 0    = 13
       switchDamage 0    0    = 20
       switchDamage 1    0    = 13
       switchDamage 2    0    = 7
       switchDamage (-1) 1    = 11
       switchDamage 0    1    = 13
       switchDamage 1    1    = 11
       switchDamage 0    2    = 7
       switchDamage   _  _    = 0

-- Damage Pattern (as per damage template)
-- [     7,
--   11,13,11,
-- 7,13,20,13,7,
--   11,13,11,
--       7]

-- Hint: Your function should close over the GameMap and WormPositions
foldOverBlastCoordsInRange :: (Show b) => Coord -> (b -> Coord -> b) -> b -> b
foldOverBlastCoordsInRange epicentre f accumulator =
  go (0::Int) accumulator
  where
    go 0  !acc = let acc' = f acc epicentre
                 in if isOnWesternBorder epicentre
                    then if isOnNorthernBorder epicentre
                         then go 7 acc'
                         else go 4 acc'
                    else go 1 acc'
    go 1  !acc = let coord' = (epicentre - 1)
                     acc'   = f acc coord'
                 in if isOnWesternBorder coord'
                    then if isOnNorthernBorder coord'
                         then go 7 acc'
                         else go 3 acc'
                    else go 2 acc'
    go 2  !acc = let coord' = (epicentre - 2)
                     acc'   = f acc coord'
                 in if isOnNorthernBorder coord'
                    then if isOnEasternBorder epicentre
                         then go 10 acc'
                         else go 7 acc'
                    else go 3 acc'
    go 3  !acc = let coord' = (epicentre - 1 - mapDim)
                     acc'   = f acc coord'
                 in go 4 acc'
    go 4  !acc = let coord' = (epicentre - mapDim)
                     acc'   = f acc coord'
                 in if isOnNorthernBorder coord'
                    then if isOnEasternBorder coord'
                         then go 10 acc'
                         else go 6 acc'
                    else go 5 acc'
    go 5  !acc = let coord' = (epicentre - 2 * mapDim)
                     acc'   = f acc coord'
                 in if isOnEasternBorder coord'
                    then if isOnSouthernBorder epicentre
                         then acc'
                         else go 10 acc'
                    else go 6 acc'
    go 6  !acc = let coord' = epicentre - mapDim + 1
                     acc'   = f acc coord'
                 in go 7 acc'
    go 7  !acc = let coord' = epicentre + 1
                     acc'   = f acc coord'
                 in if isOnEasternBorder coord'
                       then if isOnSouthernBorder coord'
                            then acc'
                            else go 9 acc'
                    else go 8 acc'
    go 8  !acc = let coord' = epicentre + 2
                     acc'   = f acc coord'
                 in if isOnSouthernBorder coord'
                    then acc'
                    else go 9 acc'
    go 9  !acc = let coord' = epicentre + mapDim + 1
                     acc'   = f acc coord'
                 in go 10 acc'
    go 10 !acc = let coord' = epicentre + mapDim
                     acc'   = f acc coord'
                 in if isOnSouthernBorder coord'
                    then if isOnWesternBorder coord'
                         then acc
                         else go 12 acc'
                    else go 11 acc'
    go 11 !acc = let coord' = epicentre + 2 * mapDim
                     acc'   = f acc coord'
                 in if isOnWesternBorder coord'
                    then acc'
                    else go 12 acc'
    go 12 !acc = let coord' = epicentre + mapDim - 1
                     acc'   = f acc coord'
                 in acc'
    go i  !acc = error $ "go: " ++ show i ++ " with accumulator: " ++ show acc

-- Pattern for iteration:
-- [         5,
--      3,   4,  6,
--  2,  1,   0,  7, 8,
--     12,  10,  9,
--          11]

containsAnyWorm :: Coord -> WormPositions -> Bool
containsAnyWorm coord' = anyWormData (== coord')

bananaBlast :: WormId -> GameMap -> WormPositions -> ModifyState -> (Int -> ModifyState) -> (Int -> ModifyState) -> ModifyState -> ModifyState -> Coord -> ModifyState
bananaBlast wormId'
            gameMapPriorToBlast
            wormPositions'
            awardPointsForDigging'
            awardPointsForDamage'
            penaliseForDamage'
            awardPointsForMissing'
            rewardKill
            targetCoord
            state =
  -- TODO: I removed this condition: (wormHits == [] && dirtHits == [] && packHits == [])
  if deepSpaceAt targetCoord gameMapPriorToBlast
  then awardPointsForMissing' state
  -- Effect the current state (could have changed as a result of
  -- the other worm blasting too)
  else foldOverBlastCoordsInRange
         targetCoord
         (\ !state' !nextCoord ->
             if containsAnyWorm nextCoord wormPositions'
             then let damage' = bananaDamageAt targetCoord nextCoord
                  in harmWorm wormId'
                     wormPositions'
                     damage'
                     (penaliseForDamage'    damage')
                     (awardPointsForDamage' damage')
                     rewardKill
                     nextCoord
                     state'
             else if dirtAt nextCoord gameMapPriorToBlast
                  then awardPointsForDigging' $ removeDirtFromMapAt nextCoord state'
                  else if medipackAt nextCoord gameMapPriorToBlast
                       then removeMedipack nextCoord state'
                       else state')
       state

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

makeMyMoveMove :: WormPositions -> Coord -> WormId -> GameMap -> Move -> ModifyState
makeMyMoveMove = makeMoveMove giveMedipackToThisWorm penaliseThisPlayerForAnInvalidCommand awardPointsToThisPlayerForMovingToAir

makeOpponentsMoveMove :: WormPositions -> Coord -> WormId -> GameMap -> Move -> ModifyState
makeOpponentsMoveMove  = makeMoveMove giveMedipackToThatWorm penaliseThatPlayerForAnInvalidCommand awardPointsToThatPlayerForMovingToAir

makeMoveMove :: ModifyState -> ModifyState -> ModifyState -> WormPositions -> Coord -> WormId -> GameMap -> Move -> ModifyState
makeMoveMove giveMedipackToWorm
             penaliseForInvalidMove
             awardPointsForMovingToAir'
             !wormPositions'
             !coord'
             !wormId'
             !gameMap'
             !move =
  let target            = displaceCoordByMove coord' move
      moveIsValid       = not (moveWouldGoOOB coord' move ||
                               anyWormData (== target) wormPositions')
      targetCell        = mapAt target gameMap'
      targetIsValid     = moveIsValid && (targetCell == AIR || targetCell == MEDIPACK)
      targetIsAMedipack = moveIsValid && targetCell == MEDIPACK
      -- TODO: Handle collisions in getting the medipack
      medipackWorm      = if targetIsAMedipack
                          then giveMedipackToWorm . removeMedipack target
                          else id
      applyPenalty      = if targetIsValid then id else penaliseForInvalidMove
      moveWormToTarget  = if targetIsValid then moveWorm wormId' target else id
      awardPoints       = if targetIsValid then awardPointsForMovingToAir' else id
  in medipackWorm . applyPenalty . moveWormToTarget . awardPoints

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

isAMoveMove :: Move -> Bool
isAMoveMove (Move x) = x >= 8 && x < 16

-- TODO: get actual amount of damage
knockBackDamageAmount :: Int
knockBackDamageAmount = 20

-- ASSUME: that there is a worm to apply knockback damage to
-- because both worms must have moved for this to happen
knockBackDamage :: ModifyState
knockBackDamage state =
  knockBackDamageToOne thisPlayersCurrentWormId $
  knockBackDamageToOne thatPlayersCurrentWormId state
  where
    knockBackDamage' = (+) (-knockBackDamageAmount)
    knockBackDamageToOne wormsId =
      let wormId'      = wormsId state
          wormsHealth' = aListFindDataById wormId' $
                         wormHealths state
          wormDied     = knockBackDamageAmount >= wormsHealth'
          cleanUp      = cleanUpDeadWorm wormId'
      in if wormDied
         then cleanUp
         else withWormHealths $ aListMapWormById wormId' knockBackDamage'

cleanUpDeadWorm :: WormId -> ModifyState
cleanUpDeadWorm wormId' =
  withWormHealths         (aListRemoveWormById wormId') .
  withWormPositions       (aListRemoveWormById wormId') .
  withWormBananas         (aListRemoveWormById wormId') .
  withWormSnowballs       (aListRemoveWormById wormId') .
  withWormFrozenDurations (aListRemoveWormById wormId')

moveWorm :: WormId -> Coord -> ModifyState
moveWorm wormId' newCoord' state =
  withWormPositions (aListMapWormById wormId' (always newCoord')) state

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

makeMyDigMove :: Coord -> GameMap -> Move -> ModifyState
makeMyDigMove = makeDigMove awardPointsToThisPlayerForDigging penaliseThisPlayerForAnInvalidCommand

makeOpponentsDigMove :: Coord -> GameMap -> Move -> ModifyState
makeOpponentsDigMove = makeDigMove awardPointsToThatPlayerForDigging penaliseThatPlayerForAnInvalidCommand

makeDigMove :: ModifyState -> ModifyState -> Coord -> GameMap -> Move -> ModifyState
makeDigMove awardPointsForDigging' penalise !wormsCoord !gameMap' !move =
  -- Target of move works with moves and not digs
  let moveAsDig        = shiftDigToMoveRange move
      target           = displaceCoordByMove wormsCoord moveAsDig
      targetCell       = mapAt target gameMap'
      notDiggingOffMap = not $ moveWouldGoOOB wormsCoord moveAsDig
      targetIsValid    = notDiggingOffMap && targetCell == DIRT
      digOutTarget     = if targetIsValid then removeDirtFromMapAt target else id
      penaliseBadDig   = if not targetIsValid then penalise else id
      awardPoints      = if targetIsValid then awardPointsForDigging' else penaliseBadDig
  in awardPoints . digOutTarget

digPoints :: Int
digPoints = 7

awardPointsForDigging :: Player -> Player
awardPointsForDigging = modifyScore digPoints

awardPointsToThisPlayerForDigging :: ModifyState
awardPointsToThisPlayerForDigging = mapThisPlayer awardPointsForDigging

awardPointsToThatPlayerForDigging :: ModifyState
awardPointsToThatPlayerForDigging = mapThatPlayer awardPointsForDigging

isMyWorm :: WormId -> Bool
isMyWorm (WormId 1) = True
isMyWorm (WormId 2) = True
isMyWorm (WormId 3) = True
isMyWorm _          = False

makeMyShootMove :: Coord -> WormId -> GameMap -> WormPositions -> Move -> ModifyState
makeMyShootMove =
  makeShootMove penaliseThisPlayerForHittingHisFriendlyWorm
                awardPointsToThisPlayerForHittingAnEnemy
                awardPointsToThisPlayerForKillingAnEnemy
                awardPointsToThisPlayerForMissing

makeOpponentsShootMove :: Coord -> WormId -> GameMap -> WormPositions -> Move -> ModifyState
makeOpponentsShootMove =
  makeShootMove penaliseThatPlayerForHittingHisFriendlyWorm
                awardPointsToThatPlayerForHittingAnEnemy
                awardPointsToThatPlayerForKillingAnEnemy
                awardPointsToThatPlayerForMissing

makeShootMove :: ModifyState -> ModifyState -> ModifyState -> ModifyState -> Coord -> WormId -> GameMap -> WormPositions -> Move -> ModifyState
makeShootMove penalise
              awardPlayer
              awardPlayerForKill
              awardPointsForMiss
              !wormsPosition
              !wormId'
              !gameMap'
              !wormPositions'
              !move =
      let coord    = shotHitsWorm wormsPosition gameMap' wormPositions' move
          isHit    = isJust coord
          coord'   = fromJust coord
      in if isHit
         then harmWormWithRocket wormId'
                                 wormPositions'
                                 penalise
                                 awardPlayer
                                 awardPlayerForKill
                                 coord'
         else awardPointsForMiss

awardPointsForMissing :: ModifyPlayer
awardPointsForMissing = modifyScore 2

awardPointsToThisPlayerForMissing :: ModifyState
awardPointsToThisPlayerForMissing = mapThisPlayer awardPointsForMissing

awardPointsToThatPlayerForMissing :: ModifyState
awardPointsToThatPlayerForMissing = mapThatPlayer awardPointsForMissing

awardPointsForDamage :: Int -> ModifyPlayer
awardPointsForDamage damage' = modifyScore (2 * damage')

awardPointsForHittingAnEnemy :: ModifyPlayer
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

awardPointsForKillingAnEnemy :: ModifyPlayer
awardPointsForKillingAnEnemy = modifyScore 40

harmWormWithRocket :: WormId -> WormPositions -> ModifyState -> ModifyState -> ModifyState -> Coord -> ModifyState
harmWormWithRocket wormId'
                   wormPositions'
                   penalisePlayer
                   awardPlayer
                   awardPlayerForKill =
  harmWorm wormId'
           wormPositions'
           rocketDamage
           penalisePlayer
           awardPlayer
           awardPlayerForKill

harmWormById :: Int -> WormId -> WormHealths -> WormHealths
harmWormById damage' wormId' = aListMapWormById wormId' (+ (-damage'))

-- DEBUG: This is for debugging.  I should comment out the lines
-- bellow when I'm done using it...
-- errorWithMessageIfJust :: String -> Maybe a -> Maybe a
-- errorWithMessageIfJust message Nothing = error message
-- errorWithMessageIfJust _       x       = x

-- ASSUME: that the given coord maps to a worm.
--
-- NOTE: Worms set to zero health are flagged for later removal.
-- Don't use a negative number because that's more difficult to test
-- for.
harmWorm :: WormId -> WormPositions -> Int -> ModifyState -> ModifyState -> ModifyState -> Coord -> ModifyState
harmWorm shootingWormId'
         wormPositions'
         damage'
         penalisePlayer
         awardPlayer
         awardPlayerForKill
         coord
         state =
  let wormId'       = aListFindIdByData coord wormPositions'
      samePlayer    = wormsBelongToSamePlayer wormId' shootingWormId'
      wormHealth'   = aListFindDataById wormId' $ wormHealths state
      wormDied      = wormHealth' <= damage'
      awardPoints   = if wormDied then (awardPlayer . awardPlayerForKill) else awardPlayer
      dishOutPoints = if samePlayer
                      then penalisePlayer
                      else awardPoints
      cleanUp       = flagWormForCleaning wormId'
      harm          = withWormHealths (harmWormById damage' wormId')
      go            = dishOutPoints . if wormDied then cleanUp else harm
  in go state

flagWormForCleaning :: WormId -> ModifyState
flagWormForCleaning wormId' =
  withWormHealths (aListMapWormById wormId' (always 0))

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
  case findFirstWormHit origin gameMap' direction worms' of
    HitWorm coord' -> Just coord'
    _              -> Nothing

findFirstWormHit ::  Coord -> GameMap -> Direction -> WormPositions -> Hit
findFirstWormHit coord' gameMap' N  worms' =
  searchForHitRectalinearly (-mapDim) isOnNorthernBorder coord' gameMap' worms'
findFirstWormHit coord' gameMap' NE worms' =
  searchForHitDiagonally (1 - mapDim)
                         (\ coord'' -> isOnNorthernBorder coord'' || isOnEasternBorder coord'')
                         coord'
                         gameMap'
                         worms'
findFirstWormHit coord' gameMap' E  worms' =
  searchForHitRectalinearly 1 isOnEasternBorder coord' gameMap' worms'
findFirstWormHit coord' gameMap' SE worms' =
  searchForHitDiagonally (mapDim + 1)
                         (\ coord'' -> isOnSouthernBorder coord'' || isOnEasternBorder coord'')
                         coord'
                         gameMap'
                         worms'
findFirstWormHit coord' gameMap' S  worms' =
  searchForHitRectalinearly mapDim isOnSouthernBorder coord' gameMap' worms'
findFirstWormHit coord' gameMap' SW worms' =
  searchForHitDiagonally (mapDim - 1)
                         (\ coord'' -> isOnSouthernBorder coord'' || isOnWesternBorder coord'')
                         coord'
                         gameMap'
                         worms'
findFirstWormHit coord' gameMap' W  worms' =
  searchForHitRectalinearly (-1) isOnWesternBorder coord' gameMap' worms'
findFirstWormHit coord' gameMap' NW worms' =
  searchForHitDiagonally (-mapDim - 1)
                         (\ coord'' -> isOnNorthernBorder coord'' || isOnWesternBorder coord'')
                         coord'
                         gameMap'
                         worms'

-- There's a bit of repitition between these two functions but I'm
-- fine with that given that I'm optimising this code
searchForHitRectalinearly :: Int -> (Coord -> Bool) -> Coord -> GameMap -> WormPositions -> Hit
searchForHitRectalinearly !add isOnBoundary !coord' gameMap' wormPositions' =
  if isOnBoundary coord' then HitNothing else go 3 (coord' + add)
  where
    go :: Int -> Coord -> Hit
    go 0  _             = HitNothing
    go !n !currentCoord =
      if obstacleAt currentCoord gameMap'
      then HitObstacle
      else case isAPositionOfAWorm currentCoord wormPositions' of
        hit@(HitWorm _) -> hit
        HitNothing      ->
          if isOnBoundary currentCoord
          then HitNothing
          else go (n - 1) (currentCoord + add)
        HitObstacle     -> HitObstacle

searchForHitDiagonally :: Int -> (Coord -> Bool) -> Coord -> GameMap -> WormPositions -> Hit
searchForHitDiagonally !add isOnBoundary !coord' gameMap' wormPositions' =
  if isOnBoundary coord' then HitNothing else go 2 (coord' + add)
  where
    go :: Int -> Coord -> Hit
    go 0  _             = HitNothing
    go !n !currentCoord =
      if obstacleAt currentCoord gameMap'
      then HitObstacle
      else case isAPositionOfAWorm currentCoord wormPositions' of
        hit@(HitWorm _) -> hit
        HitNothing      ->
          if isOnBoundary currentCoord
          then HitNothing
          else go (n - 1) (currentCoord + add)
        HitObstacle     -> HitObstacle

isAPositionOfAWorm :: Coord -> WormPositions -> Hit
isAPositionOfAWorm coord' wormPositions' =
  if aListContainsData coord' wormPositions'
  then HitWorm $ aListFindDataByData coord' wormPositions'
  else HitNothing

isOnNorthernBorder :: Coord -> Bool
isOnNorthernBorder 0  = True
isOnNorthernBorder 1  = True
isOnNorthernBorder 2  = True
isOnNorthernBorder 3  = True
isOnNorthernBorder 4  = True
isOnNorthernBorder 5  = True
isOnNorthernBorder 6  = True
isOnNorthernBorder 7  = True
isOnNorthernBorder 8  = True
isOnNorthernBorder 9  = True
isOnNorthernBorder 10 = True
isOnNorthernBorder 11 = True
isOnNorthernBorder 12 = True
isOnNorthernBorder 13 = True
isOnNorthernBorder 14 = True
isOnNorthernBorder 15 = True
isOnNorthernBorder 16 = True
isOnNorthernBorder 17 = True
isOnNorthernBorder 18 = True
isOnNorthernBorder 19 = True
isOnNorthernBorder 20 = True
isOnNorthernBorder 21 = True
isOnNorthernBorder 22 = True
isOnNorthernBorder 23 = True
isOnNorthernBorder 24 = True
isOnNorthernBorder 25 = True
isOnNorthernBorder 26 = True
isOnNorthernBorder 27 = True
isOnNorthernBorder 28 = True
isOnNorthernBorder 29 = True
isOnNorthernBorder 30 = True
isOnNorthernBorder 31 = True
isOnNorthernBorder 32 = True
isOnNorthernBorder _  = False

isOnEasternBorder :: Coord -> Bool
isOnEasternBorder 32   = True
isOnEasternBorder 65   = True
isOnEasternBorder 98   = True
isOnEasternBorder 131  = True
isOnEasternBorder 164  = True
isOnEasternBorder 197  = True
isOnEasternBorder 230  = True
isOnEasternBorder 263  = True
isOnEasternBorder 296  = True
isOnEasternBorder 329  = True
isOnEasternBorder 362  = True
isOnEasternBorder 395  = True
isOnEasternBorder 428  = True
isOnEasternBorder 461  = True
isOnEasternBorder 494  = True
isOnEasternBorder 527  = True
isOnEasternBorder 560  = True
isOnEasternBorder 593  = True
isOnEasternBorder 626  = True
isOnEasternBorder 659  = True
isOnEasternBorder 692  = True
isOnEasternBorder 725  = True
isOnEasternBorder 758  = True
isOnEasternBorder 791  = True
isOnEasternBorder 824  = True
isOnEasternBorder 857  = True
isOnEasternBorder 890  = True
isOnEasternBorder 923  = True
isOnEasternBorder 956  = True
isOnEasternBorder 989  = True
isOnEasternBorder 1022 = True
isOnEasternBorder 1055 = True
isOnEasternBorder 1088 = True
isOnEasternBorder _    = False

isOnSouthernBorder :: Coord -> Bool
isOnSouthernBorder 1056 = True
isOnSouthernBorder 1057 = True
isOnSouthernBorder 1058 = True
isOnSouthernBorder 1059 = True
isOnSouthernBorder 1060 = True
isOnSouthernBorder 1061 = True
isOnSouthernBorder 1062 = True
isOnSouthernBorder 1063 = True
isOnSouthernBorder 1064 = True
isOnSouthernBorder 1065 = True
isOnSouthernBorder 1066 = True
isOnSouthernBorder 1067 = True
isOnSouthernBorder 1068 = True
isOnSouthernBorder 1069 = True
isOnSouthernBorder 1070 = True
isOnSouthernBorder 1071 = True
isOnSouthernBorder 1072 = True
isOnSouthernBorder 1073 = True
isOnSouthernBorder 1074 = True
isOnSouthernBorder 1075 = True
isOnSouthernBorder 1076 = True
isOnSouthernBorder 1077 = True
isOnSouthernBorder 1078 = True
isOnSouthernBorder 1079 = True
isOnSouthernBorder 1080 = True
isOnSouthernBorder 1081 = True
isOnSouthernBorder 1082 = True
isOnSouthernBorder 1083 = True
isOnSouthernBorder 1084 = True
isOnSouthernBorder 1085 = True
isOnSouthernBorder 1086 = True
isOnSouthernBorder 1087 = True
isOnSouthernBorder 1088 = True
isOnSouthernBorder _    = False

isOnWesternBorder :: Coord -> Bool
isOnWesternBorder 0    = True
isOnWesternBorder 33   = True
isOnWesternBorder 66   = True
isOnWesternBorder 99   = True
isOnWesternBorder 132  = True
isOnWesternBorder 165  = True
isOnWesternBorder 198  = True
isOnWesternBorder 231  = True
isOnWesternBorder 264  = True
isOnWesternBorder 297  = True
isOnWesternBorder 330  = True
isOnWesternBorder 363  = True
isOnWesternBorder 396  = True
isOnWesternBorder 429  = True
isOnWesternBorder 462  = True
isOnWesternBorder 495  = True
isOnWesternBorder 528  = True
isOnWesternBorder 561  = True
isOnWesternBorder 594  = True
isOnWesternBorder 627  = True
isOnWesternBorder 660  = True
isOnWesternBorder 693  = True
isOnWesternBorder 726  = True
isOnWesternBorder 759  = True
isOnWesternBorder 792  = True
isOnWesternBorder 825  = True
isOnWesternBorder 858  = True
isOnWesternBorder 891  = True
isOnWesternBorder 924  = True
isOnWesternBorder 957  = True
isOnWesternBorder 990  = True
isOnWesternBorder 1023 = True
isOnWesternBorder 1056 = True
isOnWesternBorder _    = False

-- ASSUME: that this worm is never at an invalid position.
--
-- This assumption is wrong because a worm could die before we get to
-- later stages where we use it.
thisWormsCoord :: State -> Coord
thisWormsCoord state =
  let thisWormId = thisPlayersCurrentWormId state
  in  coordForWorm thisWormId $ wormPositions state

coordForWorm :: WormId -> WormPositions -> Coord
coordForWorm = aListFindDataById

-- ASSUME: that that worm is never at an invalid position.
--
-- This assumption is wrong because a worm could die before we get to
-- later stages where we use it.
thatWormsCoord :: State -> Coord
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

directionOfShot :: Move -> Direction
directionOfShot (Move 0) = N
directionOfShot (Move 1) = NE
directionOfShot (Move 2) = E
directionOfShot (Move 3) = SE
directionOfShot (Move 4) = S
directionOfShot (Move 5) = SW
directionOfShot (Move 6) = W
directionOfShot (Move 7) = NW
directionOfShot move     = error $ "Direction of shot: " ++ show move

isAShootMove :: Move -> Bool
isAShootMove (Move x) = x < 8 && x >= 0

readRound :: IO Int
readRound = readLn

myMovesFromTree :: SearchTree -> SuccessRecords
myMovesFromTree (SearchedLevel   _ (MyMoves myMoves) _ _) = myMoves
myMovesFromTree (UnSearchedLevel _ (MyMoves myMoves) _)   = myMoves
myMovesFromTree SearchFront                               =
  error $ "myMovesFromTree of SearchFront"

iterationsBeforeComms :: Int
iterationsBeforeComms = 10

type CommsChannel = TMVar.TMVar

pollComms :: CommsChannel a -> IO (Maybe a)
pollComms channel = STM.atomically $ TMVar.tryTakeTMVar channel

writeComms :: CommsChannel a -> a -> IO ()
writeComms channel value' = fmap (\ _ -> ()) $ STM.atomically $ TMVar.putTMVar channel value'

newComms :: IO (CommsChannel a)
newComms = STM.atomically $ TMVar.newEmptyTMVar

type CommsVariable = TVar.TVar

readVariable :: CommsVariable a -> IO a
readVariable channel = STM.atomically $ TVar.readTVar channel

writeVariable :: CommsVariable a -> a -> IO ()
writeVariable channel value' = STM.atomically $ TVar.writeTVar channel value'

newVariable :: a -> IO (CommsVariable a)
newVariable initialValue = STM.atomically $ TVar.newTVar initialValue

logStdErr :: String -> IO ()
logStdErr = hPutStrLn stderr

-- First iteration I think that I'll suspend the thread until a new
-- state comes along.
-- TODO: don't suspend the thread when the new state comes along.
iterativelyImproveSearch :: StdGen -> State -> SearchTree -> CommsChannel (CombinedMove, State) -> CommsVariable SearchTree -> IO ()
iterativelyImproveSearch gen initialState tree stateChannel treeVariable = do
  let treeFromPreviousRound = if strategy == Dig || strategy == GetToTheChoppa
                              then SearchFront
                              else tree
  E.catch (go gen iterationsBeforeComms treeFromPreviousRound) exceptionHandler
  where
    exceptionHandler e = do
      -- TODO restart worker??
      logStdErr $ "Worker died during [" ++
        show strategy ++ "] with exception " ++ show (e::SomeException) ++ "\n" ++
        "State: " ++ readableShow initialState ++ "\n" ++
        "state':" ++ readableShow state'
    nearbyWorms = wormsNearMyCurrentWorm initialState
    strategy    = determineStrategy (thisWormsCoord initialState) nearbyWorms
    state'      = if strategy == Dig || strategy == GetToTheChoppa
                  then withOnlyWormsContainedIn nearbyWorms initialState
                  else initialState
    go :: StdGen -> Int -> SearchTree-> IO ()
    go gen' 0      !searchTree = do
      writeVariable treeVariable searchTree
      newRoundsState <- pollComms stateChannel
      case newRoundsState of
        -- TODO: use the move (first arg in tuple to modify the
        -- initial state rather than passing it in the whole time.)
        Just (move', state'') -> do
          -- This isn't good enough.  I need to have a mode of searching in
          -- between, when the runner hasn't yet told me to move because it's
          -- 900ms from that point that I communicate back.
          let tree'' = if strategy == Kill
                       then makeMoveInTree move' searchTree
                       else SearchFront
          let (myMove', opponentsMove') = (toMoves move')
          when (tree'' == SearchFront) $
            logStdErr $
            "Not in search tree: " ++
            "\n\tCombined: " ++ show move' ++
            "\n\tMy move: " ++ prettyPrintThisMove initialState myMove' ++
            "\n\tOpponents move: " ++ prettyPrintThatMove initialState opponentsMove'
          iterativelyImproveSearch gen' state'' tree'' stateChannel treeVariable
        Nothing -> go gen' iterationsBeforeComms searchTree
    go gen' !count' !searchTree =
      let (result, gen'', finalState) = search gen' strategy state' searchTree
          newTree                     = updateTree searchTree strategy finalState result
      in go gen'' (count' - 1) newTree

makeMoveInTree :: CombinedMove -> SearchTree -> SearchTree
makeMoveInTree move' (SearchedLevel   _ _ _ transitions) = findSubTree move' transitions
makeMoveInTree _     (UnSearchedLevel _ _ _)             = SearchFront
makeMoveInTree _     SearchFront                         = SearchFront

-- In nanoseconds
maxSearchTime :: Integer
maxSearchTime = 850000000

-- In microseconds
pollInterval :: Int
pollInterval = 10000

joinWith :: (a -> String) -> String -> [a] -> String
joinWith toString joinString strings =
  let withExtra = concat $ map (\ x -> toString x ++ "\n\t") strings
  in take ((length withExtra) - (length joinString)) withExtra

prettyPrintMove :: (State -> Coord) -> (Move -> ModifyState) -> State -> Move -> String
prettyPrintMove wormsCoord makeSelections' state move =
  let coord' = wormsCoord state
  in formatMove wormsCoord makeSelections' move coord' state

prettyPrintThisMove :: State -> Move -> String
prettyPrintThisMove = prettyPrintMove thisWormsCoord makeMySelection

prettyPrintThatMove :: State -> Move -> String
prettyPrintThatMove = prettyPrintMove thatWormsCoord makeOpponentsSelection

prettyPrintSuccessRecord :: (State -> Move -> String) -> State -> SuccessRecord -> String
prettyPrintSuccessRecord printMove
                         state
                         (SuccessRecord (GamesPlayed gamesPlayed) (PayoffRatio ratio) move') =
    printMove state move' ++
              " (played " ++
              show gamesPlayed ++
              ")" ++
              ": " ++
              "(" ++
              show ratio ++
              " / " ++
              show gamesPlayed ++
              ") " ++
              show (ratio / fromIntegral gamesPlayed)

prettyPrintThisSuccessRecord :: State -> SuccessRecord -> String
prettyPrintThisSuccessRecord = prettyPrintSuccessRecord prettyPrintThisMove

prettyPrintThatSuccessRecord :: State -> SuccessRecord -> String
prettyPrintThatSuccessRecord = prettyPrintSuccessRecord prettyPrintThatMove

prettyPrintSearchTree :: State -> SearchTree -> String
prettyPrintSearchTree state (SearchedLevel gamesPlayed (MyMoves myMoves) (OpponentsMoves opponentsMoves) _) =
    "Searched:\n" ++
    "Games played: " ++ show gamesPlayed ++ "\n" ++
    "My moves:\n\t" ++ (joinWith (prettyPrintThisSuccessRecord state) "\n\t" myMoves) ++ "\n" ++
    "Opponents moves:\n\t" ++ (joinWith (prettyPrintThatSuccessRecord state) "\n\t" opponentsMoves)
prettyPrintSearchTree state (UnSearchedLevel gamesPlayed (MyMoves myMoves) (OpponentsMoves opponentsMoves)) =
    "UnSearched:\n" ++
    "Games played: " ++ show gamesPlayed ++ "\n" ++
    "My moves:\n\t" ++ (joinWith (prettyPrintThisSuccessRecord state) "\n\t" myMoves) ++ "\n" ++
    "Opponents moves:\n\t" ++ (joinWith (prettyPrintThatSuccessRecord state) "\n\t" opponentsMoves)
prettyPrintSearchTree _     SearchFront =
    "SearchFront"

treeAfterAlottedTime :: State -> CommsVariable SearchTree -> IO SearchTree
treeAfterAlottedTime state treeVariable = do
  startingTime <- fmap toNanoSecs $ getTime clock
  searchTree   <- go SearchFront startingTime
  return searchTree
  where
    clock = Realtime
    go searchTree startingTime =
      (getTime clock) >>=
      \ timeNow ->
        if ((toNanoSecs timeNow) - startingTime) > maxSearchTime
        then (logStdErr $ "Current round: " ++
                          (show $ currentRound state) ++
                          ", MyWorm: " ++
                          (show $ thisPlayersCurrentWormId state) ++
                          ", Opponent worm: " ++
                          (show $ thatPlayersCurrentWormId state) ++
                          "\n" ++
                          prettyPrintSearchTree state searchTree) >> return searchTree
        else do
          searchTree' <- readVariable treeVariable
          Control.Concurrent.threadDelay pollInterval
          go searchTree' startingTime

searchForAlottedTime :: State -> CommsVariable SearchTree -> IO Move
searchForAlottedTime state treeChannel = do
  searchTree     <- treeAfterAlottedTime state treeChannel
  let gamesPlayed = countGames searchTree
  return $ successRecordMove . chooseBestMove gamesPlayed $ myMovesFromTree searchTree

runRound :: Int -> State -> CommsChannel (CombinedMove, State) -> CommsVariable SearchTree -> IO ()
runRound roundNumber previousState stateChannel treeVariable = do
  move                 <- liftIO $ searchForAlottedTime previousState treeVariable
  liftIO $
    putStrLn $
    -- ASSUME: that the worm is on a valid square to begin with
    "C;" ++
    show roundNumber ++
    ";" ++
    formatMove thisWormsCoord makeMySelection move (thisWormsCoord previousState) previousState ++ "\n"
  roundNumber'         <- readRound
  state                <- readGameState roundNumber'
  -- TODO fromJust?
  let state'            = force $ fromJust state
  let opponentsLastMove = force $ parseLastCommand previousState $ opponentsLastCommand state'
  -- TODO!!!!!  I shouldn't be reading this state in the searcher.
  -- All I care about is the opponents move...
  -- EXTRA NOTE: And the fact that I don't know whether we swapped.
  writeComms stateChannel $ (fromMoves move opponentsLastMove, state')
  runRound roundNumber' state' stateChannel treeVariable

parseLastCommand :: State -> Maybe String -> Move
parseLastCommand _             Nothing             = doNothing
parseLastCommand previousState (Just lastCommand') =
  let coord'  = thatWormsCoord previousState
  in fromJust $ readThatMove previousState coord' lastCommand'

startBot :: StdGen -> RIO App ()
startBot g = do
  treeVariable   <- liftIO $ newVariable SearchFront
  stateChannel  <- liftIO newComms
  -- This is where I seed it with a search front
  initialRound' <- liftIO $ readRound
  initialState  <- liftIO $ fmap fromJust $ readGameState initialRound'
  _             <- liftIO $ forkIO (iterativelyImproveSearch g initialState SearchFront stateChannel treeVariable)
  liftIO $ runRound initialRound' initialState stateChannel treeVariable

data PayoffRatio = PayoffRatio !Double
  deriving (Eq)

instance NFData PayoffRatio where
  rnf (PayoffRatio ratio) = ratio `deepseq` ()

data GamesPlayed = GamesPlayed !Int
  deriving (Eq)

instance NFData GamesPlayed where
  rnf (GamesPlayed gamesPlayed) = gamesPlayed `deepseq` ()

data SuccessRecord = SuccessRecord !GamesPlayed !PayoffRatio !Move
  deriving (Eq)

instance NFData SuccessRecord where
  rnf (SuccessRecord gamesPlayed ratio move) =
    gamesPlayed `deepseq `ratio `deepseq` move `deepseq` ()

instance Show SuccessRecord where
  show (SuccessRecord (GamesPlayed gamesPlayed) (PayoffRatio ratio) move') =
    show move' ++ " (" ++ show gamesPlayed ++ "): " ++ show (ratio / fromIntegral gamesPlayed)

type SuccessRecords = [SuccessRecord]

successRecordMove :: SuccessRecord -> Move
successRecordMove (SuccessRecord _ _ move) = move

data MyMoves = MyMoves SuccessRecords
  deriving (Eq)

instance NFData MyMoves where
  rnf (MyMoves myMoves) = myMoves `deepseq` ()

data OpponentsMoves = OpponentsMoves SuccessRecords
  deriving (Eq)

instance NFData OpponentsMoves where
  rnf (OpponentsMoves opponentsMoves) =  opponentsMoves `deepseq` ()

data StateTransition = StateTransition CombinedMove SearchTree
  deriving (Eq, Show)

instance NFData StateTransition where
  rnf (StateTransition combinedMove searchTree) =
    combinedMove `deepseq` searchTree `deepseq` ()

hasMove :: CombinedMove -> StateTransition -> Bool
hasMove move' (StateTransition move'' _) = move' == move''

subTree :: StateTransition -> SearchTree
subTree (StateTransition _ tree) = tree

type StateTransitions = [StateTransition]

data SearchTree = SearchedLevel   Int MyMoves OpponentsMoves StateTransitions
                | UnSearchedLevel Int MyMoves OpponentsMoves
                | SearchFront
                deriving (Eq)

instance NFData SearchTree where
  rnf (SearchedLevel gamesPlayed myMoves opponentsMoves stateTransitions) =
    gamesPlayed `deepseq` myMoves `deepseq` opponentsMoves `deepseq` stateTransitions `deepseq` ()
  rnf (UnSearchedLevel gamesPlayed myMoves opponentsMoves) =
    gamesPlayed `deepseq` myMoves `deepseq` opponentsMoves `deepseq` ()
  rnf x = x `deepseq` ()

join' :: Show a => String -> [a] -> String
join' joinString strings =
  let withExtra = concat $ map (\ x -> show x ++ "\n\t") strings
  in take ((length withExtra) - (length joinString)) withExtra

instance Show SearchTree where
  show (SearchedLevel gamesPlayed (MyMoves myMoves) (OpponentsMoves opponentsMoves) _) =
    "Searched:\n" ++
    "Games played: " ++ show gamesPlayed ++ "\n" ++
    "My moves:\n\t" ++ (join' "\n\t" myMoves) ++ "\n" ++
    "Opponents moves:\n\t" ++ (join' "\n\t" opponentsMoves)
  show (UnSearchedLevel gamesPlayed (MyMoves myMoves) (OpponentsMoves opponentsMoves)) =
    "UnSearched:\n" ++
    "Games played: " ++ show gamesPlayed ++ "\n" ++
    "My moves:\n\t" ++ (join' "\n\t" myMoves) ++ "\n" ++
    "Opponents moves:\n\t" ++ (join' "\n\t" opponentsMoves)
  show SearchFront =
    "SearchFront"

type Reward = Int

data SearchResult = SearchResult Payoff Moves

instance Show SearchResult where
  show (SearchResult payoff moves') =
    "SearchResult (" ++ show payoff ++ ") (Moves " ++ (show $ map toMoves moves') ++ ")"

instance NFData SearchResult where
  rnf (SearchResult payoff moves) = payoff `deepseq` moves `deepseq` ()

incInc :: Int -> Int -> SuccessRecord -> SuccessRecord
incInc reward' maxScore' (SuccessRecord (GamesPlayed gamesPlayed) (PayoffRatio ratio) playerMove') =
  SuccessRecord (GamesPlayed $ gamesPlayed + 1)
                (PayoffRatio $ ratio + (fromIntegral reward' / fromIntegral maxScore'))
                playerMove'

countGames :: SearchTree -> Int
countGames (SearchedLevel   playedAtLevel _ _ _) = playedAtLevel
countGames (UnSearchedLevel playedAtLevel _ _)   = playedAtLevel
countGames SearchFront                           = 0

initialiseLevel :: Strategy -> State -> SearchResult -> SearchTree
initialiseLevel strategy state result =
  let myMovesFrom'  = case strategy of
        GetToTheChoppa -> myGetToTheChoppaMoves
        Kill           -> myMovesFrom
        Dig            -> myDigMovesFrom
      opponentsMovesFrom' = case strategy of
        GetToTheChoppa -> (always [doNothing])
        Kill           -> opponentsMovesFrom
        Dig            -> (always [doNothing])
  in updateTree
     (UnSearchedLevel
      0
      (MyMoves        $ map (SuccessRecord (GamesPlayed 0) (PayoffRatio 0)) $ myMovesFrom'        state)
      (OpponentsMoves $ map (SuccessRecord (GamesPlayed 0) (PayoffRatio 0)) $ opponentsMovesFrom' state))
     strategy state result

updateTree :: SearchTree -> Strategy -> State-> SearchResult -> SearchTree
updateTree SearchFront strategy state result = initialiseLevel strategy state result
updateTree level@(UnSearchedLevel gamesPlayed (MyMoves myMoves) (OpponentsMoves opponentsMoves)) _ _ result =
  case result of
    (SearchResult  (Payoff (MyPayoff myPayoff) (OpponentsPayoff opponentsPayoff) (MaxScore maxScore')) (move':_)) ->
      let (thisMove, thatMove) = toMoves move'
          myMoves'             = MyMoves        $ updateCount (incInc myPayoff        maxScore') myMoves        thisMove
          opponentsMoves'      = OpponentsMoves $ updateCount (incInc opponentsPayoff maxScore') opponentsMoves thatMove
      in (transitionLevelType myMoves' opponentsMoves') (gamesPlayed + 1) myMoves' opponentsMoves'
    _                           -> level
updateTree level@(SearchedLevel gamesPlayed (MyMoves myMoves) (OpponentsMoves opponentsMoves) stateTransitions) strategy state result =
  case result of
    (SearchResult  (Payoff (MyPayoff myPayoff) (OpponentsPayoff opponentsPayoff) (MaxScore maxScore')) (move':_)) ->
      let (thisMove, thatMove) = toMoves move'
          myMoves'             = MyMoves        $ updateCount (incInc myPayoff        maxScore') myMoves        thisMove
          opponentsMoves'      = OpponentsMoves $ updateCount (incInc opponentsPayoff maxScore') opponentsMoves thatMove
      in SearchedLevel (gamesPlayed + 1) myMoves' opponentsMoves' $ updateSubTree strategy state result stateTransitions
    _                           -> level

-- TODO: consider whether I should be treating the rewards like I do moves when transitioning down a tree..?
updateSubTree :: Strategy -> State -> SearchResult -> StateTransitions -> StateTransitions
updateSubTree strategy state (SearchResult payoff (move':moves')) [] =
  [StateTransition move' $ updateTree SearchFront strategy state (SearchResult payoff moves')]
updateSubTree _ _ (SearchResult _ []) transitions = transitions
updateSubTree strategy
              state
              result@(SearchResult payoff (move':moves'))
              (transition@(StateTransition transitionMove' subTree'):transitions)
  | move' == transitionMove' = (StateTransition transitionMove' $
                                updateTree subTree'
                                           strategy
                                           (makeMove False transitionMove' state)
                                           (SearchResult payoff moves')) : transitions
  | otherwise                = transition : updateSubTree strategy state result transitions

transitionLevelType :: MyMoves -> OpponentsMoves -> (Int -> MyMoves -> OpponentsMoves -> SearchTree)
transitionLevelType myMoves opponentsMoves =
    if allGamesPlayed myMoves opponentsMoves
    then \ gamesPlayed myMoves' opponentsMoves' -> SearchedLevel   gamesPlayed myMoves' opponentsMoves' []
    else \ gamesPlayed myMoves' opponentsMoves' -> UnSearchedLevel gamesPlayed myMoves' opponentsMoves'

allGamesPlayed :: MyMoves -> OpponentsMoves -> Bool
allGamesPlayed (MyMoves myMoves) (OpponentsMoves opponentsMoves) =
  all hasBeenPlayed myMoves && all hasBeenPlayed opponentsMoves
  where
    hasBeenPlayed (SuccessRecord (GamesPlayed gamesPlayed) _ _) = gamesPlayed /= 0

updateCount :: (SuccessRecord -> SuccessRecord) -> SuccessRecords -> Move -> SuccessRecords
updateCount _           []            _ = []
updateCount changeCount (record:rest) move'
  | successRecordMove record == move' = (changeCount record):rest
  | otherwise                         = record:(updateCount changeCount rest move')

digReward :: Move -> Reward
digReward myMove
  | isADigMove myMove = digPoints
  | otherwise         = 0

rangeToConsiderInMinigame :: Int
rangeToConsiderInMinigame = 7

wormsNearMyCurrentWorm :: State -> AList
wormsNearMyCurrentWorm state =
  let coord' = thisWormsCoord state
  in aListFilterByData (\ xy -> inRange xy coord' rangeToConsiderInMinigame) $
     wormPositions state

data Strategy = Dig
              | Kill
              | GetToTheChoppa
              deriving (Eq, Show)

choppaRadius :: Int
choppaRadius = 5

determineStrategy :: Coord -> AList -> Strategy
determineStrategy currentWormsCoord' wormPositions' =
  if aListCountOpponentsEntries wormPositions' == 0
  then if manhattanDistanceToMiddle currentWormsCoord' < choppaRadius
       then Dig
       else GetToTheChoppa
  else Kill

withOnlyWormsContainedIn :: AList -> ModifyState
withOnlyWormsContainedIn toKeep =
  let keep = aListKeepWormsDefinedInFirst toKeep
  in fixOpponentsCurrentWorm  .
     withWormHealths     keep .
     withWormPositions   keep .
     withWormBananas     keep .
     withWormSnowballs   keep .
     withFrozenDurations keep

fixOpponentsCurrentWorm :: ModifyState
fixOpponentsCurrentWorm state =
  let wormId'  = thatPlayersCurrentWormId state
      isInGame = aListContainsId wormId' $ wormHealths state
  in if isInGame
     then state
     else advanceThatWormSelection state

search :: StdGen -> Strategy -> State -> SearchTree -> (SearchResult, StdGen, State)
search g strategy state searchTree =
  let round' = (currentRound state)
  in case strategy of
       Dig            -> digSearch  g 0      state searchTree [] 0
       Kill           -> killSearch g round' state searchTree []
       -- TODO implement a custom search for getting to the choppa! XD
       GetToTheChoppa -> digSearch  g 0      state searchTree [] 0

digSearch :: StdGen -> Int -> State -> SearchTree -> Moves -> Reward -> (SearchResult, StdGen, State)
-- The first iteration of play randomly is here because we need to use
-- that move when we write the first entry in an unsearched level.
digSearch !g !round' !state SearchFront                   moves !reward =
  case digGameOver round' reward state of
    GameOver payoff -> (SearchResult payoff (reverse moves), g, state)
    NoResult        ->
      let availableMoves = digMovesFrom state
          (move, g')     = pickOneAtRandom g availableMoves
          state'         = makeMove False move state
          reward'        = digReward $ fst $ toMoves move
      in digPlayRandomly state g' (round' + 1) state' (move:moves) (reward' + reward)
digSearch !g !round' !state  tree@(SearchedLevel _ _ _ _) moves !reward =
  case digGameOver round' reward state of
    GameOver payoff -> (SearchResult payoff (reverse moves), g, state)
    NoResult        -> digSearchSearchedLevel g round' state tree moves reward
digSearch !g
          !round'
          !state
          (UnSearchedLevel _ (MyMoves myMoves) (OpponentsMoves opponentsMoves))
          moves
          !reward =
  case digGameOver round' reward state of
    GameOver payoff -> (SearchResult payoff (reverse moves), g, state)
    NoResult        ->
      let (myRecord,        g')  = pickOneAtRandom g  myMoves
          (opponentsRecord, g'') = pickOneAtRandom g' opponentsMoves
          myMove                 = successRecordMove myRecord
          opponentsMove          = successRecordMove opponentsRecord
          combinedMove           = fromMoves myMove opponentsMove
          state'                 = makeMove False combinedMove state
          reward'                = digReward myMove
      in digSearch g''
                   (round' + 1)
                   state'
                   SearchFront
                   (combinedMove:moves)
                   (reward' + reward)

digSearchSearchedLevel :: StdGen -> Int -> State -> SearchTree -> Moves -> Reward -> (SearchResult, StdGen, State)
digSearchSearchedLevel _ _ _ SearchFront                     _ _ = error "killSearchSearchedLevel: SearchFront"
digSearchSearchedLevel _ _ _ level@(UnSearchedLevel _ _ _ )  _ _ = error $ "killSearchSearchedLevel: " ++ show level
digSearchSearchedLevel !g
                       !round'
                       !state
                       (SearchedLevel gamesPlayed (MyMoves myMoves) (OpponentsMoves opponentsMoves) transitions)
                       moves
                       !reward =
  let myBestMove        = successRecordMove $ chooseBestMove gamesPlayed myMoves
      opponentsBestMove = successRecordMove $ chooseBestMove gamesPlayed opponentsMoves
      combinedMove      = fromMoves myBestMove opponentsBestMove
      state'            = makeMove True combinedMove state
      reward'           = digReward $ myBestMove
  in digSearch g
               (round' + 1)
               state'
               (findSubTree combinedMove transitions)
               (combinedMove:moves)
               (reward' + reward)

digPlayRandomly :: State -> StdGen -> Int -> State -> Moves -> Reward -> (SearchResult, StdGen, State)
digPlayRandomly initialState g round' state moves reward =
  case digGameOver round' reward state of
    GameOver payoff -> (SearchResult payoff (reverse moves), g, initialState)
    NoResult        ->
      let availableMoves  = digMovesFrom state
          (move, g')      = if availableMoves == []
                            then (fromMoves doNothing doNothing, g)
                            else pickOneAtRandom g availableMoves
          state'          = makeMove False move state
          reward'         = digReward $ fst $ toMoves move
      in digPlayRandomly initialState g' (round' + 1) state' moves (reward' + reward)

maximumHealth :: Int
maximumHealth = 100 + 100 + 150

maximumFrozenDuration :: Int
maximumFrozenDuration = 150

maxPayoffScore :: Int
maxPayoffScore =
  -- Maximum health (times two because it's the biggest difference between you and your opponent)
  2 * maximumHealth +
  -- Maximum banana ammo (scaled to be similar to health)
  30 +
  -- Maximum snowball ammo (scaled to be similar to health)
  30 +
  -- Maximum selects left (scaled to be similar to health)
  50 +
  -- Maximum duration spent unfrozen (scaled to be similar to health)
  maximumFrozenDuration -- All three unfrozen for five turns

payOff :: State -> Payoff
payOff (State { wormHealths     = wormHealths',
                wormBananas     = wormBananas',
                wormSnowballs   = wormSnowballs',
                frozenDurations = frozenDurations',
                myPlayer        = myPlayer',
                opponent        = opponent' }) =
  let myTotalHealth                = aListSumMyEntries wormHealths'
      myBananasLeft                = aListSumMyEntries wormBananas'
      mySnowballsLeft              = aListSumMyEntries wormSnowballs'
      myFrozenDurations            = aListSumMyEntries frozenDurations'
      mySelectionsRemaining        = playerSelections myPlayer'
      opponentsTotalHealth         = aListSumOpponentsEntries wormHealths'
      opponentsBananasLeft         = aListSumOpponentsEntries wormBananas'
      opponentsSnowballsLeft       = aListSumOpponentsEntries wormSnowballs'
      opponentsFrozenDurations     = aListSumOpponentsEntries frozenDurations'
      opponentsSelectionsRemaining = playerSelections opponent'
      myPayoff                     = myTotalHealth +
                                     (maximumHealth - opponentsTotalHealth) +
                                     10 * myBananasLeft +
                                     10 * mySnowballsLeft +
                                     (maximumFrozenDuration - myFrozenDurations) +
                                     10 * mySelectionsRemaining
      opponentsPayoff              = opponentsTotalHealth +
                                     (maximumHealth - myTotalHealth) +
                                     10 * opponentsBananasLeft +
                                     10 * opponentsSnowballsLeft +
                                     (maximumFrozenDuration - opponentsFrozenDurations) +
                                     10 * opponentsSelectionsRemaining
  in Payoff (MyPayoff myPayoff) (OpponentsPayoff opponentsPayoff) (MaxScore maxPayoffScore)

killSearch :: StdGen -> Int -> State -> SearchTree -> Moves -> (SearchResult, StdGen, State)
-- The first iteration of play randomly is here because we need to use
-- that move when we write the first entry in an unsearched level.
killSearch !g _       !state SearchFront                  moves =
  (SearchResult (payOff state) (reverse moves), g, state)
killSearch !g !round' !state tree@(SearchedLevel _ _ _ _) moves =
  case gameOver state round' of
    GameOver payoff -> (SearchResult payoff (reverse moves), g, state)
    NoResult        -> killSearchSearchedLevel g round' state tree moves
killSearch !g
           !round'
           !state
           (UnSearchedLevel _ (MyMoves myMoves) (OpponentsMoves opponentsMoves))
           !moves =
  case gameOver state round' of
    GameOver payoff -> (SearchResult payoff (reverse moves), g, state)
    NoResult        ->
      let (myRecord,        g')  = pickOneAtRandom g  myMoves
          (opponentsRecord, g'') = pickOneAtRandom g' opponentsMoves
          myMove                 = successRecordMove myRecord
          opponentsMove          = successRecordMove opponentsRecord
          combinedMove           = fromMoves myMove opponentsMove
          state'                 = makeMove False combinedMove state
      in (SearchResult (payOff state') (reverse (combinedMove:moves)), g'', state)

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

killSearchSearchedLevel :: StdGen -> Int -> State -> SearchTree -> Moves -> (SearchResult, StdGen, State)
killSearchSearchedLevel _ _ _ SearchFront                   _ = error "killSearchSearchedLevel: SearchFront"
killSearchSearchedLevel _ _ _ level@(UnSearchedLevel _ _ _) _ = error $ "killSearchSearchedLevel: " ++ show level
killSearchSearchedLevel !g
                        !round'
                        !state
                        (SearchedLevel gamesPlayed (MyMoves myMoves) (OpponentsMoves opponentsMoves) transitions)
                        moves =
  let myBestMove        = successRecordMove $ chooseBestMove gamesPlayed myMoves
      opponentsBestMove = successRecordMove $ chooseBestMove gamesPlayed opponentsMoves
      combinedMove      = fromMoves myBestMove opponentsBestMove
      state'            = makeMove True combinedMove state
  in killSearch g
                (round' + 1)
                state'
                (findSubTree combinedMove transitions)
                (combinedMove:moves)

data GameOver = GameOver Payoff
              | NoResult

maxDigRound :: Int
maxDigRound = 20

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

maxRound :: Int
maxRound = 400

killMaxScore :: MaxScore
killMaxScore = MaxScore 1

gameOver :: State -> Int -> GameOver
gameOver state round' =
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
               then GameOver $ Payoff (MyPayoff 1) (OpponentsPayoff 0) killMaxScore
               -- I lost because of points when both players are dead
               else GameOver $ Payoff (MyPayoff 0) (OpponentsPayoff 1) killMaxScore
          -- The opponent killed all my worms and I didn't kill his
          else GameOver $ Payoff (MyPayoff 0) (OpponentsPayoff 1) killMaxScore
     else if opponentWormCount == 0
          -- I Killed his worms and he didn't kill mine
          then GameOver $ Payoff (MyPayoff 1) (OpponentsPayoff 0) killMaxScore
          else if round' >= maxRound
               -- Simulation was terminated early.  Decide based on how valuable the moves were
               then if myScoreIsHigher
                    -- I won because of points when both players are dead
                    then GameOver $ Payoff (MyPayoff 1) (OpponentsPayoff 0) killMaxScore
                    -- I lost because of points when both players are dead
                    else GameOver $ Payoff (MyPayoff 0) (OpponentsPayoff 1) killMaxScore
               -- Simulation isn't over yet
               else NoResult

digMaxScore :: MaxScore
digMaxScore = MaxScore $ maxDigRound * digPoints

digGameOver :: Int -> Reward -> State -> GameOver
digGameOver round' reward state =
  if round' > maxDigRound || ((aListCountMyEntries $ wormPositions state) == 0)
  then GameOver $ diffMax reward
  else NoResult

myTotalWormHealth :: State -> Int
myTotalWormHealth = aListSumMyEntries . wormHealths

opponentsTotalWormHealth :: State -> Int
opponentsTotalWormHealth = aListSumOpponentsEntries . wormHealths

data OpponentsPayoff = OpponentsPayoff !Int
  deriving (Eq, Show)

instance NFData OpponentsPayoff where
  rnf (OpponentsPayoff opponentsPayoff) = opponentsPayoff `deepseq` ()

data MyPayoff = MyPayoff !Int
  deriving (Eq, Show)

instance NFData MyPayoff where
  rnf (MyPayoff myPayoff) = myPayoff `deepseq` ()

data MaxScore = MaxScore !Int
  deriving (Eq, Show)

instance NFData MaxScore where
  rnf (MaxScore maxScore') = maxScore' `deepseq` ()

data Payoff = Payoff !MyPayoff !OpponentsPayoff !MaxScore
  deriving (Eq, Show)

instance NFData Payoff where
  rnf (Payoff myPayoff opponentsPayoff maxScore') =
    myPayoff `deepseq` opponentsPayoff `deepseq` maxScore' `deepseq` ()

diffMax :: Reward -> Payoff
diffMax !rewardTotal = Payoff (MyPayoff rewardTotal) (OpponentsPayoff 0) digMaxScore

chooseBestMove :: Int -> [SuccessRecord] -> SuccessRecord
chooseBestMove totalGames successRecords =
  let computeConfidence (SuccessRecord (GamesPlayed gamesPlayed) (PayoffRatio ratio) _) =
        confidence totalGames gamesPlayed ratio
  in maximumBy (\ oneTree otherTree -> compare (computeConfidence oneTree) (computeConfidence otherTree)) successRecords

confidence :: Int -> Int -> Double -> Double
confidence !totalCount !gamesPlayed !ratio =
  (ratio / n_i) +
  c * sqrt ((log count_i) / n_i)
  where
    n_i     = fromIntegral gamesPlayed
    count_i = fromIntegral totalCount
    c       = sqrt 2

digMovesFrom :: State -> [CombinedMove]
digMovesFrom = map ((flip fromMoves) doNothing) . myDigMovesFrom

myDigMovesFrom :: State -> [Move]
myDigMovesFrom state =
  let coord'  = thisWormsCoord state
  in filter (\ move ->
              (isAMoveMove move && isValidMoveMove coord' state move) ||
              (isADigMove  move && isValidDigMove coord' (shiftDigToMoveRange move) (gameMap state))) $
     map Move [8..23]

-- A dig move as a move move is a dig move shifted into the move range
-- so as to re-use the direction logic for moves.
isValidDigMove :: Coord -> Move -> GameMap -> Bool
isValidDigMove origin digMoveAsMoveMove gameMap' =
  (not $ moveWouldGoOOB origin digMoveAsMoveMove) &&
  mapAt (displaceCoordByMove origin digMoveAsMoveMove) gameMap' == DIRT

myMovesFrom :: State -> [Move]
myMovesFrom state = do
  let moves              = map Move [0..185]
  let hasMoreThanOneWorm = (aListCountMyEntries $ wormPositions state) > 1
  let moves'             = if hasMoreThanOneWorm
                           then addThisPlayersSelects state moves
                           else moves
  myMove <- moves'
  guard (moveWouldBeValuableToMe state myMove)
  return myMove

manhattanDistanceToMiddle :: Coord -> Int
manhattanDistanceToMiddle coord' =
  let (x, y)     = fromCoord coord'
      halfMapDim = 16
  in (abs $ x - halfMapDim) + (abs $ y - halfMapDim)

isCloserByManhattanDistance :: Coord -> Coord -> Bool
isCloserByManhattanDistance this that =
  manhattanDistanceToMiddle this < manhattanDistanceToMiddle that

myGetToTheChoppaMoves :: State -> [Move]
myGetToTheChoppaMoves state =
  let coord'  = thisWormsCoord state
  in filter (\ move ->
              (let targetOfMove' = displaceCoordByMove coord' move
                in isAMoveMove move &&
                   isValidMoveMove coord' state move &&
                   (manhattanDistanceToMiddle targetOfMove' < choppaRadius ||
                    isCloserByManhattanDistance targetOfMove' coord')) ||
              (let targetOfMove' = displaceCoordByMove coord' (shiftDigToMoveRange move)
                in isADigMove  move &&
                   isValidDigMove coord' (shiftDigToMoveRange move) (gameMap state) &&
                   (manhattanDistanceToMiddle targetOfMove' < choppaRadius ||
                    isCloserByManhattanDistance targetOfMove' coord'))) $
     map Move [8..23]

-- ASSUME: that the player has selections left
moveWouldBeValuableToMe :: State -> Move -> Bool
moveWouldBeValuableToMe state move =
  let coord'         = thisWormsCoord state
      gameMap'       = gameMap state
      wormPositions' = wormPositions state
  in (isAMoveMove      move && isValidMoveMove coord' state move) ||
     (isADigMove      move && isValidDigMove  coord' (shiftDigToMoveRange move) (gameMap state)) ||
     (isAShootMove    move && (isJust $ shotHitsWorm coord' gameMap' wormPositions' move)) ||
     (isABananaMove   move && thisWormHasBananasLeft state &&
      any (\ target -> bananaBlastHitOpponent target wormPositions')
          (displaceToBananaDestination move coord')) ||
     (isASnowballMove move && thisWormHasSnowballsLeft state &&
      any (\ target -> (snowballBlastHitOpponent target wormPositions'))
          (displaceToBananaDestination (snowballMoveToBananaRange move) coord')) ||
     (hasASelection move && moveWouldBeValuableToMe (makeMySelection move state) (removeSelectionFromMove move))

bananaBlastHitOpponent :: Coord -> WormPositions -> Bool
bananaBlastHitOpponent coord' wormPositions' =
  foldOverBlastCoordsInRange
    coord'
    (\ !hitsAWorm !nextCoord -> hitsAWorm || aListAnyOpponentData (== nextCoord) wormPositions')
    False

bananaBlastHitMe :: Coord -> WormPositions -> Bool
bananaBlastHitMe coord' wormPositions' =
  foldOverBlastCoordsInRange
    coord'
    (\ !hitsAWorm !nextCoord -> hitsAWorm || aListAnyOfMyData (== nextCoord) wormPositions')
    False

snowballBlastHitOpponent :: Coord -> WormPositions -> Bool
snowballBlastHitOpponent coord' wormPositions' =
  foldOverSnowballBlastCoordsInRange
    coord'
    (\ !hitsAWorm !nextCoord -> hitsAWorm || aListAnyOpponentData (== nextCoord) wormPositions')
    False

snowballBlastHitMe :: Coord -> WormPositions -> Bool
snowballBlastHitMe coord' wormPositions' =
  foldOverSnowballBlastCoordsInRange
    coord'
    (\ !hitsAWorm !nextCoord -> hitsAWorm || aListAnyOfMyData (== nextCoord) wormPositions')
    False

shotHitsWorm :: Coord -> GameMap -> WormPositions -> Move -> Maybe Coord
shotHitsWorm coord' gameMap' wormPositions' move =
  let shotsDir = directionOfShot move
  in hitsWorm coord' gameMap' shotsDir wormPositions'

opponentsMovesFrom :: State -> [Move]
opponentsMovesFrom state = do
  let moves              = map Move [0..185]
  -- This looks wrong
  let hasMoreThanOneWorm = (aListCountOpponentsEntries $ wormPositions state) > 1
  let moves'             = if hasMoreThanOneWorm
                           then addThatPlayersSelects state moves
                           else moves
  opponentsMove <- moves'
  guard (moveWouldBeValuableToOpponent state opponentsMove)
  return $ opponentsMove

-- ASSUME: That the opponent has selections left
moveWouldBeValuableToOpponent :: State -> Move -> Bool
moveWouldBeValuableToOpponent state move =
  let coord'         = thatWormsCoord state
      gameMap'       = gameMap state
      wormPositions' = wormPositions state
  in (isAMoveMove     move && isValidMoveMove coord' state move) ||
     (isADigMove      move && isValidDigMove  coord' (shiftDigToMoveRange move) (gameMap state)) ||
     (isAShootMove    move && (isJust $ shotHitsWorm coord' gameMap' wormPositions' move)) ||
     (isABananaMove   move && thatWormHasBananasLeft state &&
      any (\ target -> bananaBlastHitMe target wormPositions')
          (displaceToBananaDestination move coord')) ||
     (isASnowballMove move && thatWormHasSnowballsLeft state &&
      any (\ target -> (snowballBlastHitMe target wormPositions'))
          (displaceToBananaDestination (snowballMoveToBananaRange move) coord')) ||
     (hasASelection move && moveWouldBeValuableToOpponent (makeOpponentsSelection move state) (removeSelectionFromMove move))

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

withSelection :: WormId -> Move -> Move
withSelection  (WormId id') (Move x) =
  Move $ x .|. (shiftL id' selectEncodingRange)

doNothing :: Move
doNothing = Move 187

isValidMoveMove :: Coord -> State -> Move -> Bool
isValidMoveMove wormCoord state move =
  let moveIsNotOOB = not $ moveWouldGoOOB wormCoord move
      targetCoord  = displaceCoordByMove wormCoord move
      target       = mapAtCoord state targetCoord
  in moveIsNotOOB &&
     (target == AIR || target == MEDIPACK) &&
     (not $ containsAnyWorm targetCoord (wormPositions state))

inRange :: Coord -> Coord -> Int -> Bool
inRange xy' xy'' range' =
  let (x', y')   = fromCoord xy'
      (x'', y'') = fromCoord xy''
      dx         = (fromIntegral (x' - x''))
      dy         = (fromIntegral (y' - y''))
  in sqrt (((dx::Double) ** 2) + (dy ** 2)) <= (fromIntegral range')
