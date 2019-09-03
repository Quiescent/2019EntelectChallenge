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

import qualified Data.IntMap.Lazy as IM
import qualified RIO.Vector.Boxed as V
import qualified RIO.Vector.Boxed.Partial as PV
import qualified RIO.Vector.Unboxed.Unsafe as UBoxedUnsafe
import qualified RIO.Vector.Unboxed as UnBoxed
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
aListSumThisPlayersValues (AList a b c _ _ _) =
  (if a /= (-1) then a else 0) +
  (if b /= (-1) then b else 0) +
  (if c /= (-1) then c else 0)

aListSumThatPlayersValues :: AList -> Int
aListSumThatPlayersValues (AList _ _ _ d e f) =
  (if d /= (-1) then d else 0) +
  (if e /= (-1) then e else 0) +
  (if f /= (-1) then f else 0)

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

aListAveragePairOffs :: (Int -> Int -> Int) -> AList -> Int
aListAveragePairOffs fun aList@(AList a b c d e f) =
  let count' = aListCountMyEntries aList + aListCountOpponentsEntries aList
      result = (if a /= (-1)
                then (if d /= (-1)
                      then fun a d
                      else 0) +
                     (if e /= (-1)
                      then fun a e
                      else 0) +
                     (if f /= (-1)
                      then fun a f
                      else 0)
                else 0) +
               (if b /= (-1)
                then (if d /= (-1)
                      then fun b d
                      else 0) +
                     (if e /= (-1)
                      then fun b e
                      else 0) +
                     (if f /= (-1)
                      then fun b f
                      else 0)
                else 0) +
               (if c /= (-1)
                then (if d /= (-1)
                      then fun c d
                      else 0) +
                     (if e /= (-1)
                      then fun c e
                      else 0) +
                     (if f /= (-1)
                      then fun c f
                      else 0)
                else 0)
  in result `div` count'

aListMinPairOff :: WormId -> (Int -> Int -> Int) -> AList -> Int
aListMinPairOff wormId' fun (AList a b c d e f) =
 case wormId' of
  (WormId 1)  -> (if a /= (-1)
                  then min (if d /= (-1)
                           then fun a d
                           else (maxBound::Int)) $!
                       min (if e /= (-1)
                            then fun a e
                            else (maxBound::Int))
                           (if f /= (-1)
                            then fun a f
                            else (maxBound::Int))
                  else maxBound::Int)
  (WormId 2)  -> (if b /= (-1)
                  then min (if d /= (-1)
                            then fun b d
                              else (maxBound::Int)) $!
                       min (if e /= (-1)
                            then fun b e
                            else (maxBound::Int))
                           (if f /= (-1)
                            then fun b f
                            else (maxBound::Int))
                  else maxBound::Int)
  (WormId 3)  -> (if c /= (-1)
                  then min (if d /= (-1)
                            then fun c d
                            else (maxBound::Int)) $!
                      min (if e /= (-1)
                           then fun c e
                           else (maxBound::Int))
                          (if f /= (-1)
                           then fun c f
                           else (maxBound::Int))
                  else maxBound::Int)
  (WormId 4)  -> (if d /= (-1)
                  then min (if a /= (-1)
                            then fun d a
                            else (maxBound::Int)) $!
                       min (if b /= (-1)
                            then fun d b
                            else (maxBound::Int))
                           (if c /= (-1)
                            then fun d c
                            else (maxBound::Int))
                  else maxBound::Int)
  (WormId 8)  -> (if e /= (-1)
                  then min (if a /= (-1)
                            then fun e a
                            else (maxBound::Int)) $!
                       min (if b /= (-1)
                            then fun e b
                            else (maxBound::Int))
                           (if c /= (-1)
                            then fun e c
                            else (maxBound::Int))
                  else maxBound::Int)
  (WormId 12) -> (if f /= (-1)
                  then min (if a /= (-1)
                            then fun f a
                            else (maxBound::Int)) $!
                       min (if b /= (-1)
                            then fun f b
                            else (maxBound::Int))
                           (if c /= (-1)
                            then fun f c
                            else (maxBound::Int))
                  else maxBound::Int)
  _           -> error $ "aListMinPairOff: " ++ show wormId'

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

-- Generated with:
-- putStrLn $ concat $ map (\ x -> "fromCoord " ++ show x ++ " = " ++ (show $ fromCoord x) ++ "\n") [0..(mapDim*mapDim)]
fromCoord :: Coord -> (Int, Int)
fromCoord 0    = (0,0)
fromCoord 1    = (1,0)
fromCoord 2    = (2,0)
fromCoord 3    = (3,0)
fromCoord 4    = (4,0)
fromCoord 5    = (5,0)
fromCoord 6    = (6,0)
fromCoord 7    = (7,0)
fromCoord 8    = (8,0)
fromCoord 9    = (9,0)
fromCoord 10   = (10,0)
fromCoord 11   = (11,0)
fromCoord 12   = (12,0)
fromCoord 13   = (13,0)
fromCoord 14   = (14,0)
fromCoord 15   = (15,0)
fromCoord 16   = (16,0)
fromCoord 17   = (17,0)
fromCoord 18   = (18,0)
fromCoord 19   = (19,0)
fromCoord 20   = (20,0)
fromCoord 21   = (21,0)
fromCoord 22   = (22,0)
fromCoord 23   = (23,0)
fromCoord 24   = (24,0)
fromCoord 25   = (25,0)
fromCoord 26   = (26,0)
fromCoord 27   = (27,0)
fromCoord 28   = (28,0)
fromCoord 29   = (29,0)
fromCoord 30   = (30,0)
fromCoord 31   = (31,0)
fromCoord 32   = (32,0)
fromCoord 33   = (0,1)
fromCoord 34   = (1,1)
fromCoord 35   = (2,1)
fromCoord 36   = (3,1)
fromCoord 37   = (4,1)
fromCoord 38   = (5,1)
fromCoord 39   = (6,1)
fromCoord 40   = (7,1)
fromCoord 41   = (8,1)
fromCoord 42   = (9,1)
fromCoord 43   = (10,1)
fromCoord 44   = (11,1)
fromCoord 45   = (12,1)
fromCoord 46   = (13,1)
fromCoord 47   = (14,1)
fromCoord 48   = (15,1)
fromCoord 49   = (16,1)
fromCoord 50   = (17,1)
fromCoord 51   = (18,1)
fromCoord 52   = (19,1)
fromCoord 53   = (20,1)
fromCoord 54   = (21,1)
fromCoord 55   = (22,1)
fromCoord 56   = (23,1)
fromCoord 57   = (24,1)
fromCoord 58   = (25,1)
fromCoord 59   = (26,1)
fromCoord 60   = (27,1)
fromCoord 61   = (28,1)
fromCoord 62   = (29,1)
fromCoord 63   = (30,1)
fromCoord 64   = (31,1)
fromCoord 65   = (32,1)
fromCoord 66   = (0,2)
fromCoord 67   = (1,2)
fromCoord 68   = (2,2)
fromCoord 69   = (3,2)
fromCoord 70   = (4,2)
fromCoord 71   = (5,2)
fromCoord 72   = (6,2)
fromCoord 73   = (7,2)
fromCoord 74   = (8,2)
fromCoord 75   = (9,2)
fromCoord 76   = (10,2)
fromCoord 77   = (11,2)
fromCoord 78   = (12,2)
fromCoord 79   = (13,2)
fromCoord 80   = (14,2)
fromCoord 81   = (15,2)
fromCoord 82   = (16,2)
fromCoord 83   = (17,2)
fromCoord 84   = (18,2)
fromCoord 85   = (19,2)
fromCoord 86   = (20,2)
fromCoord 87   = (21,2)
fromCoord 88   = (22,2)
fromCoord 89   = (23,2)
fromCoord 90   = (24,2)
fromCoord 91   = (25,2)
fromCoord 92   = (26,2)
fromCoord 93   = (27,2)
fromCoord 94   = (28,2)
fromCoord 95   = (29,2)
fromCoord 96   = (30,2)
fromCoord 97   = (31,2)
fromCoord 98   = (32,2)
fromCoord 99   = (0,3)
fromCoord 100  = (1,3)
fromCoord 101  = (2,3)
fromCoord 102  = (3,3)
fromCoord 103  = (4,3)
fromCoord 104  = (5,3)
fromCoord 105  = (6,3)
fromCoord 106  = (7,3)
fromCoord 107  = (8,3)
fromCoord 108  = (9,3)
fromCoord 109  = (10,3)
fromCoord 110  = (11,3)
fromCoord 111  = (12,3)
fromCoord 112  = (13,3)
fromCoord 113  = (14,3)
fromCoord 114  = (15,3)
fromCoord 115  = (16,3)
fromCoord 116  = (17,3)
fromCoord 117  = (18,3)
fromCoord 118  = (19,3)
fromCoord 119  = (20,3)
fromCoord 120  = (21,3)
fromCoord 121  = (22,3)
fromCoord 122  = (23,3)
fromCoord 123  = (24,3)
fromCoord 124  = (25,3)
fromCoord 125  = (26,3)
fromCoord 126  = (27,3)
fromCoord 127  = (28,3)
fromCoord 128  = (29,3)
fromCoord 129  = (30,3)
fromCoord 130  = (31,3)
fromCoord 131  = (32,3)
fromCoord 132  = (0,4)
fromCoord 133  = (1,4)
fromCoord 134  = (2,4)
fromCoord 135  = (3,4)
fromCoord 136  = (4,4)
fromCoord 137  = (5,4)
fromCoord 138  = (6,4)
fromCoord 139  = (7,4)
fromCoord 140  = (8,4)
fromCoord 141  = (9,4)
fromCoord 142  = (10,4)
fromCoord 143  = (11,4)
fromCoord 144  = (12,4)
fromCoord 145  = (13,4)
fromCoord 146  = (14,4)
fromCoord 147  = (15,4)
fromCoord 148  = (16,4)
fromCoord 149  = (17,4)
fromCoord 150  = (18,4)
fromCoord 151  = (19,4)
fromCoord 152  = (20,4)
fromCoord 153  = (21,4)
fromCoord 154  = (22,4)
fromCoord 155  = (23,4)
fromCoord 156  = (24,4)
fromCoord 157  = (25,4)
fromCoord 158  = (26,4)
fromCoord 159  = (27,4)
fromCoord 160  = (28,4)
fromCoord 161  = (29,4)
fromCoord 162  = (30,4)
fromCoord 163  = (31,4)
fromCoord 164  = (32,4)
fromCoord 165  = (0,5)
fromCoord 166  = (1,5)
fromCoord 167  = (2,5)
fromCoord 168  = (3,5)
fromCoord 169  = (4,5)
fromCoord 170  = (5,5)
fromCoord 171  = (6,5)
fromCoord 172  = (7,5)
fromCoord 173  = (8,5)
fromCoord 174  = (9,5)
fromCoord 175  = (10,5)
fromCoord 176  = (11,5)
fromCoord 177  = (12,5)
fromCoord 178  = (13,5)
fromCoord 179  = (14,5)
fromCoord 180  = (15,5)
fromCoord 181  = (16,5)
fromCoord 182  = (17,5)
fromCoord 183  = (18,5)
fromCoord 184  = (19,5)
fromCoord 185  = (20,5)
fromCoord 186  = (21,5)
fromCoord 187  = (22,5)
fromCoord 188  = (23,5)
fromCoord 189  = (24,5)
fromCoord 190  = (25,5)
fromCoord 191  = (26,5)
fromCoord 192  = (27,5)
fromCoord 193  = (28,5)
fromCoord 194  = (29,5)
fromCoord 195  = (30,5)
fromCoord 196  = (31,5)
fromCoord 197  = (32,5)
fromCoord 198  = (0,6)
fromCoord 199  = (1,6)
fromCoord 200  = (2,6)
fromCoord 201  = (3,6)
fromCoord 202  = (4,6)
fromCoord 203  = (5,6)
fromCoord 204  = (6,6)
fromCoord 205  = (7,6)
fromCoord 206  = (8,6)
fromCoord 207  = (9,6)
fromCoord 208  = (10,6)
fromCoord 209  = (11,6)
fromCoord 210  = (12,6)
fromCoord 211  = (13,6)
fromCoord 212  = (14,6)
fromCoord 213  = (15,6)
fromCoord 214  = (16,6)
fromCoord 215  = (17,6)
fromCoord 216  = (18,6)
fromCoord 217  = (19,6)
fromCoord 218  = (20,6)
fromCoord 219  = (21,6)
fromCoord 220  = (22,6)
fromCoord 221  = (23,6)
fromCoord 222  = (24,6)
fromCoord 223  = (25,6)
fromCoord 224  = (26,6)
fromCoord 225  = (27,6)
fromCoord 226  = (28,6)
fromCoord 227  = (29,6)
fromCoord 228  = (30,6)
fromCoord 229  = (31,6)
fromCoord 230  = (32,6)
fromCoord 231  = (0,7)
fromCoord 232  = (1,7)
fromCoord 233  = (2,7)
fromCoord 234  = (3,7)
fromCoord 235  = (4,7)
fromCoord 236  = (5,7)
fromCoord 237  = (6,7)
fromCoord 238  = (7,7)
fromCoord 239  = (8,7)
fromCoord 240  = (9,7)
fromCoord 241  = (10,7)
fromCoord 242  = (11,7)
fromCoord 243  = (12,7)
fromCoord 244  = (13,7)
fromCoord 245  = (14,7)
fromCoord 246  = (15,7)
fromCoord 247  = (16,7)
fromCoord 248  = (17,7)
fromCoord 249  = (18,7)
fromCoord 250  = (19,7)
fromCoord 251  = (20,7)
fromCoord 252  = (21,7)
fromCoord 253  = (22,7)
fromCoord 254  = (23,7)
fromCoord 255  = (24,7)
fromCoord 256  = (25,7)
fromCoord 257  = (26,7)
fromCoord 258  = (27,7)
fromCoord 259  = (28,7)
fromCoord 260  = (29,7)
fromCoord 261  = (30,7)
fromCoord 262  = (31,7)
fromCoord 263  = (32,7)
fromCoord 264  = (0,8)
fromCoord 265  = (1,8)
fromCoord 266  = (2,8)
fromCoord 267  = (3,8)
fromCoord 268  = (4,8)
fromCoord 269  = (5,8)
fromCoord 270  = (6,8)
fromCoord 271  = (7,8)
fromCoord 272  = (8,8)
fromCoord 273  = (9,8)
fromCoord 274  = (10,8)
fromCoord 275  = (11,8)
fromCoord 276  = (12,8)
fromCoord 277  = (13,8)
fromCoord 278  = (14,8)
fromCoord 279  = (15,8)
fromCoord 280  = (16,8)
fromCoord 281  = (17,8)
fromCoord 282  = (18,8)
fromCoord 283  = (19,8)
fromCoord 284  = (20,8)
fromCoord 285  = (21,8)
fromCoord 286  = (22,8)
fromCoord 287  = (23,8)
fromCoord 288  = (24,8)
fromCoord 289  = (25,8)
fromCoord 290  = (26,8)
fromCoord 291  = (27,8)
fromCoord 292  = (28,8)
fromCoord 293  = (29,8)
fromCoord 294  = (30,8)
fromCoord 295  = (31,8)
fromCoord 296  = (32,8)
fromCoord 297  = (0,9)
fromCoord 298  = (1,9)
fromCoord 299  = (2,9)
fromCoord 300  = (3,9)
fromCoord 301  = (4,9)
fromCoord 302  = (5,9)
fromCoord 303  = (6,9)
fromCoord 304  = (7,9)
fromCoord 305  = (8,9)
fromCoord 306  = (9,9)
fromCoord 307  = (10,9)
fromCoord 308  = (11,9)
fromCoord 309  = (12,9)
fromCoord 310  = (13,9)
fromCoord 311  = (14,9)
fromCoord 312  = (15,9)
fromCoord 313  = (16,9)
fromCoord 314  = (17,9)
fromCoord 315  = (18,9)
fromCoord 316  = (19,9)
fromCoord 317  = (20,9)
fromCoord 318  = (21,9)
fromCoord 319  = (22,9)
fromCoord 320  = (23,9)
fromCoord 321  = (24,9)
fromCoord 322  = (25,9)
fromCoord 323  = (26,9)
fromCoord 324  = (27,9)
fromCoord 325  = (28,9)
fromCoord 326  = (29,9)
fromCoord 327  = (30,9)
fromCoord 328  = (31,9)
fromCoord 329  = (32,9)
fromCoord 330  = (0,10)
fromCoord 331  = (1,10)
fromCoord 332  = (2,10)
fromCoord 333  = (3,10)
fromCoord 334  = (4,10)
fromCoord 335  = (5,10)
fromCoord 336  = (6,10)
fromCoord 337  = (7,10)
fromCoord 338  = (8,10)
fromCoord 339  = (9,10)
fromCoord 340  = (10,10)
fromCoord 341  = (11,10)
fromCoord 342  = (12,10)
fromCoord 343  = (13,10)
fromCoord 344  = (14,10)
fromCoord 345  = (15,10)
fromCoord 346  = (16,10)
fromCoord 347  = (17,10)
fromCoord 348  = (18,10)
fromCoord 349  = (19,10)
fromCoord 350  = (20,10)
fromCoord 351  = (21,10)
fromCoord 352  = (22,10)
fromCoord 353  = (23,10)
fromCoord 354  = (24,10)
fromCoord 355  = (25,10)
fromCoord 356  = (26,10)
fromCoord 357  = (27,10)
fromCoord 358  = (28,10)
fromCoord 359  = (29,10)
fromCoord 360  = (30,10)
fromCoord 361  = (31,10)
fromCoord 362  = (32,10)
fromCoord 363  = (0,11)
fromCoord 364  = (1,11)
fromCoord 365  = (2,11)
fromCoord 366  = (3,11)
fromCoord 367  = (4,11)
fromCoord 368  = (5,11)
fromCoord 369  = (6,11)
fromCoord 370  = (7,11)
fromCoord 371  = (8,11)
fromCoord 372  = (9,11)
fromCoord 373  = (10,11)
fromCoord 374  = (11,11)
fromCoord 375  = (12,11)
fromCoord 376  = (13,11)
fromCoord 377  = (14,11)
fromCoord 378  = (15,11)
fromCoord 379  = (16,11)
fromCoord 380  = (17,11)
fromCoord 381  = (18,11)
fromCoord 382  = (19,11)
fromCoord 383  = (20,11)
fromCoord 384  = (21,11)
fromCoord 385  = (22,11)
fromCoord 386  = (23,11)
fromCoord 387  = (24,11)
fromCoord 388  = (25,11)
fromCoord 389  = (26,11)
fromCoord 390  = (27,11)
fromCoord 391  = (28,11)
fromCoord 392  = (29,11)
fromCoord 393  = (30,11)
fromCoord 394  = (31,11)
fromCoord 395  = (32,11)
fromCoord 396  = (0,12)
fromCoord 397  = (1,12)
fromCoord 398  = (2,12)
fromCoord 399  = (3,12)
fromCoord 400  = (4,12)
fromCoord 401  = (5,12)
fromCoord 402  = (6,12)
fromCoord 403  = (7,12)
fromCoord 404  = (8,12)
fromCoord 405  = (9,12)
fromCoord 406  = (10,12)
fromCoord 407  = (11,12)
fromCoord 408  = (12,12)
fromCoord 409  = (13,12)
fromCoord 410  = (14,12)
fromCoord 411  = (15,12)
fromCoord 412  = (16,12)
fromCoord 413  = (17,12)
fromCoord 414  = (18,12)
fromCoord 415  = (19,12)
fromCoord 416  = (20,12)
fromCoord 417  = (21,12)
fromCoord 418  = (22,12)
fromCoord 419  = (23,12)
fromCoord 420  = (24,12)
fromCoord 421  = (25,12)
fromCoord 422  = (26,12)
fromCoord 423  = (27,12)
fromCoord 424  = (28,12)
fromCoord 425  = (29,12)
fromCoord 426  = (30,12)
fromCoord 427  = (31,12)
fromCoord 428  = (32,12)
fromCoord 429  = (0,13)
fromCoord 430  = (1,13)
fromCoord 431  = (2,13)
fromCoord 432  = (3,13)
fromCoord 433  = (4,13)
fromCoord 434  = (5,13)
fromCoord 435  = (6,13)
fromCoord 436  = (7,13)
fromCoord 437  = (8,13)
fromCoord 438  = (9,13)
fromCoord 439  = (10,13)
fromCoord 440  = (11,13)
fromCoord 441  = (12,13)
fromCoord 442  = (13,13)
fromCoord 443  = (14,13)
fromCoord 444  = (15,13)
fromCoord 445  = (16,13)
fromCoord 446  = (17,13)
fromCoord 447  = (18,13)
fromCoord 448  = (19,13)
fromCoord 449  = (20,13)
fromCoord 450  = (21,13)
fromCoord 451  = (22,13)
fromCoord 452  = (23,13)
fromCoord 453  = (24,13)
fromCoord 454  = (25,13)
fromCoord 455  = (26,13)
fromCoord 456  = (27,13)
fromCoord 457  = (28,13)
fromCoord 458  = (29,13)
fromCoord 459  = (30,13)
fromCoord 460  = (31,13)
fromCoord 461  = (32,13)
fromCoord 462  = (0,14)
fromCoord 463  = (1,14)
fromCoord 464  = (2,14)
fromCoord 465  = (3,14)
fromCoord 466  = (4,14)
fromCoord 467  = (5,14)
fromCoord 468  = (6,14)
fromCoord 469  = (7,14)
fromCoord 470  = (8,14)
fromCoord 471  = (9,14)
fromCoord 472  = (10,14)
fromCoord 473  = (11,14)
fromCoord 474  = (12,14)
fromCoord 475  = (13,14)
fromCoord 476  = (14,14)
fromCoord 477  = (15,14)
fromCoord 478  = (16,14)
fromCoord 479  = (17,14)
fromCoord 480  = (18,14)
fromCoord 481  = (19,14)
fromCoord 482  = (20,14)
fromCoord 483  = (21,14)
fromCoord 484  = (22,14)
fromCoord 485  = (23,14)
fromCoord 486  = (24,14)
fromCoord 487  = (25,14)
fromCoord 488  = (26,14)
fromCoord 489  = (27,14)
fromCoord 490  = (28,14)
fromCoord 491  = (29,14)
fromCoord 492  = (30,14)
fromCoord 493  = (31,14)
fromCoord 494  = (32,14)
fromCoord 495  = (0,15)
fromCoord 496  = (1,15)
fromCoord 497  = (2,15)
fromCoord 498  = (3,15)
fromCoord 499  = (4,15)
fromCoord 500  = (5,15)
fromCoord 501  = (6,15)
fromCoord 502  = (7,15)
fromCoord 503  = (8,15)
fromCoord 504  = (9,15)
fromCoord 505  = (10,15)
fromCoord 506  = (11,15)
fromCoord 507  = (12,15)
fromCoord 508  = (13,15)
fromCoord 509  = (14,15)
fromCoord 510  = (15,15)
fromCoord 511  = (16,15)
fromCoord 512  = (17,15)
fromCoord 513  = (18,15)
fromCoord 514  = (19,15)
fromCoord 515  = (20,15)
fromCoord 516  = (21,15)
fromCoord 517  = (22,15)
fromCoord 518  = (23,15)
fromCoord 519  = (24,15)
fromCoord 520  = (25,15)
fromCoord 521  = (26,15)
fromCoord 522  = (27,15)
fromCoord 523  = (28,15)
fromCoord 524  = (29,15)
fromCoord 525  = (30,15)
fromCoord 526  = (31,15)
fromCoord 527  = (32,15)
fromCoord 528  = (0,16)
fromCoord 529  = (1,16)
fromCoord 530  = (2,16)
fromCoord 531  = (3,16)
fromCoord 532  = (4,16)
fromCoord 533  = (5,16)
fromCoord 534  = (6,16)
fromCoord 535  = (7,16)
fromCoord 536  = (8,16)
fromCoord 537  = (9,16)
fromCoord 538  = (10,16)
fromCoord 539  = (11,16)
fromCoord 540  = (12,16)
fromCoord 541  = (13,16)
fromCoord 542  = (14,16)
fromCoord 543  = (15,16)
fromCoord 544  = (16,16)
fromCoord 545  = (17,16)
fromCoord 546  = (18,16)
fromCoord 547  = (19,16)
fromCoord 548  = (20,16)
fromCoord 549  = (21,16)
fromCoord 550  = (22,16)
fromCoord 551  = (23,16)
fromCoord 552  = (24,16)
fromCoord 553  = (25,16)
fromCoord 554  = (26,16)
fromCoord 555  = (27,16)
fromCoord 556  = (28,16)
fromCoord 557  = (29,16)
fromCoord 558  = (30,16)
fromCoord 559  = (31,16)
fromCoord 560  = (32,16)
fromCoord 561  = (0,17)
fromCoord 562  = (1,17)
fromCoord 563  = (2,17)
fromCoord 564  = (3,17)
fromCoord 565  = (4,17)
fromCoord 566  = (5,17)
fromCoord 567  = (6,17)
fromCoord 568  = (7,17)
fromCoord 569  = (8,17)
fromCoord 570  = (9,17)
fromCoord 571  = (10,17)
fromCoord 572  = (11,17)
fromCoord 573  = (12,17)
fromCoord 574  = (13,17)
fromCoord 575  = (14,17)
fromCoord 576  = (15,17)
fromCoord 577  = (16,17)
fromCoord 578  = (17,17)
fromCoord 579  = (18,17)
fromCoord 580  = (19,17)
fromCoord 581  = (20,17)
fromCoord 582  = (21,17)
fromCoord 583  = (22,17)
fromCoord 584  = (23,17)
fromCoord 585  = (24,17)
fromCoord 586  = (25,17)
fromCoord 587  = (26,17)
fromCoord 588  = (27,17)
fromCoord 589  = (28,17)
fromCoord 590  = (29,17)
fromCoord 591  = (30,17)
fromCoord 592  = (31,17)
fromCoord 593  = (32,17)
fromCoord 594  = (0,18)
fromCoord 595  = (1,18)
fromCoord 596  = (2,18)
fromCoord 597  = (3,18)
fromCoord 598  = (4,18)
fromCoord 599  = (5,18)
fromCoord 600  = (6,18)
fromCoord 601  = (7,18)
fromCoord 602  = (8,18)
fromCoord 603  = (9,18)
fromCoord 604  = (10,18)
fromCoord 605  = (11,18)
fromCoord 606  = (12,18)
fromCoord 607  = (13,18)
fromCoord 608  = (14,18)
fromCoord 609  = (15,18)
fromCoord 610  = (16,18)
fromCoord 611  = (17,18)
fromCoord 612  = (18,18)
fromCoord 613  = (19,18)
fromCoord 614  = (20,18)
fromCoord 615  = (21,18)
fromCoord 616  = (22,18)
fromCoord 617  = (23,18)
fromCoord 618  = (24,18)
fromCoord 619  = (25,18)
fromCoord 620  = (26,18)
fromCoord 621  = (27,18)
fromCoord 622  = (28,18)
fromCoord 623  = (29,18)
fromCoord 624  = (30,18)
fromCoord 625  = (31,18)
fromCoord 626  = (32,18)
fromCoord 627  = (0,19)
fromCoord 628  = (1,19)
fromCoord 629  = (2,19)
fromCoord 630  = (3,19)
fromCoord 631  = (4,19)
fromCoord 632  = (5,19)
fromCoord 633  = (6,19)
fromCoord 634  = (7,19)
fromCoord 635  = (8,19)
fromCoord 636  = (9,19)
fromCoord 637  = (10,19)
fromCoord 638  = (11,19)
fromCoord 639  = (12,19)
fromCoord 640  = (13,19)
fromCoord 641  = (14,19)
fromCoord 642  = (15,19)
fromCoord 643  = (16,19)
fromCoord 644  = (17,19)
fromCoord 645  = (18,19)
fromCoord 646  = (19,19)
fromCoord 647  = (20,19)
fromCoord 648  = (21,19)
fromCoord 649  = (22,19)
fromCoord 650  = (23,19)
fromCoord 651  = (24,19)
fromCoord 652  = (25,19)
fromCoord 653  = (26,19)
fromCoord 654  = (27,19)
fromCoord 655  = (28,19)
fromCoord 656  = (29,19)
fromCoord 657  = (30,19)
fromCoord 658  = (31,19)
fromCoord 659  = (32,19)
fromCoord 660  = (0,20)
fromCoord 661  = (1,20)
fromCoord 662  = (2,20)
fromCoord 663  = (3,20)
fromCoord 664  = (4,20)
fromCoord 665  = (5,20)
fromCoord 666  = (6,20)
fromCoord 667  = (7,20)
fromCoord 668  = (8,20)
fromCoord 669  = (9,20)
fromCoord 670  = (10,20)
fromCoord 671  = (11,20)
fromCoord 672  = (12,20)
fromCoord 673  = (13,20)
fromCoord 674  = (14,20)
fromCoord 675  = (15,20)
fromCoord 676  = (16,20)
fromCoord 677  = (17,20)
fromCoord 678  = (18,20)
fromCoord 679  = (19,20)
fromCoord 680  = (20,20)
fromCoord 681  = (21,20)
fromCoord 682  = (22,20)
fromCoord 683  = (23,20)
fromCoord 684  = (24,20)
fromCoord 685  = (25,20)
fromCoord 686  = (26,20)
fromCoord 687  = (27,20)
fromCoord 688  = (28,20)
fromCoord 689  = (29,20)
fromCoord 690  = (30,20)
fromCoord 691  = (31,20)
fromCoord 692  = (32,20)
fromCoord 693  = (0,21)
fromCoord 694  = (1,21)
fromCoord 695  = (2,21)
fromCoord 696  = (3,21)
fromCoord 697  = (4,21)
fromCoord 698  = (5,21)
fromCoord 699  = (6,21)
fromCoord 700  = (7,21)
fromCoord 701  = (8,21)
fromCoord 702  = (9,21)
fromCoord 703  = (10,21)
fromCoord 704  = (11,21)
fromCoord 705  = (12,21)
fromCoord 706  = (13,21)
fromCoord 707  = (14,21)
fromCoord 708  = (15,21)
fromCoord 709  = (16,21)
fromCoord 710  = (17,21)
fromCoord 711  = (18,21)
fromCoord 712  = (19,21)
fromCoord 713  = (20,21)
fromCoord 714  = (21,21)
fromCoord 715  = (22,21)
fromCoord 716  = (23,21)
fromCoord 717  = (24,21)
fromCoord 718  = (25,21)
fromCoord 719  = (26,21)
fromCoord 720  = (27,21)
fromCoord 721  = (28,21)
fromCoord 722  = (29,21)
fromCoord 723  = (30,21)
fromCoord 724  = (31,21)
fromCoord 725  = (32,21)
fromCoord 726  = (0,22)
fromCoord 727  = (1,22)
fromCoord 728  = (2,22)
fromCoord 729  = (3,22)
fromCoord 730  = (4,22)
fromCoord 731  = (5,22)
fromCoord 732  = (6,22)
fromCoord 733  = (7,22)
fromCoord 734  = (8,22)
fromCoord 735  = (9,22)
fromCoord 736  = (10,22)
fromCoord 737  = (11,22)
fromCoord 738  = (12,22)
fromCoord 739  = (13,22)
fromCoord 740  = (14,22)
fromCoord 741  = (15,22)
fromCoord 742  = (16,22)
fromCoord 743  = (17,22)
fromCoord 744  = (18,22)
fromCoord 745  = (19,22)
fromCoord 746  = (20,22)
fromCoord 747  = (21,22)
fromCoord 748  = (22,22)
fromCoord 749  = (23,22)
fromCoord 750  = (24,22)
fromCoord 751  = (25,22)
fromCoord 752  = (26,22)
fromCoord 753  = (27,22)
fromCoord 754  = (28,22)
fromCoord 755  = (29,22)
fromCoord 756  = (30,22)
fromCoord 757  = (31,22)
fromCoord 758  = (32,22)
fromCoord 759  = (0,23)
fromCoord 760  = (1,23)
fromCoord 761  = (2,23)
fromCoord 762  = (3,23)
fromCoord 763  = (4,23)
fromCoord 764  = (5,23)
fromCoord 765  = (6,23)
fromCoord 766  = (7,23)
fromCoord 767  = (8,23)
fromCoord 768  = (9,23)
fromCoord 769  = (10,23)
fromCoord 770  = (11,23)
fromCoord 771  = (12,23)
fromCoord 772  = (13,23)
fromCoord 773  = (14,23)
fromCoord 774  = (15,23)
fromCoord 775  = (16,23)
fromCoord 776  = (17,23)
fromCoord 777  = (18,23)
fromCoord 778  = (19,23)
fromCoord 779  = (20,23)
fromCoord 780  = (21,23)
fromCoord 781  = (22,23)
fromCoord 782  = (23,23)
fromCoord 783  = (24,23)
fromCoord 784  = (25,23)
fromCoord 785  = (26,23)
fromCoord 786  = (27,23)
fromCoord 787  = (28,23)
fromCoord 788  = (29,23)
fromCoord 789  = (30,23)
fromCoord 790  = (31,23)
fromCoord 791  = (32,23)
fromCoord 792  = (0,24)
fromCoord 793  = (1,24)
fromCoord 794  = (2,24)
fromCoord 795  = (3,24)
fromCoord 796  = (4,24)
fromCoord 797  = (5,24)
fromCoord 798  = (6,24)
fromCoord 799  = (7,24)
fromCoord 800  = (8,24)
fromCoord 801  = (9,24)
fromCoord 802  = (10,24)
fromCoord 803  = (11,24)
fromCoord 804  = (12,24)
fromCoord 805  = (13,24)
fromCoord 806  = (14,24)
fromCoord 807  = (15,24)
fromCoord 808  = (16,24)
fromCoord 809  = (17,24)
fromCoord 810  = (18,24)
fromCoord 811  = (19,24)
fromCoord 812  = (20,24)
fromCoord 813  = (21,24)
fromCoord 814  = (22,24)
fromCoord 815  = (23,24)
fromCoord 816  = (24,24)
fromCoord 817  = (25,24)
fromCoord 818  = (26,24)
fromCoord 819  = (27,24)
fromCoord 820  = (28,24)
fromCoord 821  = (29,24)
fromCoord 822  = (30,24)
fromCoord 823  = (31,24)
fromCoord 824  = (32,24)
fromCoord 825  = (0,25)
fromCoord 826  = (1,25)
fromCoord 827  = (2,25)
fromCoord 828  = (3,25)
fromCoord 829  = (4,25)
fromCoord 830  = (5,25)
fromCoord 831  = (6,25)
fromCoord 832  = (7,25)
fromCoord 833  = (8,25)
fromCoord 834  = (9,25)
fromCoord 835  = (10,25)
fromCoord 836  = (11,25)
fromCoord 837  = (12,25)
fromCoord 838  = (13,25)
fromCoord 839  = (14,25)
fromCoord 840  = (15,25)
fromCoord 841  = (16,25)
fromCoord 842  = (17,25)
fromCoord 843  = (18,25)
fromCoord 844  = (19,25)
fromCoord 845  = (20,25)
fromCoord 846  = (21,25)
fromCoord 847  = (22,25)
fromCoord 848  = (23,25)
fromCoord 849  = (24,25)
fromCoord 850  = (25,25)
fromCoord 851  = (26,25)
fromCoord 852  = (27,25)
fromCoord 853  = (28,25)
fromCoord 854  = (29,25)
fromCoord 855  = (30,25)
fromCoord 856  = (31,25)
fromCoord 857  = (32,25)
fromCoord 858  = (0,26)
fromCoord 859  = (1,26)
fromCoord 860  = (2,26)
fromCoord 861  = (3,26)
fromCoord 862  = (4,26)
fromCoord 863  = (5,26)
fromCoord 864  = (6,26)
fromCoord 865  = (7,26)
fromCoord 866  = (8,26)
fromCoord 867  = (9,26)
fromCoord 868  = (10,26)
fromCoord 869  = (11,26)
fromCoord 870  = (12,26)
fromCoord 871  = (13,26)
fromCoord 872  = (14,26)
fromCoord 873  = (15,26)
fromCoord 874  = (16,26)
fromCoord 875  = (17,26)
fromCoord 876  = (18,26)
fromCoord 877  = (19,26)
fromCoord 878  = (20,26)
fromCoord 879  = (21,26)
fromCoord 880  = (22,26)
fromCoord 881  = (23,26)
fromCoord 882  = (24,26)
fromCoord 883  = (25,26)
fromCoord 884  = (26,26)
fromCoord 885  = (27,26)
fromCoord 886  = (28,26)
fromCoord 887  = (29,26)
fromCoord 888  = (30,26)
fromCoord 889  = (31,26)
fromCoord 890  = (32,26)
fromCoord 891  = (0,27)
fromCoord 892  = (1,27)
fromCoord 893  = (2,27)
fromCoord 894  = (3,27)
fromCoord 895  = (4,27)
fromCoord 896  = (5,27)
fromCoord 897  = (6,27)
fromCoord 898  = (7,27)
fromCoord 899  = (8,27)
fromCoord 900  = (9,27)
fromCoord 901  = (10,27)
fromCoord 902  = (11,27)
fromCoord 903  = (12,27)
fromCoord 904  = (13,27)
fromCoord 905  = (14,27)
fromCoord 906  = (15,27)
fromCoord 907  = (16,27)
fromCoord 908  = (17,27)
fromCoord 909  = (18,27)
fromCoord 910  = (19,27)
fromCoord 911  = (20,27)
fromCoord 912  = (21,27)
fromCoord 913  = (22,27)
fromCoord 914  = (23,27)
fromCoord 915  = (24,27)
fromCoord 916  = (25,27)
fromCoord 917  = (26,27)
fromCoord 918  = (27,27)
fromCoord 919  = (28,27)
fromCoord 920  = (29,27)
fromCoord 921  = (30,27)
fromCoord 922  = (31,27)
fromCoord 923  = (32,27)
fromCoord 924  = (0,28)
fromCoord 925  = (1,28)
fromCoord 926  = (2,28)
fromCoord 927  = (3,28)
fromCoord 928  = (4,28)
fromCoord 929  = (5,28)
fromCoord 930  = (6,28)
fromCoord 931  = (7,28)
fromCoord 932  = (8,28)
fromCoord 933  = (9,28)
fromCoord 934  = (10,28)
fromCoord 935  = (11,28)
fromCoord 936  = (12,28)
fromCoord 937  = (13,28)
fromCoord 938  = (14,28)
fromCoord 939  = (15,28)
fromCoord 940  = (16,28)
fromCoord 941  = (17,28)
fromCoord 942  = (18,28)
fromCoord 943  = (19,28)
fromCoord 944  = (20,28)
fromCoord 945  = (21,28)
fromCoord 946  = (22,28)
fromCoord 947  = (23,28)
fromCoord 948  = (24,28)
fromCoord 949  = (25,28)
fromCoord 950  = (26,28)
fromCoord 951  = (27,28)
fromCoord 952  = (28,28)
fromCoord 953  = (29,28)
fromCoord 954  = (30,28)
fromCoord 955  = (31,28)
fromCoord 956  = (32,28)
fromCoord 957  = (0,29)
fromCoord 958  = (1,29)
fromCoord 959  = (2,29)
fromCoord 960  = (3,29)
fromCoord 961  = (4,29)
fromCoord 962  = (5,29)
fromCoord 963  = (6,29)
fromCoord 964  = (7,29)
fromCoord 965  = (8,29)
fromCoord 966  = (9,29)
fromCoord 967  = (10,29)
fromCoord 968  = (11,29)
fromCoord 969  = (12,29)
fromCoord 970  = (13,29)
fromCoord 971  = (14,29)
fromCoord 972  = (15,29)
fromCoord 973  = (16,29)
fromCoord 974  = (17,29)
fromCoord 975  = (18,29)
fromCoord 976  = (19,29)
fromCoord 977  = (20,29)
fromCoord 978  = (21,29)
fromCoord 979  = (22,29)
fromCoord 980  = (23,29)
fromCoord 981  = (24,29)
fromCoord 982  = (25,29)
fromCoord 983  = (26,29)
fromCoord 984  = (27,29)
fromCoord 985  = (28,29)
fromCoord 986  = (29,29)
fromCoord 987  = (30,29)
fromCoord 988  = (31,29)
fromCoord 989  = (32,29)
fromCoord 990  = (0,30)
fromCoord 991  = (1,30)
fromCoord 992  = (2,30)
fromCoord 993  = (3,30)
fromCoord 994  = (4,30)
fromCoord 995  = (5,30)
fromCoord 996  = (6,30)
fromCoord 997  = (7,30)
fromCoord 998  = (8,30)
fromCoord 999  = (9,30)
fromCoord 1000 = (10,30)
fromCoord 1001 = (11,30)
fromCoord 1002 = (12,30)
fromCoord 1003 = (13,30)
fromCoord 1004 = (14,30)
fromCoord 1005 = (15,30)
fromCoord 1006 = (16,30)
fromCoord 1007 = (17,30)
fromCoord 1008 = (18,30)
fromCoord 1009 = (19,30)
fromCoord 1010 = (20,30)
fromCoord 1011 = (21,30)
fromCoord 1012 = (22,30)
fromCoord 1013 = (23,30)
fromCoord 1014 = (24,30)
fromCoord 1015 = (25,30)
fromCoord 1016 = (26,30)
fromCoord 1017 = (27,30)
fromCoord 1018 = (28,30)
fromCoord 1019 = (29,30)
fromCoord 1020 = (30,30)
fromCoord 1021 = (31,30)
fromCoord 1022 = (32,30)
fromCoord 1023 = (0,31)
fromCoord 1024 = (1,31)
fromCoord 1025 = (2,31)
fromCoord 1026 = (3,31)
fromCoord 1027 = (4,31)
fromCoord 1028 = (5,31)
fromCoord 1029 = (6,31)
fromCoord 1030 = (7,31)
fromCoord 1031 = (8,31)
fromCoord 1032 = (9,31)
fromCoord 1033 = (10,31)
fromCoord 1034 = (11,31)
fromCoord 1035 = (12,31)
fromCoord 1036 = (13,31)
fromCoord 1037 = (14,31)
fromCoord 1038 = (15,31)
fromCoord 1039 = (16,31)
fromCoord 1040 = (17,31)
fromCoord 1041 = (18,31)
fromCoord 1042 = (19,31)
fromCoord 1043 = (20,31)
fromCoord 1044 = (21,31)
fromCoord 1045 = (22,31)
fromCoord 1046 = (23,31)
fromCoord 1047 = (24,31)
fromCoord 1048 = (25,31)
fromCoord 1049 = (26,31)
fromCoord 1050 = (27,31)
fromCoord 1051 = (28,31)
fromCoord 1052 = (29,31)
fromCoord 1053 = (30,31)
fromCoord 1054 = (31,31)
fromCoord 1055 = (32,31)
fromCoord 1056 = (0,32)
fromCoord 1057 = (1,32)
fromCoord 1058 = (2,32)
fromCoord 1059 = (3,32)
fromCoord 1060 = (4,32)
fromCoord 1061 = (5,32)
fromCoord 1062 = (6,32)
fromCoord 1063 = (7,32)
fromCoord 1064 = (8,32)
fromCoord 1065 = (9,32)
fromCoord 1066 = (10,32)
fromCoord 1067 = (11,32)
fromCoord 1068 = (12,32)
fromCoord 1069 = (13,32)
fromCoord 1070 = (14,32)
fromCoord 1071 = (15,32)
fromCoord 1072 = (16,32)
fromCoord 1073 = (17,32)
fromCoord 1074 = (18,32)
fromCoord 1075 = (19,32)
fromCoord 1076 = (20,32)
fromCoord 1077 = (21,32)
fromCoord 1078 = (22,32)
fromCoord 1079 = (23,32)
fromCoord 1080 = (24,32)
fromCoord 1081 = (25,32)
fromCoord 1082 = (26,32)
fromCoord 1083 = (27,32)
fromCoord 1084 = (28,32)
fromCoord 1085 = (29,32)
fromCoord 1086 = (30,32)
fromCoord 1087 = (31,32)
fromCoord 1088 = (32,32)
fromCoord x    = error $ "fromCoord: " ++ show x

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
  | isAMoveMove   dir   = if moveWouldGoOOB xy dir
                          then "nothing"
                          else "move " ++ (showCoord $ displaceCoordByMove xy dir)
  -- Dig
  | isADigMove    dir   = if moveWouldGoOOB xy (Move $ x - 8)
                          then "nothing"
                          else "dig "  ++ (showCoord $ displaceCoordByMove xy (Move $ x - 8))
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
     "; " ++
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
  let stateAfterTickingFreeze  = tickFreezeDurations state
      (myMove,  opponentsMove) = freezeActions stateAfterTickingFreeze $ toMoves moves
  in -- assertValidState state  myMove  opponentsMove  $

     -- Post movement actions
     incrementRound                                  $
     setOpponentsLastMove      state   opponentsMove $
     advanceWormSelections                           $
     cleanUpDeadWorms                                $

     -- Make moves
     go (decodeMoveType myMove) (decodeMoveType opponentsMove) myMove opponentsMove $

     -- Pre move actions
     dealLavaDamage stateAfterTickingFreeze
  where
    go !myMoveType !opponentsMoveType !myMove !opponentsMove state' =
      -- For Debugging
      -- $ Debug.Trace.trace ("makeMove' with " ++ show myMoveType ++ ", " ++ show opponentsMoveType ++ ", " ++ show myMove ++ ", " ++ show opponentsMove)
      makeMove' myMoveType opponentsMoveType state'
      where
        wormHealths'              = wormHealths state'
        wormPositions'            = wormPositions state'
        gameMap'                  = gameMap state'
        thisWormsCoord'           = thisWormsCoord state'
        thatWormsCoord'           = thatWormsCoord state'
        thisWormsId               = thisPlayersCurrentWormId state'
        thatWormsId               = thatPlayersCurrentWormId state'
        thisWormHasBananasLeft'   = wormHasBananasLeft thisWormsId state'
        thatWormHasBananasLeft'   = wormHasBananasLeft thatWormsId state'
        thisWormHasSnowballsLeft' = wormHasSnowballsLeft thisWormsId state'
        thatWormHasSnowballsLeft' = wormHasSnowballsLeft thatWormsId state'
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
             makeMySelection myMove
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

-- The ruinEverythingBit ruins my model of going fowards and backwards
-- between combined and non-combined moves.  It will only work in the
-- case of freezing a move subsequent to the select.
ruinEverythingBit :: Int
ruinEverythingBit = shiftL 1 (2 * selectEncodingRange)

keepOnlySelection :: Move -> Move
keepOnlySelection (Move x) = Move $ (x .&. selectMoveMask) .|. ruinEverythingBit

freezeActions :: State -> (Move, Move) -> (Move, Move)
freezeActions state (myMove, opponentsMove) =
  let thisWormId       = if hasASelection myMove
                         then thisPlayersCurrentWormId $ makeMySelection myMove state
                         else thisPlayersCurrentWormId state
      thatWormId       = if hasASelection opponentsMove
                         then thatPlayersCurrentWormId $ makeOpponentsSelection opponentsMove state
                         else thatPlayersCurrentWormId state
      frozenDurations' = frozenDurations state
      freezeThisWorm   = aListContainsId thisWormId frozenDurations'
      freezeThatWorm   = aListContainsId thatWormId frozenDurations'
  in case (freezeThisWorm, freezeThatWorm) of
    (True,  True)  -> (keepOnlySelection myMove, keepOnlySelection opponentsMove)
    (True,  False) -> (keepOnlySelection myMove, opponentsMove)
    (False, True)  -> (myMove,                   keepOnlySelection opponentsMove)
    (False, False) -> (myMove,                   opponentsMove)

tickFreezeDurations :: ModifyState
tickFreezeDurations =
  withFrozenDurations (aListMap decFrozen)
  where
    decFrozen (-1) = (-1)
    decFrozen 1    = (-1)
    decFrozen x    = x - 1

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
setOpponentsLastMove :: State -> Move -> ModifyState
setOpponentsLastMove stateWhenMoveWasMade move' state =
  state { opponentsLastCommand =
          Just $ if move' == doNothing ||
                    (isAMoveMove move' && moveWouldGoOOB (thatWormsCoord stateWhenMoveWasMade) move') ||
                    (\ (Move x) -> x > 4096) move'
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
hasASelection (Move x) =
  let x' = x .&. selectMoveMask
  in x' >= 256 && x' < 4096

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

wormHasBananasLeft :: WormId -> State -> Bool
wormHasBananasLeft wormId' = aListContainsId wormId' . wormBananas

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
              penaliseThisPlayerForKillingOwnWorm
              penaliseThisPlayerForAnInvalidCommand

makeOpponentsBananaMove :: WormId -> Coord -> Bool -> GameMap -> WormPositions -> Move -> ModifyState
makeOpponentsBananaMove =
  throwBanana decrementThatWormsBananas
              awardPointsToThatPlayerForDigging
              awardPointsToThatPlayerForDamage
              penaliseThatPlayerForDamage
              awardPointsToThatPlayerForMissing
              awardPointsToThatPlayerForKillingAnEnemy
              penaliseThatPlayerForKillingOwnWorm
              penaliseThatPlayerForAnInvalidCommand

throwBanana :: ModifyState -> ModifyState -> (Int -> ModifyState) -> (Int -> ModifyState) -> ModifyState -> ModifyState -> ModifyState -> ModifyState -> WormId -> Coord -> Bool -> GameMap -> WormPositions -> Move -> ModifyState
throwBanana decrementWormsBananas'
            awardPointsToPlayerForDigging'
            awardPointsToPlayerForDamage'
            penalisePlayerForDamage'
            awardPointsToPlayerForMissing'
            awardPointsToPlayerForKillingAnEnemy'
            penalisePlayerForKillingOwnWorm'
            penalisePlayerForAnInvalidCommand'
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
                      penalisePlayerForKillingOwnWorm'
                      target
     else penalisePlayerForAnInvalidCommand'

makeMySnowballMove ::  WormId -> Coord -> Bool -> GameMap -> WormPositions -> Move -> ModifyState
makeMySnowballMove =
  throwSnowball decrementThisWormsSnowballs
                awardThisPlayerForFreezingAWorm
                penaliseThisPlayerForFreezingAWorm
                awardPointsToThisPlayerForMissing
                penaliseThisPlayerForAnInvalidCommand

makeOpponentsSnowballMove :: WormId -> Coord -> Bool -> GameMap -> WormPositions -> Move -> ModifyState
makeOpponentsSnowballMove =
  throwSnowball decrementThatWormsSnowballs
                awardThatPlayerForFreezingAWorm
                penaliseThatPlayerForFreezingAWorm
                awardPointsToThatPlayerForMissing
                penaliseThatPlayerForAnInvalidCommand

throwSnowball :: ModifyState -> ModifyState -> ModifyState -> ModifyState -> ModifyState -> WormId -> Coord -> Bool -> GameMap -> WormPositions -> Move -> ModifyState
throwSnowball decrementWormsSnowballs'
              awardPlayerForFreezingAWorm'
              penalisePlayerForFreezingAWorm'
              awardPointsToPlayerForMissing'
              penalisePlayerForInvalidCommand'
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
     else penalisePlayerForInvalidCommand'

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

wormHasSnowballsLeft :: WormId -> State -> Bool
wormHasSnowballsLeft wormId' = aListContainsId wormId' . wormSnowballs

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

bananaBlast :: WormId -> GameMap -> WormPositions -> ModifyState -> (Int -> ModifyState) -> (Int -> ModifyState) -> ModifyState -> ModifyState -> ModifyState -> Coord -> ModifyState
bananaBlast wormId'
            gameMapPriorToBlast
            wormPositions'
            awardPointsForDigging'
            awardPointsForDamage'
            penaliseForDamage'
            awardPointsForMissing'
            rewardKill
            penaliseKill
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
                  in harmWorm
                     wormId'
                     wormPositions'
                     damage'
                     (penaliseForDamage'    damage')
                     (awardPointsForDamage' damage')
                     rewardKill
                     penaliseKill
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
advanceThisWormSelection state =
  advanceWormSelectionByWorms (thisPlayersCurrentWormId state) mapThisPlayer state

advanceThatWormSelection :: ModifyState
advanceThatWormSelection state =
  advanceWormSelectionByWorms (thatPlayersCurrentWormId state) mapThatPlayer state

withCurrentWormId :: WormId -> Player -> Player
withCurrentWormId wormId' (Player score' _ selections') = (Player score' wormId' selections')

-- ASSUME: that there are worm ids to search through
nextWormId :: WormId -> WormHealths -> WormId
nextWormId (WormId 1) wormHealths'
  | aListContainsId (WormId 2) wormHealths' = (WormId 2)
  | aListContainsId (WormId 3) wormHealths' = (WormId 3)
  | otherwise                               = (WormId 1)
nextWormId (WormId 2) wormHealths'
  | aListContainsId (WormId 3) wormHealths' = (WormId 3)
  | aListContainsId (WormId 1) wormHealths' = (WormId 1)
  | otherwise                               = (WormId 2)
nextWormId (WormId 3) wormHealths'
  | aListContainsId (WormId 1) wormHealths' = (WormId 1)
  | aListContainsId (WormId 2) wormHealths' = (WormId 2)
  | otherwise                               = (WormId 3)
nextWormId (WormId 4) wormHealths'
  | aListContainsId (WormId 8)  wormHealths' = (WormId 8)
  | aListContainsId (WormId 12) wormHealths' = (WormId 12)
  | otherwise                                = (WormId 4)
nextWormId (WormId 8) wormHealths'
  | aListContainsId (WormId 12) wormHealths' = (WormId 12)
  | aListContainsId (WormId 4)  wormHealths' = (WormId 4)
  | otherwise                                = (WormId 8)
nextWormId (WormId 12) wormHealths'
  | aListContainsId (WormId 4)  wormHealths' = (WormId 4)
  | aListContainsId (WormId 8)  wormHealths' = (WormId 8)
  | otherwise                                = (WormId 12)
nextWormId wormId' _ = error $ "Next worm id of: " ++ show wormId'

advanceWormSelectionByWorms :: WormId -> (ModifyPlayer -> ModifyState) -> ModifyState
advanceWormSelectionByWorms currentWormId' mapPlayer state@(State { wormHealths = wormHealths' }) =
  mapPlayer (withCurrentWormId (nextWormId currentWormId' wormHealths')) state

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
                penaliseThisPlayerForKillingOwnWorm
                awardPointsToThisPlayerForMissing

makeOpponentsShootMove :: Coord -> WormId -> GameMap -> WormPositions -> Move -> ModifyState
makeOpponentsShootMove =
  makeShootMove penaliseThatPlayerForHittingHisFriendlyWorm
                awardPointsToThatPlayerForHittingAnEnemy
                awardPointsToThatPlayerForKillingAnEnemy
                penaliseThatPlayerForKillingOwnWorm
                awardPointsToThatPlayerForMissing

makeShootMove :: ModifyState -> ModifyState -> ModifyState -> ModifyState -> ModifyState -> Coord -> WormId -> GameMap -> WormPositions -> Move -> ModifyState
makeShootMove penalise
              awardPlayer
              awardPlayerForKill
              penalisePlayerForKill
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
                                 penalisePlayerForKill
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

penaliseThisPlayerForKillingOwnWorm :: ModifyState
penaliseThisPlayerForKillingOwnWorm =
  mapThisPlayer penaliseForKillingOwnWorm

penaliseThatPlayerForKillingOwnWorm :: ModifyState
penaliseThatPlayerForKillingOwnWorm =
  mapThatPlayer penaliseForKillingOwnWorm

penaliseForKillingOwnWorm :: ModifyPlayer
penaliseForKillingOwnWorm = modifyScore (-40)

harmWormWithRocket :: WormId -> WormPositions -> ModifyState -> ModifyState -> ModifyState -> ModifyState -> Coord -> ModifyState
harmWormWithRocket wormId'
                   wormPositions'
                   penalisePlayer
                   awardPlayer
                   awardPlayerForKill
                   penalisePlayerForKill =
  harmWorm wormId'
           wormPositions'
           rocketDamage
           penalisePlayer
           awardPlayer
           awardPlayerForKill
           penalisePlayerForKill

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
harmWorm :: WormId -> WormPositions -> Int -> ModifyState -> ModifyState -> ModifyState -> ModifyState -> Coord -> ModifyState
harmWorm shootingWormId'
         wormPositions'
         damage'
         penalisePlayer
         awardPlayer
         awardPlayerForKill
         penalisePlayerForKill
         coord
         state =
  let wormId'       = aListFindIdByData coord wormPositions'
      samePlayer    = wormsBelongToSamePlayer wormId' shootingWormId'
      wormHealth'   = aListFindDataById wormId' $ wormHealths state
      wormDied      = wormHealth' <= damage'
      awardPoints   = if wormDied then (awardPlayer    . awardPlayerForKill)    else awardPlayer
      penalise'     = if wormDied then (penalisePlayer . penalisePlayerForKill) else penalisePlayer
      dishOutPoints = if samePlayer
                      then penalise'
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
  if isOnBoundary coord' then HitNothing else go 4 (coord' + add)
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

opponentsMovesFromTree :: SearchTree -> SuccessRecords
opponentsMovesFromTree (SearchedLevel   _ _ (OpponentsMoves opponentsMoves) _) = opponentsMoves
opponentsMovesFromTree (UnSearchedLevel _ _ (OpponentsMoves opponentsMoves))   = opponentsMoves
opponentsMovesFromTree SearchFront                                             =
  error $ "opponentsMovesFromTree of SearchFront"

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
iterativelyImproveSearch :: StdGen -> State -> SearchTree -> CommsChannel (CombinedMove, Bool, State) -> CommsVariable SearchTree -> IO ()
iterativelyImproveSearch !gen !initialState tree stateChannel treeVariable = do
  let treeFromPreviousRound = if strategy == Dig || strategy == GetToTheChoppa
                              then SearchFront
                              else tree
  E.catch (go gen iterationsBeforeComms treeFromPreviousRound) exceptionHandler
  where
    exceptionHandler e = do
      -- Comment for final submission
      logStdErr $ "Worker died during [" ++
        show strategy ++ "] with exception " ++ show (e::SomeException) ++ "\n" ++
        "State: " ++ readableShow initialState ++ "\n" ++
        "state':" ++ readableShow state'
    nearbyWorms = wormsNearMyCurrentWorm initialState
    strategy    = determineStrategy (currentRound initialState) (thisWormsCoord initialState) nearbyWorms
    state'      = if strategy == Dig || strategy == GetToTheChoppa
                  then withOnlyWormsContainedIn nearbyWorms initialState
                  else initialState
    go :: StdGen -> Int -> SearchTree-> IO ()
    go !gen' 0      !searchTree = do
      writeVariable treeVariable searchTree
      newRoundsState <- pollComms stateChannel
      case newRoundsState of
        Just (move', thatLastMoveWasInvalid, nextState) -> do
          let tree''   = if strategy == Kill
                         then makeMoveInTree move' searchTree
                         else SearchFront
          let state''  = postStateTransformation (wormPositions nextState)
                                                 thatLastMoveWasInvalid
                                                 move'
                                                 initialState $
                         makeMove False move' initialState
          let diverged = nextState /= state''
          when diverged $
            logStdErr $ "States diverged (this could be an error in game run)!\n" ++
              "Worker state:\n" ++
              show state'' ++
              "Read state:\n" ++
              show nextState ++ "\n" ++
              "Recovering..."
          let (myMove, opponentsMove) = toMoves move'
          -- Comment for final submission
          logStdErr $ "Received moves:\n" ++
            "My move: " ++ prettyPrintThisMove initialState myMove ++ "\n" ++
            "Opponents move: " ++ prettyPrintThatMove initialState opponentsMove
          -- logStdErr $ "Making moves: " ++ show (toMoves move') ++ ", to state:\n" ++
          --   show initialState ++ "\n" ++
          --   "To create new state:\n" ++
          --   show state''
          -- let (myMove', opponentsMove') = (toMoves move')
          -- when (tree'' == SearchFront) $
          --   logStdErr $
          --   "Not in search tree: " ++
          --   "\n\tCombined: " ++ show move' ++
          --   "\n\tMy move: " ++ prettyPrintThisMove initialState myMove' ++
          --   "\n\tOpponents move: " ++ prettyPrintThatMove initialState opponentsMove'
          iterativelyImproveSearch gen'
                                   (if diverged then nextState else state'')
                                   (if diverged then SearchFront else tree'')
                                   stateChannel
                                   treeVariable
        Nothing -> go gen' iterationsBeforeComms searchTree
    go !gen' !count' !searchTree =
      let (result, gen'', finalState) = search gen' strategy state' searchTree
          newTree                     = updateTree searchTree strategy finalState result
      in -- logStdErr ("Updating search tree:\n========================================\n" ++
         --            prettyPrintSearchTree initialState searchTree ++
         --            "\nWith moves: " ++ prettyPrintSearchResult initialState result ++ "\n" ++
         --            "Resulting in new search tree:\n" ++ prettyPrintSearchTree initialState newTree ++
         --            "\n========================================\n") >>
         go gen'' (count' - 1) newTree

postStateTransformation :: WormPositions -> Bool -> CombinedMove -> State -> State -> State
postStateTransformation wormPositions' thatLastMoveWasInvalid move' initialState state =
  let (myMove, opponentsMove) = toMoves move'
      thisHasASelection       = hasASelection myMove
      myMove'                 = removeSelectionFromMove myMove
      thatHasASelection       = hasASelection opponentsMove
      opponentsMove'          = removeSelectionFromMove opponentsMove
      state'                  = (if thisHasASelection
                                 then makeMySelection myMove
                                 else id .
                                 if thatHasASelection
                                 then makeOpponentsSelection opponentsMove
                                 else id) state
      myCoord                 = thisWormsCoord initialState
      opponentsCoord          = thatWormsCoord initialState
  in if isAMoveMove myMove' && isAMoveMove opponentsMove' &&
        (not $ moveWouldGoOOB myCoord myMove') && (not $ moveWouldGoOOB opponentsCoord opponentsMove') &&
        displaceCoordByMove myCoord myMove' == displaceCoordByMove opponentsCoord opponentsMove'
     then withWormPositions (always wormPositions') state'
     else if thatLastMoveWasInvalid
          then penaliseThatPlayerForAnInvalidCommand state
          else state

makeMoveInTree :: CombinedMove -> SearchTree -> SearchTree
makeMoveInTree move' (SearchedLevel   _ _ _ transitions) = findSubTree move' transitions
makeMoveInTree _     (UnSearchedLevel _ _ _)             = SearchFront
makeMoveInTree _     SearchFront                         = SearchFront

-- In nanoseconds
maxSearchTime :: Integer
maxSearchTime = 750000000

-- In microseconds
pollInterval :: Int
pollInterval = 10000

joinWith :: (a -> String) -> String -> [a] -> String
joinWith toString joinString strings =
  let withExtra = concat $ map (\ x -> toString x ++ joinString) strings
  in take ((length withExtra) - (length joinString)) withExtra

indent :: String -> String
indent = (\ x -> if length x > 0 then tail x else x) . concat . map ((++) "\n    ") . lines

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

prettyPrintStateTransition :: State -> StateTransition -> String
prettyPrintStateTransition state (StateTransition combinedMove searchTree) =
  let (myMove, opponentsMove) = toMoves combinedMove
  in "Transition (" ++ prettyPrintThisMove state myMove ++ ", " ++ prettyPrintThatMove state opponentsMove ++ "):\n" ++
     prettyPrintSearchTree (makeMove False combinedMove state) searchTree

intMapValues :: IntMap a -> [a]
intMapValues = map snd . IM.toList

prettyPrintSearchTree :: State -> SearchTree -> String
prettyPrintSearchTree state (SearchedLevel gamesPlayed (MyMoves myMoves) (OpponentsMoves opponentsMoves) _) =
    "Searched:\n" ++
    "Games played: " ++ show gamesPlayed ++ "\n" ++
    "My moves:\n\t" ++ (joinWith (prettyPrintThisSuccessRecord state) "\n\t" (intMapValues myMoves)) ++ "\n" ++
    "Opponents moves:\n\t" ++ (joinWith (prettyPrintThatSuccessRecord state) "\n\t" (intMapValues opponentsMoves))
    -- ++ "\n"
    -- ++ "Transitions:\n" ++ (indent $ joinWith (prettyPrintStateTransition state) "\n" transitions)
prettyPrintSearchTree state (UnSearchedLevel gamesPlayed (MyMoves myMoves) (OpponentsMoves opponentsMoves)) =
    "UnSearched:\n" ++
    "Games played: " ++ show gamesPlayed ++ "\n" ++
    "My moves:\n\t" ++ (joinWith (prettyPrintThisSuccessRecord state) "\n\t" (intMapValues myMoves)) ++ "\n" ++
    "Opponents moves:\n\t" ++ (joinWith (prettyPrintThatSuccessRecord state) "\n\t" (intMapValues opponentsMoves))
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

hasAnyBananaMove :: Move -> Bool
hasAnyBananaMove = isABananaMove . removeSelectionFromMove

hasAnySnowballMove :: Move -> Bool
hasAnySnowballMove = isASnowballMove . removeSelectionFromMove

zeroToMinusOne :: Int -> Int
zeroToMinusOne 0 = (-1)
zeroToMinusOne x = x

decrementIfBananaMove :: WormId -> Move -> State -> Int
decrementIfBananaMove wormId' move state =
  if hasAnyBananaMove move && (aListContainsId wormId' $ wormBananas state)
  then (-1)
  else 0

decrementIfSnowballMove :: WormId -> Move -> State -> Int
decrementIfSnowballMove wormId' move state =
  if hasAnySnowballMove move && (aListContainsId wormId' $ wormSnowballs state)
  then (-1)
  else 0

nextStateAndAmmoCounts :: Move -> Move -> Int -> Int -> Int -> Int -> State -> State -> (Int, Int, Int, Int, State)
nextStateAndAmmoCounts thisMove
                       thatMove
                       thisBananaCount
                       thatBananaCount
                       thisSnowballCount
                       thatSnowballCount
                       currentState
                       nextState =
  let thatIdAfterSelect  = thatPlayersCurrentWormId $ (if hasASelection thatMove
                                                       then makeOpponentsSelection thatMove
                                                       else id) currentState
      thatMove'          = if (\ (State { frozenDurations = frozenDurations' }) ->
                                 aListContainsId   thatIdAfterSelect frozenDurations' &&
                                 aListFindDataById thatIdAfterSelect frozenDurations' > 1) currentState
                           then doNothing
                           else thatMove
      thisIdAfterSelect  = thisPlayersCurrentWormId $ (if hasASelection thisMove
                                                       then makeMySelection thisMove
                                                       else id) currentState
      thisMove'          = if (\ (State { frozenDurations = frozenDurations' }) ->
                                 aListContainsId thisIdAfterSelect frozenDurations') currentState
                           then doNothing
                           else thisMove
      thisBananaWormDied = not $ aListContainsId (WormId 2) (wormHealths nextState)
      thatBananaWormDied = not $ aListContainsId (WormId 8) (wormHealths nextState)
      thisBananaCount'   = if thisBananaWormDied
                           then (-1)
                           else zeroToMinusOne $
                                thisBananaCount   + decrementIfBananaMove thisIdAfterSelect thisMove' currentState
      thatBananaCount'   = if thatBananaWormDied
                           then (-1)
                           else zeroToMinusOne $
                                thatBananaCount   + decrementIfBananaMove thatIdAfterSelect thatMove' currentState
      thisSnowyWormDied  = not $ aListContainsId (WormId 3) (wormHealths nextState)
      thatSnowyWormDied  = not $ aListContainsId (WormId 12) (wormHealths nextState)
      thisSnowballCount' = if thisSnowyWormDied
                           then (-1)
                           else zeroToMinusOne $
                                thisSnowballCount + decrementIfSnowballMove thisIdAfterSelect thisMove' currentState
      thatSnowballCount' = if thatSnowyWormDied
                           then (-1)
                           else zeroToMinusOne $
                                thatSnowballCount + decrementIfSnowballMove thatIdAfterSelect thatMove' currentState
      nextState'         = (setOpponentsLastMove currentState thatMove' .
                            withWormBananas (always $
                              aListFromList [(2, thisBananaCount'),   (8,  thatBananaCount')]) .
                            withWormSnowballs (always $
                              aListFromList [(3, thisSnowballCount'), (12, thatSnowballCount')])) nextState
  in (thisBananaCount', thatBananaCount', thisSnowballCount', thatSnowballCount', nextState')

searchForAlottedTime :: State -> CommsVariable SearchTree -> IO Move
searchForAlottedTime state treeChannel = do
  searchTree <- treeAfterAlottedTime state treeChannel
  let gamesPlayed = countGames searchTree
  return . successRecordMove . chooseBestMove gamesPlayed $ myMovesFromTree searchTree

isDoNothing :: Move -> Bool
isDoNothing = (==) doNothing

chooseBestMove :: Int -> SuccessRecords -> SuccessRecord
chooseBestMove totalGamesPlayed records =
  maximumBy (\ oneTree otherTree -> compare (gamesPlayed oneTree) (gamesPlayed otherTree)) records'''
  where
    records'                    = if noClearWinner
                                  then IM.filter (\ (SuccessRecord _ _ move) ->
                                                    isAMoveMove move  ||
                                                    isAShootMove move ||
                                                    isADigMove move   ||
                                                    isDoNothing move) records
                                  else records
    records''                   = IM.filter (\ record -> (not . hasASelection $ successRecordMove record) ||
                                                         (not $ withinPercentile 5 record)) records'
    records'''                  = if records'' == IM.empty then IM.singleton doNothingValue (SuccessRecord (GamesPlayed 1) (PayoffRatio 1) doNothing) else records''
    averagePlayed               = totalGamesPlayed `div` length records
    withinPercentile percentile = (<= percentile) . abs . (100 -) . (`div` averagePlayed) . (* 100) . gamesPlayed
    noClearWinner               = all (withinPercentile 1) records
    gamesPlayed (SuccessRecord (GamesPlayed x) _ _) = x

runRound :: Int -> Int -> Int -> Int -> Int -> State -> CommsChannel (CombinedMove, Bool, State) -> CommsVariable SearchTree -> IO ()
runRound !thisBananaCount
         !thatBananaCount
         !thisSnowballCount
         !thatSnowballCount
         !roundNumber
         previousState
         stateChannel
         treeVariable = do
  move                  <- searchForAlottedTime previousState treeVariable
  putStrLn $
    -- ASSUME: that the worm is on a valid square to begin with
    "C;" ++
    show roundNumber ++
    ";" ++
    formatMove thisWormsCoord makeMySelection move (thisWormsCoord previousState) previousState ++ "\n"
  roundNumber'          <- readRound
  state                 <- readGameState roundNumber'
  -- TODO fromJust?
  let state'             = force $ fromJust state
  let opponentsLastMove  = force $ parseLastCommand previousState $ opponentsLastCommand state'
  let thatMoveWasInvalid = force $ any (== "invalid") $ opponentsLastCommand state'
  -- TODO!!!!!  I shouldn't be reading this state in the searcher.
  -- All I care about is the opponents move...
  -- EXTRA NOTE: And the fact that I don't know whether we swapped.
  let (thisBananaCount',
       thatBananaCount',
       thisSnowballCount',
       thatSnowballCount',
       nextState') = nextStateAndAmmoCounts move
                                            opponentsLastMove
                                            thisBananaCount
                                            thatBananaCount
                                            thisSnowballCount
                                            thatSnowballCount
                                            previousState
                                            state'
  writeComms stateChannel $ (fromMoves move opponentsLastMove, thatMoveWasInvalid, nextState')
  runRound thisBananaCount'
           thatBananaCount'
           thisSnowballCount'
           thatSnowballCount'
           roundNumber'
           nextState'
           stateChannel
           treeVariable

parseLastCommand :: State -> Maybe String -> Move
parseLastCommand _             Nothing             = doNothing
parseLastCommand previousState (Just lastCommand') =
  let coord'  = thatWormsCoord previousState
  in fromJust $ readThatMove previousState coord' lastCommand'

startBot :: StdGen -> IO ()
startBot g = do
  treeVariable  <- newVariable SearchFront
  stateChannel  <- newComms
  -- This is where I seed it with a search front
  initialRound' <- readRound
  initialState  <- fmap fromJust $ readGameState initialRound'
  _             <- forkIO (iterativelyImproveSearch g initialState SearchFront stateChannel treeVariable)
  runRound 3 3 3 3 initialRound' initialState stateChannel treeVariable

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

type SuccessRecords = IM.IntMap SuccessRecord

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

type StateTransitions = IM.IntMap StateTransition

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
    "My moves:\n\t" ++ (join' "\n\t" (intMapValues myMoves)) ++ "\n" ++
    "Opponents moves:\n\t" ++ (join' "\n\t" (intMapValues opponentsMoves))
  show (UnSearchedLevel gamesPlayed (MyMoves myMoves) (OpponentsMoves opponentsMoves)) =
    "UnSearched:\n" ++
    "Games played: " ++ show gamesPlayed ++ "\n" ++
    "My moves:\n\t" ++ (join' "\n\t" (intMapValues myMoves)) ++ "\n" ++
    "Opponents moves:\n\t" ++ (join' "\n\t" (intMapValues opponentsMoves))
  show SearchFront =
    "SearchFront"

type Reward = Int

data SearchResult = SearchResult Payoff Moves

prettyPrintSearchResult :: State -> SearchResult -> String
prettyPrintSearchResult state (SearchResult payoff moves) =
  "SearchResult (Payoff " ++ show payoff ++ ") (Moves " ++ go moves state ++ ")"
  where
    go [] _                      = ""
    go (move:moves') currentState =
      let (myMove, opponentsMove) = toMoves move
      in "(Moves (" ++ prettyPrintThisMove currentState myMove ++ ", " ++
         prettyPrintThatMove currentState opponentsMove ++ "))" ++
         go moves' (makeMove False move currentState)

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

initSuccessRecord :: Int -> Double -> Move -> SuccessRecord
initSuccessRecord initPlayed initPayoff move =
  SuccessRecord (GamesPlayed initPlayed) (PayoffRatio initPayoff) move

initSuccessRecordKeyValue :: Int -> Double -> Move -> (Int, SuccessRecord)
initSuccessRecordKeyValue initPlayed initPayoff move@(Move idx) =
  (idx, initSuccessRecord initPlayed initPayoff move)


initialiseLevel :: Strategy -> State -> SearchResult -> SearchTree
initialiseLevel strategy state result =
  let myMoveMoves         = myMoveMovesFrom        state
      opponentsMoveMoves  = opponentsMoveMovesFrom state
      myMovesFrom'        = case strategy of
        GetToTheChoppa -> myGetToTheChoppaMoves
        Kill           -> myMovesFrom myMoveMoves opponentsMoveMoves
        Points         -> myMovesFrom myMoveMoves opponentsMoveMoves
        Dig            -> myDigMovesFrom
      opponentsMovesFrom' = case strategy of
        GetToTheChoppa -> (always [doNothing])
        Kill           -> opponentsMovesFrom myMoveMoves opponentsMoveMoves
        Points         -> opponentsMovesFrom myMoveMoves opponentsMoveMoves
        Dig            -> (always [doNothing])
  in updateTree
     (UnSearchedLevel
      0
      (MyMoves        $
       IM.fromList    $
       map (initSuccessRecordKeyValue 0 0) $
       myMovesFrom' state)
      (OpponentsMoves $
       IM.fromList    $
       map (initSuccessRecordKeyValue 0 0) $
       opponentsMovesFrom' state))
     strategy state result

updateTree :: SearchTree -> Strategy -> State-> SearchResult -> SearchTree
updateTree SearchFront strategy finalState result = initialiseLevel strategy finalState result
updateTree level@(UnSearchedLevel gamesPlayed (MyMoves myMoves) (OpponentsMoves opponentsMoves)) _ _ result =
  case result of
    (SearchResult  (Payoff (MyPayoff myPayoff) (OpponentsPayoff opponentsPayoff) (MaxScore maxScore')) (move':_)) ->
      let (thisMove, thatMove) = toMoves move'
          myMoves'             = MyMoves        $ updateCount (incInc myPayoff        maxScore') myMoves        thisMove
          opponentsMoves'      = OpponentsMoves $ updateCount (incInc opponentsPayoff maxScore') opponentsMoves thatMove
      in (transitionLevelType myMoves' opponentsMoves') (gamesPlayed + 1) myMoves' opponentsMoves'
    _                           -> level
updateTree level@(SearchedLevel gamesPlayed (MyMoves myMoves) (OpponentsMoves opponentsMoves) stateTransitions) strategy finalState result =
  case result of
    (SearchResult  (Payoff (MyPayoff myPayoff) (OpponentsPayoff opponentsPayoff) (MaxScore maxScore')) (move':_)) ->
      let (thisMove, thatMove) = toMoves move'
          myMoves'             = MyMoves        $ updateCount (incInc myPayoff        maxScore') myMoves        thisMove
          opponentsMoves'      = OpponentsMoves $ updateCount (incInc opponentsPayoff maxScore') opponentsMoves thatMove
      in SearchedLevel (gamesPlayed + 1) myMoves' opponentsMoves' $ updateSubTree strategy finalState result stateTransitions
    _                           -> level

updateSubTree :: Strategy -> State -> SearchResult -> StateTransitions -> StateTransitions
updateSubTree _ _ (SearchResult _ []) transitions = transitions
updateSubTree strategy
              finalState
              (SearchResult payoff (move'@(CombinedMove idx):moves'))
              transitions =
  IM.insertWith (\ (StateTransition transitionMove' subTree') _ ->
                   (StateTransition transitionMove' $
                                    updateTree subTree'
                                               strategy
                                               finalState
                                               (SearchResult payoff moves')))
                idx
                (StateTransition move' $
                                 updateTree SearchFront
                                            strategy
                                            finalState
                                            (SearchResult payoff moves'))
                transitions

transitionLevelType :: MyMoves -> OpponentsMoves -> (Int -> MyMoves -> OpponentsMoves -> SearchTree)
transitionLevelType myMoves opponentsMoves =
    if allGamesPlayed myMoves opponentsMoves
    then \ gamesPlayed myMoves' opponentsMoves' -> SearchedLevel   gamesPlayed myMoves' opponentsMoves' IM.empty
    else \ gamesPlayed myMoves' opponentsMoves' -> UnSearchedLevel gamesPlayed myMoves' opponentsMoves'

allGamesPlayed :: MyMoves -> OpponentsMoves -> Bool
allGamesPlayed (MyMoves myMoves) (OpponentsMoves opponentsMoves) =
  all hasBeenPlayed myMoves && all hasBeenPlayed opponentsMoves
  where
    hasBeenPlayed (SuccessRecord (GamesPlayed gamesPlayed) _ _) = gamesPlayed /= 0

updateCount :: (SuccessRecord -> SuccessRecord) -> SuccessRecords -> Move -> SuccessRecords
updateCount changeCount successRecords (Move idx) =
  IM.adjust changeCount idx successRecords

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
              | Points
              | Kill
              | GetToTheChoppa
              deriving (Eq, Show)

choppaRadius :: Int
choppaRadius = 5

determineStrategy :: Int -> Coord -> WormPositions -> Strategy
determineStrategy currentRound' currentWormsCoord' wormPositions' =
  if currentRound' > 30
  then if currentRound' < startUsingAmmoRound
       then Points
       else Kill
  else if aListCountOpponentsEntries wormPositions' == 0
       then if manhattanDistanceToMiddle currentWormsCoord' < choppaRadius
            then Points
            else GetToTheChoppa
       else Points

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
       Dig            -> digSearch    g 0                    state searchTree [] 0
       Kill           -> killSearch   g state  round'        state searchTree []
       Points         -> pointsSearch g        round' round' state searchTree [] 0
       GetToTheChoppa -> digSearch    g 0                    state searchTree [] 0

pointsSearch :: StdGen -> Int -> Int -> State -> SearchTree -> Moves -> Reward -> (SearchResult, StdGen, State)
pointsSearch !g initialRound _ !state SearchFront moves !reward =
  (SearchResult (pointAndHealthPayOff initialRound state reward) (reverse moves), g, state)
pointsSearch !g initialRound !round' !state tree@(SearchedLevel _ _ _ _) moves !reward =
  case gameOver state round' of
    GameOver payoff -> (SearchResult payoff (reverse moves), g, state)
    NoResult        -> pointsSearchSearchedLevel g initialRound round' state tree moves reward
pointsSearch !g
             initialRound
             !round'
             !state
             (UnSearchedLevel _ (MyMoves myMoves) (OpponentsMoves opponentsMoves))
             moves
             !reward =
  case gameOver state round' of
    GameOver payoff -> (SearchResult payoff (reverse moves), g, state)
    NoResult        ->
      let (myRecord,        g')  = pickOneAtRandom g  $ IM.elems myMoves
          (opponentsRecord, g'') = pickOneAtRandom g' $ IM.elems opponentsMoves
          myMove                 = successRecordMove myRecord
          opponentsMove          = successRecordMove opponentsRecord
          combinedMove           = fromMoves myMove opponentsMove
          state'                 = makeMove False combinedMove state
          reward'                = digReward myMove
      in (SearchResult (pointAndHealthPayOff initialRound state' (reward' + reward)) (reverse (combinedMove:moves)), g'', state')

pointsSearchSearchedLevel :: StdGen -> Int -> Int -> State -> SearchTree -> Moves -> Reward -> (SearchResult, StdGen, State)
pointsSearchSearchedLevel _ _ _ _ SearchFront                   _ _ = error "pointsSearchSearchedLevel: SearchFront"
pointsSearchSearchedLevel _ _ _ _ level@(UnSearchedLevel _ _ _) _ _ = error $ "pointsSearchSearchedLevel: " ++ show level
pointsSearchSearchedLevel !g
                          initialRound
                          !round'
                          !state
                          (SearchedLevel gamesPlayed (MyMoves myMoves) (OpponentsMoves opponentsMoves) transitions)
                          moves
                          !reward =
  let myBestMove        = successRecordMove $ nextSearchMove gamesPlayed myMoves
      opponentsBestMove = successRecordMove $ nextSearchMove gamesPlayed opponentsMoves
      combinedMove      = fromMoves myBestMove opponentsBestMove
      state'            = makeMove True combinedMove state
      reward'           = digReward $ myBestMove
  in pointsSearch g
                  initialRound
                  (round' + 1)
                  state'
                  (findSubTree combinedMove transitions)
                  (combinedMove:moves)
                  (reward' + reward)

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
      let (myRecord,        g')  = pickOneAtRandom g  $ IM.elems myMoves
          (opponentsRecord, g'') = pickOneAtRandom g' $ IM.elems opponentsMoves
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
digSearchSearchedLevel _ _ _ SearchFront                    _ _ = error "killSearchSearchedLevel: SearchFront"
digSearchSearchedLevel _ _ _ level@(UnSearchedLevel _ _ _)  _ _ = error $ "killSearchSearchedLevel: " ++ show level
digSearchSearchedLevel !g
                       !round'
                       !state
                       (SearchedLevel gamesPlayed (MyMoves myMoves) (OpponentsMoves opponentsMoves) transitions)
                       moves
                       !reward =
  let myBestMove        = successRecordMove $ nextSearchMove gamesPlayed myMoves
      opponentsBestMove = successRecordMove $ nextSearchMove gamesPlayed opponentsMoves
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

maxDamageDealt :: (AList -> Int) -> (AList -> Int) -> State -> Int -> Int
maxDamageDealt countEntries
               sumEntries
               state
               rounds =
  let wormCount'    = countEntries $ wormHealths state
      bananaAmmo    = sumEntries   $ wormBananas state
      bananasThrown = max bananaAmmo $ rounds `div` wormCount'
  in rocketDamage * (rounds - bananasThrown) +
     bananasThrown * 20

maximumHealth :: Int
maximumHealth = 100 + 100 + 150

maxPayoffScore :: Int
maxPayoffScore =
  -- Maximum health (times two because it's the biggest difference between you and your opponent)
  3 * maximumHealth

payOff :: State -> State -> Payoff
payOff _ (State { wormHealths     = wormHealths' }) =
  let myTotalHealth                = aListSumMyEntries wormHealths'
      opponentsTotalHealth         = aListSumOpponentsEntries wormHealths'
      myPayoff                     = 2 * myTotalHealth +
                                     (maximumHealth - opponentsTotalHealth)
      opponentsPayoff              = opponentsTotalHealth +
                                     2 * (maximumHealth - myTotalHealth)
   in Payoff (MyPayoff myPayoff) (OpponentsPayoff opponentsPayoff) (MaxScore maxPayoffScore)

maxAverageDistance :: Int
maxAverageDistance = mapDim

allManhattanDistances :: UnBoxed.Vector Int
allManhattanDistances =
  UnBoxed.fromList $ do
    this <- [0..mapDim - 1]
    that <- [0..mapDim - 1]
    return $ manhattanDistance' this that
  where
    manhattanDistance' :: Coord -> Coord -> Int
    manhattanDistance' xy' xy'' =
      let (x',  y')  = fromCoord xy'
          (x'', y'') = fromCoord xy''
      in (abs $ x'' - x') + (abs $ y'' - y')

mapLength :: Int
mapLength = mapDim * mapDim

manhattanDistance :: Coord -> Coord -> Int
manhattanDistance xy' xy'' =
  allManhattanDistances `UBoxedUnsafe.unsafeIndex` (xy' * mapLength + xy'')

pointAndHealthPayOff :: Int -> State -> Int -> Payoff
pointAndHealthPayOff initialRound (State { wormHealths   = wormHealths',
                                           wormPositions = wormPositions',
                                           currentRound  = currentRound' }) !reward =
  let myTotalHealth        = aListSumMyEntries wormHealths'
      opponentsTotalHealth = aListSumOpponentsEntries wormHealths'
      maxPoints            = (currentRound' - initialRound) * digPoints
      -- Health is much more important than reward and the reward term
      -- gets bigger as you go deeper.
      myPayoff             = 10 * (30 * myTotalHealth +
                                   reward) +
                             aListAveragePairOffs manhattanDistance  wormPositions'
      opponentsPayoff      = maxAverageDistance +
                             10 * (maxPoints +
                                   10 * (opponentsTotalHealth +
                                         2 * (maximumHealth - myTotalHealth)))
   in Payoff (MyPayoff myPayoff)
             (OpponentsPayoff opponentsPayoff)
             (MaxScore (10 * ((10 * maxPayoffScore) + maxPoints) + maxAverageDistance))

killSearch :: StdGen -> State -> Int -> State -> SearchTree -> Moves -> (SearchResult, StdGen, State)
-- The first iteration of play randomly is here because we need to use
-- that move when we write the first entry in an unsearched level.
killSearch !g initialState _       !state SearchFront                  moves =
  (SearchResult (payOff initialState state) (reverse moves), g, state)
killSearch !g initialState !round' !state tree@(SearchedLevel _ _ _ _) moves =
  case gameOver state round' of
    GameOver payoff -> (SearchResult payoff (reverse moves), g, state)
    NoResult        -> killSearchSearchedLevel g initialState round' state tree moves
killSearch !g
           initialState
           !round'
           !state
           (UnSearchedLevel _ (MyMoves myMoves) (OpponentsMoves opponentsMoves))
           moves =
  case gameOver state round' of
    GameOver payoff -> (SearchResult payoff (reverse moves), g, state)
    NoResult        ->
      -- TODO this might be slow :/
      let (myRecord,        g')  = pickOneAtRandom g  $ IM.elems myMoves
          (opponentsRecord, g'') = pickOneAtRandom g' $ IM.elems opponentsMoves
          myMove                 = successRecordMove myRecord
          opponentsMove          = successRecordMove opponentsRecord
          combinedMove           = fromMoves myMove opponentsMove
          state'                 = makeMove False combinedMove state
      in (SearchResult (payOff initialState state') (reverse (combinedMove:moves)), g'', state')

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

killSearchSearchedLevel :: StdGen -> State -> Int -> State -> SearchTree -> Moves -> (SearchResult, StdGen, State)
killSearchSearchedLevel _ _ _ _ SearchFront                   _ = error "killSearchSearchedLevel: SearchFront"
killSearchSearchedLevel _ _ _ _ level@(UnSearchedLevel _ _ _) _ = error $ "killSearchSearchedLevel: " ++ show level
killSearchSearchedLevel !g
                        initialState
                        !round'
                        !state
                        (SearchedLevel gamesPlayed (MyMoves myMoves) (OpponentsMoves opponentsMoves) transitions)
                        moves =
  let myBestMove        = successRecordMove $ nextSearchMove gamesPlayed myMoves
      opponentsBestMove = successRecordMove $ nextSearchMove gamesPlayed opponentsMoves
      combinedMove      = fromMoves myBestMove opponentsBestMove
      state'            = makeMove True combinedMove state
  in killSearch g
                initialState
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
          else if round' > maxRound
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

nextSearchMove :: Int -> SuccessRecords -> SuccessRecord
nextSearchMove totalGames successRecords =
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

startUsingAmmoRound :: Int
startUsingAmmoRound = 200

myMoveMovesFrom :: State -> [Move]
myMoveMovesFrom state = do
  let moves              = map Move [8..15]
  let hasMoreThanOneWorm = (aListCountMyEntries $ wormPositions state) > 1
  let moves'             = if hasMoreThanOneWorm
                           then addThisPlayersSelects state moves
                           else moves
  myMove <- moves'
  guard (moveMoveWouldBeValuableToMe state myMove)
  return myMove

myUsefulNonMoveMoves :: [Move] -> State -> [Move]
myUsefulNonMoveMoves opponentsMoveMoves state = do
  let currentRound'      = currentRound state
  let moves              = map Move $ if currentRound' < startUsingAmmoRound
                                      then [0..7] ++ [16..24]
                                      else [0..7] ++ [16..185]
  let hasMoreThanOneWorm = (aListCountMyEntries $ wormPositions state) > 1
  let moves'             = if hasMoreThanOneWorm
                           then addThisPlayersSelects state moves
                           else moves
  let opponentsPossiblePositions = map (\ opponentsMoveMove -> wormPositions $
                                       makeMove False (fromMoves doNothing opponentsMoveMove) state)
                                       opponentsMoveMoves
  myMove <- moves'
  guard (nonMoveMoveWouldBeValuableToMe opponentsPossiblePositions state myMove)
  return myMove

myMovesFrom :: [Move] -> [Move] -> State -> [Move]
myMovesFrom myMoveMoves opponentsMoveMoves state =
  let myMoves = myMoveMoves ++ myUsefulNonMoveMoves opponentsMoveMoves state
  in if myMoves == []
     then [doNothing]
     else myMoves

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
  let coord' = thisWormsCoord state
      moves  = filter (\ move ->
                        (let targetOfMove' = displaceCoordByMove coord' move
                          in isAMoveMove move &&
                             isValidMoveMove coord' state move &&
                             (manhattanDistanceToMiddle targetOfMove' <= choppaRadius ||
                              isCloserByManhattanDistance targetOfMove' coord')) ||
                        (let targetOfMove' = displaceCoordByMove coord' (shiftDigToMoveRange move)
                          in isADigMove  move &&
                             isValidDigMove coord' (shiftDigToMoveRange move) (gameMap state) &&
                             (manhattanDistanceToMiddle targetOfMove' <= choppaRadius ||
                              isCloserByManhattanDistance targetOfMove' coord'))) $
               map Move [8..23]
  in if moves == []
     then [doNothing]
     else moves

moveMoveWouldBeValuableToMe :: State -> Move -> Bool
moveMoveWouldBeValuableToMe state move =
  let thisWormsId      = thisPlayersCurrentWormId state
      wormIsNotFrozen' = not $ wormIsFrozen thisWormsId state
      coord'           = thisWormsCoord state
  in wormIsNotFrozen' &&
     isAMoveMove move &&
     isValidMoveMove coord' state move

-- Determined by moving the point around in the reference for banana
-- bomb destinations.  Use the same for shooting because a worm could
-- move into range.
maxManhattanDistanceForBombs :: Int
maxManhattanDistanceForBombs = 7

-- ASSUME: that the player has selections left (it's checked
-- elsewhere!)
nonMoveMoveWouldBeValuableToMe :: [WormPositions] -> State -> Move -> Bool
nonMoveMoveWouldBeValuableToMe opponentsPossibleWormPositions state move =
  let coord'           = thisWormsCoord state
      gameMap'         = gameMap state
      wormPositions'   = wormPositions state
      thisWormsId      = thisPlayersCurrentWormId state
      wormIsNotFrozen' = not $ wormIsFrozen thisWormsId state
      wormsAreClose    = aListMinPairOff thisWormsId manhattanDistance wormPositions' < maxManhattanDistanceForBombs
  in (wormIsNotFrozen' &&
      isAMoveMove move &&
      isValidMoveMove coord' state move) ||
     (wormIsNotFrozen' &&
      isADigMove move  &&
      isValidDigMove  coord' (shiftDigToMoveRange move) (gameMap state)) ||
     (wormIsNotFrozen'  &&
      wormsAreClose     &&
      isAShootMove move &&
      (any (\ positions -> isJust $
                           shotHitsWorm coord' gameMap' positions move) $
           wormPositions' : opponentsPossibleWormPositions)) ||
     (wormIsNotFrozen'                     &&
      wormsAreClose                        &&
      isABananaMove move                   &&
      wormHasBananasLeft thisWormsId state &&
      any (\ target -> bananaBlastHitOpponent target wormPositions')
          (displaceToBananaDestination move coord')) ||
     (wormIsNotFrozen'                       &&
      wormsAreClose                          &&
      isASnowballMove move                   &&
      wormHasSnowballsLeft thisWormsId state &&
      any (\ target -> (snowballBlastHitOpponent target wormPositions'))
          (displaceToBananaDestination (snowballMoveToBananaRange move) coord')) ||
     (hasASelection move &&
      nonMoveMoveWouldBeValuableToMe opponentsPossibleWormPositions
                                     (makeMySelection move state)
                                     (removeSelectionFromMove move))

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

opponentsMoveMovesFrom :: State -> [Move]
opponentsMoveMovesFrom state = do
  let moves              = map Move [8..15]
  let hasMoreThanOneWorm = (aListCountOpponentsEntries $ wormPositions state) > 1
  let moves'             = if hasMoreThanOneWorm
                           then addThatPlayersSelects state moves
                           else moves
  opponentsMove <- moves'
  guard (moveMoveWouldBeValuableToOpponent state opponentsMove)
  return opponentsMove

opponentsUsefulNonMoveMoves :: [Move] -> State -> [Move]
opponentsUsefulNonMoveMoves myMoveMoves state = do
  let moves               = map Move $ [0..7] ++ [16..185]
  let hasMoreThanOneWorm  = (aListCountOpponentsEntries $ wormPositions state) > 1
  let moves'              = if hasMoreThanOneWorm
                            then addThatPlayersSelects state moves
                            else moves
  let myPossiblePositions = map (\ myMoveMove -> wormPositions $
                                makeMove False (fromMoves myMoveMove doNothing) state) myMoveMoves
  opponentsMove          <- moves'
  guard (nonMoveMoveWouldBeValuableToOpponent myPossiblePositions state opponentsMove)
  return $ opponentsMove

opponentsMovesFrom :: [Move] -> [Move] -> State -> [Move]
opponentsMovesFrom myMoveMoves opponentsMoveMoves state =
  let opponentsMoves = opponentsMoveMoves ++ opponentsUsefulNonMoveMoves myMoveMoves state
  in if opponentsMoves == []
     then [doNothing]
     else opponentsMoves

wormIsFrozen :: WormId -> State -> Bool
wormIsFrozen wormId' = aListContainsId wormId' . frozenDurations

moveMoveWouldBeValuableToOpponent :: State -> Move -> Bool
moveMoveWouldBeValuableToOpponent state move =
  let thatWormsId      = thatPlayersCurrentWormId state
      wormIsNotFrozen' = not $ wormIsFrozen thatWormsId state
      coord'           = thatWormsCoord state
  in wormIsNotFrozen' &&
     isAMoveMove move &&
     isValidMoveMove coord' state move

-- ASSUME: That the opponent has selections left
nonMoveMoveWouldBeValuableToOpponent :: [WormPositions] -> State -> Move -> Bool
nonMoveMoveWouldBeValuableToOpponent myPossibleWormPositions state move =
  let coord'           = thatWormsCoord state
      gameMap'         = gameMap state
      wormPositions'   = wormPositions state
      thatWormsId      = thatPlayersCurrentWormId state
      wormIsNotFrozen' = not $ wormIsFrozen thatWormsId state
      wormsAreClose    = aListMinPairOff thatWormsId manhattanDistance wormPositions' < maxManhattanDistanceForBombs
  in (wormIsNotFrozen' &&
      isADigMove move  &&
      isValidDigMove  coord' (shiftDigToMoveRange move) (gameMap state)) ||
     (wormIsNotFrozen'  &&
      wormsAreClose     &&
      isAShootMove move &&
      (any (\ positions -> isJust $
                           shotHitsWorm coord' gameMap' positions move) $
       wormPositions' : myPossibleWormPositions)) ||
     (wormIsNotFrozen'                     &&
      wormsAreClose                        &&
      isABananaMove move                   &&
      wormHasBananasLeft thatWormsId state &&
      any (\ target -> bananaBlastHitMe target wormPositions')
          (displaceToBananaDestination move coord')) ||
     (wormIsNotFrozen'                       &&
      wormsAreClose                          &&
      isASnowballMove move                   &&
      wormHasSnowballsLeft thatWormsId state &&
      any (\ target -> (snowballBlastHitMe target wormPositions'))
          (displaceToBananaDestination (snowballMoveToBananaRange move) coord')) ||
     (hasASelection move &&
      nonMoveMoveWouldBeValuableToOpponent myPossibleWormPositions
                                    (makeOpponentsSelection move state)
                                    (removeSelectionFromMove move))

addPlayersSelects :: (State -> Bool) -> (AList -> [WormId]) -> (State -> WormId) -> State -> [Move] -> [Move]
addPlayersSelects playerHasSelectionsLeft playersWormIds playersWormId state moves =
  if not $ playerHasSelectionsLeft state
  then moves
  else moves ++ do
    selection  <- filter (/= (playersWormId state)) $ playersWormIds $ wormPositions state
    move       <- moves
    return $ withSelection selection move

addThisPlayersSelects :: State -> [Move] -> [Move]
addThisPlayersSelects = addPlayersSelects thisPlayerHasSelectionsLeft aListMyIds       thisPlayersCurrentWormId

addThatPlayersSelects :: State -> [Move] -> [Move]
addThatPlayersSelects = addPlayersSelects thatPlayerHasSelectionsLeft aListOpponentIds thatPlayersCurrentWormId

withSelection :: WormId -> Move -> Move
withSelection  (WormId id') (Move x) =
  Move $ x .|. (shiftL id' selectEncodingRange)

doNothing :: Move
doNothing = Move doNothingValue

doNothingValue :: Int
doNothingValue = 187

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
