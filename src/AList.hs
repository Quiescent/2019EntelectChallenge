{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module AList
  where

import Import

import Control.DeepSeq

data AList = AList Int Int Int Int Int Int
  deriving (Eq, Ord)

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

aListWithOnlyOneOfMyIds :: WormId -> AList -> AList
aListWithOnlyOneOfMyIds (WormId 1) (AList a _ _ d e f) = (AList a    (-1) (-1) d e f)
aListWithOnlyOneOfMyIds (WormId 2) (AList _ b _ d e f) = (AList (-1)    b (-1) d e f)
aListWithOnlyOneOfMyIds (WormId 3) (AList _ _ c d e f) = (AList (-1) (-1)    c d e f)
aListWithOnlyOneOfMyIds wormId'    _                            = error $ "aListWithOnlyOneOfMyIds: " ++ show wormId'

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

aListAllPairOffs :: (Int -> Int -> Int) -> AList -> Int
aListAllPairOffs fun aList@(AList a b c d e f) =
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

aListFoldOverOtherValues ::  WormId -> (b -> Int -> b) -> b -> AList -> b
aListFoldOverOtherValues (WormId 1) fun acc (AList _ b c d e f) =
  go (0::Int) acc
  where
    go 0 !acc' = go 1 (fun acc' b)
    go 1 !acc' = go 2 (fun acc' c)
    go 2 !acc' = go 3 (fun acc' d)
    go 3 !acc' = go 4 (fun acc' e)
    go 4 !acc' = fun acc' f
    go x _     = error $ "Can't get here... " ++ show x
aListFoldOverOtherValues (WormId 2) fun acc (AList a _ c d e f) =
  go (0::Int) acc
  where
    go 0 !acc' = go 1 (fun acc' a)
    go 1 !acc' = go 2 (fun acc' c)
    go 2 !acc' = go 3 (fun acc' d)
    go 3 !acc' = go 4 (fun acc' e)
    go 4 !acc' = fun acc' f
    go x _     = error $ "Can't get here... " ++ show x
aListFoldOverOtherValues (WormId 3) fun acc (AList a b _ d e f) =
  go (0::Int) acc
  where
    go 0 !acc' = go 1 (fun acc' a)
    go 1 !acc' = go 2 (fun acc' b)
    go 2 !acc' = go 3 (fun acc' d)
    go 3 !acc' = go 4 (fun acc' e)
    go 4 !acc' = fun acc' f
    go x _     = error $ "Can't get here... " ++ show x
aListFoldOverOtherValues (WormId 4) fun acc (AList a b c _ e f) =
  go (0::Int) acc
  where
    go 0 !acc' = go 1 (fun acc' a)
    go 1 !acc' = go 2 (fun acc' b)
    go 2 !acc' = go 3 (fun acc' c)
    go 3 !acc' = go 4 (fun acc' e)
    go 4 !acc' = fun acc' f
    go x _     = error $ "Can't get here... " ++ show x
aListFoldOverOtherValues (WormId 8) fun acc (AList a b c d _ f) =
  go (0::Int) acc
  where
    go 0 !acc' = go 1 (fun acc' a)
    go 1 !acc' = go 2 (fun acc' b)
    go 2 !acc' = go 3 (fun acc' c)
    go 3 !acc' = go 4 (fun acc' d)
    go 4 !acc' = fun acc' f
    go x _     = error $ "Can't get here... " ++ show x
aListFoldOverOtherValues (WormId 12) fun acc (AList a b c d e _) =
  go (0::Int) acc
  where
    go 0 !acc' = go 1 (fun acc' a)
    go 1 !acc' = go 2 (fun acc' b)
    go 2 !acc' = go 3 (fun acc' c)
    go 3 !acc' = go 4 (fun acc' d)
    go 4 !acc' = fun acc' e
    go x _     = error $ "Can't get here... " ++ show x
aListFoldOverOtherValues wormId' _ _ _ = error $ "aListFoldOverOtherValues: " ++ show wormId'


aListFoldOverOpponentValues :: (b -> Int -> b) -> b -> AList -> b
aListFoldOverOpponentValues fun acc (AList _ _ _ d e f) =
  go (0::Int) acc
  where
    go 0 !acc' = go 1 (fun acc' d)
    go 1 !acc' = go 2 (fun acc' e)
    go 2 !acc' = fun acc' f
    go x _     = error $ "Can't get here... " ++ show x

aListFoldOverMyValues :: (b -> Int -> b) -> b -> AList -> b
aListFoldOverMyValues fun acc (AList a b c _ _ _) =
  go (0::Int) acc
  where
    go 0 !acc' = go 1 (fun acc' a)
    go 1 !acc' = go 2 (fun acc' b)
    go 2 !acc' = fun acc' c
    go x _     = error $ "Can't get here... " ++ show x

aListMinPairOff :: WormId -> (Int -> Int -> Int) -> AList -> Int
aListMinPairOff wormId' fun (AList a b c d e f) =
 case wormId' of
  (WormId 1)  -> (if a /= (-1)
                  then min (if d /= (-1)
                           then fun a d
                           else (maxBound::Int)) $
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
                              else (maxBound::Int)) $
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
                            else (maxBound::Int)) $
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
                            else (maxBound::Int)) $
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
                            else (maxBound::Int)) $
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
                            else (maxBound::Int)) $
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
