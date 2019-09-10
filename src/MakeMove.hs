{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module MakeMove
  where

import qualified RIO.Vector.Boxed as V
import RIO.List
import RIO.List.Partial
import Data.Bits
import qualified RIO.Vector.Boxed.Unsafe as UV
import Control.DeepSeq
import Data.Maybe

import Ternary
import Lava
import Import
import AList
import MapArithmetic

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
  deriving (Show, Eq, Ord)

instance NFData Move where
  rnf (Move move) = move `deepseq` ()

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
        thatDirectionsFrom        = directionsFrom thatWormsId thatWormsCoord' wormPositions'
        thisDirectionsFrom        = directionsFrom thisWormsId thisWormsCoord' wormPositions'
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
                          thisDirectionsFrom
                          wormPositions'
                          myMove .
          makeOpponentsShootMove thatWormsCoord'
                                 thatWormsId
                                 gameMap'
                                 thatDirectionsFrom
                                 wormPositions'
                                 opponentsMove
        makeMove' SHOOT           THROW_BANANA =
          makeMyShootMove thisWormsCoord'
                          thisWormsId
                          gameMap'
                          thisDirectionsFrom
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
                          thisDirectionsFrom
                          wormPositions'
                          myMove
        makeMove' SHOOT           NOTHING =
          makeMyShootMove thisWormsCoord'
                          thisWormsId
                          gameMap'
                          thisDirectionsFrom
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
                                 thatDirectionsFrom
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
                                 thatDirectionsFrom
                                 wormPositions'
                                 opponentsMove
        makeMove' NOTHING         SHOOT =
          makeOpponentsShootMove thatWormsCoord'
                                 thatWormsId
                                 gameMap'
                                 thatDirectionsFrom
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

doNothing :: Move
doNothing = Move doNothingValue

doNothingValue :: Int
doNothingValue = 187

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
  testBit (lava `UV.unsafeIndex` currentRound') coord'

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
-- assertValidState :: State -> Move -> Move -> ModifyState
-- assertValidState previousState myMove opponentsMove state =
--   let wormHealths'       = map snd $ aListToList $ wormHealths   state
--       wormPositions'     = map snd $ aListToList $ wormPositions state
--       lengthMismatch     = length wormHealths' /= length wormPositions'
--       invalidWormHealths = any (< (-1)) wormHealths'
--       coordinatesOOB     = any (not . isJust . isOOB) $ map fromCoord wormPositions'
--   in if lengthMismatch || invalidWormHealths || coordinatesOOB
--      then error ("\nMy move:        " ++ prettyPrintThisMove previousState myMove ++ "(" ++ show myMove ++ ")"++ "\n" ++
--                  "Opponents Move: " ++ prettyPrintThatMove previousState opponentsMove ++ "(" ++ show opponentsMove ++ ")" ++ "\n" ++
--                  "Led to a bad state transition: " ++
--                  "lengthMismatch: " ++ show lengthMismatch ++
--                  ", invalidWormHealths: " ++ show invalidWormHealths ++
--                  ", coordinatesOOB: " ++ show coordinatesOOB ++ ".\n" ++
--                  "Moving from:\n" ++ show previousState ++ "\n" ++
--                  "To:\n" ++ show state ++ "\n" ++
--                  "Readable input state:\n" ++ readableShow previousState)
--      else state

-- TODO I shouldn't even be doing this at all.
setOpponentsLastMove :: State -> Move -> ModifyState
setOpponentsLastMove stateWhenMoveWasMade move' state =
  state { opponentsLastCommand =
          Just $ if move' == doNothing ||
          (isAMoveMove move' && moveWouldGoOOB (thatWormsCoord stateWhenMoveWasMade) move') ||
          (\ (Move x) -> x > 4096) move'
                 then "nothing \"Player chose to do nothing\""
                 else prettyPrintThatMove stateWhenMoveWasMade move' }

prettyPrintMove :: (State -> Coord) -> (Move -> ModifyState) -> State -> Move -> String
prettyPrintMove wormsCoord makeSelections' state move =
  let coord' = wormsCoord state
  in formatMove wormsCoord makeSelections' move coord' state

prettyPrintThisMove :: State -> Move -> String
prettyPrintThisMove = prettyPrintMove thisWormsCoord makeMySelection

prettyPrintThatMove :: State -> Move -> String
prettyPrintThatMove = prettyPrintMove thatWormsCoord makeOpponentsSelection

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

makeMyShootMove :: Coord -> WormId -> GameMap -> DirectionZone -> WormPositions -> Move -> ModifyState
makeMyShootMove =
  makeShootMove penaliseThisPlayerForHittingHisFriendlyWorm
                awardPointsToThisPlayerForHittingAnEnemy
                awardPointsToThisPlayerForKillingAnEnemy
                penaliseThisPlayerForKillingOwnWorm
                awardPointsToThisPlayerForMissing

makeOpponentsShootMove :: Coord -> WormId -> GameMap -> DirectionZone -> WormPositions -> Move -> ModifyState
makeOpponentsShootMove =
  makeShootMove penaliseThatPlayerForHittingHisFriendlyWorm
                awardPointsToThatPlayerForHittingAnEnemy
                awardPointsToThatPlayerForKillingAnEnemy
                penaliseThatPlayerForKillingOwnWorm
                awardPointsToThatPlayerForMissing

makeShootMove :: ModifyState -> ModifyState -> ModifyState -> ModifyState -> ModifyState -> Coord -> WormId -> GameMap -> DirectionZone -> WormPositions -> Move -> ModifyState
makeShootMove penalise
              awardPlayer
              awardPlayerForKill
              penalisePlayerForKill
              awardPointsForMiss
              !wormsPosition
              !wormId'
              !gameMap'
              !directionZone
              !wormPositions'
              !move =
      let coord    = shotHitsWorm wormsPosition gameMap' directionZone wormPositions' move
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

shotHitsWorm :: Coord -> GameMap -> DirectionZone -> WormPositions -> Move -> Maybe Coord
shotHitsWorm coord' gameMap' directionZone wormPositions' move =
  let shotsDir = directionOfShot move
  in if shotGoesInDirection move directionZone
     then hitsWorm coord' gameMap' shotsDir wormPositions'
     else Nothing

-- First bit is NW, then it proceeds clockwise:
-- 0  0  0  0
-- NW NE SE SW
type DirectionZone = Int

nwBit :: Int
nwBit = 3

nwZone :: DirectionZone
nwZone = shiftL 1 nwBit

neBit :: Int
neBit = 2

neZone :: DirectionZone
neZone = shiftL 1 neBit

seBit :: Int
seBit = 1

seZone :: DirectionZone
seZone = shiftL 1 seBit

swBit :: Int
swBit = 0

swZone :: DirectionZone
swZone = shiftL 1 swBit

nZone :: DirectionZone
nZone = mergeZones nwZone neZone

eZone :: DirectionZone
eZone = mergeZones neZone seZone

sZone :: DirectionZone
sZone = mergeZones seZone swZone

wZone :: DirectionZone
wZone = mergeZones swZone nwZone

allZones :: DirectionZone
allZones = (shiftL 1 4) - 1

opponentsDirectionsFrom :: Coord -> AList -> DirectionZone
opponentsDirectionsFrom coord' positions =
  aListFoldOverOpponentValues updateZone 0 positions
  where
    updateZone :: DirectionZone -> Coord -> DirectionZone
    updateZone zone x = mergeZones zone (directionFrom coord' x)

myDirectionsFrom :: Coord -> AList -> DirectionZone
myDirectionsFrom coord' positions =
  aListFoldOverMyValues updateZone 0 positions
  where
    updateZone :: DirectionZone -> Coord -> DirectionZone
    updateZone zone x = mergeZones zone (directionFrom coord' x)

directionsFrom :: WormId -> Coord -> AList -> DirectionZone
directionsFrom wormId' coord' positions =
  aListFoldOverOtherValues wormId' updateZone 0 positions
  where
    updateZone :: DirectionZone -> Coord -> DirectionZone
    updateZone zone x = mergeZones zone (directionFrom coord' x)

-- Generated with:
-- let zones = [NW_ZONE, NE_ZONE, SE_ZONE, SW_ZONE, N_ZONE, E_ZONE, S_ZONE, W_ZONE, NOT_SW_ZONE, NOT_NW_ZONE, NOT_NE_ZONE, NOT_SE_ZONE, ALL_ZONE, NONE]
-- putStrLn $ concat $ do { one <- zones; other <- zones; return $ "mergeZones " ++ show one ++ " " ++ show other ++ " = undefined\n" }
mergeZones :: DirectionZone -> DirectionZone -> DirectionZone
mergeZones = (.|.)

directionFrom :: Coord -> Coord -> DirectionZone
directionFrom xy' xy'' =
  allDirectionsFrom `UV.unsafeIndex` (xy' * mapLength + xy'')

allDirectionsFrom :: V.Vector DirectionZone
allDirectionsFrom =
  V.fromList $ do
    this <- [0..mapLength - 1]
    that <- [0..mapLength - 1]
    return $ directionFrom' this that
  where
    directionFrom' :: Coord -> Coord -> DirectionZone
    directionFrom' xy' xy'' =
      let (x',  y')  = fromCoord xy'
          (x'', y'') = fromCoord xy''
      in case (compareToTernary x'' x', compareToTernary y'' y') of
        (Zero,   NegOne) -> nZone
        (One,    NegOne) -> neZone
        (One,    Zero)   -> eZone
        (One,    One)    -> seZone
        (Zero,   One)    -> sZone
        (NegOne, One)    -> swZone
        (NegOne, Zero)   -> wZone
        (NegOne, NegOne) -> nwZone
        (Zero,   Zero)   -> allZones

shotGoesInDirection :: Move -> DirectionZone -> Bool
shotGoesInDirection (Move 0) direction = testBit direction nwBit || testBit direction neBit
shotGoesInDirection (Move 1) direction = testBit direction neBit
shotGoesInDirection (Move 2) direction = testBit direction seBit || testBit direction neBit
shotGoesInDirection (Move 3) direction = testBit direction seBit
shotGoesInDirection (Move 4) direction = testBit direction swBit || testBit direction seBit
shotGoesInDirection (Move 5) direction = testBit direction swBit
shotGoesInDirection (Move 6) direction = testBit direction nwBit || testBit direction swBit
shotGoesInDirection (Move 7) direction = testBit direction nwBit
shotGoesInDirection move'    _         = error $ "shotGoesInDirection:" ++ show move'

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

healthPackHealth :: Int
healthPackHealth = 10

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
             deriving (Generic, Eq, Ord)

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
  deriving (Generic, Eq, Ord)

instance NFData GameMap where
  rnf (GameMap air dirt space medipacks) =
    air `deepseq` dirt `deepseq` space `deepseq` medipacks `deepseq` ()

instance Show GameMap where
  show = showRows . splitGameMap


showRows :: [[Cell]] -> String
showRows xs =
  "|" ++ (foldr (++) "" $ take mapDim $ repeat "-") ++ "|\n" ++
  (foldr (\ nextRow gameMap' -> gameMap' ++ "|" ++ (foldr (++) "" $ fmap show nextRow) ++ "|\n") "" xs) ++
  "|" ++ (foldr (++) "" $ take mapDim $ repeat "-") ++ "|"

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

splitGameMap :: GameMap -> [[Cell]]
splitGameMap gameMap' =
  let cells = zip [(1::Int)..]
                  (foldl' (\ acc coord' -> mapAt coord' gameMap' : acc) []
                   [0..(mapLength - 1)])
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
  V.zip (V.fromList [0..mapLength - 1])

-- Deprecated
mapAtCoord :: State -> Coord -> Cell
mapAtCoord State { gameMap = gameMap' } target = mapAt target gameMap'

cellTo :: Coord -> Cell -> GameMap -> GameMap
cellTo position' newCell gameMap' =
  modifyMapCellAt position' (always newCell) gameMap'

removeDirtAt :: Coord -> GameMap -> GameMap
removeDirtAt = (flip cellTo) AIR

type ModifyState = State -> State

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

data Selections = Selections Int
  deriving (Eq, Show, Ord)

instance NFData Selections where
  rnf (Selections selections) = selections `deepseq` ()

-- TODO: Change Int to PlayerScore for stronger types
data Player = Player Int WormId Selections
  deriving (Show, Generic, Eq, Ord)

instance NFData Player where
  rnf (Player score' wormId' selections') =
    score' `deepseq` wormId' `deepseq` selections' `deepseq` ()

showCoord :: Coord -> String
showCoord xy = case fromCoord xy of
    (x', y') -> show x' ++ " " ++ show y'
