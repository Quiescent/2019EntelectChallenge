{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Search
  where

import Control.DeepSeq
import qualified Data.IntMap.Lazy as IM
import System.Random
import qualified RIO.Set as Set
import qualified Data.PriorityQueue.FingerTree as PQ
import Data.Maybe
import RIO.List.Partial
import Data.Bits

import ReadState
import Import
import AList
import MapArithmetic
import MakeMove

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

makeMoveInTree :: CombinedMove -> SearchTree -> SearchTree
makeMoveInTree move' (SearchedLevel   _ _ _ transitions) = findSubTree move' transitions
makeMoveInTree _     (UnSearchedLevel _ _ _)             = SearchFront
makeMoveInTree _     SearchFront                         = SearchFront

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

intMapValues :: IM.IntMap a -> [a]
intMapValues = IM.elems

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
                  | Instruction Move

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
prettyPrintSearchResult state (Instruction move) =
  "Instruction: " ++ prettyPrintThisMove state move

instance Show SearchResult where
  show (SearchResult payoff moves') =
    "SearchResult (" ++ show payoff ++ ") (Moves " ++ (show $ map toMoves moves') ++ ")"
  show (Instruction move) =
    "Instruction: " ++ show move

instance NFData SearchResult where
  rnf (SearchResult payoff moves) = payoff `deepseq` moves `deepseq` ()
  rnf (Instruction move) = move `deepseq` ()

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


initialiseLevel :: Strategy -> WormId -> State -> SearchResult -> SearchTree
initialiseLevel strategy wormId' state result =
  let wormHealths'          = wormHealths state
      gameIsOver            = strategy /= GetToTheChoppa &&
                              (aListCountMyEntries wormHealths' == 0 || aListCountOpponentsEntries wormHealths' == 0)
      doNothingMoves        = always [doNothing]
      nothingOrTheseMoves f = if gameIsOver then doNothingMoves else f
      nothingOrThoseMoves f = if gameIsOver then doNothingMoves else f
      myMoveMoves           = myMoveMovesFrom        state
      opponentsMoveMoves    = opponentsMoveMovesFrom state
      myCoord               = thisWormsCoord state
      opponentsCoord        = thatWormsCoord state
      weAlignForAShot       = aligns myCoord opponentsCoord && inRange myCoord opponentsCoord missileRange
      myMovesFrom'          = case strategy of
        GetToTheChoppa -> nothingOrTheseMoves (myGetToTheChoppaMoves)
        Kill           -> nothingOrTheseMoves (myMovesFrom myMoveMoves opponentsMoveMoves)
        Points         -> nothingOrTheseMoves (myMovesFrom myMoveMoves opponentsMoveMoves)
        Dig            -> nothingOrTheseMoves myDigMovesFrom
        Runaway        -> nothingOrTheseMoves (myRunawayMovesFrom wormId')
        FindDemDirt    -> nothingOrTheseMoves (myRunawayMovesFrom wormId')
        EndGame        -> nothingOrTheseMoves (myEndGameMovesFrom myMoveMoves opponentsMoveMoves weAlignForAShot)
      opponentsMovesFrom' = case strategy of
        GetToTheChoppa -> doNothingMoves
        Kill           -> nothingOrThoseMoves (opponentsMovesFrom myMoveMoves opponentsMoveMoves)
        Points         -> nothingOrThoseMoves (opponentsMovesFrom myMoveMoves opponentsMoveMoves)
        Dig            -> doNothingMoves
        Runaway        -> nothingOrThoseMoves opponentsRunawayMovesFrom
        FindDemDirt    -> nothingOrThoseMoves opponentsRunawayMovesFrom
        EndGame        -> nothingOrThoseMoves (opponentsEndGameMovesFrom myMoveMoves opponentsMoveMoves weAlignForAShot)
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
     strategy wormId' state result

updateTree :: SearchTree -> Strategy -> WormId -> State -> SearchResult -> SearchTree
updateTree _           _        _       _          (Instruction move) =
  SearchedLevel 1 (MyMoves . IM.fromList $ [initSuccessRecordKeyValue 1 1 move]) (OpponentsMoves IM.empty) IM.empty
updateTree SearchFront strategy wormId' finalState result = initialiseLevel strategy wormId' finalState result
updateTree level@(UnSearchedLevel gamesPlayed (MyMoves myMoves) (OpponentsMoves opponentsMoves)) _ _ _ result =
  case result of
    (SearchResult  (Payoff (MyPayoff myPayoff) (OpponentsPayoff opponentsPayoff) (MaxScore maxScore')) (move':_)) ->
      let (thisMove, thatMove) = toMoves move'
          myMoves'             = MyMoves        $ updateCount (incInc myPayoff        maxScore') myMoves        thisMove
          opponentsMoves'      = OpponentsMoves $ updateCount (incInc opponentsPayoff maxScore') opponentsMoves thatMove
      in (transitionLevelType myMoves' opponentsMoves') (gamesPlayed + 1) myMoves' opponentsMoves'
    _                           -> level
updateTree level@(SearchedLevel gamesPlayed (MyMoves myMoves) (OpponentsMoves opponentsMoves) stateTransitions) strategy wormId' finalState result =
  case result of
    (SearchResult  (Payoff (MyPayoff myPayoff) (OpponentsPayoff opponentsPayoff) (MaxScore maxScore')) (move':_)) ->
      let (thisMove, thatMove) = toMoves move'
          myMoves'             = MyMoves        $ updateCount (incInc myPayoff        maxScore') myMoves        thisMove
          opponentsMoves'      = OpponentsMoves $ updateCount (incInc opponentsPayoff maxScore') opponentsMoves thatMove
      in SearchedLevel (gamesPlayed + 1) myMoves' opponentsMoves' $ updateSubTree strategy wormId' finalState result stateTransitions
    _                           -> level

updateSubTree :: Strategy -> WormId -> State -> SearchResult -> StateTransitions -> StateTransitions
updateSubTree _ _ _ (Instruction _)     transitions = transitions
updateSubTree _ _ _ (SearchResult _ []) transitions = transitions
updateSubTree strategy
              wormId'
              finalState
              (SearchResult payoff (move'@(CombinedMove idx):moves'))
              transitions =
  -- Second is old value
  IM.insertWith (\ _ (StateTransition transitionMove' subTree') ->
                   (StateTransition transitionMove' $
                                    updateTree subTree'
                                               strategy
                                               wormId'
                                               finalState
                                               (SearchResult payoff moves')))
                idx
                (StateTransition move' $
                                 updateTree SearchFront
                                            strategy
                                            wormId'
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
rangeToConsiderInMinigame = 10

wormsNearMyCurrentWorm :: State -> AList
wormsNearMyCurrentWorm state =
  let coord' = thisWormsCoord state
  in aListFilterByData (\ xy -> inRange xy coord' rangeToConsiderInMinigame) $
     wormPositions state

data Strategy = Dig
              | Points
              | Kill
              | GetToTheChoppa
              | Runaway
              | EndGame
              | FindDemDirt
              deriving (Eq, Show)

choppaRadius :: Int
choppaRadius = 5

dirtIsNorth :: Coord -> GameMap -> Bool
dirtIsNorth coord gameMap' =
  if isOnNorthernBorder coord
  then False
  else dirtAt (displaceCoordByMove coord (Move 8)) gameMap'

dirtIsNorthEast :: Coord -> GameMap -> Bool
dirtIsNorthEast coord gameMap' =
  if isOnNorthernBorder coord || isOnEasternBorder coord
  then False
  else dirtAt (displaceCoordByMove coord (Move 9)) gameMap'

dirtIsEast :: Coord -> GameMap -> Bool
dirtIsEast coord gameMap' =
  if isOnEasternBorder coord
  then False
  else dirtAt (displaceCoordByMove coord (Move 10)) gameMap'

dirtIsSouthEast :: Coord -> GameMap -> Bool
dirtIsSouthEast coord gameMap' =
  if isOnEasternBorder coord || isOnSouthernBorder coord
  then False
  else dirtAt (displaceCoordByMove coord (Move 11)) gameMap'

dirtIsSouth :: Coord -> GameMap -> Bool
dirtIsSouth coord gameMap' =
  if isOnSouthernBorder coord
  then False
  else dirtAt (displaceCoordByMove coord (Move 12)) gameMap'

dirtIsSouthWest :: Coord -> GameMap -> Bool
dirtIsSouthWest coord gameMap' =
  if isOnSouthernBorder coord || isOnWesternBorder coord
  then False
  else dirtAt (displaceCoordByMove coord (Move 13)) gameMap'

dirtIsWest :: Coord -> GameMap -> Bool
dirtIsWest coord gameMap' =
  if isOnWesternBorder coord
  then False
  else dirtAt (displaceCoordByMove coord (Move 14)) gameMap'

dirtIsNorthWest :: Coord -> GameMap -> Bool
dirtIsNorthWest coord gameMap' =
  if isOnWesternBorder coord || isOnNorthernBorder coord
  then False
  else dirtAt (displaceCoordByMove coord (Move 15)) gameMap'

countDirtAroundWorm :: Coord -> GameMap -> Int
countDirtAroundWorm coord gameMap' =
  (if dirtIsNorth     coord gameMap' then 1 else 0) +
  (if dirtIsNorthEast coord gameMap' then 1 else 0) +
  (if dirtIsEast      coord gameMap' then 1 else 0) +
  (if dirtIsSouthEast coord gameMap' then 1 else 0) +
  (if dirtIsSouth     coord gameMap' then 1 else 0) +
  (if dirtIsSouthWest coord gameMap' then 1 else 0) +
  (if dirtIsWest      coord gameMap' then 1 else 0) +
  (if dirtIsNorthWest coord gameMap' then 1 else 0)

thereIsNoJuicyDirtAllAround :: Coord -> GameMap -> Bool
thereIsNoJuicyDirtAllAround coord gameMap' =
  countDirtAroundWorm coord gameMap' == 0

determineStrategy :: Int -> Coord -> WormPositions -> GameMap -> Strategy
determineStrategy currentRound' currentWormsCoord' wormPositions' gameMap' =
  let pointsStrat = if thereIsNoJuicyDirtAllAround currentWormsCoord' gameMap'
                    then FindDemDirt
                    else Points
  in if currentRound' > 30
     then if currentRound' < startUsingAmmoRound
          then if aListCountOpponentsEntries wormPositions' > 1
               then Runaway
               else pointsStrat
          else if currentRound' > 302
               then Kill
               else if aListCountOpponentsEntries wormPositions' == 0
                    then pointsStrat
                    else Kill
     else if aListCountOpponentsEntries wormPositions' == 0
          then if manhattanDistanceToMiddle currentWormsCoord' < choppaRadius
               then pointsStrat
               else GetToTheChoppa
          else pointsStrat

search :: StdGen -> Strategy -> State -> SearchTree -> (SearchResult, StdGen, State)
search g strategy state searchTree =
  let round' = (currentRound state)
  in case strategy of
       Dig            -> digSearch    g       0      state searchTree [] 0
       Kill           -> killSearch   g state        state searchTree []
       Points         -> pointsSearch g       round' state searchTree [] 0
       GetToTheChoppa -> digSearch    g       0      state searchTree [] 0
       EndGame        -> endGame      g state        state searchTree []
       FindDemDirt    ->
         let result = findDemDirt state (thisPlayersCurrentWormId state)
         in (result, g, state)
       Runaway        ->
         let result = runaway state (thisPlayersCurrentWormId state)
         in (result, g, state)

maxDirtAroundWorm :: Int
maxDirtAroundWorm = 8

findDemDirt :: State -> WormId -> SearchResult
findDemDirt !state wormId' =
  go 0 (Set.fromList $ map possibilityToSeen initialPossibilities) $ PQ.fromList initialPossibilities
  where
    possibilityToSeen :: (Int, (Move, Int, State)) -> (Coord, UniqueDigMove)
    possibilityToSeen (_, (move, _, state')) = (wormsCoord state', onlyDigMovesAreUnique move)
    myInitialMoves = myRunawayMovesFrom wormId' state
    initialPossibilities = prepareForEnqueuing 0 state myInitialMoves
    prepareForEnqueuing :: Int -> State -> [Move] -> [(Int, (Move, Int, State))]
    prepareForEnqueuing steps state' =
      let steps' = steps + 1
      in map (\ move ->
                let nextState = (makeRunawayMove move state')
                in (priority steps' nextState, (move, steps', nextState)))
    makeRunawayMove move state'
      | isADigMove  move = makeMyDigMove  (thisWormsCoord state') (gameMap state') move state'
      | isAMoveMove move = makeMyMoveMove (wormPositions state')
                                          (thisWormsCoord state')
                                          wormId'
                                          (gameMap state')
                                          move
                                          state'
      | otherwise        = state'
    wormsCoord state' = aListFindDataById wormId' $ wormPositions state'
    priority steps state' = steps + (maxDirtAroundWorm - countDirtAroundWorm (wormsCoord state') (gameMap state'))
    notAlreadySeen :: Set.Set (Coord, UniqueDigMove) -> (Coord, (Move, Int, State)) -> Bool
    notAlreadySeen seen (_, (move, _, state')) =
      not $ Set.member (wormsCoord state', onlyDigMovesAreUnique move) seen
    withMove :: Move -> (Int, (Move, Int, State)) -> (Int, (Move, Int, State))
    withMove move (priority', (_, steps', state')) =  (priority', (move, steps', state'))
    go :: Int -> Set.Set (Coord, UniqueDigMove) -> PQ.PQueue Int (Move, Int, State) -> SearchResult
    go 1000 _ searchFront =
      let ((move, _, _), _) = fromJust $ PQ.minView searchFront
      in Instruction $ move
    go !n seen searchFront =
      let ((move, steps, state'), searchFront') = fromJust $ PQ.minView searchFront
          nextPossibilities                     =
            filter (notAlreadySeen seen) $ prepareForEnqueuing steps state' $ myRunawayMovesFrom wormId' state'
      in if foundDemDirts wormId' state'
         then Instruction $ move
         else go (n + 1)
                 (Set.union seen $ Set.fromList $ map possibilityToSeen nextPossibilities)
                 (PQ.union searchFront' . PQ.fromList $ map (withMove move) nextPossibilities)

foundDemDirts :: WormId -> State -> Bool
foundDemDirts wormId' state' =
  countDirtAroundWorm (aListFindDataById wormId' $ wormPositions state') (gameMap state') >= 3

endGame :: StdGen -> State -> State -> SearchTree -> Moves -> (SearchResult, StdGen, State)
endGame !g initialState !state searchTree moves =
  case gameOver state of
    GameOver payoff -> (SearchResult payoff (reverse moves), g, state)
    NoResult        -> go searchTree
  where
    go SearchFront = (SearchResult (payOff initialState state) (reverse moves), g, state)
    go (UnSearchedLevel _ (MyMoves myMoves) (OpponentsMoves opponentsMoves)) =
      let (myRecord,        g')  = intMapPickOneAtRandom g  myMoves
          (opponentsRecord, g'') = intMapPickOneAtRandom g' opponentsMoves
          myMove                 = successRecordMove myRecord
          opponentsMove          = successRecordMove opponentsRecord
          combinedMove           = fromMoves myMove opponentsMove
          state'                 = makeMove False combinedMove state
      in endGame g'' initialState state' SearchFront (combinedMove:moves)
    go (SearchedLevel _ (MyMoves myMoves) (OpponentsMoves opponentsMoves) transitions) =
      let (myRecord,        g')  = intMapPickOneAtRandom g  myMoves
          (opponentsRecord, g'') = intMapPickOneAtRandom g' opponentsMoves
          myMove                 = successRecordMove myRecord
          opponentsMove          = successRecordMove opponentsRecord
          combinedMove           = fromMoves myMove opponentsMove
          state'                 = makeMove False combinedMove state
          searchTree'            = findSubTree combinedMove transitions
      in endGame g'' initialState state' searchTree' (combinedMove:moves)

missileRange :: Int
missileRange = 4

runaway :: State -> WormId -> SearchResult
runaway !state wormId' =
  go 0 (Set.fromList $ map possibilityToSeen initialPossibilities) $ PQ.fromList initialPossibilities
  where
    possibilityToSeen :: (Int, (Move, Int, State)) -> (Coord, UniqueDigMove)
    possibilityToSeen (_, (move, _, state')) = (wormsCoord state', onlyDigMovesAreUnique move)
    opponentsDamagingMoves = filter (\ move -> (not $ isADigMove move) &&
                                               (not $ hasASelection move)) $
                             opponentsMovesFrom (filter isAMoveMove myInitialMoves) [] state
    myInitialMoves = myRunawayMovesFrom wormId' state
    initialPossibilities = prepareForEnqueuing initialPriority 0 state myInitialMoves
    initialPriority steps state' =
      3 * integerDistanceResolution * steps +
      (maxSumOfDistancePairs - aListAllPairOffs integerDistance (myWormWithHis state')) +
      if anyDamageMoveHitsWithPositions opponentsDamagingMoves (myWormWithHis state') state'
      then integerDistanceResolution
      else 0
    prepareForEnqueuing :: (Int -> State -> Int) -> Int -> State -> [Move] -> [(Int, (Move, Int, State))]
    prepareForEnqueuing priority' steps state' =
      let steps' = steps + 1
      in map (\ move ->
                let nextState = (makeRunawayMove move state')
                in (priority' steps' nextState, (move, steps', nextState)))
    prepareForEnqueuing' = prepareForEnqueuing priority
    makeRunawayMove move state'
      | isADigMove  move = makeMyDigMove  (thisWormsCoord state') (gameMap state') move state'
      | isAMoveMove move = makeMyMoveMove (wormPositions state')
                                          (thisWormsCoord state')
                                          wormId'
                                          (gameMap state')
                                          move
                                          state'
      | otherwise        = state'
    myWormWithHis state' = aListWithOnlyOneOfMyIds wormId' $ wormPositions state'
    wormsCoord state' = aListFindDataById wormId' $ wormPositions state'
    priority steps state' =
      3 * integerDistanceResolution * steps +
      (maxSumOfDistancePairs - aListAllPairOffs integerDistance (myWormWithHis state'))
    notAlreadySeen :: Set.Set (Coord, UniqueDigMove) -> (Coord, (Move, Int, State)) -> Bool
    notAlreadySeen seen (_, (move, _, state')) =
      not $ Set.member (wormsCoord state', onlyDigMovesAreUnique move) seen
    withMove :: Move -> (Int, (Move, Int, State)) -> (Int, (Move, Int, State))
    withMove move (priority', (_, steps', state')) =  (priority', (move, steps', state'))
    go :: Int -> Set.Set (Coord, UniqueDigMove) -> PQ.PQueue Int (Move, Int, State) -> SearchResult
    go 1000 _ searchFront =
      let ((move, _, _), _) = fromJust $ PQ.minView searchFront
      in Instruction $ move
    go !n seen searchFront =
      let ((move, steps, state'), searchFront') = fromJust $ PQ.minView searchFront
          nextPossibilities                     =
            filter (notAlreadySeen seen) $ prepareForEnqueuing' steps state' $ myRunawayMovesFrom wormId' state'
      in if escaped wormId' state'
         then Instruction $ move
         else go (n + 1)
                 (Set.union seen $ Set.fromList $ map possibilityToSeen nextPossibilities)
                 (PQ.union searchFront' . PQ.fromList $ map (withMove move) nextPossibilities)

anyDamageMoveHitsWithPositions :: [Move] -> WormPositions -> State -> Bool
anyDamageMoveHitsWithPositions opponentsDamagingMoves wormPositions' state =
  let coord'           = thatWormsCoord state
      gameMap'         = gameMap state
      thatWormsId      = thatPlayersCurrentWormId state
      wormIsNotFrozen' = not $ wormIsFrozen thatWormsId state
      wormsAreClose    = aListMinPairOff thatWormsId manhattanDistance wormPositions' < maxManhattanDistanceForBombs
  in any (\ move ->
            (wormIsNotFrozen'                       &&
             wormsAreClose                          &&
             isAShootMove move                      &&
             (isJust $
              shotHitsWorm coord'
                           gameMap'
                           (myDirectionsFrom coord' wormPositions')
                           wormPositions'
                           move)) ||
           (wormIsNotFrozen'                                                        &&
            wormsAreClose                                                           &&
            isABananaMove move                                                      &&
            wormHasBananasLeft thatWormsId state                                    &&
            bananaIsThrownInDirection move (myDirectionsFrom coord' wormPositions') &&
            any (\ target -> bananaBlastHitMe target wormPositions')
            (displaceToBananaDestination move coord')) ||
           (wormIsNotFrozen'                                                   &&
            wormsAreClose                                                      &&
            isASnowballMove move                                               &&
            wormHasSnowballsLeft thatWormsId state                             &&
            bananaIsThrownInDirection (snowballMoveToBananaRange move)
                                      (myDirectionsFrom coord' wormPositions') &&
            any (\ target -> (snowballBlastHitMe target wormPositions'))
            (displaceToBananaDestination (snowballMoveToBananaRange move) coord')))
     opponentsDamagingMoves

data UniqueDigMove = UniqueDigMove Int
                   | A_MOVE
                   | A_SHOOT
                   | A_SELECT
                   | A_BANANA_THROW
                   | A_SNOWBALL_THROW
                   | UNKNOWN_MOVE
                   deriving (Show, Eq, Ord)

onlyDigMovesAreUnique :: Move -> UniqueDigMove
onlyDigMovesAreUnique move@(Move x)
  | isAMoveMove     move = A_MOVE
  | isADigMove      move = UniqueDigMove x
  | hasASelection   move = A_SELECT
  | isABananaMove   move = A_BANANA_THROW
  | isASnowballMove move = A_SNOWBALL_THROW
  | otherwise            = UNKNOWN_MOVE

rangeToEscape :: Int
rangeToEscape = 10

escaped :: WormId -> State -> Bool
escaped wormId' state =
  let coord' = aListFindDataById wormId' $ wormPositions state
  in (== 0) .
     aListCountOpponentsEntries .
     aListFilterByData (\ xy -> inRange xy coord' rangeToEscape) $
     wormPositions state

pointsSearch :: StdGen -> Int -> State -> SearchTree -> Moves -> Reward -> (SearchResult, StdGen, State)
pointsSearch !g initialRound !state SearchFront moves !reward =
  (SearchResult (pointAndHealthPayOff initialRound state reward) (reverse moves), g, state)
pointsSearch !g initialRound !state tree@(SearchedLevel _ _ _ _) moves !reward =
  case gameOver state of
    GameOver payoff -> (SearchResult payoff (reverse moves), g, state)
    NoResult        -> pointsSearchSearchedLevel g initialRound state tree moves reward
pointsSearch !g
             initialRound
             !state
             (UnSearchedLevel _ (MyMoves myMoves) (OpponentsMoves opponentsMoves))
             moves
             !reward =
  case gameOver state of
    GameOver payoff -> (SearchResult payoff (reverse moves), g, state)
    NoResult        ->
      let (myRecord,        g')  = intMapPickOneAtRandom g  myMoves
          (opponentsRecord, g'') = intMapPickOneAtRandom g' opponentsMoves
          myMove                 = successRecordMove myRecord
          opponentsMove          = successRecordMove opponentsRecord
          combinedMove           = fromMoves myMove opponentsMove
          state'                 = makeMove False combinedMove state
          reward'                = digReward myMove
      in (SearchResult (pointAndHealthPayOff initialRound state' (reward' + reward)) (reverse (combinedMove:moves)), g'', state')

pointsSearchSearchedLevel :: StdGen -> Int -> State -> SearchTree -> Moves -> Reward -> (SearchResult, StdGen, State)
pointsSearchSearchedLevel _ _ _ SearchFront                   _ _ = error "pointsSearchSearchedLevel: SearchFront"
pointsSearchSearchedLevel _ _ _ level@(UnSearchedLevel _ _ _) _ _ = error $ "pointsSearchSearchedLevel: " ++ show level
pointsSearchSearchedLevel !g
                          initialRound
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
      let (myRecord,        g')  = intMapPickOneAtRandom g  myMoves
          (opponentsRecord, g'') = intMapPickOneAtRandom g' opponentsMoves
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

pointAndHealthPayOff :: Int -> State -> Int -> Payoff
pointAndHealthPayOff initialRound (State { wormHealths   = wormHealths',
                                           currentRound  = currentRound' }) !reward =
  let myTotalHealth        = aListSumMyEntries wormHealths'
      opponentsTotalHealth = aListSumOpponentsEntries wormHealths'
      maxPoints            = (currentRound' - initialRound) * digPoints
      -- Health is much more important than reward and the reward term
      -- gets bigger as you go deeper.
      myPayoff             = (30 * myTotalHealth + reward)
      opponentsPayoff      = (maxPoints + 10 * (opponentsTotalHealth + 2 * (maximumHealth - myTotalHealth)))
   in Payoff (MyPayoff myPayoff)
             (OpponentsPayoff opponentsPayoff)
             (MaxScore $ 10 * maxPayoffScore + maxPoints)

distancePayOff :: State -> WormId -> Payoff
distancePayOff (State { wormPositions = wormPositions' }) wormId' =
  let totalDistanceToOpponents = aListAllPairOffs integerDistance (aListWithOnlyOneOfMyIds wormId' wormPositions')
  in if (not $ aListContainsId wormId' wormPositions')
     then Payoff (MyPayoff 0)
                 (OpponentsPayoff 1)
                 (MaxScore 1)
     else Payoff (MyPayoff totalDistanceToOpponents)
                 (OpponentsPayoff (maxSumOfDistancePairs - totalDistanceToOpponents))
                 (MaxScore maxSumOfDistancePairs)

killSearch :: StdGen -> State -> State -> SearchTree -> Moves -> (SearchResult, StdGen, State)
-- The first iteration of play randomly is here because we need to use
-- that move when we write the first entry in an unsearched level.
killSearch !g initialState !state SearchFront                  moves =
  (SearchResult (payOff initialState state) (reverse moves), g, state)
killSearch !g initialState !state tree@(SearchedLevel _ _ _ _) moves =
  case gameOver state of
    GameOver payoff -> (SearchResult payoff (reverse moves), g, state)
    NoResult        -> killSearchSearchedLevel g initialState state tree moves
killSearch !g
           initialState
           !state
           (UnSearchedLevel _ (MyMoves myMoves) (OpponentsMoves opponentsMoves))
           moves =
  case gameOver state of
    GameOver payoff -> (SearchResult payoff (reverse moves), g, state)
    NoResult        ->
      -- TODO this might be slow :/
      let (myRecord,        g')  = intMapPickOneAtRandom g  myMoves
          (opponentsRecord, g'') = intMapPickOneAtRandom g' opponentsMoves
          myMove                 = successRecordMove myRecord
          opponentsMove          = successRecordMove opponentsRecord
          combinedMove           = fromMoves myMove opponentsMove
          state'                 = makeMove False combinedMove state
      in (SearchResult (payOff initialState state') (reverse (combinedMove:moves)), g'', state')

findSubTree :: CombinedMove -> StateTransitions -> SearchTree
findSubTree (CombinedMove idx) stateTransitions =
  subTree $ IM.findWithDefault (StateTransition (CombinedMove 0) SearchFront) idx stateTransitions

pickOneAtRandom :: StdGen -> [a] -> (a, StdGen)
pickOneAtRandom g xs =
  let (i, g') = next g
      index   = i `mod` (length xs)
  in (xs !! index, g')

intMapNth :: Int -> IM.IntMap a -> a
intMapNth n xs =
  snd $
  IM.foldl' (\ current@(!cnt, _) next' -> if cnt == n then current else (cnt + 1, next'))
  (-1, snd $ IM.findMin xs)
  xs

intMapPickOneAtRandom :: StdGen -> IM.IntMap a -> (a, StdGen)
intMapPickOneAtRandom g xs =
  let (i, g') = next g
      index   = i `mod` (IM.size xs)
  in (intMapNth index xs, g')

type Moves = [CombinedMove]

killSearchSearchedLevel :: StdGen -> State -> State -> SearchTree -> Moves -> (SearchResult, StdGen, State)
killSearchSearchedLevel _ _ _ SearchFront                   _ = error "killSearchSearchedLevel: SearchFront"
killSearchSearchedLevel _ _ _ level@(UnSearchedLevel _ _ _) _ = error $ "killSearchSearchedLevel: " ++ show level
killSearchSearchedLevel !g
                        initialState
                        !state
                        (SearchedLevel gamesPlayed (MyMoves myMoves) (OpponentsMoves opponentsMoves) transitions)
                        moves =
  let myBestMove        = successRecordMove $ nextSearchMove gamesPlayed myMoves
      opponentsBestMove = successRecordMove $ nextSearchMove gamesPlayed opponentsMoves
      combinedMove      = fromMoves myBestMove opponentsBestMove
      state'            = makeMove True combinedMove state
  in killSearch g
                initialState
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

forceGameOver :: State -> GameOver
forceGameOver state =
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
          else if myScoreIsHigher
               -- I won because of points when both players are dead
               then GameOver $ Payoff (MyPayoff 1) (OpponentsPayoff 0) killMaxScore
               -- I lost because of points when both players are dead
               else GameOver $ Payoff (MyPayoff 0) (OpponentsPayoff 1) killMaxScore

gameOver :: State -> GameOver
gameOver state =
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
          else if (currentRound state) > maxRound
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
      firstRecord = snd $ IM.findMin successRecords
  in snd $
     IM.foldl' (\ current@(!best, _) otherTree -> let next' = computeConfidence otherTree
                                                  in if next' > best
                                                     then (next', otherTree)
                                                     else current)
               (computeConfidence firstRecord, firstRecord)
               successRecords

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


myRunawayMovesFrom :: WormId -> State -> [Move]
myRunawayMovesFrom wormId' state =
  let coord' = thisWormsCoord state
  in if thisPlayersCurrentWormId state /= wormId'
     then [doNothing]
     else filter (\ move ->
                    (isAMoveMove move                        &&
                     isValidMoveMove coord' state move       &&
                     (not $ isOnLavaForRound (currentRound state) (displaceCoordByMove coord' move))) ||
                    (isADigMove  move && isValidDigMove coord' (shiftDigToMoveRange move) (gameMap state))) $
          map Move [8..23]

opponentsRunawayMovesFrom :: State -> [Move]
opponentsRunawayMovesFrom state =
  let coord' = thisWormsCoord state
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

myEndGameMovesFrom :: [Move] -> [Move] -> Bool -> State -> [Move]
myEndGameMovesFrom myMoveMoves
                   opponentsMoveMoves
                   theOpponentAndIAlignForAShot
                   state =
  let myUsefulNonMoveMoves' = myUsefulNonMoveMoves opponentsMoveMoves state
      coord'                = thisWormsCoord state
      thatCoord'            = thatWormsCoord state
      currentRound'         = currentRound state
      isOnLava              = isOnLavaForRound currentRound' coord'
      roundsExhausted       = currentRound' > maxRound
      notOnLavaAnd f pos    = (not $ isOnLavaForRound currentRound' pos) && f pos
      myMovesOutOfAlignment = filter (notOnLavaAnd (not . aligns thatCoord') . displaceCoordByMove coord') myMoveMoves
      myMovesIntoAlignment  = filter (notOnLavaAnd (aligns thatCoord')       . displaceCoordByMove coord') myMoveMoves
      moves                 =
        if isOnLava
        then filter (\ move -> let targetOfMove = displaceCoordByMove coord' move
                               in isCloserByManhattanDistance targetOfMove coord') myMoveMoves
        else if theOpponentAndIAlignForAShot
             then [doNothing] ++ myMovesOutOfAlignment ++ myUsefulNonMoveMoves'
             else [doNothing] ++ myMovesIntoAlignment  ++ myUsefulNonMoveMoves'
  in if roundsExhausted || moves == [] then [doNothing] else moves

opponentsEndGameMovesFrom :: [Move] -> [Move] -> Bool -> State -> [Move]
opponentsEndGameMovesFrom myMoveMoves
                          opponentsMoveMoves
                          theOpponentAndIAlignForAShot
                          state =
  let opponentsUsefulNonMoveMoves' = opponentsUsefulNonMoveMoves myMoveMoves state
      coord'                       = thatWormsCoord state
      thisCoord'                   = thisWormsCoord state
      currentRound'                = currentRound state
      isOnLava                     = isOnLavaForRound (currentRound state) coord'
      roundsExhausted              = currentRound' > maxRound
      notOnLavaAnd f pos           = (not $ isOnLavaForRound currentRound' pos) && f pos
      movesOutOfAlignment          = filter (notOnLavaAnd (not . aligns thisCoord') . displaceCoordByMove coord')
                                     opponentsMoveMoves
      movesIntoAlignment           = filter (notOnLavaAnd (aligns thisCoord')       . displaceCoordByMove coord')
                                     opponentsMoveMoves
      moves                        =
        if isOnLava
        then filter (\ move -> let targetOfMove = displaceCoordByMove coord' move
                               in isCloserByManhattanDistance targetOfMove coord') opponentsMoveMoves
        else if theOpponentAndIAlignForAShot
             then movesOutOfAlignment ++ opponentsUsefulNonMoveMoves'
             else movesIntoAlignment  ++ opponentsUsefulNonMoveMoves'
  in if roundsExhausted || moves == [] then [doNothing] else moves

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
     (wormIsNotFrozen'                       &&
      wormsAreClose                          &&
      isAShootMove move                      &&
      (any (\ positions -> isJust $
                           shotHitsWorm coord'
                                        gameMap'
                                        (opponentsDirectionsFrom coord' positions)
                                        positions move) $
           wormPositions' : opponentsPossibleWormPositions)) ||
     (wormIsNotFrozen'                                                               &&
      wormsAreClose                                                                  &&
      isABananaMove move                                                             &&
      wormHasBananasLeft thisWormsId state                                           &&
      bananaIsThrownInDirection move (opponentsDirectionsFrom coord' wormPositions') &&
      any (\ target -> bananaBlastHitOpponent target wormPositions')
          (displaceToBananaDestination move coord')) ||
     (wormIsNotFrozen'                                                          &&
      wormsAreClose                                                             &&
      isASnowballMove move                                                      &&
      wormHasSnowballsLeft thisWormsId state                                    &&
      bananaIsThrownInDirection (snowballMoveToBananaRange move)
                                (opponentsDirectionsFrom coord' wormPositions') &&
      any (\ target -> (snowballBlastHitOpponent target wormPositions'))
          (displaceToBananaDestination (snowballMoveToBananaRange move) coord')) ||
     (hasASelection move &&
      nonMoveMoveWouldBeValuableToMe opponentsPossibleWormPositions
                                     (makeMySelection move state)
                                     (removeSelectionFromMove move))

bananaIsThrownInDirection :: Move -> DirectionZone -> Bool
bananaIsThrownInDirection (Move 24)  direction = testBit direction neBit || testBit direction nwBit
bananaIsThrownInDirection (Move 25)  direction = testBit direction nwBit
bananaIsThrownInDirection (Move 26)  direction = testBit direction nwBit
bananaIsThrownInDirection (Move 27)  direction = testBit direction neBit || testBit direction nwBit
bananaIsThrownInDirection (Move 28)  direction = testBit direction neBit || testBit direction nwBit
bananaIsThrownInDirection (Move 29)  direction = testBit direction neBit || testBit direction nwBit
bananaIsThrownInDirection (Move 30)  direction = testBit direction neBit
bananaIsThrownInDirection (Move 31)  direction = testBit direction neBit
bananaIsThrownInDirection (Move 32)  direction = testBit direction nwBit
bananaIsThrownInDirection (Move 33)  direction = testBit direction nwBit
bananaIsThrownInDirection (Move 34)  direction = testBit direction nwBit
bananaIsThrownInDirection (Move 35)  direction = testBit direction neBit || testBit direction nwBit
bananaIsThrownInDirection (Move 36)  direction = testBit direction neBit || testBit direction nwBit
bananaIsThrownInDirection (Move 37)  direction = testBit direction neBit || testBit direction nwBit
bananaIsThrownInDirection (Move 38)  direction = testBit direction neBit
bananaIsThrownInDirection (Move 39)  direction = testBit direction neBit
bananaIsThrownInDirection (Move 40)  direction = testBit direction neBit
bananaIsThrownInDirection (Move 41)  direction = testBit direction nwBit
bananaIsThrownInDirection (Move 42)  direction = testBit direction nwBit
bananaIsThrownInDirection (Move 43)  direction = testBit direction nwBit
bananaIsThrownInDirection (Move 44)  direction = testBit direction neBit || testBit direction nwBit
bananaIsThrownInDirection (Move 45)  direction = testBit direction neBit || testBit direction nwBit
bananaIsThrownInDirection (Move 46)  direction = testBit direction neBit || testBit direction nwBit
bananaIsThrownInDirection (Move 47)  direction = testBit direction neBit
bananaIsThrownInDirection (Move 48)  direction = testBit direction neBit
bananaIsThrownInDirection (Move 49)  direction = testBit direction neBit
bananaIsThrownInDirection (Move 50)  direction = testBit direction swBit || testBit direction nwBit
bananaIsThrownInDirection (Move 51)  direction = testBit direction swBit || testBit direction nwBit
bananaIsThrownInDirection (Move 52)  direction = testBit direction swBit || testBit direction nwBit
bananaIsThrownInDirection (Move 53)  direction = testBit direction swBit || testBit direction neBit || testBit direction nwBit
bananaIsThrownInDirection (Move 54)  direction = testBit direction neBit || testBit direction nwBit
bananaIsThrownInDirection (Move 55)  direction = testBit direction neBit || testBit direction nwBit || testBit direction seBit
bananaIsThrownInDirection (Move 56)  direction = testBit direction neBit || testBit direction seBit
bananaIsThrownInDirection (Move 57)  direction = testBit direction neBit || testBit direction seBit
bananaIsThrownInDirection (Move 58)  direction = testBit direction neBit || testBit direction seBit
bananaIsThrownInDirection (Move 59)  direction = testBit direction swBit || testBit direction nwBit
bananaIsThrownInDirection (Move 60)  direction = testBit direction swBit || testBit direction nwBit
bananaIsThrownInDirection (Move 61)  direction = testBit direction swBit || testBit direction nwBit
bananaIsThrownInDirection (Move 62)  direction = testBit direction swBit || testBit direction nwBit
bananaIsThrownInDirection (Move 63)  direction = testBit direction swBit || testBit direction nwBit
bananaIsThrownInDirection (Move 64)  _         = True
bananaIsThrownInDirection (Move 65)  direction = testBit direction neBit || testBit direction seBit
bananaIsThrownInDirection (Move 66)  direction = testBit direction neBit || testBit direction seBit
bananaIsThrownInDirection (Move 67)  direction = testBit direction neBit || testBit direction seBit
bananaIsThrownInDirection (Move 68)  direction = testBit direction neBit || testBit direction seBit
bananaIsThrownInDirection (Move 69)  direction = testBit direction neBit || testBit direction seBit
bananaIsThrownInDirection (Move 70)  direction = testBit direction swBit || testBit direction nwBit
bananaIsThrownInDirection (Move 71)  direction = testBit direction swBit || testBit direction nwBit
bananaIsThrownInDirection (Move 72)  direction = testBit direction swBit || testBit direction nwBit
bananaIsThrownInDirection (Move 73)  direction = testBit direction swBit || testBit direction nwBit || testBit direction seBit
bananaIsThrownInDirection (Move 74)  direction = testBit direction swBit || testBit direction seBit
bananaIsThrownInDirection (Move 75)  direction = testBit direction seBit || testBit direction neBit
bananaIsThrownInDirection (Move 76)  direction = testBit direction seBit || testBit direction neBit
bananaIsThrownInDirection (Move 77)  direction = testBit direction seBit || testBit direction neBit
bananaIsThrownInDirection (Move 78)  direction = testBit direction seBit || testBit direction neBit
bananaIsThrownInDirection (Move 79)  direction = testBit direction swBit
bananaIsThrownInDirection (Move 80)  direction = testBit direction swBit
bananaIsThrownInDirection (Move 81)  direction = testBit direction swBit
bananaIsThrownInDirection (Move 82)  direction = testBit direction swBit || testBit direction seBit
bananaIsThrownInDirection (Move 83)  direction = testBit direction swBit || testBit direction seBit
bananaIsThrownInDirection (Move 84)  direction = testBit direction swBit || testBit direction seBit
bananaIsThrownInDirection (Move 85)  direction = testBit direction seBit
bananaIsThrownInDirection (Move 86)  direction = testBit direction seBit
bananaIsThrownInDirection (Move 87)  direction = testBit direction seBit
bananaIsThrownInDirection (Move 88)  direction = testBit direction swBit
bananaIsThrownInDirection (Move 89)  direction = testBit direction swBit
bananaIsThrownInDirection (Move 90)  direction = testBit direction swBit
bananaIsThrownInDirection (Move 91)  direction = testBit direction swBit || testBit direction seBit
bananaIsThrownInDirection (Move 92)  direction = testBit direction swBit || testBit direction seBit
bananaIsThrownInDirection (Move 93)  direction = testBit direction swBit || testBit direction seBit
bananaIsThrownInDirection (Move 94)  direction = testBit direction seBit
bananaIsThrownInDirection (Move 95)  direction = testBit direction seBit
bananaIsThrownInDirection (Move 96)  direction = testBit direction seBit
bananaIsThrownInDirection (Move 97)  direction = testBit direction swBit
bananaIsThrownInDirection (Move 98)  direction = testBit direction swBit
bananaIsThrownInDirection (Move 99)  direction = testBit direction swBit || testBit direction seBit
bananaIsThrownInDirection (Move 100) direction = testBit direction swBit || testBit direction seBit
bananaIsThrownInDirection (Move 101) direction = testBit direction swBit || testBit direction seBit
bananaIsThrownInDirection (Move 102) direction = testBit direction seBit
bananaIsThrownInDirection (Move 103) direction = testBit direction seBit
bananaIsThrownInDirection (Move 104) direction = testBit direction swBit || testBit direction seBit
bananaIsThrownInDirection move       _         = error $ "bananaIsThrownInDirection: " ++ show move


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
     (wormIsNotFrozen'                       &&
      wormsAreClose                          &&
      isAShootMove move                      &&
      (any (\ positions -> isJust $
                           shotHitsWorm coord'
                                        gameMap'
                                        (myDirectionsFrom coord' positions)
                                        positions
                                        move) $
       wormPositions' : myPossibleWormPositions)) ||
     (wormIsNotFrozen'                                                        &&
      wormsAreClose                                                           &&
      isABananaMove move                                                      &&
      wormHasBananasLeft thatWormsId state                                    &&
      bananaIsThrownInDirection move (myDirectionsFrom coord' wormPositions') &&
      any (\ target -> bananaBlastHitMe target wormPositions')
          (displaceToBananaDestination move coord')) ||
     (wormIsNotFrozen'                                                   &&
      wormsAreClose                                                      &&
      isASnowballMove move                                               &&
      wormHasSnowballsLeft thatWormsId state                             &&
      bananaIsThrownInDirection (snowballMoveToBananaRange move)
                                (myDirectionsFrom coord' wormPositions') &&
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
