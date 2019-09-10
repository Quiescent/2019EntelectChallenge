{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Bot
  where

import Import
import AList
import MakeMove
import Search
import ReadState

import Data.Aeson (decode)
import qualified Data.IntMap.Lazy as IM
import qualified RIO.ByteString.Lazy as B
import RIO.List.Partial
import Data.Maybe
import System.IO
import System.Random
import System.Clock
import Control.Exception as E
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Control.Concurrent.STM.TMVar as TMVar
import qualified Control.Monad.STM as STM
import Control.Concurrent

readRound :: IO Int
readRound = readLn

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
  let treeFromPreviousRound = case strategy of
        Dig            -> SearchFront
        GetToTheChoppa -> SearchFront
        Runaway        -> SearchFront
        FindDemDirt    -> SearchFront
        _              -> tree
  -- Comment for final submission
  -- let myMoveMoves        = myMoveMovesFrom initialState
  -- let opponentsMoveMoves = opponentsMoveMovesFrom initialState
  -- let myMovesFromTree' = sort $ map successRecordMove $ intMapValues $ myMovesFromTree tree
  -- let myMovesFromState = sort $ myMovesFrom myMoveMoves opponentsMoveMoves initialState
  -- when (strategy == Kill && tree /= SearchFront && myMovesFromTree' /= myMovesFromState) $
  --   logStdErr $ "My moves from tree diverged from moves from state!\n" ++
  --   "From tree:  " ++ joinWith (prettyPrintThisMove initialState) ", " myMovesFromTree' ++ "\n" ++
  --   "From state: " ++ joinWith (prettyPrintThisMove initialState) ", " myMovesFromState
  -- let opponentsMovesFromTree' = sort $ map successRecordMove $ intMapValues $ opponentsMovesFromTree tree
  -- let opponentsMovesFromState = sort $ opponentsMovesFrom myMoveMoves opponentsMoveMoves initialState
  -- when (strategy == Kill && tree /= SearchFront && opponentsMovesFromTree' /= opponentsMovesFromState) $
  --   logStdErr $ "Opponents moves from tree diverged from moves from state!\n" ++
  --   "From tree:  " ++ joinWith (prettyPrintThisMove initialState) ", " opponentsMovesFromTree' ++ "\n" ++
  --   "From state: " ++ joinWith (prettyPrintThisMove initialState) ", " opponentsMovesFromState
  E.catch (go gen iterationsBeforeComms treeFromPreviousRound) exceptionHandler
  where
    exceptionHandler e = do
      logStdErr $ "Worker died during [" ++
        show strategy ++ "] with exception " ++ show (e::SomeException) ++ "\n" ++
        "State: " ++ readableShow initialState ++ "\n" ++
        "state':" ++ readableShow state' ++ "\n" ++
        "restarting worker"
      iterativelyImproveSearch gen initialState SearchFront stateChannel treeVariable
    nearbyWorms    = wormsNearMyCurrentWorm initialState
    strategy       = determineStrategy (currentRound initialState)
                                       (thisWormsCoord initialState)
                                       nearbyWorms
                                       (gameMap initialState)
    startingWormId = thisPlayersCurrentWormId initialState
    state'         = if strategy == Dig || strategy == GetToTheChoppa
                     then withOnlyWormsContainedIn nearbyWorms initialState
                     else initialState
    go :: StdGen -> Int -> SearchTree-> IO ()
    go !gen' 0      !searchTree = do
      writeVariable treeVariable searchTree
      newRoundsState <- pollComms stateChannel
      case newRoundsState of
        Just (move', thatLastMoveWasInvalid, nextState) -> do
          let tree''   = if strategy == Kill || strategy == Points || strategy == EndGame
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
          -- let (myMove, opponentsMove) = toMoves move'
          -- Comment for final submission
          -- logStdErr $ "Strategy: " ++ show strategy
          -- logStdErr $ "Received moves:\n" ++
          --   "My move: " ++ prettyPrintThisMove initialState myMove ++ "\n" ++
          --   "Opponents move: " ++ prettyPrintThatMove initialState opponentsMove
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
          newTree                     = updateTree searchTree strategy startingWormId finalState result
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

treeAfterAlottedTime :: State -> Clock -> Integer -> CommsVariable SearchTree -> IO SearchTree
treeAfterAlottedTime _ clock startingTime treeVariable = do
  searchTree   <- go SearchFront
  return searchTree
  where
    go searchTree =
      (getTime clock) >>=
      \ timeNow ->
        if ((toNanoSecs timeNow) - startingTime) > maxSearchTime
        then return searchTree
        -- (logStdErr $ "Current round: " ++
        --                   (show $ currentRound state) ++
        --                   ", MyWorm: " ++
        --                   (show $ thisPlayersCurrentWormId state) ++
        --                   ", Opponent worm: " ++
        --                   (show $ thatPlayersCurrentWormId state) ++
        --                   "\n" ++
        --                   prettyPrintSearchTree state searchTree) >> return searchTree
        else do
          searchTree' <- readVariable treeVariable
          Control.Concurrent.threadDelay pollInterval
          go searchTree'

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

searchForAlottedTime :: State -> Clock -> Integer -> CommsVariable SearchTree -> IO Move
searchForAlottedTime state clock startingTime treeChannel = do
  searchTree <- treeAfterAlottedTime state clock startingTime treeChannel
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

runRound :: Clock -> Int -> Int -> Int -> Int -> Int -> Integer -> State -> CommsChannel (CombinedMove, Bool, State) -> CommsVariable SearchTree -> IO ()
runRound clock
         !thisBananaCount
         !thatBananaCount
         !thisSnowballCount
         !thatSnowballCount
         !roundNumber
         !startingTime
         previousState
         stateChannel
         treeVariable = do
  move                  <- searchForAlottedTime previousState clock startingTime treeVariable
  putStrLn $
    -- ASSUME: that the worm is on a valid square to begin with
    "C;" ++
    show roundNumber ++
    ";" ++
    formatMove thisWormsCoord makeMySelection move (thisWormsCoord previousState) previousState ++ "\n"
  roundNumber'          <- readRound
  startingTime'         <- fmap toNanoSecs $ getTime clock
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
  runRound clock
           thisBananaCount'
           thatBananaCount'
           thisSnowballCount'
           thatSnowballCount'
           roundNumber'
           startingTime'
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
  let clock      = Realtime
  startingTime  <- fmap toNanoSecs $ getTime clock
  runRound clock 3 3 3 3 initialRound' startingTime initialState stateChannel treeVariable

testStartBot :: Int -> IO ()
testStartBot initialRound' = do
  g             <- getStdGen
  treeVariable  <- newVariable SearchFront
  stateChannel  <- newComms
  -- This is where I seed it with a search front
  initialState  <- fmap fromJust $ testReadGameState initialRound'
  _             <- forkIO (iterativelyImproveSearch g initialState SearchFront stateChannel treeVariable)
  let clock      = Realtime
  startingTime  <- fmap toNanoSecs $ getTime clock
  runRound clock 3 3 3 3 initialRound' startingTime initialState stateChannel treeVariable

testReadGameState :: Int -> IO (Maybe State)
testReadGameState r = do
  stateString <- B.readFile $ "./bin/rounds/" ++ show r ++ "/state.json"
  return $ decode stateString

-- For the REPL!

-- breaking in find dem dirts
-- testState187 :: State
-- testState187 =
--   (State
--    (Just "select 3; dig 17 5")
--    187
--    (AList (160) (100) (110) (123) (91) (85))
--    (AList (545) (742) (611) (633) (274) (216))
--    (AList (-1) (3) (-1) (-1) (3) (-1))
--    (AList (-1) (-1) (3) (-1) (-1) (3))
--    (AList (-1) (-1) (-1) (-1) (-1) (-1))
--    (Player 1160 (WormId 1) (Selections 5))
--    (Player 1084 (WormId 4) (Selections 2))
--    (GameMap
--     2775124646409943009228158515019022988580037962621154450005064524925937445917531323816066477060741902244711699549173179094742987229293815672868550073815573958634717850699497623248287039361628013177544008898171705275408368628524840431373953259369421963076471376506955832789102466419356333912760901708876983602379523405260808192
--     461730141919069112966204890948188048117285585132839840697117102691058026238134878848317247081208776451390859129598223171522807555865524979589596785817672934885409474733140438573963904600491578174786154778482733412274828278219012483167922290370537401437223625473595017119086523317565819725161272384357849595246425290990393344
--     6629080181585625330052373157879515106363174975923654940028241518215220371491915860753093469404549744779594064899254521310154986911872521309325836942398486246214762831369093891129029381948948393478392377124518255967952178019953586495403468882361920382811150236794991609840953141122136184821043657629219098402869341311455385880575
--     0))

-- -- Find Dem Dirts!!
-- testState43 :: State
-- testState43 =
--   (State
--    (Just "move 28 16")
--    43
--    (AList (150) (100) (100) (150) (100) (100))
--    (AList (284) (846) (534) (866) (173) (556))
--    (AList (-1) (3) (-1) (-1) (3) (-1))
--    (AList (-1) (-1) (3) (-1) (-1) (3))
--    (AList (-1) (-1) (-1) (-1) (-1) (-1))
--    (Player 262 (WormId 1) (Selections 5))
--    (Player 254 (WormId 4) (Selections 5))
--    (GameMap
--     2844700420965163311041976884317162057974934749860000826718989907603361309118698670306926875172876881664589861104911072158037988748915008805522805526729089215337638267007066831950586129104366355190549572757151167676510612553583939327946616286844151702683026119086585080986569288734369253394095407848434568514722489531990343680
--     392154367363848811152386521650048978722388797893993463983191720013634163036967532357456848969073797031512697573860330108227806036244331846935341332902179034970704222153085533727516394512834738615846120469424831641701832503135443204052262833902104108541787277198565791278347526745825579807729677768633626018327300780945504256
--     6629080181585625330052373157879515106363174975923654940028241518215220371491915860753093469404549744779594064899254521310154986911872521309325836942398486246214762831369093891129029381948948393478392377124518255967952178019953586495403468882361920382811150236794991609840953141122136184821043657629219098402869341311455385880575
--     1978643211784836272485696144148420344918497545934470450078439369470751850024470382542996428993703553288881605695399977643272174256727320436097088476166638664576158383315353600))

-- -- Doesn't consider moves...?
-- testState328 :: State
-- testState328 =
--   (State
--    (Just "shoot SW")
--    328
--    (AList (-1) (51) (67) (65) (-1) (73))
--    (AList (-1) (608) (509) (643) (-1) (546))
--    (AList (-1) (-1) (-1) (-1) (-1) (-1))
--    (AList (-1) (-1) (-1) (-1) (-1) (3))
--    (AList (-1) (-1) (-1) (-1) (-1) (-1))
--    (Player 2015 (WormId 3) (Selections 0))
--    (Player 2113 (WormId 4) (Selections 0))
--    (GameMap
--     392154368467753200308222789086845107198235664832265322512484285449355973663161007594130246896250464269242745620875853393446270557393929643013886612542446335111972769073621254468917052616290614421657851628593264601412103767494372747745587554778849154299448939945692627391346620501347579663592500645446491991156003112111290368
--     2844700419861258921886140616880365929499087882921728968189697342167639498492505195070253477245700214426859813057895548872819524227765411009444260247090800558408154556359016807353333891345828976930672312048061174086271093139249480166796287994961110210214246062034858222516842369235574573974329673447788341206469945584139911168
--     6629080181585625330052373157879515106363174975923654940028241518215220371491915860753093469404549744779594064899254521310154986911872521309325836942398486246214762831369093891129029381948948393478392377124518255967952178019953586495403468882361920382811150236794991609840953141122136184821043657629219098402869341311455385880575
--     0))

-- movesFromTestState328 :: [String]
-- movesFromTestState328 = myEndGameMovesFromTestState testState328

-- myEndGameMovesFromTestState :: State -> [String]
-- myEndGameMovesFromTestState state =
--   map (prettyPrintThisMove state) $
--   myEndGameMovesFrom myMoveMoves opponentsMoveMoves weAlignForAShot state
--   where
--     myCoord            = thisWormsCoord         state
--     opponentsCoord     = thatWormsCoord         state
--     myMoveMoves        = myMoveMovesFrom        state
--     opponentsMoveMoves = opponentsMoveMovesFrom state
--     weAlignForAShot    = aligns myCoord opponentsCoord

-- -- New strategy!
-- movesFromTestState303 :: [String]
-- movesFromTestState303 = myEndGameMovesFromTestState testState303

-- movesFromTestState301 :: [String]
-- movesFromTestState301 = myEndGameMovesFromTestState testState301

-- testState301 :: State
-- testState301 =
--   (State
--    (Just "shoot E")
--    301
--    (AList (136) (4) (-1) (61) (44) (-1))
--    (AList (514) (579) (-1) (580) (541) (-1))
--    (AList (-1) (-1) (-1) (-1) (3) (-1))
--    (AList (-1) (-1) (-1) (-1) (-1) (-1))
--    (AList (-1) (-1) (-1) (-1) (-1) (-1))
--    (Player 1978 (WormId 1) (Selections 0))
--    (Player 1851 (WormId 4) (Selections 0))
--    (GameMap
--     2895300985501996642351983068914040202468893999336434146066818669370418585578807241609410932770553900377407933211378412767593448588841211744953159031173460542801299703248116179099406268322295059615953197263182445386071456303889118670444195050004127800399549782087402911509180114555925630967462634863609815423110506390675666944
--     341553802827015479842380337053170834228429548417560144635362958246576886576858961054972791371396778318694625467392989498672346196318128907504987828459786350718827622184521882722844675639824531736376966413471993301611740602854734244097680499735831564114145219893147938399008875180996522670459539229625017774515442305575534592
--     6629080181585625330052373157879515106363174975923654940028241518215220371491915860753093469404549744779594064899254521310154986911872521309325836942398486246214762831369093891129029381948948393478392377124518255967952178019953586495403468882361920382811150236794991609840953141122136184821043657629219098402869341311455385880575
--     0))

-- -- Spurious Shoot East
-- movesFromTestState305 :: [String]
-- movesFromTestState305 =
--   map (prettyPrintThisMove testState305) $ myMovesFrom myMoveMoves opponentsMoveMoves testState305
--   where
--     myMoveMoves = myMoveMovesFrom testState305
--     opponentsMoveMoves = opponentsMoveMovesFrom testState305

-- testState305 :: State
-- testState305 =
--   (State
--    (Just "shoot E")
--    305
--    (AList (124) (4) (-1) (34) (44) (-1))
--    (AList (482) (579) (-1) (547) (542) (-1))
--    (AList (-1) (-1) (-1) (-1) (3) (-1))
--    (AList (-1) (-1) (-1) (-1) (-1) (-1))
--    (AList (-1) (-1) (-1) (-1) (-1) (-1))
--    (Player 2031 (WormId 1) (Selections 0))
--    (Player 1865 (WormId 4) (Selections 0))
--    (GameMap
--     2895300985501996642351983068914040202468893999336434146066818669370418585578807241609410932770553900377407933211378412767593448588841211744953159031173460542801299703248116179099406268322295059615953197263182445386071456303889118670444195050004127800399549782087402911509180114555925630967462634863609815423110506390675666944
--     341553802827015479842380337053170834228429548417560144635362958246576886576858961054972791371396778318694625467392989498672346196318128907504987828459786350718827622184521882722844675639824531736376966413471993301611740602854734244097680499735831564114145219893147938399008875180996522670459539229625017774515442305575534592
--     6629080181585625330052373157879515106363174975923654940028241518215220371491915860753093469404549744779594064899254521310154986911872521309325836942398486246214762831369093891129029381948948393478392377124518255967952178019953586495403468882361920382811150236794991609840953141122136184821043657629219098402869341311455385880575
--     0))

-- -- Worm 1's move makes no sense
-- testState303 :: State
-- testState303 =
--   (State
--    (Just "move 14 16")
--    303
--    (AList (130) (4) (-1) (42) (44) (-1))
--    (AList (514) (579) (-1) (547) (542) (-1))
--    (AList (-1) (-1) (-1) (-1) (3) (-1))
--    (AList (-1) (-1) (-1) (-1) (-1) (-1))
--    (AList (-1) (-1) (-1) (-1) (-1) (-1))
--    (Player 2010 (WormId 1) (Selections 0))
--    (Player 1861 (WormId 4) (Selections 0))
--    (GameMap
--      2895300985501996642351983068914040202468893999336434146066818669370418585578807241609410932770553900377407933211378412767593448588841211744953159031173460542801299703248116179099406268322295059615953197263182445386071456303889118670444195050004127800399549782087402911509180114555925630967462634863609815423110506390675666944
--      341553802827015479842380337053170834228429548417560144635362958246576886576858961054972791371396778318694625467392989498672346196318128907504987828459786350718827622184521882722844675639824531736376966413471993301611740602854734244097680499735831564114145219893147938399008875180996522670459539229625017774515442305575534592
--      6629080181585625330052373157879515106363174975923654940028241518215220371491915860753093469404549744779594064899254521310154986911872521309325836942398486246214762831369093891129029381948948393478392377124518255967952178019953586495403468882361920382811150236794991609840953141122136184821043657629219098402869341311455385880575
--      0))

-- -- Move made by A* doesn't make sense
-- testState76 :: State
-- testState76 =
--   (State
--    (Just "move 19 12")
--     76
--     (AList (150) (100) (100) (160) (100) (110))
--     (AList (19) (943) (462) (414) (283) (415))
--     (AList (-1) (3) (-1) (-1) (3) (-1))
--     (AList (-1) (-1) (3) (-1) (-1) (3))
--     (AList (-1) (-1) (-1) (-1) (-1) (-1))
--     (Player 455 (WormId 1) (Selections 5))
--     (Player 427 (WormId 4) (Selections 5))
--     (GameMap
--      177101973057246010007088502272008021349969125392387019413652502442261681726196448344601591883428298204685332856652425341562982105922833805549248207047338801495165573235598857144367595282362889442918087487227333925542213989842716180058114072779974267850699573722427242817880452745772758106395701803791653783951318362750738432
--      3059752815271766112187274903695203015347354422361607271288529125174733790429469754319782132258522380491417225822118976924702812679236506846908898652585908092024961752197039204677883348679756701909412076189427104762140982916901136734483761476959985096662995428258123607090308536991149395531526472289443179413674630333500463104
--      6629080181585625330052373157879515106363174975923654940028241518215220371491915860753093469404549744779594064899254521310154986911872521309325836942398486246214762831369093891129029381948948393478392377124518255967952178019953586495403468882361920382811150236794991609840953141122136184821043657629219098402869341311455385880575
--      0))

-- -- Developing A*
-- testState :: State
-- testState = (State
--               (Just "move 15 14")
--               110
--               (AList (-1) (100) (100) (130) (102) (97))
--               (AList (-1) (709) (506) (510) (477) (508))
--               (AList (-1) (2) (-1) (-1) (-1) (-1))
--               (AList (-1) (-1) (1) (-1) (-1) (-1))
--               (AList (-1) (-1) (-1) (-1) (-1) (-1))
--               (Player 597 (WormId 2) (Selections 0))
--               (Player 914 (WormId 12) (Selections 0))
--               (GameMap
--                  1565454930231473823837895396229990475607854401278457579774236360629880979412587761549012591647444578646527032219811307949308211523267453224341183386807278228319714149932817716098433278288596511710084570560671852138118127359991934255893602400826930665374109081280696252634551283511551620636920227031347549690603421397308999680
--                  1671399858097538298356468009737220561089469146475536710927945266987114492743078441115371132494506100049575526458960094316957583261891887428116963472825968665200413175499820345723817665673523079642245593115982586549565069546751918658648273148913028699139585920699854597273637706225370533001001947061887283507022527298942201856
--                  6629080181585625330052373157879515106363174975923654940028241518215220371491915860753093469404549744779594064899254521310154986911872521309325836942398486246214762831369093891129029381948948393478392377124518255967952178019953586495403468882361920382811150236794991609840953141122136184821043657629219098402869341311455385880575
--                  0))
