{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module BotSpec (spec) where

import Bot
import Import

import qualified RIO.Vector.Boxed as V
import qualified RIO.List.Partial as L
import RIO.List
import qualified RIO.HashSet as S
import Data.Bits
import Data.Maybe

import Test.Hspec
import Test.Hspec.QuickCheck

countMyWins :: SearchTree -> Int
countMyWins = sum . map ( (\ (Wins x) -> x) . wins) . myMovesFromTree

opponentsMovesFromTree :: SearchTree -> SuccessRecords
opponentsMovesFromTree (SearchedLevel   _ (OpponentsMoves opponentsMoves) _) = opponentsMoves
opponentsMovesFromTree (UnSearchedLevel _ (OpponentsMoves opponentsMoves))   = opponentsMoves
opponentsMovesFromTree SearchFront                                           =
  error $ "myMovesFromTree of SearchFront"

countOpponentsWins :: SearchTree -> Int
countOpponentsWins = sum . map ( (\ (Wins x) -> x) . wins) . opponentsMovesFromTree

isSearched :: SearchTree -> Bool
isSearched (SearchedLevel myMoves opponentsMoves _) = allGamesPlayed myMoves opponentsMoves
isSearched _                                        = False

countGames' :: SearchTree -> Int
countGames' = gamesPlayedForRecords . myMovesFromTree

transitions :: SearchTree -> StateTransitions
transitions (SearchedLevel   _ _ transitions') = transitions'
transitions (UnSearchedLevel _ _)              = []
transitions SearchFront                        = []

spec :: Spec
spec = do
  describe "takeLavaDamage" $ do
    context "when the current round is 0" $ do
      let aStateWithRoundAtZero = withCurrentRound 0 aState
      it "shouldn't deal any damage" $
        dealLavaDamage aStateWithRoundAtZero `shouldBe`
        aStateWithRoundAtZero
    context "when the round is 105" $ do
      let aStateWithRoundAtOneHundredAndFive =
            withCurrentRound 105 $
            withWormPositions (always $ AList (toCoord 11 0)
                                              (toCoord 4  10)
                                              (toCoord 5  20)
                                              (toCoord 6  15)
                                              (toCoord 7  20)
                                              (toCoord 8  23)) $
            withWormHealths (always (AList 20 20 20 20 20 20)) $
            aState
      it "should deal 3 damage to any worm at coord (11, 0)" $ do
        dealLavaDamage aStateWithRoundAtOneHundredAndFive `shouldBe`
          (withWormHealths (always (AList 17 20 20 20 20 20)) $
           aStateWithRoundAtOneHundredAndFive)
      let aStateWithRoundAtOneHundredAndFiveAndAWormAtTwelveZero =
            withCurrentRound 105 $
            withWormPositions (always $ AList (toCoord 12 0)
                                              (toCoord 4  10)
                                              (toCoord 5  20)
                                              (toCoord 6  15)
                                              (toCoord 7  20)
                                              (toCoord 8  23)) $
            withWormHealths (always (AList 17 20 20 20 20 20)) $
            aState
      it "should not deal 3 damage to any worm at coord (12, 0)" $ do
        dealLavaDamage aStateWithRoundAtOneHundredAndFiveAndAWormAtTwelveZero `shouldBe`
          aStateWithRoundAtOneHundredAndFiveAndAWormAtTwelveZero
      let aStateWithLowHealthWormOnLava = withCurrentRound 105 $
            withWormPositions (always $ AList (toCoord 11 0)
                                              (toCoord 4  10)
                                              (toCoord 5  20)
                                              (toCoord 6  15)
                                              (toCoord 7  20)
                                              (toCoord 30 30)) $
            withWormHealths (always (AList 3 20 20 20 20 1)) $
            withWormSnowballs (always (AList (-1) (-1) 3 (-1) (-1) 3)) $
            withWormFrozenDurations (always (AList 0 0 0 0 0 0))
            aState
      it "should kill any worms with low enough health, removing them fcom the state" $
        dealLavaDamage aStateWithLowHealthWormOnLava `shouldBe`
        (cleanUpDeadWorm (WormId 1) $ cleanUpDeadWorm (WormId 12) $ aStateWithLowHealthWormOnLava)
  describe "wormsNearMyCurrentWorm" $ do
    context "when there are no worms nearby" $ do
      let aStateWithNoWormsNearMyWorm =
            withWormPositions (always $ AList (toCoord 15 31)
                                              (toCoord 1 31)
                                              (toCoord 1 30)
                                              (toCoord 0 31)
                                              (toCoord 0 1)
                                              (toCoord 2 1))
            aState
      it "should produce just the current worm" $
        wormsNearMyCurrentWorm aStateWithNoWormsNearMyWorm `shouldBe`
        (aListFromList [(1, toCoord 15 31)])
    context "when there is one of my worms nearby" $ do
      let aStateWithOneOfMyWormsNearby =
            withWormPositions (always $ AList (toCoord 15 31)
                                              (toCoord 16 31)
                                              (toCoord 1 30)
                                              (toCoord 0 31)
                                              (toCoord 0 1)
                                              (toCoord 2 1))
            aState
      it "should produce just my two worms" $
        wormsNearMyCurrentWorm aStateWithOneOfMyWormsNearby `shouldBe`
        (aListFromList [(1, toCoord 15 31),
                        (2, toCoord 16 31)])
    context "when there is an enemy worm nearby" $ do
      let aStateWithAnEnemyWormNearby =
            withWormPositions (always $ AList (toCoord 15 31)
                                              (toCoord 16 31)
                                              (toCoord 1 30)
                                              (toCoord 14 31)
                                              (toCoord 0 1)
                                              (toCoord 2 1))
            aState
      it "should produce the nearby enemy worm" $
        wormsNearMyCurrentWorm aStateWithAnEnemyWormNearby `shouldBe`
        (aListFromList [(1, toCoord 15 31),
                        (2, toCoord 16 31),
                        (4, toCoord 14 31)])
  describe "determineStrategy" $ do
    context "when no werms are nearby" $ do
      let positionsWithNooneNearby = aListFromList [(1, toCoord 15 31)]
      it "should produce a strategy of Dig" $
        determineStrategy positionsWithNooneNearby `shouldBe` Dig
    context "when there is another friendly worm nearby" $ do
      let positionsWithOneOfMyWormsNearby = aListFromList [(1, toCoord 15 31),
                                                           (2, toCoord 16 31)]
      it "should produce a strategy of Dig" $
        determineStrategy positionsWithOneOfMyWormsNearby `shouldBe` Dig
    context "when there is an enemy nearby" $ do
      let positionsWithAnEnemyNearby = aListFromList [(1, toCoord 15 31),
                                                      (2, toCoord 16 31),
                                                      (4, toCoord 14 31)]
      it "should produce strategy of kill" $
        determineStrategy positionsWithAnEnemyNearby `shouldBe` Kill
  describe "mapAt" $ do
    prop "it should produce an error for any coordinate when the map is empty" $ \ x ->
      let coord' = (abs x) `mod` (mapDim * mapDim)
      in evaluate (mapAt coord' emptyGameMap) `shouldThrow` anyException
    prop "it should always produce AIR for a map with only air on it" $ \ x ->
      let coord' = (abs x) `mod` (mapDim * mapDim)
      in mapAt coord' airOnlyGameMap `shouldBe` AIR
    prop "it should always produce DIRT for a map with only dirt on it" $ \ x ->
      let coord' = (abs x) `mod` (mapDim * mapDim)
      in mapAt coord' dirtOnlyGameMap `shouldBe` DIRT
    prop "it should always produce DEEP_SPACE for a map with only deep_space on it" $ \ x ->
      let coord' = (abs x) `mod` (mapDim * mapDim)
      in mapAt coord' deepSpaceOnlyGameMap `shouldBe` DEEP_SPACE
    prop "it should always produce MEDIPACK for a map with only medipack on it" $ \ x ->
      let coord' = (abs x) `mod` (mapDim * mapDim)
      in mapAt coord' medipackOnlyGameMap `shouldBe` MEDIPACK
  describe "modifyMapCellAt" $ do
    context "supplied with the all AIR map" $ do
      prop "it should produce the desired square at the desired coordinate when changed to that type" $ \ (x, y) ->
        let coord = (abs x) `mod` (mapDim * mapDim)
            cell  = [AIR, DIRT, DEEP_SPACE, MEDIPACK] L.!! ((abs y) `mod` 4)
        in (mapAt coord $ modifyMapCellAt coord (always cell) airOnlyGameMap) `shouldBe` cell
  describe "parseLastCommand" $ do
    it "should be able to parse all of the opponents moves from a state" $
      let opponentsMoves          = opponentsMovesFrom aState
          opponentsMovesAsStrings = map (prettyPrintThatMove aState) opponentsMoves
      in (map (parseLastCommand aState . Just) opponentsMovesAsStrings) `shouldBe` opponentsMoves
  describe "diffMax" $ do
    it "should produce a very high payoff when the score which I got was very high" $ do
      diffMax [Reward (MyReward 100)  (OpponentsReward 1500),
               Reward (MyReward 100)  (OpponentsReward 1500),
               Reward (MyReward 100)  (OpponentsReward 1500),
               Reward (MyReward 100)  (OpponentsReward 1500)] `shouldBe`
        (Payoff (MyPayoff 400) (OpponentsPayoff 6000) digMaxScore)
    it "should decrease with every move down the chain" $
      diffMax [Reward (MyReward 0) (OpponentsReward 7),
               Reward (MyReward 0) (OpponentsReward 7),
               Reward (MyReward 0) (OpponentsReward 7),
               Reward (MyReward 0) (OpponentsReward 7)] `shouldBe`
      (Payoff (MyPayoff 0) (OpponentsPayoff $ 7 + 7 + 7 + 7) digMaxScore)
  let maxScore = 20
  describe "updateCount" $ do
    prop "should produce the same number of records when updating a record regardless of whether it's there or not" $ \ (i, k) ->
      let myMoves      = myMovesFrom aState
          thisMove     = myMoves L.!! (i `mod` length myMoves)
          k'           = k `mod` maxScore + 1
          updateCount' = incInc k' ((\ (MaxScore x) -> x) digMaxScore)
          oldCounts    = map (\ move -> (SuccessRecord (Wins 1) (Played 1) move)) myMoves
          newCounts    = updateCount updateCount' oldCounts thisMove
      in newCounts `shouldSatisfy` ((== (length oldCounts)) . length)
    prop "should change the played count of the selected record and might change the win count" $ \ (i, k) ->
      let myMoves      = myMovesFrom aState
          thisMove     = myMoves L.!! (i `mod` length myMoves)
          k'           = k `mod` maxScore + 1
          updateCount' = incInc k' ((\ (MaxScore x) -> x) digMaxScore)
          oldCounts    = map (\ move -> (SuccessRecord (Wins 1) (Played 1) move)) myMoves
          newCounts    = updateCount updateCount' oldCounts thisMove
      in newCounts `shouldSatisfy` ((((== (Played $ 2))      . played) .&&.
                                    (((== (Wins   $ 1 + k')) . wins))) .
                                    fromJust . find ((== thisMove) . successRecordMove))
  describe "myMovesFrom" $ do
    it "should  not contain repeats" $
      let myMoves = myMovesFrom aState
      in myMoves `shouldSatisfy` (== (length (nub myMoves))) . length
  describe "updateTree" $ do
    prop "should produce a searched level from an unsearched level when the last game is played for the level" $  \ (k) ->
      let myMoves        = myMovesFrom aState
          thisMove       = L.head myMoves
          opponentsMoves = opponentsMovesFrom aState
          thatMove       = L.head opponentsMoves
          k'             = k `mod` maxScore + 1
          oldTree        = UnSearchedLevel
                           (MyMoves        $
                            (SuccessRecord (Wins 0) (Played 0) $ L.head myMoves) :
                            (map (\ move -> (SuccessRecord (Wins 1) (Played 1) move)) $ L.tail myMoves))
                           (OpponentsMoves $
                            (SuccessRecord (Wins 0) (Played 0) $ L.head opponentsMoves) :
                            (map (\ move -> (SuccessRecord (Wins 1) (Played 1) move)) $ L.tail opponentsMoves))
          newTree        = updateTree Dig
                                      aState
                                      (SearchResult
                                       (Payoff (MyPayoff $ abs k') (OpponentsPayoff $ maxScore - abs k') digMaxScore)
                                       [fromMoves thisMove thatMove])
                                      oldTree
      in ((thisMove, thatMove), newTree) `shouldSatisfy` isSearched . snd
    let aStateWithAnEnemyWormNearby =
            withWormPositions (always $ AList (toCoord 15 31)
                                              (toCoord 16 31)
                                              (toCoord 1 30)
                                              (toCoord 14 31)
                                              (toCoord 0 1)
                                              (toCoord 2 1))
            aState
    prop "should produce a tree with one result on it when given a SearchFront" $ \ (i, j, k) ->
      let myMoves        = myMovesFrom aStateWithAnEnemyWormNearby
          thisMove       = myMoves L.!! (i `mod` length myMoves)
          opponentsMoves = opponentsMovesFrom aStateWithAnEnemyWormNearby
          thatMove       = opponentsMoves L.!! (j `mod` length opponentsMoves)
          k'             = k `mod` (maxScore + 1)
          newTree        = updateTree Kill
                                      aStateWithAnEnemyWormNearby
                                      (SearchResult
                                       (Payoff (MyPayoff $ abs k') (OpponentsPayoff $ maxScore - abs k') digMaxScore)
                                       [fromMoves thisMove thatMove])
                                      SearchFront
      in newTree `shouldSatisfy` (((== (maxScore - k')) . countOpponentsWins) .&&.
                                  ((== k')              . countMyWins)        .&&.
                                  ((== 1)               . countGames'))
    prop "should increment the game count when given an UnSearchedLevel" $ \ (i, j, k) ->
      let myMoves        = myMovesFrom aState
          thisMove       = myMoves L.!! (i `mod` length myMoves)
          opponentsMoves = opponentsMovesFrom aState
          thatMove       = opponentsMoves L.!! (j `mod` length opponentsMoves)
          k'             = k `mod` (maxScore + 1)
          oldTree        = UnSearchedLevel
                           (MyMoves        $ map (\ move -> (SuccessRecord (Wins 1) (Played 1) move)) myMoves)
                           (OpponentsMoves $ map (\ move -> (SuccessRecord (Wins 1) (Played 1) move)) opponentsMoves)
          newTree        = updateTree Dig
                                      aState
                                      (SearchResult
                                       (Payoff (MyPayoff k') (OpponentsPayoff $ maxScore - k') digMaxScore)
                                       [fromMoves thisMove thatMove])
                                      oldTree
      in newTree `shouldSatisfy` (((== (1     + countGames' oldTree))                  . countGames')         .&&.
                                  ((== ((maxScore - k') + countOpponentsWins oldTree)) . countOpponentsWins) .&&.
                                  ((== (k'              + countMyWins        oldTree)) . countMyWins))
    prop "should increment the game count when given a SearchedLevel" $ \ (i, j, k) ->
      let myMoves        = myMovesFrom aState
          thisMove       = myMoves L.!! (i `mod` length myMoves)
          opponentsMoves = opponentsMovesFrom aState
          thatMove       = opponentsMoves L.!! (j `mod` length opponentsMoves)
          k'             = k `mod` (maxScore + 1)
          oldTree        = SearchedLevel
                           (MyMoves        $ map (\ move -> (SuccessRecord (Wins 1) (Played 1) move)) myMoves)
                           (OpponentsMoves $ map (\ move -> (SuccessRecord (Wins 1) (Played 1) move)) opponentsMoves)
                           []
          newTree        = updateTree Dig
                                      aState
                                      (SearchResult
                                       (Payoff (MyPayoff k') (OpponentsPayoff $ maxScore - k') digMaxScore)
                                       [fromMoves thisMove thatMove])
                                      oldTree
      in newTree `shouldSatisfy` (((== (1     + countGames' oldTree))                  . countGames')         .&&.
                                  ((== ((maxScore - k') + countOpponentsWins oldTree)) . countOpponentsWins) .&&.
                                  ((== (k'              + countMyWins        oldTree)) . countMyWins))
    prop "should add an unsearched level to the state transitions for this tree" $ \ (i, j, k) ->
      let myMoves        = myMovesFrom aStateWithAnEnemyWormNearby
          thisMove       = myMoves L.!! (i `mod` length myMoves)
          opponentsMoves = opponentsMovesFrom aStateWithAnEnemyWormNearby
          thatMove       = opponentsMoves L.!! (j `mod` length opponentsMoves)
          k'             = k `mod` (maxScore + 1)
          oldTree        = SearchedLevel
                           (MyMoves        $ map (\ move -> (SuccessRecord (Wins 1) (Played 1) move)) myMoves)
                           (OpponentsMoves $ map (\ move -> (SuccessRecord (Wins 1) (Played 1) move)) opponentsMoves)
                           []
          newTree        = updateTree Dig
                                      aStateWithAnEnemyWormNearby
                                      (SearchResult
                                       (Payoff (MyPayoff k') (OpponentsPayoff $ maxScore - k') digMaxScore)
                                       [fromMoves thisMove thatMove, fromMoves thisMove thatMove])
                                      oldTree
      in ((thisMove, thatMove), newTree) `shouldSatisfy`
         (((== 1) . length . transitions) .&&.
          ((((== (Wins $ 1 +  k'))         . wins) .&&.
            ((== (Played $ 1 +  1))        . played)) .
           (fromJust . find ((== thisMove) . successRecordMove) . myMovesFromTree)) .&&.
          ((((== (Wins $ 1 + maxScore - k')) . wins) .&&.
            ((== (Played $ 1 + 1))           . played)) .
           (fromJust . find ((== thatMove) . successRecordMove) . opponentsMovesFromTree))) . snd
    prop "should add an unsearched level to the state transitions for this tree" $ \ (i, j, k, l, m) ->
      let myMoves        = myMovesFrom aStateWithAnEnemyWormNearby
          thisMove       = myMoves L.!! (i `mod` length myMoves)
          thisMove'      = myMoves L.!! (m `mod` length myMoves)
          opponentsMoves = opponentsMovesFrom aStateWithAnEnemyWormNearby
          thatMove       = opponentsMoves L.!! (j `mod` length opponentsMoves)
          thatMove'      = opponentsMoves L.!! (l `mod` length opponentsMoves)
          k'             = k `mod` (maxScore + 1)
          oldTree        = SearchedLevel
                           (MyMoves        $ map (\ move -> (SuccessRecord (Wins 1) (Played 1) move)) myMoves)
                           (OpponentsMoves $ map (\ move -> (SuccessRecord (Wins 1) (Played 1) move)) opponentsMoves)
                           []
          newTree        = updateTree Kill
                                      aStateWithAnEnemyWormNearby
                                      (SearchResult
                                        (Payoff (MyPayoff $ abs k') (OpponentsPayoff $ maxScore - abs k') digMaxScore)
                                        [fromMoves thisMove thatMove,
                                         fromMoves thisMove' thatMove'])
                                      oldTree
      in ((thisMove', thatMove'), newTree) `shouldSatisfy`
         (((((== (Wins k'))  . wins) .&&.
            ((== (Played 1)) . played)) .
           (fromJust . find ((== thisMove') . successRecordMove) . myMovesFromTree)) .&&.
          ((((== (Wins $ maxScore - k')) . wins) .&&.
            ((== (Played 1))             . played)) .
           (fromJust . find ((== thatMove') . successRecordMove) . opponentsMovesFromTree))) .
         makeMoveInTree (fromMoves thisMove thatMove) . snd
  describe "formatMove" $ do
    prop "should produce the correct type of move for the correct range" $ \ (x, y) ->
      let x'            = abs x `mod` 186
          y'            = shiftL (abs y `mod` 4) selectEncodingRange
          move'         = x' .|. y'
          formattedMove = formatMove thisWormsCoord makeMySelection (Move move') (toCoord 6 6) aState
      in if move' < 8
         then formattedMove `shouldStartWith` "shoot"
         else if move' < 16
              then formattedMove `shouldStartWith` "move"
              else if move' < 24
                   then formattedMove `shouldStartWith` "dig"
                   else if move' < 105
                        then formattedMove `shouldStartWith` "banana"
                        else if move' < 187
                             then formattedMove `shouldStartWith` "snowball"
                             else formattedMove `shouldStartWith` "select"
  describe "blastCoordDeltasInRange" $ do
    prop "should always produce a coord within range of 2 (blast radius of banana bomb)" $ \ (i, j) ->
      let x'     = abs i `mod` mapDim
          y'     = abs j `mod` mapDim
          coord' = toCoord x' y'
          coords = catMaybes $ map ($ coord') blastCoordDeltasInRange
      in (coord', zip coords (map (flip (inRange coord') 2) $ map snd coords))
         `shouldSatisfy`
         (all snd . snd)
    prop "should produce 13 possible coords when the from coord is at least 2 inside of borders" $ \ (i, j) ->
      let x'     = 2 + (abs i `mod` (mapDim - 4))
          y'     = 2 + (abs j `mod` (mapDim - 4))
          coord' = toCoord x' y'
          coords = catMaybes $ map ($ coord') blastCoordDeltasInRange
      in (coord', coords) `shouldSatisfy` ((== 13) . S.size . S.fromList . map snd . snd)
  describe "coordDeltasInRange" $ do
    prop "should always produce a coord within range of 5 (banana bomb range)" $  \ (i, j) ->
      let x'     = abs i `mod` mapDim
          y'     = abs j `mod` mapDim
          coord' = toCoord x' y'
          coords = catMaybes $ map ($ coord') coordDeltasInRange
      in (coord', zip coords (map (flip (inRange coord') 5) coords))
         `shouldSatisfy`
         (all snd . snd)
    prop "should produce 81 possible coords when the from coord is at least 5 inside of borders" $ \ (i, j) ->
      let x'     = 5 + (abs i `mod` (mapDim - 10))
          y'     = 5 + (abs j `mod` (mapDim - 10))
          coord' = toCoord x' y'
          coords = catMaybes $ map ($ coord') coordDeltasInRange
      in (coord', coords) `shouldSatisfy` ((== 81) . S.size . S.fromList . snd)
  describe "displaceCoordByMove" $ do
    it "N  (not on boundry)" $ displaceCoordByMove (toCoord 1 1) (Move 8)  `shouldBe` (toCoord 1 0)
    it "NE (not on boundry)" $ displaceCoordByMove (toCoord 1 1) (Move 9)  `shouldBe` (toCoord 2 0)
    it "E  (not on boundry)" $ displaceCoordByMove (toCoord 1 1) (Move 10) `shouldBe` (toCoord 2 1)
    it "SE (not on boundry)" $ displaceCoordByMove (toCoord 1 1) (Move 11) `shouldBe` (toCoord 2 2)
    it "S  (not on boundry)" $ displaceCoordByMove (toCoord 1 1) (Move 12) `shouldBe` (toCoord 1 2)
    it "SW (not on boundry)" $ displaceCoordByMove (toCoord 1 1) (Move 13) `shouldBe` (toCoord 0 2)
    it "W  (not on boundry)" $ displaceCoordByMove (toCoord 1 1) (Move 14) `shouldBe` (toCoord 0 1)
    it "NW (not on boundry)" $ displaceCoordByMove (toCoord 1 1) (Move 15) `shouldBe` (toCoord 0 0)
    prop "Move back and forth" $ \ (i, j, k) ->
      let x'          = 1 + (j `mod` (mapDim - 2))
          y'          = 1 + (k `mod` (mapDim - 2))
          coordInMap  = toCoord x' y'
          indexOfMove = ((abs i) `mod` 8)
          randomMove  = Move $ indexOfMove + 8
          moveBack    = Move $ ((indexOfMove + 4) `mod` 8) + 8
      in displaceCoordByMove (displaceCoordByMove coordInMap randomMove) moveBack `shouldBe` coordInMap
  describe "combined moves" $ do
    it "should be able to extract the maximum move" $
      let iMove = Move 2047
          jMove = Move 2047
      in toMoves (fromMoves iMove jMove) `shouldBe` (iMove, jMove)
    prop "Can always be extracted" $ \ (i, j) ->
      -- TODO: this should be failing :/
      -- There aren't 2048 many moves but there are that many slots.
      let iMove = Move $ (abs i) `mod` 2048
          jMove = Move $ (abs j) `mod` 2048
      in toMoves (fromMoves iMove jMove) `shouldBe` (iMove, jMove)
  describe "coordinates" $ do
    prop "Can always be extracted" $ \ (i, j) ->
      let x' = inBoundsWithNoPadding (abs i)
          y' = inBoundsWithNoPadding (abs j)
      in fromCoord (toCoord x' y') `shouldBe` (x', y')
  describe "penaliseForInvalidCommand" $ do
    it "should reduce the given players score by 4" $
      penaliseForInvalidCommand aPlayer `shouldBe`
      Player 296 (WormId 1) startingSelections
  describe "penaliseThatPlayerForAnInvalidCommand" $ do
    it "should reduce the points of the opponent by 4" $
      penaliseThatPlayerForAnInvalidCommand aState `shouldBe`
      aState { opponent = Player 296 (WormId 4) startingSelections }
  describe "penaliseThisPlayerForAnInvalidCommand" $ do
    it "should reduce the points of the player by 4" $
      penaliseThisPlayerForAnInvalidCommand aState `shouldBe`
      aState { myPlayer = Player 296 (WormId 1) startingSelections }
  describe "awardPointsForMovingToAir" $ do
    it "should increment the points of a player by 5" $
      awardPointsForMovingToAir aPlayer `shouldBe`
      Player 305 (WormId 1) startingSelections
  describe "awardPointsToThatPlayerForMovingToAir" $ do
    it "should increment the points of opponent by 5" $
      awardPointsToThatPlayerForMovingToAir aState `shouldBe`
      aState { opponent = Player 305 (WormId 4) startingSelections }
  describe "awardPointsToThisPlayerForMovingToAir" $ do
    it "should increment the points of my player by 5" $
      awardPointsToThisPlayerForMovingToAir aState `shouldBe`
      aState { myPlayer = Player 305 (WormId 1) startingSelections }
  describe "awardPointsForDigging" $ do
    it "should increment the points of a player by 7" $
      awardPointsForDigging aPlayer `shouldBe`
      Player 307 (WormId 1) startingSelections
  describe "awardPointsToThisPlayerForDigging" $ do
    it "should increment this players points by 7" $
      awardPointsToThisPlayerForDigging aState `shouldBe`
      aState { myPlayer = Player 307 (WormId 1) startingSelections }
  describe "awardPointsToThatPlayerForDigging" $ do
    it "should increment that players points by 7" $
      awardPointsToThatPlayerForDigging aState `shouldBe`
      aState { opponent = Player 307 (WormId 4) startingSelections }
  describe "harmWormWithRocket" $ do
    it "should remove health from the worm" $
      (harmWormWithRocket (WormId (-1)) aState id id id (toCoord 15 31) aState) `shouldBe`
       aState { wormHealths = aListRemoveWormById (WormId 1) $ wormHealths aState,
                wormPositions = aListRemoveWormById (WormId 1) $ wormPositions aState }
  describe "generateShotSwitch" $ do
    prop "produces a function which produces the first given a negative number and the second given a positive number" $
      let switchFunction = generateShotSwitch shootEast shootNorth
      in \ x ->
        switchFunction x `shouldBe` if x > 0 then shootEast else shootNorth
  describe "takeBothWorms" $ do
    prop "creates a map of with worm 4 and 8 at distinct coordinates given two random distinct coordinates" $ \ (i, j) ->
      let thisCoord = generateInBoundsCoordinate i j
          thatCoord = generateInBoundsCoordinate (i + 20) (j + 20) -- Ensure distinct coordinates
      in (length $
          filter (\ (WormId x, _) -> x == 4 || x == 8) $
          aListToList $
          wormHealths $
          takeBothWorms (WormId 4) (WormId 8) thisCoord thatCoord aStateWithoutWorms) `shouldBe` 2
  describe "generateInBoundsCoordinate" $ do
    prop "creates coordinates which are positive or zero on the x-axis" $ \ (i, j) ->
      let (x, _) = fromCoord $ generateInBoundsCoordinate i j
      in x >= 0
  describe "generateInBoundsCoordinate" $ do
    prop "creates coordinates which are less than the map dimension in the x-axis" $ \ (i, j) ->
      let (x, _) = fromCoord $ generateInBoundsCoordinate i j
      in x < mapDim
  describe "generateInBoundsCoordinate" $ do
    prop "creates coordinates which are positive or zero on the y-axis" $ \ (i, j) ->
      let (_, y) = fromCoord $ generateInBoundsCoordinate i j
      in y >= 0
  describe "generateInBoundsCoordinate" $ do
    prop "creates coordinates which are less than the map dimension in the y-axis" $ \ (i, j) ->
      let (_, y) = fromCoord $ generateInBoundsCoordinate i j
      in y < mapDim
  describe "nonDiagonalDelta" $ do
    prop "creates numbers greater than or equal to -3" $ \ x ->
      nonDiagonalDelta x >= (-3)
    prop "creates numbers less than or equal to 3" $ \ x ->
      nonDiagonalDelta x <= 3
    prop "never creates the number 0" $ \ x ->
      nonDiagonalDelta x /= 0
  describe "diagonalDelta" $ do
    prop "creates numbers greater than or equal to -2" $ \ x ->
      diagonalDelta x >= (-2)
    prop "creates numbers less than or equal to 2" $ \ x ->
      diagonalDelta x <= 2
    prop "never creates the number 0" $ \ x ->
      diagonalDelta x /= 0
  describe "nonDiagonalDeltaOfAtLeastTwo" $ do
    prop "creates the numbers -2, -3, 2 and 3" $ \ x ->
      elem (nonDiagonalDeltaOfAtLeastTwo x) [-2, -3, 2, 3]
  describe "divergeFromZero" $ do
    prop "creates numbers of at most a difference of deltaMagnitude from the original number" $ \ x ->
      abs (divergeFromZero 2 x - x) `shouldBe` 2
    prop "doesn't change the sign of the given number" $ \ x ->
      (divergeFromZero 2 x) * x >= 0
  describe "inBoundsWithNonDiagonalPadding" $ do
    prop "creates numbers greater than or equal to 3" $ \ x ->
      inBoundsWithNonDiagonalPadding x >= 3
    prop "creates numbers less than or equal to `mapDim' - 3" $ \ x ->
      inBoundsWithNonDiagonalPadding x <= (mapDim - 3)
  describe "inBoundsWithDiagonalPadding" $ do
    prop "creates numbers greater than or equal to 2" $ \ x ->
      inBoundsWithNonDiagonalPadding x >= 2
    prop "creates numbers less than or equal to `mapDim' - 2" $ \ x ->
      inBoundsWithNonDiagonalPadding x <= (mapDim - 2)
  describe "inBoundsWithNoPadding" $ do
    prop "creates numbers greater than or equal to 0" $ \ x ->
      inBoundsWithNoPadding x >= 0
    prop "creates numbers less than the map dimension" $ \ x ->
      inBoundsWithNoPadding x < mapDim
  describe "isAPositionOfAWorm" $ do
    it "should produce HitNothing when the position is not held by any of the given worms" $
      isAPositionOfAWorm (toCoord 0 0) someWormPositions `shouldBe` HitNothing
    it "should produce HitWorm of the worm hit when a coord shares it's position with a worm" $
      isAPositionOfAWorm (toCoord 1 31) someWormPositions `shouldBe`
      HitWorm (toCoord 1 31)
  describe "obstacleAt" $ do
    it "should produce HitObstacle when targetting DEEP_SPACE" $
      obstacleAt (toCoord 0 0) aGameMap `shouldBe` True
    it "should produce HitObstacle when targetting DIRT" $
      obstacleAt (toCoord 20 20) aGameMap `shouldBe` True
    it "should produce HitNothing when targetting AIR" $
      obstacleAt (toCoord 1 1) aGameMap `shouldBe` False
    it "should produce HitNothing when targetting a MEDIPACK" $
      obstacleAt (toCoord 31 31) aGameMapWithAMedipack `shouldBe` False
  describe ".&&." $ do
    context "when the first predicate produces false" $
      it "should produce false" $
      (((always False) .&&. (always True)) (10::Int)) `shouldBe` False
    context "when the second predicate produces false" $
      it "should produce false" $
      (((always True) .&&. (always False)) (10::Int)) `shouldBe` False
    context "when both predicates produce true" $
      it "should produce true" $
      (((always True) .&&. (always True)) (10::Int)) `shouldBe` True
  describe "one worm harmed" $ do
    context "given a collection of no worms" $
      it "should produce false" $
      oneWormHarmed 20 emptyWormHealths `shouldBe` False
    context "given a collection of one unharmed worm" $
      it "should produce false" $
      oneWormHarmed 20 (aListFromList [(1, 20)]) `shouldBe` False
    context "given a collection with two harmed worms" $
      it "should produce false" $
      oneWormHarmed 20 (aListFromList [(1, 10), (2, 10)]) `shouldBe`
      False
    context "given a collection with one worm harmed" $
      it "should produce true" $
      oneWormHarmed 20 (aListFromList [(1, 10), (2, 20)])
  describe "noWormHarmed" $ do
    context "given an empty collection" $
      it "should produce true" $
      noWormHarmed 20 emptyWormHealths
    context "given a collection of one unharmed worm" $
      it "should produce true" $
      noWormHarmed 20 (aListFromList [(1, 20)])
    context "given a collection of more than one unharmed worms" $
      it "should produce true" $
      noWormHarmed 20 (aListFromList [(1, 20), (2, 20)])
    context "given a collection with a harmed worm" $
      it "should produce false" $
      noWormHarmed 20 (aListFromList [(1, startingHealth), (2, 20)]) `shouldBe`
      False
  describe "makeMove" $ do
    -- TODO make this a property test...?
    it "should not change anything when it receives two 'nothing's" $
      makeMove True (fromMoves doNothing doNothing) aState `shouldBe`
      (incrementRound $ selectNextWormsDefault $ setOpponentsLastMoveToDummy aState)
    it "moving my worm to dirt should dig out that dirt" $
      makeMove True (fromMoves digNorth doNothing) aState `shouldBe`
      (incrementRound $
       setOpponentsLastMoveToDummy $
       selectNextWormsDefault $ awardPointsToThisPlayerForDigging aStateWithDirtMissingAboveMyWorm)
    it "moving opponents worm to space should not move the worm" $
      makeMove True (fromMoves doNothing moveNorth) aState `shouldBe`
      (setOpponentsLastMove aState moveNorth $
       incrementRound $
       selectNextWormsDefault         $
       penaliseThatPlayerForAnInvalidCommand aState)
    it "moving my worm into space should not move the worm" $
      makeMove True (fromMoves moveSouth doNothing) aState `shouldBe`
      (incrementRound $
       setOpponentsLastMoveToDummy $
       selectNextWormsDefault $ penaliseThisPlayerForAnInvalidCommand aState)
    it "moving opponents worm into dirt should dig out the dirt" $
      makeMove True (fromMoves doNothing digSouth) aState `shouldBe`
      (setOpponentsLastMove aState digSouth     $
       incrementRound $
       selectNextWormsDefault            $
       awardPointsToThatPlayerForDigging $
       removeDirtFromMapAt (toCoord 16 2) aState)
    it "moving my worm into air should move the worm to that spot" $
      makeMove True (fromMoves moveEast doNothing) aState `shouldBe`
      (incrementRound $
       setOpponentsLastMoveToDummy $
       selectNextWormsDefault $ awardPointsToThisPlayerForMovingToAir $ moveThisWorm (toCoord 16 31) aState)
    it "moving opponents worm into air should move the worm to that spot" $
      makeMove True (fromMoves doNothing moveEast) aState `shouldBe`
      (setOpponentsLastMove aState moveEast         $
       incrementRound $
       selectNextWormsDefault                $
       awardPointsToThatPlayerForMovingToAir $
       moveThatWorm (toCoord 17 1) aState)
    it "moving to the same square should swap the worms if true and damage both worms" $
      makeMove True (fromMoves moveEast moveWest) aStateWithImpendingCollision `shouldBe`
      (setOpponentsLastMove aStateWithImpendingCollision moveWest         $
       incrementRound $
       selectNextWormsDefault                         $
       withWormPositions (aListRemoveWormById (WormId 1))  $
       withWormPositions (aListRemoveWormById (WormId 4))  $
       awardPointsToThatPlayerForMovingToAir          $
       awardPointsToThisPlayerForMovingToAir          $
       moveThisWorm (toCoord 17 31)                   $
       moveThatWorm (toCoord 15 31)                   $
       harmWorm (WormId (-1)) aStateWithImpendingCollision knockBackDamageAmount id id id (toCoord 17 31) $
       harmWorm (WormId (-1)) aStateWithImpendingCollision knockBackDamageAmount id id id (toCoord 15 31)
       aStateWithImpendingCollision)
    it "moving to the same square should not swap the worms if false and damage both worms" $
      makeMove False (fromMoves moveEast moveWest) aStateWithImpendingCollision `shouldBe`
      (setOpponentsLastMove aStateWithImpendingCollision moveWest         $
       incrementRound $
       selectNextWormsDefault                $
       awardPointsToThatPlayerForMovingToAir $
       awardPointsToThisPlayerForMovingToAir $
       harmWorm (WormId (-1)) aStateWithImpendingCollision knockBackDamageAmount id id id (toCoord 17 31) $
       harmWorm (WormId (-1)) aStateWithImpendingCollision knockBackDamageAmount id id id (toCoord 15 31)
       aStateWithImpendingCollision)
    it "moving my worm to a square occupied by one of my worms does nothing" $
      makeMove True (fromMoves moveEast doNothing) aStateWithMyWormsNextToEachOther `shouldBe`
      (incrementRound $
       setOpponentsLastMoveToDummy $
       selectNextWormsDefault $ penaliseThisPlayerForAnInvalidCommand aStateWithMyWormsNextToEachOther)
    it "moving my worm to a square occupied by one of the the opponents worms does nothing " $
      makeMove True (fromMoves moveEast doNothing) aStateWithMyWormNextToAnEnemy `shouldBe`
      (incrementRound $
       setOpponentsLastMoveToDummy $
       selectNextWormsDefault $ penaliseThisPlayerForAnInvalidCommand aStateWithMyWormNextToAnEnemy)
    it "moving an opponents worm to a square occupied by one of my worms does nothing" $
      makeMove True (fromMoves doNothing moveWest) aStateWithMyWormNextToAnEnemy `shouldBe`
      (setOpponentsLastMove aStateWithMyWormNextToAnEnemy moveWest $
       incrementRound $
       selectNextWormsDefault        $
       penaliseThatPlayerForAnInvalidCommand aStateWithMyWormNextToAnEnemy)
    it "moving an opponents worm to a square occupied by one of the opponents worms does nothing" $
      makeMove True (fromMoves doNothing moveEast) aStateWithEnemyWormsNextToEachother `shouldBe`
      (setOpponentsLastMove aStateWithEnemyWormsNextToEachother moveEast $
       incrementRound $
       selectNextWormsDefault        $
       penaliseThatPlayerForAnInvalidCommand aStateWithEnemyWormsNextToEachother)
    it "moving my worm onto the medipack increases my worms health by 10 and changes that square to AIR" $
      makeMove True (fromMoves moveEast doNothing) aStateWithMyWormNextToTheMedipack `shouldBe`
      (incrementRound $
       setOpponentsLastMoveToDummy $
       selectNextWormsDefault $
       awardPointsToThisPlayerForMovingToAir aStateWithMyWormOnTheMedipack)
    it "moving the opponents worm onto the medipack should increase its health by ten and change that square to AIR" $
      makeMove True (fromMoves doNothing moveSouth) aStateWithOpponentsWormNextToTheMedipack `shouldBe`
      (setOpponentsLastMove aStateWithOpponentsWormNextToTheMedipack moveSouth $
       incrementRound $
       selectNextWormsDefault         $
       awardPointsToThatPlayerForMovingToAir aStateWithOpponentsWormOnTheMedipack)
    it "moving both worms onto the same medipack results in a swap when the bit is set" $
      makeMove True (fromMoves moveEast moveSouth) aStateWithBothWormsNextToTheMedipack `shouldBe`
      (setOpponentsLastMove aStateWithBothWormsNextToTheMedipack moveSouth        $
       incrementRound $
       selectNextWormsDefault                $
       awardPointsToThatPlayerForMovingToAir $
       awardPointsToThisPlayerForMovingToAir aStateWhereWeSwappedOverTheMedipack)
    it "moving both worms onto the same medipack results no swap when the bit is set" $
      makeMove False (fromMoves moveEast moveSouth) aStateWithBothWormsNextToTheMedipack `shouldBe`
      (setOpponentsLastMove aStateWithBothWormsNextToTheMedipack moveSouth        $
       incrementRound $
       selectNextWormsDefault                $
       knockBackDamage                       $
       awardPointsToThatPlayerForMovingToAir $
       awardPointsToThisPlayerForMovingToAir aStateWhereNoSwapHappened)
    -- Top
    it "moving my worm off the top edge of the map changes nothing" $
      makeMove True (fromMoves moveNorth doNothing) aStateWithMyWormOnTop `shouldBe`
      (incrementRound $
       setOpponentsLastMoveToDummy $
       selectNextWormsDefault $ penaliseThisPlayerForAnInvalidCommand aStateWithMyWormOnTop)
    it "moving opponent worm off the top edge of the map changes nothing" $
      makeMove True (fromMoves doNothing moveNorth) aStateWithOpponentWormOnTop `shouldBe`
      (setOpponentsLastMove aStateWithOpponentWormOnTop moveNorth $
       incrementRound $
       selectNextWormsDefault         $
       penaliseThatPlayerForAnInvalidCommand aStateWithOpponentWormOnTop)
    it "moving my worm on the top to the east results in the worm moving east" $
      makeMove True (fromMoves moveEast doNothing) aStateWithMyWormOnTop `shouldBe`
      (incrementRound $
       setOpponentsLastMoveToDummy $
       selectNextWormsDefault $ awardPointsToThisPlayerForMovingToAir aStateWithMyWormOnTopMovedRight)
    it "moving opponent worm on the top to the east results in the worm moving east" $
      makeMove True (fromMoves doNothing moveEast) aStateWithOpponentWormOnTop `shouldBe`
      (setOpponentsLastMove aStateWithOpponentWormOnTop moveEast $
       incrementRound $
       selectNextWormsDefault        $
       awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormOnTopMovedRight)
    it "moving my worm on the top to the west results in the worm moving west" $
      makeMove True (fromMoves moveWest doNothing) aStateWithMyWormOnTop `shouldBe`
      (incrementRound $
       setOpponentsLastMoveToDummy $
       selectNextWormsDefault $ awardPointsToThisPlayerForMovingToAir aStateWithMyWormOnTopMovedLeft)
    it "moving opponent worm on the top to the west results in the worm moving west" $
      makeMove True (fromMoves doNothing moveWest) aStateWithOpponentWormOnTop `shouldBe`
      (setOpponentsLastMove aStateWithOpponentWormOnTop moveWest $
       incrementRound $
       selectNextWormsDefault        $
       awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormOnTopMovedLeft)
    it "moving my worm south from the top of the map results in that worm moving down" $
      makeMove True (fromMoves moveSouth doNothing) aStateWithMyWormOnTop `shouldBe`
      (incrementRound $
       setOpponentsLastMoveToDummy $
       selectNextWormsDefault $ awardPointsToThisPlayerForMovingToAir aStateWithMyWormOnTopMovedDown)
    it "moving opponent worm south from the top of the map results in that worm moving down" $
      makeMove True (fromMoves doNothing moveSouth) aStateWithOpponentWormOnTop `shouldBe`
      (setOpponentsLastMove aStateWithOpponentWormOnTop moveSouth $
       incrementRound $
       selectNextWormsDefault         $
       awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormOnTopMovedDown)
    -- Left edge
    it "moving my worm north on the left edge of the map moves that worm north" $
      makeMove True (fromMoves moveNorth doNothing) aStateWithMyWormOnLeftEdge `shouldBe`
      (incrementRound $
       setOpponentsLastMoveToDummy $
       selectNextWormsDefault $ awardPointsToThisPlayerForMovingToAir aStateWithMyWormUpwardsOnLeftEdge)
    it "moving opponent worm north on the left edge of the map moves that worm north" $
      makeMove True (fromMoves doNothing moveNorth) aStateWithOpponentWormOnLeftEdge `shouldBe`
      (setOpponentsLastMove aStateWithOpponentWormOnLeftEdge moveNorth $
       incrementRound $
       selectNextWormsDefault         $
       awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormUpwardOnLeftEdge)
    it "moving my worm south on the left edge of the map moves that worm south" $
      makeMove True (fromMoves moveSouth doNothing) aStateWithMyWormOnLeftEdge `shouldBe`
      (incrementRound $
       setOpponentsLastMoveToDummy $
       selectNextWormsDefault $ awardPointsToThisPlayerForMovingToAir aStateWithMyWormDownwardOnLeftEdge)
    it "moving opponent worm south on the left edge of the map moves that worm south" $
      makeMove True (fromMoves doNothing moveSouth) aStateWithOpponentWormOnLeftEdge `shouldBe`
      (setOpponentsLastMove aStateWithOpponentWormOnLeftEdge moveSouth $
       incrementRound $
       selectNextWormsDefault         $
       awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormDownwardOnLeftEdge)
    it "moving my worm east on the left edge of the map moves that worm east" $
      makeMove True (fromMoves moveEast doNothing) aStateWithMyWormOnLeftEdge `shouldBe`
      (incrementRound $
       setOpponentsLastMoveToDummy $
       selectNextWormsDefault $ awardPointsToThisPlayerForMovingToAir aStateWithMyWormRightFromLeftEdge)
    it "moving opponent worm east on the left edge of the map moves that worm east" $
      makeMove True (fromMoves doNothing moveEast) aStateWithOpponentWormOnLeftEdge `shouldBe`
      (setOpponentsLastMove aStateWithOpponentWormOnLeftEdge moveEast $
       incrementRound $
       selectNextWormsDefault        $
       awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormRightFromLeftEdge)
    it "moving my worm off the edge on the left of the map changes nothing" $
      makeMove True (fromMoves moveWest doNothing) aStateWithMyWormOnLeftEdge `shouldBe`
      (incrementRound $
       setOpponentsLastMoveToDummy $
       selectNextWormsDefault $ penaliseThisPlayerForAnInvalidCommand aStateWithMyWormOnLeftEdge)
    it "moving opponent worm off the edge on left of the map changes nothing" $
      makeMove True (fromMoves doNothing moveWest) aStateWithOpponentWormOnLeftEdge `shouldBe`
      (setOpponentsLastMove aStateWithOpponentWormOnLeftEdge moveWest $
       incrementRound $
       selectNextWormsDefault        $
       penaliseThatPlayerForAnInvalidCommand aStateWithOpponentWormOnLeftEdge)
    -- Bottom edge
    it "moving my worm south from the bottom edge results in no change" $
      makeMove True (fromMoves moveSouth doNothing) aStateWithMyWormOnTheBottomEdge `shouldBe`
      (incrementRound $
       setOpponentsLastMoveToDummy $
       selectNextWormsDefault $ penaliseThisPlayerForAnInvalidCommand aStateWithMyWormOnTheBottomEdge)
    it "moving opponent worm south from the bottom edge results in no change" $
      makeMove True (fromMoves doNothing moveSouth) aStateWithOpponentWormOnTheBottomEdge `shouldBe`
      (setOpponentsLastMove aStateWithOpponentWormOnTheBottomEdge moveSouth $
       incrementRound $
       selectNextWormsDefault         $
       penaliseThatPlayerForAnInvalidCommand aStateWithOpponentWormOnTheBottomEdge)
    it "moving my worm to the east from the bottom edge results in that worm moving right" $
      makeMove True (fromMoves moveEast doNothing) aStateWithMyWormOnTheBottomEdge `shouldBe`
      (incrementRound $
       setOpponentsLastMoveToDummy $
       selectNextWormsDefault $ awardPointsToThisPlayerForMovingToAir aStateWithMyWormOnTheBottomEdgeMovedRight)
    it "moving opponent worm to the east from the bottom edge results in that worm moving right" $
      makeMove True (fromMoves doNothing moveEast) aStateWithOpponentWormOnTheBottomEdge `shouldBe`
      (setOpponentsLastMove aStateWithOpponentWormOnTheBottomEdge moveEast $
       incrementRound $
       selectNextWormsDefault        $
       awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormOnTheBottomEdgeMovedRight)
    it "moving my worm to the west from the bottom edge results in that worm moving left" $
      makeMove True (fromMoves moveWest doNothing) aStateWithMyWormOnTheBottomEdge `shouldBe`
      (incrementRound $
       setOpponentsLastMoveToDummy $
       selectNextWormsDefault $ awardPointsToThisPlayerForMovingToAir aStateWithMyWormOnTheBottomEdgeMovedLeft)
    it "moving opponent to the west from the bottom edge results in that worm moving left" $
      makeMove True (fromMoves doNothing moveWest) aStateWithOpponentWormOnTheBottomEdge `shouldBe`
      (setOpponentsLastMove aStateWithOpponentWormOnTheBottomEdge moveWest $
       incrementRound $
       selectNextWormsDefault        $
       awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormOnTheBottomEdgeMovedLeft)
    it "moving my worm to the north from the bottom edge results in that worm moving up" $
      makeMove True (fromMoves moveNorth doNothing) aStateWithMyWormOnTheBottomEdge `shouldBe`
      (incrementRound $
       setOpponentsLastMoveToDummy $
       selectNextWormsDefault $ awardPointsToThisPlayerForMovingToAir aStateWithMyWormUpFromTheBottomEdge)
    it "moving opponent worm to the north from the bottom edge results in that worm moving up" $
      makeMove True (fromMoves doNothing moveNorth) aStateWithOpponentWormOnTheBottomEdge `shouldBe`
      (setOpponentsLastMove aStateWithOpponentWormOnTheBottomEdge moveNorth $
       incrementRound $
       selectNextWormsDefault         $
       awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormUpFromTheBottomEdge)
    -- Right edge
    it "moving my worm east from the right edge results in no change" $
      makeMove True (fromMoves moveEast doNothing) aStateWithMyWormOnTheRightEdge `shouldBe`
      (incrementRound $
       setOpponentsLastMoveToDummy $
       selectNextWormsDefault $ penaliseThisPlayerForAnInvalidCommand aStateWithMyWormOnTheRightEdge)
    it "moving opponent worm east from the right edge results in no change" $
      makeMove True (fromMoves doNothing moveEast) aStateWithOpponentWormOnTheRightEdge `shouldBe`
      (setOpponentsLastMove aStateWithOpponentWormOnTheRightEdge moveEast $
       incrementRound $
       selectNextWormsDefault        $
       penaliseThatPlayerForAnInvalidCommand aStateWithOpponentWormOnTheRightEdge)
    it "moving my worm north from the right edge results in that worm moving up" $
      makeMove True (fromMoves moveNorth doNothing) aStateWithMyWormOnTheRightEdge `shouldBe`
      (incrementRound $
       setOpponentsLastMoveToDummy $
       selectNextWormsDefault $ awardPointsToThisPlayerForMovingToAir aStateWithMyWormOnTheRightEdgeMovedUp)
    it "moving opponent worm north from the right edge results in that worm moving up" $
      makeMove True (fromMoves doNothing moveNorth) aStateWithOpponentWormOnTheRightEdge `shouldBe`
      (setOpponentsLastMove aStateWithOpponentWormOnTheRightEdge moveNorth $
       incrementRound $
       selectNextWormsDefault         $
       awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormOnTheRightEdgeMovedUp)
    it "moving my worm south from the right edge results in that worm moving down" $
      makeMove True (fromMoves moveSouth doNothing) aStateWithMyWormOnTheRightEdge `shouldBe`
      (incrementRound $
       setOpponentsLastMoveToDummy $
       selectNextWormsDefault $ awardPointsToThisPlayerForMovingToAir aStateWithMyWormOnTheRightEdgeMovedDown)
    it "moving opponent worm south from the right edge results in that worm moving down" $
      makeMove True (fromMoves doNothing moveSouth) aStateWithOpponentWormOnTheRightEdge `shouldBe`
      (setOpponentsLastMove aStateWithOpponentWormOnTheRightEdge moveSouth $
       incrementRound $
       selectNextWormsDefault         $
       awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormOnTheRightEdgeMovedDown)
    it "moving my worm to the west from the right edge results in that worm moving left" $
      makeMove True (fromMoves moveWest doNothing) aStateWithMyWormOnTheRightEdge `shouldBe`
      (incrementRound $
       setOpponentsLastMoveToDummy $
       selectNextWormsDefault $ awardPointsToThisPlayerForMovingToAir aStateWithMyWormMovedLeftFromTheRightEdge)
    it "moving opponent worm to the west from the right edge results in that worm moving left" $
      makeMove True (fromMoves doNothing moveWest) aStateWithOpponentWormOnTheRightEdge `shouldBe`
      (setOpponentsLastMove aStateWithOpponentWormOnTheRightEdge moveWest $
       incrementRound $
       selectNextWormsDefault        $
       awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormMovedLeftFromTheRightEdge)
    -- Digging
    it "should not dig off of the edge of the map" $
      makeMove True (fromMoves digEast doNothing) aStateWithMyWormOnTheRightEdge `shouldBe`
      (incrementRound $
       setOpponentsLastMoveToDummy $
       selectNextWormsDefault $ penaliseThisPlayerForAnInvalidCommand aStateWithMyWormOnTheRightEdge)
    it "should remove dirt when my player digs a dirt block" $
      makeMove True (fromMoves digNorth doNothing) aState `shouldBe`
      (incrementRound $
       setOpponentsLastMoveToDummy $
       selectNextWormsDefault $ awardPointsToThisPlayerForDigging aStateWithDirtMissingAboveMyWorm)
    it "should remove dirt when opponent digs a dirt block" $
      makeMove True (fromMoves doNothing digNorth) aStateWithOpponentBeneathDirt `shouldBe`
      (setOpponentsLastMove aStateWithOpponentBeneathDirt digNorth $
       incrementRound $
       selectNextWormsDefault        $
       awardPointsToThatPlayerForDigging aStateWithDirtMissingAboveOpponentWorm)
    it "should penalise my player when I dig air" $
      makeMove True (fromMoves digEast doNothing) aState `shouldBe`
      (incrementRound $
       setOpponentsLastMoveToDummy $
       selectNextWormsDefault $ penaliseThisPlayerForAnInvalidCommand aState)
    it "should penalise my opponent when he digs air" $
      makeMove True (fromMoves doNothing digEast) aState `shouldBe`
      (setOpponentsLastMove aState digEast $
       incrementRound $
       selectNextWormsDefault       $
       penaliseThatPlayerForAnInvalidCommand aState)
    it "moving next to dirt should not dig out that dirt when it would be in our way if we continued going that way" $
      makeMove True (fromMoves doNothing moveEast) aStateWithOpponentNearDirtToTheEast `shouldBe`
      (setOpponentsLastMove aStateWithOpponentNearDirtToTheEast moveEast         $ 
       incrementRound $
       selectNextWormsDefault                $
       awardPointsToThatPlayerForMovingToAir $
       moveThatWorm (toCoord 10 2) aStateWithOpponentNearDirtToTheEast)
    it "should reward both players and remove dirt when both worms dig the same dirt block" $
      makeMove True (fromMoves digSouthEast digSouth) aStateWithBothWormsNearTheSameDirtBlock `shouldBe`
      (setOpponentsLastMove aStateWithBothWormsNearTheSameDirtBlock digSouth     $
       incrementRound $
       selectNextWormsDefault            $
       awardPointsToThatPlayerForDigging $
       awardPointsToThisPlayerForDigging $
       mapGameMap aStateWithBothWormsNearTheSameDirtBlock (removeDirtAt (toCoord 11 2)))
    -- Bananas!
    let aStateWithOposingWormsNextToEachother =
          withWormPositions (always (AList (toCoord 15 31)
                                           (toCoord 1 31)
                                           (toCoord 1 30)
                                           (toCoord 16 31)
                                           (toCoord 19 1)
                                           (toCoord 20 1))) $
          withWormHealths (always (AList 20 20 20 20 20 20)) $
          withWormBananas (always $ aListFromList [(1, 3), (4, 3)])
          aState
    context "when I'm throwing the bomb" $ do
      let aStateWithMyWormOnTheRightEdgeOfTheMap =
            withWormHealths (always (AList 20 20 20 20 20 20)) $
            withWormBananas (always $ aListFromList [(1, 3), (4, 3)]) $
            withWormPositions (always (AList (toCoord 32 20)
                                             (toCoord 1 31)
                                             (toCoord 1 30)
                                             (toCoord 16 31)
                                             (toCoord 19 1)
                                             (toCoord 20 1)))
            aStateWithOnlyAirOnMap
      it "should do nothing when thrown off of the map" $
        makeMove False (fromMoves bananaOneToRight doNothing) aStateWithMyWormOnTheRightEdgeOfTheMap `shouldBe`
        (incrementRound $
         setOpponentsLastMoveToDummy $
         selectNextWormsDefault aStateWithMyWormOnTheRightEdgeOfTheMap)
      let aStateWithBananasOnWormOneAndTwo =
            withWormHealths (always (AList 20 20 20 20 20 20)) $
            withWormBananas (always $ aListFromList [(1, 3), (4, 3)])
            aState
      it "should kill myself when I throw it at myself and no one else" $
        makeMove False (fromMoves bananaRightOnMe doNothing) aStateWithBananasOnWormOneAndTwo `shouldBe`
        (incrementRound $
         setOpponentsLastMoveToDummy $
         selectNextWormsDefault $
         setOpponentsLastMoveToDummy $
         harmWorm (WormId 1) aStateWithBananasOnWormOneAndTwo 20 id id id (toCoord 15 31) $
         -- Decrement banana bombs
         withWormBananas (always $ aListFromList [(4, 3)]) $
         -- Points for the four squares
         penaliseThisPlayerForDamage 20           $
         awardPointsToThisPlayerForDigging        $
         awardPointsToThisPlayerForDigging        $
         awardPointsToThisPlayerForDigging        $
         awardPointsToThisPlayerForDigging        $
         mapGameMap aStateWithBananasOnWormOneAndTwo
                    ((-- Up
                      addAirAt (toCoord 15 30) .
                      addAirAt (toCoord 15 29) .
                      -- Remaining
                      addAirAt (toCoord 14 30) .
                      addAirAt (toCoord 16 30))))
      it "should cause maximum damage to the worm which it lands on" $
        makeMove False (fromMoves bananaOneToRight doNothing) aStateWithOposingWormsNextToEachother `shouldBe`
        (incrementRound $
         selectNextWormsDefault $
         setOpponentsLastMoveToDummy $
         harmWorm (WormId 1) aStateWithOposingWormsNextToEachother 20 id id id (toCoord 16 31) $
         harmWorm (WormId 1) aStateWithOposingWormsNextToEachother 13 id id id (toCoord 15 31) $
         -- Decrement banana bombs
         withWormBananas (always $ aListFromList [(1, 2)]) $
         -- Points for the four squares
         awardPointsToThisPlayerForKillingAnEnemy $
         awardPointsToThisPlayerForDamage 20      $
         penaliseThisPlayerForDamage 13           $
         awardPointsToThisPlayerForDigging        $
         awardPointsToThisPlayerForDigging        $
         awardPointsToThisPlayerForDigging        $
         awardPointsToThisPlayerForDigging        $
         mapGameMap aStateWithOposingWormsNextToEachother
                    ((-- Up
                      addAirAt (toCoord 16 30) .
                      addAirAt (toCoord 16 29) .
                      -- Remaining
                      addAirAt (toCoord 15 30) .
                      addAirAt (toCoord 17 30))))
      it "should not kill the opponent worm when it moves out of the epicentre" $
        makeMove False (fromMoves bananaOneToRight moveEast) aStateWithOposingWormsNextToEachother `shouldBe`
        (incrementRound $
         selectNextWormsDefault $
         harmWorm (WormId 1) aStateWithOposingWormsNextToEachother 13 id id id (toCoord 16 31) $
         harmWorm (WormId 1) aStateWithOposingWormsNextToEachother 13 id id id (toCoord 15 31) $
         -- Move that worm
         setOpponentsLastMove aStateWithOposingWormsNextToEachother moveEast $
         awardPointsToThatPlayerForMovingToAir $
         moveThatWorm (displaceCoordByMove (thatWormsCoord aStateWithOposingWormsNextToEachother) moveEast) $
         -- Decrement banana bombs
         withWormBananas (always $ aListFromList [(1, 2), (4, 3)]) $
         -- Points for the four squares
         awardPointsToThisPlayerForDamage 13      $
         penaliseThisPlayerForDamage 13           $
         awardPointsToThisPlayerForDigging        $
         awardPointsToThisPlayerForDigging        $
         awardPointsToThisPlayerForDigging        $
         awardPointsToThisPlayerForDigging        $
         mapGameMap aStateWithOposingWormsNextToEachother
                    ((-- Up
                      addAirAt (toCoord 16 30) .
                      addAirAt (toCoord 16 29) .
                      -- Remaining
                      addAirAt (toCoord 15 30) .
                      addAirAt (toCoord 17 30))))
      let stateWithEnemyOneSquareFromEpicentre = moveThatWorm (toCoord 17 31) aStateWithOposingWormsNextToEachother
      it "should cause damage to the worms in the blast radius" $
        makeMove False (fromMoves bananaOneToRight doNothing) stateWithEnemyOneSquareFromEpicentre `shouldBe`
        (incrementRound $
         selectNextWormsDefault $
         setOpponentsLastMoveToDummy $
         harmWorm (WormId 1) stateWithEnemyOneSquareFromEpicentre 13 id id id (toCoord 17 31) $
         harmWorm (WormId 1) stateWithEnemyOneSquareFromEpicentre 13 id id id (toCoord 15 31) $
         -- Decrement banana bombs
         withWormBananas (always $ aListFromList [(1, 2), (4, 3)]) $
         -- Points for the four squares
         penaliseThisPlayerForDamage      13 $
         awardPointsToThisPlayerForDamage 13 $
         awardPointsToThisPlayerForDigging   $
         awardPointsToThisPlayerForDigging   $
         awardPointsToThisPlayerForDigging   $
         awardPointsToThisPlayerForDigging   $
         mapGameMap stateWithEnemyOneSquareFromEpicentre
                    ((-- Up
                      addAirAt (toCoord 16 30) .
                      addAirAt (toCoord 16 29) .
                      -- Remaining
                      addAirAt (toCoord 15 30) .
                      addAirAt (toCoord 17 30))))
      let stateWithEnemyTwoSquaresFromEpicentre = moveThatWorm (toCoord 18 31) aStateWithOposingWormsNextToEachother
      it "should cause damage to the worms in the blast radius" $
        makeMove False (fromMoves bananaOneToRight doNothing) stateWithEnemyTwoSquaresFromEpicentre `shouldBe`
        (incrementRound $
         setOpponentsLastMoveToDummy $
         selectNextWormsDefault $
         harmWorm (WormId 1) stateWithEnemyTwoSquaresFromEpicentre  7 id id id (toCoord 18 31) $
         harmWorm (WormId 1) stateWithEnemyTwoSquaresFromEpicentre 13 id id id (toCoord 15 31) $
         -- Decrement banana bombs
         withWormBananas (always $ aListFromList [(1, 2), (4, 3)]) $
         -- Points for the four squares
         penaliseThisPlayerForDamage     13 $
         awardPointsToThisPlayerForDamage 7 $
         awardPointsToThisPlayerForDigging  $
         awardPointsToThisPlayerForDigging  $
         awardPointsToThisPlayerForDigging  $
         awardPointsToThisPlayerForDigging  $
         mapGameMap stateWithEnemyTwoSquaresFromEpicentre
                    ((-- Up
                      addAirAt (toCoord 16 30) .
                      addAirAt (toCoord 16 29) .
                      -- Remaining
                      addAirAt (toCoord 15 30) .
                      addAirAt (toCoord 17 30))))
      it "should not cause damage to the opponent if its worm moved out of the blast radius" $
        makeMove False (fromMoves bananaOneToRight moveEast) stateWithEnemyTwoSquaresFromEpicentre `shouldBe`
        (incrementRound $
         selectNextWormsDefault $
         harmWorm (WormId 1) stateWithEnemyTwoSquaresFromEpicentre 13 id id id (toCoord 15 31) $
         -- Move that worm
         setOpponentsLastMove stateWithEnemyTwoSquaresFromEpicentre moveEast $
         awardPointsToThatPlayerForMovingToAir $
         moveThatWorm (displaceCoordByMove (thatWormsCoord stateWithEnemyTwoSquaresFromEpicentre) moveEast) $
         -- Decrement banana bombs
         withWormBananas (always $ aListFromList [(1, 2), (4, 3)]) $
         -- Points for the four squares
         penaliseThisPlayerForDamage     13 $
         awardPointsToThisPlayerForDigging  $
         awardPointsToThisPlayerForDigging  $
         awardPointsToThisPlayerForDigging  $
         awardPointsToThisPlayerForDigging  $
         mapGameMap stateWithEnemyTwoSquaresFromEpicentre
                    ((-- Up
                      addAirAt (toCoord 16 30) .
                      addAirAt (toCoord 16 29) .
                      -- Remaining
                      addAirAt (toCoord 15 30) .
                      addAirAt (toCoord 17 30))))
      let stateWithEnemyThreeSquaresFromEpicentre = moveThatWorm (toCoord 19 31) aStateWithOposingWormsNextToEachother
      it "should not cause damage to the worms outside of the blast radius" $
        makeMove False (fromMoves bananaOneToRight doNothing) stateWithEnemyThreeSquaresFromEpicentre `shouldBe`
        (incrementRound $
         setOpponentsLastMoveToDummy $
         selectNextWormsDefault $
         harmWorm (WormId 1) stateWithEnemyThreeSquaresFromEpicentre 13 id id id (toCoord 15 31) $
         -- Decrement banana bombs
         withWormBananas (always $ aListFromList [(1, 2), (4, 3)]) $
         -- Points for the four squares
         penaliseThisPlayerForDamage    13 $
         awardPointsToThisPlayerForDigging $
         awardPointsToThisPlayerForDigging $
         awardPointsToThisPlayerForDigging $
         awardPointsToThisPlayerForDigging $
         mapGameMap stateWithEnemyThreeSquaresFromEpicentre
                    ((-- Up
                      addAirAt (toCoord 16 30) .
                      addAirAt (toCoord 16 29) .
                      -- Remaining
                      addAirAt (toCoord 15 30) .
                      addAirAt (toCoord 17 30))))
      let aStateWithBananasLeftForWorms1And4 =
            withWormBananas (always $ aListFromList [(1, 3), (4, 3)]) aState
      it "should destroy all 13 squares of dirt in range fo the epicentre" $
        makeMove False (fromMoves bananaIntoDirtFromMe doNothing) aStateWithBananasLeftForWorms1And4 `shouldBe`
        (incrementRound $
         setOpponentsLastMoveToDummy $
         selectNextWormsDefault $
         -- Decrement banana bombs
         withWormBananas (always $ aListFromList [(1, 2), (4, 3)]) $
         -- Points for the 13 squares
         awardPointsToThisPlayerForDigging $
         awardPointsToThisPlayerForDigging $
         awardPointsToThisPlayerForDigging $
         awardPointsToThisPlayerForDigging $
         awardPointsToThisPlayerForDigging $
         awardPointsToThisPlayerForDigging $
         awardPointsToThisPlayerForDigging $
         awardPointsToThisPlayerForDigging $
         awardPointsToThisPlayerForDigging $
         awardPointsToThisPlayerForDigging $
         awardPointsToThisPlayerForDigging $
         awardPointsToThisPlayerForDigging $
         awardPointsToThisPlayerForDigging $
         mapGameMap aStateWithBananasLeftForWorms1And4
                           ((addAirAt (toCoord 15 26) . -- epicentre
                             -- Up
                             addAirAt (toCoord 15 25) .
                             addAirAt (toCoord 15 24) .
                             -- Down
                             addAirAt (toCoord 15 27) .
                             addAirAt (toCoord 15 28) .
                             -- Left
                             addAirAt (toCoord 14 26) .
                             addAirAt (toCoord 13 26) .
                             -- Right
                             addAirAt (toCoord 16 26) .
                             addAirAt (toCoord 17 26) .
                             -- Remaining
                             addAirAt (toCoord 14 25) .
                             addAirAt (toCoord 16 25) .
                             addAirAt (toCoord 14 27) .
                             addAirAt (toCoord 16 27))))
      let aStateWithAMedipackInTheDirt = withWormBananas (always $ aListFromList [(1, 3), (4, 3)]) $
                                         mapGameMap aState (addMedipackAt (toCoord 16 26))
      it "should destroy medipacks" $
        makeMove False (fromMoves bananaIntoDirtFromMe doNothing) aStateWithAMedipackInTheDirt `shouldBe`
        (incrementRound $
         setOpponentsLastMoveToDummy $
         selectNextWormsDefault $
         -- Decrement banana bombs
         withWormBananas (always $ aListFromList [(1, 2), (4, 3)]) $
         -- Points for the 12 squares (one is a medipack)
         awardPointsToThisPlayerForDigging $
         awardPointsToThisPlayerForDigging $
         awardPointsToThisPlayerForDigging $
         awardPointsToThisPlayerForDigging $
         awardPointsToThisPlayerForDigging $
         awardPointsToThisPlayerForDigging $
         awardPointsToThisPlayerForDigging $
         awardPointsToThisPlayerForDigging $
         awardPointsToThisPlayerForDigging $
         awardPointsToThisPlayerForDigging $
         awardPointsToThisPlayerForDigging $
         awardPointsToThisPlayerForDigging $
         mapGameMap aStateWithAMedipackInTheDirt
                    ((addAirAt (toCoord 15 26) . -- epicentre
                      -- Up
                      addAirAt (toCoord 15 25) .
                      addAirAt (toCoord 15 24) .
                      -- Down
                      addAirAt (toCoord 15 27) .
                      addAirAt (toCoord 15 28) .
                      -- Left
                      addAirAt (toCoord 14 26) .
                      addAirAt (toCoord 13 26) .
                      -- Right
                      addAirAt (toCoord 16 26) .
                      addAirAt (toCoord 17 26) .
                      -- Remaining
                      addAirAt (toCoord 14 25) .
                      addAirAt (toCoord 16 25) .
                      addAirAt (toCoord 14 27) .
                      addAirAt (toCoord 16 27))))
      it "should not throw a banana bomb when the current worm has none" $
        makeMove False (fromMoves bananaIntoDirtFromMe doNothing) aState `shouldBe`
        (incrementRound $
         setOpponentsLastMoveToDummy $
         selectNextWormsDefault $ aState)
    context "when the opponent is throwing the bomb" $ do
      it "should cause maximum damage to the worm which it lands on" $
        makeMove False (fromMoves doNothing bananaOneToLeft) aStateWithOposingWormsNextToEachother `shouldBe`
        (setOpponentsLastMove aStateWithOposingWormsNextToEachother bananaOneToLeft $
         incrementRound $
         selectNextWormsDefault $
         harmWorm (WormId 4) aStateWithOposingWormsNextToEachother 20 id id id (toCoord 15 31) $
         harmWorm (WormId 4) aStateWithOposingWormsNextToEachother 13 id id id (toCoord 16 31) $
         -- Decrement banana bombs
         withWormBananas (always $ aListFromList [(4, 2)]) $
         -- Points for the four squares
         awardPointsToThatPlayerForKillingAnEnemy $
         awardPointsToThatPlayerForDamage 20      $
         penaliseThatPlayerForDamage 13           $
         awardPointsToThatPlayerForDigging        $
         awardPointsToThatPlayerForDigging        $
         awardPointsToThatPlayerForDigging        $
         awardPointsToThatPlayerForDigging        $
         mapGameMap aStateWithOposingWormsNextToEachother
                    ((-- Up
                      addAirAt (toCoord 15 30) .
                      addAirAt (toCoord 15 29) .
                      -- Remaining
                      addAirAt (toCoord 14 30) .
                      addAirAt (toCoord 16 30))))
      it "should not kill the opponent worm when it moves out of the epicentre" $
        makeMove False (fromMoves moveWest bananaOneToLeft) aStateWithOposingWormsNextToEachother `shouldBe`
        (setOpponentsLastMove aStateWithOposingWormsNextToEachother bananaOneToLeft $
         incrementRound $
         selectNextWormsDefault $
         harmWorm (WormId 4) aStateWithOposingWormsNextToEachother 13 id id id (toCoord 15 31) $
         harmWorm (WormId 4) aStateWithOposingWormsNextToEachother 13 id id id (toCoord 16 31) $
         -- Move this worm
         awardPointsToThisPlayerForMovingToAir $
         moveThisWorm (displaceCoordByMove (thisWormsCoord aStateWithOposingWormsNextToEachother) moveWest) $
         -- Decrement banana bombs
         withWormBananas (always $ aListFromList [(1, 3), (4, 2)]) $
         -- Points for the four squares
         awardPointsToThatPlayerForDamage 13      $
         penaliseThatPlayerForDamage 13           $
         awardPointsToThatPlayerForDigging        $
         awardPointsToThatPlayerForDigging        $
         awardPointsToThatPlayerForDigging        $
         awardPointsToThatPlayerForDigging        $
         mapGameMap aStateWithOposingWormsNextToEachother
                    ((-- Up
                      addAirAt (toCoord 15 30) .
                      addAirAt (toCoord 15 29) .
                      -- Remaining
                      addAirAt (toCoord 14 30) .
                      addAirAt (toCoord 16 30))))
      let stateWithEnemyOneSquareFromEpicentre = moveThisWorm (toCoord 14 31) aStateWithOposingWormsNextToEachother
      it "should cause damage to the worms in the blast radius" $
        makeMove False (fromMoves doNothing bananaOneToLeft) stateWithEnemyOneSquareFromEpicentre `shouldBe`
        (setOpponentsLastMove stateWithEnemyOneSquareFromEpicentre bananaOneToLeft $
         incrementRound $
         selectNextWormsDefault $
         harmWorm (WormId 4) stateWithEnemyOneSquareFromEpicentre 13 id id id (toCoord 14 31) $
         harmWorm (WormId 4) stateWithEnemyOneSquareFromEpicentre 13 id id id (toCoord 16 31) $
         -- Decrement banana bombs
         withWormBananas (always $ aListFromList [(1, 3), (4, 2)]) $
         -- Points for the four squares
         penaliseThatPlayerForDamage      13 $
         awardPointsToThatPlayerForDamage 13 $
         awardPointsToThatPlayerForDigging   $
         awardPointsToThatPlayerForDigging   $
         awardPointsToThatPlayerForDigging   $
         awardPointsToThatPlayerForDigging   $
         mapGameMap stateWithEnemyOneSquareFromEpicentre
                    ((-- Up
                      addAirAt (toCoord 15 30) .
                      addAirAt (toCoord 15 29) .
                      -- Remaining
                      addAirAt (toCoord 14 30) .
                      addAirAt (toCoord 16 30))))
      let stateWithEnemyTwoSquaresFromEpicentre = moveThisWorm (toCoord 13 31) aStateWithOposingWormsNextToEachother
      it "should cause damage to the worms in the blast radius" $
        makeMove False (fromMoves doNothing bananaOneToLeft) stateWithEnemyTwoSquaresFromEpicentre `shouldBe`
        (setOpponentsLastMove stateWithEnemyTwoSquaresFromEpicentre bananaOneToLeft $
         incrementRound $
         selectNextWormsDefault $
         harmWorm (WormId 4) stateWithEnemyTwoSquaresFromEpicentre  7 id id id (toCoord 13 31) $
         harmWorm (WormId 4) stateWithEnemyTwoSquaresFromEpicentre 13 id id id (toCoord 16 31) $
         -- Decrement banana bombs
         withWormBananas (always $ aListFromList [(1, 3), (4, 2)]) $
         -- Points for the four squares
         penaliseThatPlayerForDamage     13 $
         awardPointsToThatPlayerForDamage 7 $
         awardPointsToThatPlayerForDigging  $
         awardPointsToThatPlayerForDigging  $
         awardPointsToThatPlayerForDigging  $
         awardPointsToThatPlayerForDigging  $
         mapGameMap stateWithEnemyTwoSquaresFromEpicentre
                    ((-- Up
                      addAirAt (toCoord 15 30) .
                      addAirAt (toCoord 15 29) .
                      -- Remaining
                      addAirAt (toCoord 14 30) .
                      addAirAt (toCoord 16 30))))
      it "should not cause damage to the opponent if its worm moved out of the blast radius" $
        makeMove False (fromMoves moveWest bananaOneToLeft) stateWithEnemyTwoSquaresFromEpicentre `shouldBe`
        (setOpponentsLastMove stateWithEnemyTwoSquaresFromEpicentre bananaOneToLeft $
         incrementRound $
         selectNextWormsDefault $
         harmWorm (WormId 4) stateWithEnemyTwoSquaresFromEpicentre 13 id id id (toCoord 16 31) $
         -- Move this worm
         awardPointsToThisPlayerForMovingToAir $
         moveThisWorm (displaceCoordByMove (thisWormsCoord stateWithEnemyTwoSquaresFromEpicentre) moveWest) $
         -- Decrement banana bombs
         withWormBananas (always $ aListFromList [(1, 3), (4, 2)]) $
         -- Points for the four squares
         penaliseThatPlayerForDamage     13 $
         awardPointsToThatPlayerForDigging  $
         awardPointsToThatPlayerForDigging  $
         awardPointsToThatPlayerForDigging  $
         awardPointsToThatPlayerForDigging  $
         mapGameMap stateWithEnemyTwoSquaresFromEpicentre
                    ((-- Up
                      addAirAt (toCoord 15 30) .
                      addAirAt (toCoord 15 29) .
                      -- Remaining
                      addAirAt (toCoord 14 30) .
                      addAirAt (toCoord 16 30))))
      let stateWithEnemyThreeSquaresFromEpicentre = moveThisWorm (toCoord 12 31) aStateWithOposingWormsNextToEachother
      it "should not cause damage to the worms outside of the blast radius" $
        makeMove False (fromMoves doNothing bananaOneToLeft) stateWithEnemyThreeSquaresFromEpicentre `shouldBe`
        (setOpponentsLastMove stateWithEnemyThreeSquaresFromEpicentre bananaOneToLeft $
         incrementRound $
         selectNextWormsDefault $
         harmWorm (WormId 4) stateWithEnemyThreeSquaresFromEpicentre 13 id id id (toCoord 16 31) $
         -- Decrement banana bombs
         withWormBananas (always $ aListFromList [(1, 3), (4, 2)]) $
         -- Points for the four squares
         penaliseThatPlayerForDamage    13 $
         awardPointsToThatPlayerForDigging $
         awardPointsToThatPlayerForDigging $
         awardPointsToThatPlayerForDigging $
         awardPointsToThatPlayerForDigging $
         mapGameMap stateWithEnemyThreeSquaresFromEpicentre
                    ((-- Up
                      addAirAt (toCoord 15 30) .
                      addAirAt (toCoord 15 29) .
                      -- Remaining
                      addAirAt (toCoord 14 30) .
                      addAirAt (toCoord 16 30))))
      let aStateWithBananasLeftForWorms1And4 =
            withWormBananas (always $ aListFromList [(1, 3), (4, 3)]) aState
      it "should destroy all 13 squares of dirt in range fo the epicentre" $
        makeMove False (fromMoves doNothing bananaIntoDirtFromHim) aStateWithBananasLeftForWorms1And4 `shouldBe`
        (setOpponentsLastMove aStateWithBananasLeftForWorms1And4 bananaIntoDirtFromHim $
         incrementRound $
         selectNextWormsDefault $
         -- Decrement banana bombs
         withWormBananas (always $ aListFromList [(1, 3), (4, 2)]) $
         -- Points for the 13 squares
         awardPointsToThatPlayerForDigging $
         awardPointsToThatPlayerForDigging $
         awardPointsToThatPlayerForDigging $
         awardPointsToThatPlayerForDigging $
         awardPointsToThatPlayerForDigging $
         awardPointsToThatPlayerForDigging $
         awardPointsToThatPlayerForDigging $
         awardPointsToThatPlayerForDigging $
         awardPointsToThatPlayerForDigging $
         awardPointsToThatPlayerForDigging $
         awardPointsToThatPlayerForDigging $
         awardPointsToThatPlayerForDigging $
         awardPointsToThatPlayerForDigging $
         mapGameMap aStateWithBananasLeftForWorms1And4
                           ((addAirAt (toCoord 16 6) . -- epicentre
                             -- Up
                             addAirAt (toCoord 16 5) .
                             addAirAt (toCoord 16 4) .
                             -- Down
                             addAirAt (toCoord 16 7) .
                             addAirAt (toCoord 16 8) .
                             -- Left
                             addAirAt (toCoord 15 6) .
                             addAirAt (toCoord 14 6) .
                             -- Right
                             addAirAt (toCoord 17 6) .
                             addAirAt (toCoord 18 6) .
                             -- Remaining
                             addAirAt (toCoord 15 5) .
                             addAirAt (toCoord 17 5) .
                             addAirAt (toCoord 15 7) .
                             addAirAt (toCoord 17 7))))
      let aStateWithAMedipackInTheDirt = withWormBananas (always $ aListFromList [(1, 3), (4, 3)]) $
                                         mapGameMap aState (addMedipackAt (toCoord 16 7))
      it "should destroy medipacks" $
        makeMove False (fromMoves doNothing bananaIntoDirtFromHim) aStateWithAMedipackInTheDirt `shouldBe`
        (setOpponentsLastMove aStateWithAMedipackInTheDirt bananaIntoDirtFromHim $
         incrementRound $
         selectNextWormsDefault $
         -- Decrement banana bombs
         withWormBananas (always $ aListFromList [(1, 3), (4, 2)]) $
         -- Points for the 12 squares (one is a medipack)
         awardPointsToThatPlayerForDigging $
         awardPointsToThatPlayerForDigging $
         awardPointsToThatPlayerForDigging $
         awardPointsToThatPlayerForDigging $
         awardPointsToThatPlayerForDigging $
         awardPointsToThatPlayerForDigging $
         awardPointsToThatPlayerForDigging $
         awardPointsToThatPlayerForDigging $
         awardPointsToThatPlayerForDigging $
         awardPointsToThatPlayerForDigging $
         awardPointsToThatPlayerForDigging $
         awardPointsToThatPlayerForDigging $
         mapGameMap aStateWithAMedipackInTheDirt
                    ((addAirAt (toCoord 16 6) . -- epicentre
                      -- Up
                      addAirAt (toCoord 16 5) .
                      addAirAt (toCoord 16 4) .
                      -- Down
                      addAirAt (toCoord 16 7) .
                      addAirAt (toCoord 16 8) .
                      -- Left
                      addAirAt (toCoord 15 6) .
                      addAirAt (toCoord 14 6) .
                      -- Right
                      addAirAt (toCoord 17 6) .
                      addAirAt (toCoord 18 6) .
                      -- Remaining
                      addAirAt (toCoord 15 5) .
                      addAirAt (toCoord 17 5) .
                      addAirAt (toCoord 15 7) .
                      addAirAt (toCoord 17 7))))
      it "should not throw a banana bomb when the current worm has none" $
        makeMove False (fromMoves doNothing bananaIntoDirtFromHim) aState `shouldBe`
        (setOpponentsLastMove aState bananaIntoDirtFromHim $
         incrementRound $
         selectNextWormsDefault aState)
    context "when both the opponent and I throw the bomb" $ do
      let aStateWhichIFoundFailing =
            withWormHealths (always $ aListFromList
                              [(2, 134),
                               (3, 28),
                               (4, 33),
                               (8, 20),
                               (12, 95)]) $
            withWormPositions (always $ aListFromList
                                [(2,  toCoord 26 5),
                                 (3,  toCoord 28 5),
                                 (4,  toCoord 27 7),
                                 (8,  toCoord 29 7),
                                 (12, toCoord 27 9)]) $
            withWormBananas (always $ aListFromList
                              [(3,  2),
                               (12, 1)]) $
            mapThisPlayer (always $ Player 1318 (WormId 3) (Selections 0)) $
            mapThatPlayer (always $ Player 1230 (WormId 4) (Selections 2)) $
            aStateWithOnlyAirOnMap
      it "should damage the worms correctly and not leave us in a broken state" $
        makeMove False
                 -- My move:        banana 28 7
                 -- Opponents move: select 3;banana 27 8
                 (fromMoves (Move 83) (Move 3126))
                 aStateWhichIFoundFailing `shouldBe`
        (withLastMove (Just $ prettyPrintThatMove aStateWhichIFoundFailing (Move 3126)) $
         mapThatPlayer (withSelections (always (Selections 1))) $
         incrementRound $
         selectNextWorms (WormId 2) (WormId 4) $
         -- His bombs damage
         harmWorm (WormId 12) aStateWhichIFoundFailing 13 id id id (toCoord 27 7) $
         harmWorm (WormId 12) aStateWhichIFoundFailing 13 id id id (toCoord 27 9) $
         penaliseThatPlayerForDamage 26 $
         -- My bombs damage
         harmWorm (WormId 3) aStateWhichIFoundFailing 13 id id id (toCoord 27 7) $
         harmWorm (WormId 3) aStateWhichIFoundFailing 7  id id id (toCoord 28 5) $
         harmWorm (WormId 3) aStateWhichIFoundFailing 13 id id id (toCoord 29 7) $
         awardPointsToThisPlayerForDamage 26 $
         penaliseThisPlayerForDamage 7       $
         withWormBananas (always $ aListFromList [(3,  1)]) $
         aStateWhichIFoundFailing)
      let aStateWithLowHealthOposingWormsNextToEachother =
            withWormPositions (always $ AList
                                  (toCoord 15 31)
                                  (toCoord 1 31)
                                  (toCoord 1 30)
                                  (toCoord 16 31)
                                  (toCoord 19 1)
                                  (toCoord 20 1)) $
            withWormHealths (always $ AList 10 10 10 10 10 10) $
            withWormBananas (always $ aListFromList [(1, 3), (4, 3)])
            aState
      it "should kill the worms which are next to each other" $
        makeMove False
                 (fromMoves bananaOneToRight bananaOneToLeft)
                 aStateWithLowHealthOposingWormsNextToEachother `shouldBe`
        (setOpponentsLastMove aStateWithOposingWormsNextToEachother bananaOneToLeft $
         incrementRound $
         selectNextWormsDefault $
         harmWorm (WormId 1) aStateWithLowHealthOposingWormsNextToEachother 20 id id id (toCoord 16 31) $
         harmWorm (WormId 1) aStateWithLowHealthOposingWormsNextToEachother 13 id id id (toCoord 15 31) $
         harmWorm (WormId 4) aStateWithLowHealthOposingWormsNextToEachother 13 id id id (toCoord 16 31) $
         harmWorm (WormId 4) aStateWithLowHealthOposingWormsNextToEachother 20 id id id (toCoord 15 31) $
         -- Decrement banana bombs
         withWormBananas (always $ emptyAList) $
         -- Points for the four squares
         awardPointsToThisPlayerForKillingAnEnemy $
         awardPointsToThisPlayerForDamage 20      $
         penaliseThisPlayerForDamage 13           $
         awardPointsToThisPlayerForDigging        $
         awardPointsToThisPlayerForDigging        $
         awardPointsToThisPlayerForDigging        $
         awardPointsToThisPlayerForDigging        $
         awardPointsToThatPlayerForKillingAnEnemy $
         awardPointsToThatPlayerForDamage 20      $
         penaliseThatPlayerForDamage 13           $
         awardPointsToThatPlayerForDigging        $
         awardPointsToThatPlayerForDigging        $
         awardPointsToThatPlayerForDigging        $
         awardPointsToThatPlayerForDigging        $
         mapGameMap aStateWithLowHealthOposingWormsNextToEachother
                    ((-- Up
                      addAirAt (toCoord 15 30) .
                      addAirAt (toCoord 15 29) .
                      addAirAt (toCoord 16 29) .
                      -- Remaining
                      addAirAt (toCoord 14 30) .
                      addAirAt (toCoord 16 30) .
                      addAirAt (toCoord 17 30))))
      it "should give us both points for the squares which we both hit" $
        makeMove False (fromMoves bananaIntoDirtFromMe bananaIntoDirtFromMe) aStateWithOposingWormsNextToEachother `shouldBe`
        (setOpponentsLastMove aStateWithOposingWormsNextToEachother bananaIntoDirtFromMe $
         incrementRound $
         selectNextWormsDefault $         
         -- Decrement banana bombs
         withWormBananas (always $ aListFromList [(1, 2), (4, 2)]) $
         -- Points for the 13 squares
         -- For him
         awardPointsToThatPlayerForDigging $
         awardPointsToThatPlayerForDigging $
         awardPointsToThatPlayerForDigging $
         awardPointsToThatPlayerForDigging $
         awardPointsToThatPlayerForDigging $
         awardPointsToThatPlayerForDigging $
         awardPointsToThatPlayerForDigging $
         awardPointsToThatPlayerForDigging $
         awardPointsToThatPlayerForDigging $
         awardPointsToThatPlayerForDigging $
         awardPointsToThatPlayerForDigging $
         awardPointsToThatPlayerForDigging $
         awardPointsToThatPlayerForDigging $
         -- For me
         awardPointsToThisPlayerForDigging $
         awardPointsToThisPlayerForDigging $
         awardPointsToThisPlayerForDigging $
         awardPointsToThisPlayerForDigging $
         awardPointsToThisPlayerForDigging $
         awardPointsToThisPlayerForDigging $
         awardPointsToThisPlayerForDigging $
         awardPointsToThisPlayerForDigging $
         awardPointsToThisPlayerForDigging $
         awardPointsToThisPlayerForDigging $
         awardPointsToThisPlayerForDigging $
         awardPointsToThisPlayerForDigging $
         awardPointsToThisPlayerForDigging $
         -- Add his hits. (offset one to the right)
         mapGameMap aStateWithOposingWormsNextToEachother
                    ((addAirAt (toCoord 15 26) . -- epicentre
                      -- Up
                      addAirAt (toCoord 15 25) .
                      addAirAt (toCoord 15 24) .
                      addAirAt (toCoord 16 24) .
                      -- Down
                      addAirAt (toCoord 15 27) .
                      addAirAt (toCoord 15 28) .
                      addAirAt (toCoord 16 28) .
                      -- Left
                      addAirAt (toCoord 14 26) .
                      addAirAt (toCoord 13 26) .
                      -- Right
                      addAirAt (toCoord 16 26) .
                      addAirAt (toCoord 17 26) .
                      addAirAt (toCoord 18 26) .
                      -- Remaining
                      addAirAt (toCoord 14 25) .
                      addAirAt (toCoord 16 25) .
                      addAirAt (toCoord 17 25) .
                      addAirAt (toCoord 14 27) .
                      addAirAt (toCoord 16 27) .
                      addAirAt (toCoord 17 27))))
    -- Shooting
    prop "should not hit this players first horizontal target in range when it moves out of the way" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithNonDiagonalPadding
                                                  inBoundsWithNonDiagonalPadding)
                          (generateCoordDisplacer nonDiagonalDelta addDelta ignoreDelta)
                          (generateShotSwitch     shootEast shootWest)
                          (takeBothWorms          (WormId 1) (WormId 4))
                          (i, j, k)
      in makeMove True (fromMoves shot moveNorth) state `shouldBe`
         (setOpponentsLastMove state moveNorth $
          incrementRound $
          moveThatWorm (displaceCoordByMove (thatWormsCoord state) moveNorth) $
          awardPointsToThatPlayerForMovingToAir $
          awardPointsToThisPlayerForMissing state)
    prop "should not hit that players first horizontal target in range when it moves out of the way" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithNonDiagonalPadding
                                                  inBoundsWithNonDiagonalPadding)
                          (generateCoordDisplacer nonDiagonalDelta addDelta ignoreDelta)
                          (generateShotSwitch     shootWest shootEast)
                          (takeBothWorms          (WormId 1) (WormId 4))
                          (i, j, k)
      in makeMove True (fromMoves moveNorth shot) state `shouldBe`
         (setOpponentsLastMove state shot $
          incrementRound $
          moveThisWorm (displaceCoordByMove (thisWormsCoord state) moveNorth) $
          awardPointsToThisPlayerForMovingToAir $
          awardPointsToThatPlayerForMissing state)
    prop "should hit this players first horizontal target in range when it's an opponent worm" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithNonDiagonalPadding
                                                  inBoundsWithNoPadding)
                          (generateCoordDisplacer nonDiagonalDelta addDelta ignoreDelta)
                          (generateShotSwitch     shootEast shootWest)
                          (takeBothWorms          (WormId 1) (WormId 4))
                          (i, j, k)
      in makeMove True (fromMoves shot doNothing) state `shouldBe`
         (incrementRound $
          awardPointsToThisPlayerForKillingAnEnemy $
          awardPointsToThisPlayerForHittingAnEnemy $
          setOpponentsLastMoveToDummy $
          state { wormHealths = aListRemoveWormById (WormId 4) $ wormHealths state,
                  wormPositions = aListRemoveWormById (WormId 4) $ wormPositions state })
    prop "should hit this players first horizontal target in range when it's a friendly worm" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithNonDiagonalPadding
                                                  inBoundsWithNoPadding)
                          (generateCoordDisplacer nonDiagonalDelta addDelta ignoreDelta)
                          (generateShotSwitch     shootEast shootWest)
                          (takeBothWorms          (WormId 1) (WormId 2))
                          (i, j, k)
      in makeMove True (fromMoves shot doNothing) state `shouldBe`
         (incrementRound $
          penaliseThisPlayerForHittingHisFriendlyWorm $
          setOpponentsLastMoveToDummy $
          state { wormHealths = aListRemoveWormById (WormId 2) $ wormHealths state,
                  wormPositions = aListRemoveWormById (WormId 2) $ wormPositions state })
    prop "should hit this players first vertical target in range when it's an opponent worm" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithNoPadding
                                                  inBoundsWithNonDiagonalPadding)
                          (generateCoordDisplacer nonDiagonalDelta ignoreDelta addDelta)
                          (generateShotSwitch     shootSouth shootNorth)
                          (takeBothWorms          (WormId 1) (WormId 4))
                          (i, j, k)
      in makeMove True (fromMoves shot doNothing) state `shouldBe`
         (incrementRound $
          awardPointsToThisPlayerForKillingAnEnemy $
          awardPointsToThisPlayerForHittingAnEnemy $
          setOpponentsLastMoveToDummy $
          state { wormHealths = aListRemoveWormById (WormId 4) $ wormHealths state,
                  wormPositions = aListRemoveWormById (WormId 4) $ wormPositions state })
    prop "should hit this players first vertical target in range when it's a friendly worm" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithNoPadding
                                                  inBoundsWithNonDiagonalPadding)
                          (generateCoordDisplacer nonDiagonalDelta ignoreDelta addDelta)
                          (generateShotSwitch     shootSouth shootNorth)
                          (takeBothWorms          (WormId 1) (WormId 2))
                          (i, j, k)
      in makeMove True (fromMoves shot doNothing) state `shouldBe`
         (incrementRound $
          penaliseThisPlayerForHittingHisFriendlyWorm $
          setOpponentsLastMoveToDummy $
          state { wormHealths = aListRemoveWormById (WormId 2) $ wormHealths state,
                  wormPositions = aListRemoveWormById (WormId 2) $ wormPositions state })
    prop "should hit this players first NW-SE diagonal target in range when it's an opponent worm" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithDiagonalPadding
                                                  inBoundsWithDiagonalPadding)
                          (generateCoordDisplacer diagonalDelta addDelta addDelta)
                          (generateShotSwitch     shootSouthEast shootNorthWest)
                          (takeBothWorms          (WormId 1) (WormId 4))
                          (i, j, k)
      in makeMove True (fromMoves shot doNothing) state `shouldBe`
         (incrementRound $
          setOpponentsLastMoveToDummy $
          awardPointsToThisPlayerForKillingAnEnemy $
          awardPointsToThisPlayerForHittingAnEnemy $
          state { wormHealths = aListRemoveWormById (WormId 4) $ wormHealths state,
                  wormPositions = aListRemoveWormById (WormId 4) $ wormPositions state })
    prop "should hit this players first NW-SE diagonal target in range when it's a friendly worm" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithDiagonalPadding
                                                  inBoundsWithDiagonalPadding)
                          (generateCoordDisplacer diagonalDelta addDelta addDelta)
                          (generateShotSwitch     shootSouthEast shootNorthWest)
                          (takeBothWorms          (WormId 1) (WormId 2))
                          (i, j, k)
      in makeMove True (fromMoves shot doNothing) state `shouldBe`
         (incrementRound $
          setOpponentsLastMoveToDummy $
          penaliseThisPlayerForHittingHisFriendlyWorm $
          state { wormHealths = aListRemoveWormById (WormId 2) $ wormHealths state,
                  wormPositions = aListRemoveWormById (WormId 2) $ wormPositions state })
    prop "should hit this players first NE-SW diagonal target in range when it's an opponent worm" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithDiagonalPadding
                                                  inBoundsWithDiagonalPadding)
                          (generateCoordDisplacer diagonalDelta addDelta subtractDelta)
                          (generateShotSwitch     shootNorthEast shootSouthWest)
                          (takeBothWorms          (WormId 1) (WormId 4))
                          (i, j, k)
      in makeMove True (fromMoves shot doNothing) state `shouldBe`
         (incrementRound $
          setOpponentsLastMoveToDummy $
          awardPointsToThisPlayerForKillingAnEnemy $
          awardPointsToThisPlayerForHittingAnEnemy $
          state { wormHealths = aListRemoveWormById (WormId 4) $ wormHealths state,
                  wormPositions = aListRemoveWormById (WormId 4) $ wormPositions state })
    prop "should hit that players first horizontal target in range when it's my worm" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithNonDiagonalPadding
                                                  inBoundsWithNoPadding)
                          (generateCoordDisplacer nonDiagonalDelta addDelta ignoreDelta)
                          (generateShotSwitch     shootWest shootEast)
                          (takeBothWorms          (WormId 1) (WormId 4))
                          (i, j, k)
      in makeMove True (fromMoves doNothing shot) state `shouldBe`
         (incrementRound $
          setOpponentsLastMove state shot $
          awardPointsToThatPlayerForKillingAnEnemy $
          awardPointsToThatPlayerForHittingAnEnemy $
          state { wormHealths = aListRemoveWormById (WormId 1) $ wormHealths state,
                  wormPositions = aListRemoveWormById (WormId 1) $ wormPositions state })
    prop "should hit that players first horizontal target in range when it's friendly" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithNonDiagonalPadding
                                                  inBoundsWithNoPadding)
                          (generateCoordDisplacer nonDiagonalDelta addDelta ignoreDelta)
                          (generateShotSwitch     shootEast shootWest)
                          (takeBothWorms          (WormId 4) (WormId 8))
                          (i, j, k)
      in makeMove True (fromMoves doNothing shot) state `shouldBe`
         (setOpponentsLastMove state shot $
          incrementRound $
          penaliseThatPlayerForHittingHisFriendlyWorm $
          state { wormHealths = aListRemoveWormById (WormId 8) $ wormHealths state,
                  wormPositions = aListRemoveWormById (WormId 8) $ wormPositions state })
    prop "should hit that players first vertical target in range when it's my worm" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithNoPadding
                                                  inBoundsWithNonDiagonalPadding)
                          (generateCoordDisplacer nonDiagonalDelta ignoreDelta addDelta)
                          (generateShotSwitch     shootNorth shootSouth)
                          (takeBothWorms          (WormId 1) (WormId 4))
                          (i, j, k)
      in makeMove True (fromMoves doNothing shot) state `shouldBe`
         (setOpponentsLastMove state shot $
          incrementRound $
          awardPointsToThatPlayerForKillingAnEnemy $
          awardPointsToThatPlayerForHittingAnEnemy $
          state { wormHealths = aListRemoveWormById (WormId 1) $ wormHealths state,
                  wormPositions = aListRemoveWormById (WormId 1) $ wormPositions state })
    prop "should hit that players first vertical target in range when it's a friendly worm" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithNoPadding
                                                  inBoundsWithNonDiagonalPadding)
                          (generateCoordDisplacer nonDiagonalDelta ignoreDelta addDelta)
                          (generateShotSwitch     shootSouth shootNorth)
                          (takeBothWorms          (WormId 4) (WormId 8))
                          (i, j, k)
      in makeMove True (fromMoves doNothing shot) state `shouldBe`
         (setOpponentsLastMove state shot $
          incrementRound $
          penaliseThatPlayerForHittingHisFriendlyWorm $
          state { wormHealths = aListRemoveWormById (WormId 8) $ wormHealths state,
                  wormPositions = aListRemoveWormById (WormId 8) $ wormPositions state })
    prop "should hit that players first NW-SE diagonal target in range when it's my worm" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithDiagonalPadding
                                                  inBoundsWithDiagonalPadding)
                          (generateCoordDisplacer diagonalDelta addDelta addDelta)
                          (generateShotSwitch     shootNorthWest shootSouthEast)
                          (takeBothWorms          (WormId 1) (WormId 4))
                          (i, j, k)
      in makeMove True (fromMoves doNothing shot) state `shouldBe`
         (setOpponentsLastMove state shot $
          incrementRound $
          awardPointsToThatPlayerForKillingAnEnemy $
          awardPointsToThatPlayerForHittingAnEnemy $
          state { wormHealths = aListRemoveWormById (WormId 1) $ wormHealths state,
                  wormPositions = aListRemoveWormById (WormId 1) $ wormPositions state })
    prop "should hit that players first NW-SE diagonal target in range when it's a friendly worm" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithDiagonalPadding
                                                  inBoundsWithDiagonalPadding)
                          (generateCoordDisplacer diagonalDelta addDelta addDelta)
                          (generateShotSwitch     shootNorthWest shootSouthEast)
                          (takeBothWorms          (WormId 8) (WormId 4))
                          (i, j, k)
      in makeMove True (fromMoves doNothing shot) state `shouldBe`
         (setOpponentsLastMove state shot $
          incrementRound $
          penaliseThatPlayerForHittingHisFriendlyWorm $
          state { wormHealths = aListRemoveWormById (WormId 8) $ wormHealths state,
                  wormPositions = aListRemoveWormById (WormId 8) $ wormPositions state })
    prop "should hit that players first NE-SW diagonal target in range when it's my worm" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithDiagonalPadding
                                                  inBoundsWithDiagonalPadding)
                          (generateCoordDisplacer diagonalDelta addDelta subtractDelta)
                          (generateShotSwitch     shootSouthWest shootNorthEast)
                          (takeBothWorms          (WormId 1) (WormId 4))
                          (i, j, k)
      in makeMove True (fromMoves doNothing shot) state `shouldBe`
         (setOpponentsLastMove state shot $
          incrementRound $
          awardPointsToThatPlayerForKillingAnEnemy $
          awardPointsToThatPlayerForHittingAnEnemy $
          state { wormHealths = aListRemoveWormById (WormId 1) $ wormHealths state,
                  wormPositions = aListRemoveWormById (WormId 1) $ wormPositions state })
    prop "should not hit this players first horizontal target in range when there's dirt or space in the way" $ \ (i, j, k, l) ->
      let (state, shot) = generateShotScenarioWithMapModifications
                          (generateCoordGenerator inBoundsWithNonDiagonalPadding
                                                  inBoundsWithNoPadding)
                          (generateCoordDisplacer nonDiagonalDeltaOfAtLeastTwo addDelta ignoreDelta)
                          (generateShotSwitch     shootEast shootWest)
                          (takeBothWorms          (WormId 1) (WormId 4))
                          (putDirtOrSpaceBetweenWorms l)
                          (i, j, k)
      in makeMove True (fromMoves shot doNothing) state `shouldBe`
         (incrementRound $ setOpponentsLastMoveToDummy $ awardPointsToThisPlayerForMissing state)
    prop "should not hit this players first horizontal target in range when there's a friendly worm in the way" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator              inBoundsWithNonDiagonalPadding
                                                               inBoundsWithNoPadding)
                          (generateCoordDisplacer              nonDiagonalDeltaOfAtLeastTwo addDelta ignoreDelta)
                          (generateShotSwitch                  shootEast shootWest)
                          (takeBothWormsAndPutAnotherInbetween (WormId 2) (WormId 1) (WormId 4))
                          (i, j, k)
      in makeMove True (fromMoves shot doNothing) state `shouldSatisfy`
         ((hasScore (284) . myPlayer) .&&.
          containsWormOfId (WormId 1) .&&.
          containsWormOfId (WormId 4))
    prop "should not hit that players first horizontal target when it's not in range" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithNonDiagonalPadding
                                                  inBoundsWithNoPadding)
                          (generateCoordDisplacer nonDiagonalDeltaOutOfRange addDelta ignoreDelta)
                          (generateShotSwitch     shootWest shootEast)
                          (takeBothWorms          (WormId 1) (WormId 4))
                          (i, j, k)
      in makeMove True (fromMoves doNothing shot) state `shouldBe`
         (incrementRound $
          setOpponentsLastMove state shot $
          awardPointsToThatPlayerForMissing state)
    prop "should not hit that players first vertical target when it's not in range" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithNoPadding
                                                  inBoundsWithNonDiagonalPadding)
                          (generateCoordDisplacer nonDiagonalDeltaOutOfRange ignoreDelta addDelta)
                          (generateShotSwitch     shootNorth shootSouth)
                          (takeBothWorms          (WormId 1) (WormId 4))
                          (i, j, k)
      in makeMove True (fromMoves doNothing shot) state `shouldBe`
         (incrementRound $
          setOpponentsLastMove state shot $
          awardPointsToThatPlayerForMissing state)
    prop "should not hit that players first NE-SW target when it's not in range" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithDiagonalPadding
                                                  inBoundsWithDiagonalPadding)
                          (generateCoordDisplacer diagonalDeltaOutOfRange addDelta subtractDelta)
                          (generateShotSwitch     shootSouthWest shootNorthEast)
                          (takeBothWorms          (WormId 1) (WormId 4))
                          (i, j, k)
      in makeMove True (fromMoves doNothing shot) state `shouldBe`
         (setOpponentsLastMove state shot $
          incrementRound $
          awardPointsToThatPlayerForMissing state)
    -- TODO this test is broken and that's worrying.  To reproduce
    -- this problem set the generated delta for out of range to one
    -- less than it is.  The above test fails but this doesn't.
    prop "should not hit that players first NW-SE target when it's not in range" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithDiagonalPadding
                                                  inBoundsWithDiagonalPadding)
                          (generateCoordDisplacer diagonalDeltaOutOfRange subtractDelta addDelta)
                          (generateShotSwitch     shootSouthEast shootNorthWest)
                          (takeBothWorms          (WormId 1) (WormId 4))
                          (i, j, k)
      in makeMove True (fromMoves doNothing shot) state `shouldBe`
         (incrementRound $
          setOpponentsLastMove state shot $
          awardPointsToThatPlayerForMissing state)
    context "when both worms shoot at eachother simultaneously and they're both in range of eachother" $
      prop "both worms should be harmed" $ \ (i, j, k) ->
        let (state, shot) = generateShotScenario
                            (generateCoordGenerator inBoundsWithNonDiagonalPadding
                                                    inBoundsWithNoPadding)
                            (generateCoordDisplacer nonDiagonalDelta addDelta ignoreDelta)
                            (generateShotSwitch     shootEast shootWest)
                            (takeBothWorms          (WormId 1) (WormId 4))
                            (i, j, k)
        in makeMove True (fromMoves shot (oppositeShot shot)) state `shouldBe`
           (incrementRound $
            setOpponentsLastMove state (oppositeShot shot) $
            awardPointsToThatPlayerForKillingAnEnemy $
            awardPointsToThatPlayerForHittingAnEnemy $
            awardPointsToThisPlayerForKillingAnEnemy $
            awardPointsToThisPlayerForHittingAnEnemy $
            state { wormHealths =
                      aListRemoveWormById (WormId 4) $
                      aListRemoveWormById (WormId 1) $
                      wormHealths state,
                    wormPositions =
                      aListRemoveWormById (WormId 4) $
                      aListRemoveWormById (WormId 1) $
                      wormPositions state })
    prop "should hit this players first horizontal target in range when it's an opponent worm without killing it" $ \ (i, j, k) ->
       let (state, shot) = generateShotScenario
                           (generateCoordGenerator  inBoundsWithNonDiagonalPadding
                                                    inBoundsWithNoPadding)
                           (generateCoordDisplacer  nonDiagonalDelta addDelta ignoreDelta)
                           (generateShotSwitch      shootEast shootWest)
                           (takeBothWormsWithHealth 100 (WormId 1) (WormId 4))
                           (i, j, k)
       in makeMove True (fromMoves shot doNothing) state `shouldBe`
          (incrementRound $
           awardPointsToThisPlayerForHittingAnEnemy $
           setOpponentsLastMoveToDummy $
           state { wormHealths = harmWormById rocketDamage (WormId 4) $ wormHealths state })
    prop "should hit that players first horizontal target in range when it's my worm without killing it" $ \ (i, j, k) ->
       let (state, shot) = generateShotScenario
                           (generateCoordGenerator  inBoundsWithNonDiagonalPadding
                                                    inBoundsWithNoPadding)
                           (generateCoordDisplacer  nonDiagonalDelta addDelta ignoreDelta)
                           (generateShotSwitch      shootWest shootEast)
                           (takeBothWormsWithHealth 100 (WormId 1) (WormId 4))
                           (i, j, k)
       in makeMove True (fromMoves doNothing shot) state `shouldBe`
          (setOpponentsLastMove state shot $
           incrementRound $
           awardPointsToThatPlayerForHittingAnEnemy $
           state { wormHealths = harmWormById rocketDamage (WormId 1)  $ wormHealths state })
    context "when selecting a worm" $ do
      let aStateWithWormsOn20Health =
            withWormHealths (always $ AList 20 20 20 20 20 20) $
            withWormPositions (always $ AList
                                  (toCoord 1  1)
                                  (toCoord 1  5)
                                  (toCoord 1  10)
                                  (toCoord 31 1)
                                  (toCoord 31 5)
                                  (toCoord 31 10)) aStateWithOnlyAirOnMap
      prop "the worm after the selected worm should be next" $ \ (i, j, k, l) ->
        let thisSelection  = oneIfZero $ abs i `mod` 4
            thisMove       = withSelection (WormId thisSelection) $
                             Move $ abs j `mod` 24
            thisNextWormId = nextWormId (WormId thisSelection) [WormId 1, WormId 2, WormId 3]
            thatSelection  = fourIfZero $ shiftL (abs k `mod` 4) 2
            thatMove       = withSelection (WormId thatSelection) $
                             Move $ abs l `mod` 24
            thatNextWormId = nextWormId (WormId thatSelection) [WormId 4, WormId 8, WormId 12]
        in ((debugMove thisMove, debugMove thatMove,
             thisSelection, thatSelection,
             thisNextWormId, thatNextWormId),
            makeMove False (fromMoves thisMove thatMove) aStateWithWormsOn20Health) `shouldSatisfy`
           \ (_, (State { myPlayer = (Player _ thisCurrentWormId _),
                          opponent = (Player _ thatCurrentWormId _)})) ->
             thisCurrentWormId == thisNextWormId &&
             thatCurrentWormId == thatNextWormId
      prop "should move the worm which was selected when a move move is made" $ \ (i, j, k, l) ->
        let thisSelection  = WormId $ oneIfZero $ abs i `mod` 4
            thisMove       = withSelection thisSelection $
                             moveMoves L.!! (abs j `mod` (length moveMoves))
            thatSelection  = WormId $ fourIfZero $ shiftL (abs k `mod` 4) 2
            thatMove       = withSelection thatSelection $
                             moveMoves L.!! (abs l `mod` (length moveMoves))
            positions      = wormPositions aStateWithWormsOn20Health
        in ((debugMove thisMove, debugMove thatMove),
             makeMove False (fromMoves thisMove thatMove) aStateWithWormsOn20Health) `shouldSatisfy`
           \ (_, state) ->
             (not $
              isAHit $
              isAPositionOfAWorm (aListFindDataById thisSelection positions)
                                 (wormPositions state)) &&
             (not $
              isAHit $
              isAPositionOfAWorm (aListFindDataById thatSelection positions)
                                 (wormPositions state))
      prop "should decrement the number of selections left" $ \ (i, j, k, l) ->
        let thisSelection  = WormId $ oneIfZero $ abs i `mod` 4
            thisMove       = withSelection thisSelection $
                             moveMoves L.!! (abs j `mod` (length moveMoves))
            thatSelection  = WormId $ fourIfZero $ shiftL (abs k `mod` 4) 2
            thatMove       = withSelection thatSelection $
                             moveMoves L.!! (abs l `mod` (length moveMoves))
        in ((debugMove thisMove, debugMove thatMove),
             makeMove False (fromMoves thisMove thatMove) aStateWithWormsOn20Health) `shouldSatisfy`
           \ (_, state) ->
             ((== (Selections 2)) $
              selections $
              myPlayer state) &&
             ((== (Selections 2)) $
              selections $
              opponent state)
      let aStateWithNoSelections = mapThisPlayer (withSelections (always (Selections 0))) $
                                   mapThatPlayer (withSelections (always (Selections 0)))
                                   aStateWithWormsOn20Health
      prop "should always move the next worm when no selects are left" $ \ (i, j, k, l) ->
        let thisSelection  = [WormId 2, WormId 3] L.!! (abs i `mod` 2)
            thisMove       = withSelection thisSelection $
                             moveMoves L.!! (abs j `mod` (length moveMoves))
            thatSelection  = [WormId 8, WormId 12] L.!! (abs k `mod` 2)
            thatMove       = withSelection thatSelection $
                             moveMoves L.!! (abs l `mod` (length moveMoves))
            positions      = wormPositions aStateWithNoSelections
        in ((debugMove thisMove, debugMove thatMove),
             makeMove False (fromMoves thisMove thatMove) aStateWithNoSelections) `shouldSatisfy`
           \ (_, state) ->
             (isAHit $
              isAPositionOfAWorm (aListFindDataById thisSelection positions)
                                 (wormPositions state)) &&
             (isAHit $
              isAPositionOfAWorm (aListFindDataById thatSelection positions)
                                 (wormPositions state))

diagonalRocketRange :: Int
diagonalRocketRange = 3

horizontalRocketRange :: Int
horizontalRocketRange = 4

fullOfStuff :: Integer
fullOfStuff = ((shiftL 1 (mapDim * mapDim)) - 1)

airOnlyGameMap :: GameMap
airOnlyGameMap = GameMap fullOfStuff 0 0 0

dirtOnlyGameMap :: GameMap
dirtOnlyGameMap = GameMap 0 fullOfStuff 0 0

deepSpaceOnlyGameMap :: GameMap
deepSpaceOnlyGameMap = GameMap 0 0 fullOfStuff 0

medipackOnlyGameMap :: GameMap
medipackOnlyGameMap = GameMap 0 0 0 fullOfStuff

spaceBetween :: ModifyMap
spaceBetween thisCoord thatCoord=
  addSpaceAt (coordBetween thisCoord thatCoord)

addDirtAt :: Coord -> GameMap -> GameMap
addDirtAt = (flip cellTo) DIRT

addSpaceAt :: Coord -> GameMap -> GameMap
addSpaceAt = (flip cellTo) DEEP_SPACE

addAirAt :: Coord -> GameMap -> GameMap
addAirAt = (flip cellTo) AIR

addMedipackAt :: Coord -> GameMap -> GameMap
addMedipackAt = (flip cellTo) MEDIPACK

-- Medipack is at 31 31
aGameMapWithAMedipack = vectorGameMapToGameMap $ V.fromList $
  spaceRow ++
  dirtRow ++
  foldl' (++) [] (take (mapDim - 4) $ repeat middleRow) ++
  dirtRowWithMedipack ++
  spaceRow

dirtRowWithMedipack = [DEEP_SPACE] ++ (take (mapDim - 3) $ repeat AIR) ++ [MEDIPACK, DEEP_SPACE]
dirtRow = [DEEP_SPACE] ++ (take (mapDim - 2) $ repeat AIR) ++ [DEEP_SPACE]
spaceRow = take mapDim $ repeat DEEP_SPACE
middleRow = [DEEP_SPACE] ++ tenAir ++ someDirt ++ tenAir ++ [DEEP_SPACE]
  where tenAir = (take 10 $ repeat AIR)
someDirt = (take (mapDim - 22) $ repeat DIRT)

aGameMap = vectorGameMapToGameMap $ V.fromList $
  spaceRow ++
  dirtRow ++
  foldl' (++) [] (take (mapDim - 4) $ repeat middleRow) ++
  dirtRow ++
  spaceRow

airRow = take mapDim $ repeat AIR

aGameMapWithOnlyAir = vectorGameMapToGameMap $ V.fromList $
  foldl' (++) [] (take mapDim $ repeat airRow)

withSelections :: (Selections -> Selections) -> ModifyPlayer
withSelections f (Player points' wormId' selections') =
  Player points' wormId' $ f selections'

selections :: Player -> Selections
selections (Player _ _ selections') = selections'

isAHit (HitWorm _) = True
isAHit _           = False

debugMove :: Move -> String
-- Shoot
debugMove dir@(Move x)
  -- Select
  | x >= 128 = debugSelect dir -- Calls back into this function without the select
  -- Shoot
  | x < 8   = debugShootMove dir
  -- Move
  | x < 16  = "move "   ++ (toDirection $ Move $ x - 8)
  -- Dig
  | x < 24  = "dig "    ++ (toDirection $ Move $ x - 16)
  -- Throwing the bomb
  | x < 107 = "banana " ++ (show $ Move $ x - 24)
-- Nothing
debugMove _ = "nothing"

toDirection :: Move -> String
toDirection (Move 0) = "N"
toDirection (Move 1) = "NE"
toDirection (Move 2) = "E"
toDirection (Move 3) = "SE"
toDirection (Move 4) = "S"
toDirection (Move 5) = "SW"
toDirection (Move 6) = "W"
toDirection (Move 7) = "NW"
toDirection x        = error $ "toDirection: " ++ show x

debugSelect :: Move -> String
debugSelect move =
  let selection = decodeSelection move
      move'     = removeSelectionFromMove move
  in "select " ++ show selection ++ ";" ++ debugMove move'

debugShootMove :: Move -> String
debugShootMove (Move 0) = "shoot N"
debugShootMove (Move 1) = "shoot NE"
debugShootMove (Move 2) = "shoot E"
debugShootMove (Move 3) = "shoot SE"
debugShootMove (Move 4) = "shoot S"
debugShootMove (Move 5) = "shoot SW"
debugShootMove (Move 6) = "shoot W"
debugShootMove (Move 7) = "shoot NW"
debugShootMove x        = error $ "debugShootMove: " ++ show x

moveMoves = [Move 8, Move 9, Move 10, Move 11, Move 12, Move 13, Move 14, Move 15]

oneIfZero :: Int -> Int
oneIfZero 0 = 1
oneIfZero x = x

fourIfZero :: Int -> Int
fourIfZero 0 = 4
fourIfZero x = x

-- For how to come up with this value take a look at the function
-- `coordDeltasInRange' and the accompanying doc string.
bananaRightOnMe       = Move 64
bananaOneToRight      = Move 65
bananaOneToLeft       = Move 63
bananaIntoDirtFromMe  = Move 24
bananaIntoDirtFromHim = Move 104

hasScore :: Int -> Player -> Bool
hasScore score' (Player score'' _ _) = score' == score''

oppositeShot :: Move -> Move
oppositeShot (Move x) = Move ((x + 4) `mod` 8)

emptyWormHealths = emptyAList

emptyWormPositions = emptyAList

emptyWormSnowballs = emptyAList

emptyWormFrozenDurations = emptyAList

(.&&.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.&&.) p1 p2 x =
  p1 x && p2 x

oneWormHarmed :: Int -> WormHealths -> Bool
oneWormHarmed originalHealth =
  (== 1) . length . filter (/= originalHealth) . map snd . aListToList

noWormHarmed :: Int -> WormHealths -> Bool
noWormHarmed originalHealth =
  all (== originalHealth) . map snd . aListToList

containsWormOfId :: WormId -> State -> Bool
containsWormOfId wormId' =
  any ((== wormId') . fst) . aListToList . wormHealths

putDirtOrSpaceBetweenWorms :: Int -> ModifyMap
putDirtOrSpaceBetweenWorms x =
  if x < 0
  then dirtBetween
  else spaceBetween

dirtBetween :: ModifyMap
dirtBetween thisCoord thatCoord =
  addDirtAt (coordBetween thisCoord thatCoord)

coordBetween :: Coord -> Coord -> Coord
coordBetween xy ij =
  let (x', y') = fromCoord xy
      (i,  j)  = fromCoord ij
  in toCoord ((x' + i) `div` 2) ((y' + j) `div` 2)

generateShotSwitch :: Move -> Move -> ShotSwitch
generateShotSwitch a b x =
  if x > 0 then a else b

generateInBoundsCoordinate :: Int -> Int -> Coord
generateInBoundsCoordinate = generateCoordGenerator inBoundsWithNoPadding inBoundsWithNoPadding

startingHealth = 8

unWormIds :: [(WormId, Int)] -> [(Int, Int)]
unWormIds = map (\ ((WormId x), y) -> (x, y))

aListConcat :: AList -> AList -> AList
aListConcat xs ys =
  aListFromList $ (unWormIds $ aListToList xs) ++ (unWormIds $ aListToList ys)

takeBothWormsWithHealth :: WormHealth ->  WormId -> WormId -> AddToWormFacts
takeBothWormsWithHealth startingHealth' (WormId thisWormId) (WormId thatWormId) thisCoord thatCoord state =
  state { wormHealths   =
            aListConcat (
              aListFromList [(thisWormId, startingHealth'),
                             (thatWormId, startingHealth')])
              (wormHealths   state),
          wormPositions =
            aListConcat (
              aListFromList [(thisWormId, thisCoord),
                             (thatWormId, thatCoord)])
              (wormPositions state) }


takeBothWorms :: WormId -> WormId -> AddToWormFacts
takeBothWorms (WormId thisWormId) (WormId thatWormId) thisCoord thatCoord state =
  state { wormHealths   =
            aListConcat
              (aListFromList [(thisWormId, startingHealth), (thatWormId, startingHealth)])
              (wormHealths state),
          wormPositions =
            aListConcat
              (aListFromList [
                  (thisWormId, thisCoord),
                  (thatWormId, thatCoord)])
              (wormPositions state) }

-- TODO: test
takeBothWormsAndPutAnotherInbetween :: WormId -> WormId -> WormId -> AddToWormFacts
takeBothWormsAndPutAnotherInbetween (WormId inbetweenId) (WormId thisWormId) (WormId thatWormId) thisCoord thatCoord state =
  state { wormHealths   =
            aListConcat (
              aListFromList [
                  (thisWormId,  startingHealth),
                  (inbetweenId, startingHealth),
                  (thatWormId,  startingHealth)])
              (wormHealths   state),
          wormPositions =
            aListConcat (
              aListFromList [
                  (thisWormId,  thisCoord),
                  (inbetweenId, (coordBetween thisCoord thatCoord)),
                  (thatWormId,  thatCoord)])
              (wormPositions state) }

addDelta :: Int -> Int -> Int
addDelta = (+)

subtractDelta :: Int -> Int -> Int
subtractDelta = (-)

ignoreDelta :: Int -> Int -> Int
ignoreDelta x _ = x

generateCoordGenerator :: (Int -> Int) -> (Int -> Int) -> GenerateCoordinate
generateCoordGenerator fX fY = \ x' y' ->
  toCoord (fX x') (fY y')

generateCoordDisplacer :: (Int -> Int) -> (Int -> Int -> Int) -> (Int -> Int -> Int) -> DisplaceFromCoordinate
generateCoordDisplacer fDelta fX fY coord' k =
  let (x', y') = fromCoord coord'
      delta    = fDelta k
  in (toCoord (fX x' delta) (fY y' delta), delta)

type GenerateCoordinate     = Int -> Int -> Coord
type DisplaceFromCoordinate = Coord -> Int -> (Coord, Int)
type ShotSwitch             = Int -> Move
type AddToWormFacts         = Coord -> Coord -> ModifyState
type ModifyMap              = Coord -> Coord -> GameMap -> GameMap

justNothing :: Maybe String
justNothing = Just "nothing"

generateShotScenarioWithMapModifications :: GenerateCoordinate -> DisplaceFromCoordinate -> ShotSwitch -> AddToWormFacts -> ModifyMap -> (Int, Int, Int) -> (State, Move)
generateShotScenarioWithMapModifications generateCoord displace switchShot addFacts modifyMap (i, j, k) =
  let originatingCoord        = generateCoord i j
      (displacedCoord, delta) = displace      originatingCoord k
      shot                    = switchShot    delta
      modifiedMap             = modifyMap originatingCoord displacedCoord aGameMapWithOnlyAir
      state                   = State justNothing
                                      0
                                      emptyWormHealths
                                      emptyWormPositions
                                      emptyBananaBombs
                                      emptyWormSnowballs
                                      emptyWormFrozenDurations
                                      (Player 300 (WormId 1) startingSelections)
                                      (Player 300 (WormId 4) startingSelections)
                                      modifiedMap
      state'                  = addFacts originatingCoord displacedCoord state
  in (state', shot)

generateShotScenario :: GenerateCoordinate -> DisplaceFromCoordinate -> ShotSwitch -> AddToWormFacts -> (Int, Int, Int) -> (State, Move)
generateShotScenario generateCoord displace switchShot addFacts (i, j, k) =
  generateShotScenarioWithMapModifications generateCoord
                                           displace
                                           switchShot
                                           addFacts
                                           identityMapModification
                                           (i, j, k)

identityMapModification :: ModifyMap
identityMapModification _ _ = id

nonDiagonalDeltaOutOfRange :: Int -> Int
nonDiagonalDeltaOutOfRange x =
  (horizontalRocketRange + 1) * if x < 0 then -1 else 1

diagonalDeltaOutOfRange :: Int -> Int
diagonalDeltaOutOfRange x =
  (diagonalRocketRange + 1) * if x < 0 then -1 else 1

nonDiagonalDelta :: Int -> Int
nonDiagonalDelta x =
  let y = (x `mod` 7) - 3
  in if y == 0 then -1 else y

nonDiagonalDeltaOfAtLeastTwo :: Int -> Int
nonDiagonalDeltaOfAtLeastTwo x =
  divergeFromZero 1 $ let y = (x `mod` 5) - 2
                      in if y == 0 then -1 else y

divergeFromZero :: Int -> Int -> Int
divergeFromZero deltaMagnitude x =
  x + if x < 0
      then -deltaMagnitude
      else deltaMagnitude

diagonalDelta :: Int -> Int
diagonalDelta x =
  let y = (x `mod` 5) - 2
  in if y == 0 then -1 else y

inBoundsWithNonDiagonalPadding x = 5 + (x `mod` (mapDim - 10))

inBoundsWithDiagonalPadding x = 4 + (x `mod` (mapDim - 8))

inBoundsWithNoPadding x = x `mod` mapDim

shootNorthEast = Move 1

shootSouthWest = Move 5

shootSouthEast = Move 3

shootNorthWest = Move 7

shootEast = Move 2

shootWest = Move 6

shootNorth = Move 0

shootSouth = Move 4

moveNorth = Move 8

digNorth = Move 16

digSouth = Move 20

digSouthEast = Move 19

digEast = Move 18

moveSouth = Move 12

moveEast = Move 10

moveWest = Move 14

emptyBananaBombs = emptyAList

aStateWithoutWorms = State justNothing
                           0
                           emptyWormHealths
                           emptyWormPositions
                           emptyBananaBombs
                           emptyWormSnowballs
                           emptyWormFrozenDurations
                           aPlayer
                           anOpponent
                           aGameMap

aState = State justNothing
               0
               someWormHealths
               someWormPositions
               emptyBananaBombs
               emptyWormSnowballs
               emptyWormFrozenDurations
               aPlayer
               anOpponent
               aGameMap

thisPlayersHealths = [
  (1, startingHealth),
  (2, startingHealth),
  (3, 20)]

thatPlayersHealths = [
  (4,  startingHealth),
  (8,  startingHealth),
  (12, 20)]

someWormHealths = aListFromList $ thisPlayersHealths ++ thatPlayersHealths

selectNextWorms :: WormId -> WormId -> ModifyState
selectNextWorms thisWormId thatWormId state@(State { myPlayer = myPlayer', opponent = opponent' }) =
  state { myPlayer = withCurrentWormId thisWormId myPlayer',
          opponent = withCurrentWormId thatWormId opponent' }

selectNextWormsDefault :: ModifyState
selectNextWormsDefault = selectNextWorms (WormId 2) (WormId 8)

someWormPositions = AList (toCoord 15 31)
                          (toCoord 1 31)
                          (toCoord 1 30)
                          (toCoord 16 1)
                          (toCoord 19 1)
                          (toCoord 20 1)

aStateWithOpponentBeneathDirt =
  moveThatWorm (toCoord 14 31) aState

aStateWithOpponentNearDirtToTheEast =
  moveThatWorm (toCoord 9 2) aState

aStateWithBothWormsNearTheSameDirtBlock =
  moveThisWorm (toCoord 10 1) $
  moveThatWorm (toCoord 11 1) aState

aStateWithDirtMissingAboveOpponentWorm =
  mapGameMap aStateWithOpponentBeneathDirt (removeDirtAt (toCoord 14 30))

aStateWithDirtMissingAboveMyWorm =
  mapGameMap aState (removeDirtAt (toCoord 15 30))

aStateWithOpponentWormMovedLeftFromTheRightEdge =
  moveThatWorm (toCoord 31 15) aStateWithOpponentWormOnTheRightEdge

aStateWithOpponentWormOnTheRightEdgeMovedDown =
  moveThatWorm (toCoord 32 16) aStateWithOpponentWormOnTheRightEdge

aStateWithOpponentWormOnTheRightEdgeMovedUp =
  moveThatWorm (toCoord 32 14) aStateWithOpponentWormOnTheRightEdge

aStateWithOpponentWormOnTheRightEdge =
  moveThatWorm (toCoord 32 15) aStateWithOnlyAirOnMap

aStateWithOpponentWormUpFromTheBottomEdge =
  moveThatWorm (toCoord 4 31) aStateWithOpponentWormOnTheBottomEdge

aStateWithOpponentWormOnTheBottomEdgeMovedLeft =
  moveThatWorm (toCoord 3 32) aStateWithOpponentWormOnTheBottomEdge

aStateWithOpponentWormOnTheBottomEdgeMovedRight =
  moveThatWorm (toCoord 5 32) aStateWithOpponentWormOnTheBottomEdge

aStateWithOpponentWormOnTheBottomEdge =
  moveThatWorm (toCoord 4 32) aStateWithOnlyAirOnMap

aStateWithOpponentWormDownwardOnLeftEdge =
  moveThatWorm (toCoord 0 16) aStateWithOnlyAirOnMap

aStateWithOpponentWormUpwardOnLeftEdge =
  moveThatWorm (toCoord 0 14) aStateWithOnlyAirOnMap

aStateWithOpponentWormRightFromLeftEdge =
  moveThatWorm (toCoord 1 15) aStateWithOnlyAirOnMap

aStateWithOpponentWormOnLeftEdge =
  moveThatWorm (toCoord 0 15) aStateWithOnlyAirOnMap

aStateWithOpponentWormOnTop = aStateWithOnlyAirOnMap {
  wormPositions = wormPositionsWithOpponentAtTop }

aStateWithOpponentWormOnTopMovedRight =
  moveThatWorm (toCoord 16 0) aStateWithOpponentWormOnTop

aStateWithOpponentWormOnTopMovedLeft =
  moveThatWorm (toCoord 14 0) aStateWithOpponentWormOnTop

aStateWithOpponentWormOnTopMovedDown =
  moveThatWorm (toCoord 15 1) aStateWithOpponentWormOnTop

aStateWithOnlyAirOnMap = aState {
  gameMap = aGameMapWithOnlyAir }

aStateWithMyWormDownwardOnLeftEdge =
  moveThisWorm (toCoord 0 16) aStateWithOnlyAirOnMap

aStateWithMyWormRightFromLeftEdge =
  moveThisWorm (toCoord 1 15) aStateWithOnlyAirOnMap

aStateWithMyWormUpwardsOnLeftEdge =
  moveThisWorm (toCoord 0 14) aStateWithOnlyAirOnMap

aStateWithMyWormOnLeftEdge =
  moveThisWorm (toCoord 0 15) aStateWithOnlyAirOnMap

aStateWithMyWormOnTop = aStateWithOnlyAirOnMap {
  wormPositions = aListConcat wormPositionsWithMyWormAtTop opponentWormsAllAtTopLeft }

aStateWithMyWormOnTopMovedRight =
  moveThisWorm (toCoord 16 0) aStateWithMyWormOnTop

aStateWithMyWormOnTopMovedLeft =
  moveThisWorm (toCoord 14 0) aStateWithMyWormOnTop

aStateWithMyWormOnTopMovedDown =
  moveThisWorm (toCoord 15 1) aStateWithMyWormOnTop

aStateWithMyWormOnTheBottomEdge =
  moveThisWorm (toCoord 4 32) aStateWithOnlyAirOnMap

aStateWithMyWormOnTheBottomEdgeMovedRight =
  moveThisWorm (toCoord 5 32) aStateWithMyWormOnTheBottomEdge

aStateWithMyWormOnTheBottomEdgeMovedLeft =
  moveThisWorm (toCoord 3 32) aStateWithMyWormOnTheBottomEdge

aStateWithMyWormUpFromTheBottomEdge =
  moveThisWorm (toCoord 4 31) aStateWithMyWormOnTheBottomEdge

aStateWithMyWormOnTheRightEdge =
  moveThisWorm (toCoord 32 15) aStateWithOnlyAirOnMap

aStateWithMyWormOnTheRightEdgeMovedUp =
  moveThisWorm (toCoord 32 14) aStateWithMyWormOnTheRightEdge

aStateWithMyWormOnTheRightEdgeMovedDown =
  moveThisWorm (toCoord 32 16) aStateWithMyWormOnTheRightEdge

aStateWithMyWormMovedLeftFromTheRightEdge =
  moveThisWorm (toCoord 31 15) aStateWithMyWormOnTheRightEdge

aStateWhereWeSwappedOverTheMedipack =
  knockBackDamage $
  moveThisWorm (toCoord 31 30) $
  moveThatWorm (toCoord 30 31)
  aState { gameMap = aGameMapWithAMedipack }

aStateWhereNoSwapHappened =
  aStateWithBothWormsNextToTheMedipack

aStateWithBothWormsNextToTheMedipack = aState {
  wormPositions = aListConcat wormPositionsWithMyWormNextToMedipack wormPositionsWithOpponentNextToMedipack,
  gameMap       = aGameMapWithAMedipack }

aStateWithOpponentsWormNextToTheMedipack = aState {
  wormPositions = aListConcat wormPositionsWithOpponentNextToMedipack myWormsAllAtTopLeft,
  gameMap       = aGameMapWithAMedipack }

aStateWithOpponentsWormOnTheMedipack = aState {
  wormPositions = aListConcat wormPositionsWithOpponentOnTheMedipack myWormsAllAtTopLeft,
  wormHealths   = wormHealthsWithOpponentHavingReceivedTheMedipack }

aStateWithMyWormNextToTheMedipack = aState {
  wormPositions = aListConcat wormPositionsWithMyWormNextToMedipack opponentWormsAllAtTopLeft,
  gameMap       = aGameMapWithAMedipack }

aStateWithMyWormOnTheMedipack = aState {
  wormPositions = aListConcat wormPositionsWithMyWormOnTheMedipack opponentWormsAllAtTopLeft,
  wormHealths   = wormHealthsWithMyWormHavingReceivedTheMedipack }

aStateWithEnemyWormsNextToEachother = aState { wormPositions = wormPositionsWithHisNextToHis }

aStateWithMyWormNextToAnEnemy = aState { wormPositions = wormPositionsWithHisNextToMine }

aStateWithMyWormsNextToEachOther = aState { wormPositions = wormPositionsWithMyWormsNextToEachother }

aStateWithImpendingCollision = aState {
  wormPositions = wormPositionsWithImpendingCollision,
  wormHealths   = wormHealthsForOneAndFive }

anOpponent = Player 300 (WormId 4) startingSelections

wormPositionsWithImpendingCollision = aListFromList [
  (1, (toCoord 15 31)),
  (2, (toCoord 0 0)),
  (3, (toCoord 31 31)),
  (4, (toCoord 17 31)),
  (8, (toCoord 1 1)),
  (12, (toCoord 20 20))]

wormHealthsForOneAndFive = aListFromList [
  (1,  startingHealth),
  (2,  startingHealth),
  (3,  startingHealth),
  (4,  startingHealth),
  (8,  startingHealth),
  (12, startingHealth)]

wormPositionsWithHisNextToMine = AList
  (toCoord 15 31)
  (toCoord 1 31)
  (toCoord 1 30)
  (toCoord 16 31)
  (toCoord 19 1)
  (toCoord 20 1)

wormPositionsWithHisNextToHis = aListFromList [
  (1, (toCoord 20 31)),
  (2, (toCoord 0 0)),
  (3, (toCoord 31 31)),
  (4, (toCoord 15 31)),
  (8, (toCoord 16 31)),
  (12, (toCoord 20 1))]

wormPositionsWithOpponentNextToMedipack = aListFromList [
  (4,  (toCoord 31 30)),
  (8,  (toCoord 19 1)),
  (12, (toCoord 20 1))]

myWormsAllAtTopLeft = aListFromList [
  (1, (toCoord 0 0)),
  (2, (toCoord 0 1)),
  (3, (toCoord 0 2))]

opponentWormsAllAtTopLeft = aListFromList [
  (4,  (toCoord 0 0)),
  (8,  (toCoord 0 1)),
  (12, (toCoord 0 2))]

wormPositionsWithOpponentOnTheMedipack = aListFromList [
  (4, (toCoord 31 31)),
  (8,  (toCoord 19 1)),
  (12, (toCoord 20 1))]

wormHealthsWithOpponentHavingReceivedTheMedipack = AList
  startingHealth
  startingHealth
  20
  18
  startingHealth
  20

wormPositionsWithOpponentAtTop = AList
  (toCoord 15 31)
  (toCoord 1 31)
  (toCoord 1 30)
  (toCoord 15 0)
  (toCoord 19 1)
  (toCoord 20 1)

wormPositionsWithMyWormsNextToEachother = aListFromList [
  (1, (toCoord 15 31)),
  (2, (toCoord 16 31)),
  (3, (toCoord 10 31)),
  (4, (toCoord 31 31)),
  (8,  (toCoord 19 1)),
  (12, (toCoord 20 1))]

wormPositionsWithMyWormNextToMedipack = aListFromList [
  (1, (toCoord 30 31)),
  (2, (toCoord 1 31)),
  (3, (toCoord 1 30))]

wormPositionsWithMyWormOnTheMedipack = aListFromList [
  (1, (toCoord 31 31)),
  (2, (toCoord 1 31)),
  (3, (toCoord 1 30))]

wormHealthsWithMyWormHavingReceivedTheMedipack = AList
  18
  startingHealth
  20
  startingHealth
  startingHealth
  20

wormPositionsWithMyWormAtTop = aListFromList [
  (1, (toCoord 15 0)),
  (2, (toCoord 18 0)),
  (3, (toCoord 19 0))]

startingSelections = (Selections 3)

aPlayer = Player 300 (WormId 1) startingSelections

withLastMove :: Maybe String -> State -> State
withLastMove move' state =
  state { opponentsLastCommand = move' }

withCurrentRound :: Int -> State -> State
withCurrentRound round' state =
  state { currentRound = round' }

setOpponentsLastMoveToDummy = setOpponentsLastMove aState doNothing

-- For generating the lava map
-- euclideanDistance :: Coord -> Coord -> Double
-- euclideanDistance xy' xy'' =
--   let (x', y')   = fromCoord xy'
--       (x'', y'') = fromCoord xy''
--       dx         = (fromIntegral (x' - x''))
--       dy         = (fromIntegral (y' - y''))
--   in sqrt (((dx::Double) ** 2) + (dy ** 2))

-- generateLavaProgression =
--   map lavaOn [0..400]
--   where
--     battleRoyaleStart :: Double
--     battleRoyaleStart   = 0.25  * (fromIntegral maxRound)
--     battleRoyaleEnd :: Double
--     battleRoyaleEnd     = 0.875 * (fromIntegral maxRound)
--     centreDim :: Int
--     centreDim           = round $ (fromIntegral mapDim - 1.0) / (2.0::Double)
--     mapCentre           = toCoord centreDim centreDim
--     lavaOn currentRound' =
--       (\ (GameMap air _ _ _) -> air) $
--       vectorGameMapToGameMap $
--       V.fromList $
--       map ( \ coord -> if currentRound' >= (round battleRoyaleStart) && (euclideanDistance coord mapCentre > (safeAreaRadius + 1))
--                        then AIR
--                        else DEEP_SPACE)
--       [0..(mapDim * mapDim) - 1]
--       where
--         fullPercentageRange = ((fromIntegral currentRound') - battleRoyaleStart) /
--                               (battleRoyaleEnd - battleRoyaleStart)
--         currentProgress     = min 1.0 $ max fullPercentageRange 0.0
--         safeAreaRadius      = (fromIntegral (mapDim `div` 2)) * (1 - currentProgress)
