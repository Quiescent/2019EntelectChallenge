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
               Reward (MyReward 100)  (OpponentsReward 1500)] `shouldBe` (Payoff (MyPayoff 9) (OpponentsPayoff 9))
    it "should decrease with every move down the chain" $
      let value' :: Double
          value' = (7 + 7 / 2 + 7 / 3 + 7 / 4) / normalisationFactor
      in diffMax [Reward (MyReward 0) (OpponentsReward 7),
                  Reward (MyReward 0) (OpponentsReward 7),
                  Reward (MyReward 0) (OpponentsReward 7),
                  Reward (MyReward 0) (OpponentsReward 7)] `shouldBe`
         (Payoff (MyPayoff 1) (OpponentsPayoff $ fromJust $ findIndex (>= (round value')) diffMaxScale))
    prop "should never be 10 or 0" $ \ (x, y) ->
      let inRange' = (> 0) .&&. (< maxScore)
      in diffMax [Reward (MyReward $ abs x) (OpponentsReward $ abs y)] `shouldSatisfy`
      (inRange' . (\ (Payoff _            (OpponentsPayoff x')) -> x')) .&&.
      (inRange' . (\ (Payoff (MyPayoff x') _)                   -> x'))
  describe "updateCount" $ do
    prop "should produce the same number of records when updating a record regardless of whether it's there or not" $ \ (i, k) ->
      let myMoves      = myMovesFrom aState
          thisMove     = myMoves L.!! (i `mod` length myMoves)
          k'           = k `mod` (maxScore + 1)
          updateCount' = incInc k'
          oldCounts    = map (\ move -> (SuccessRecord (Wins 1) (Played 1) move)) myMoves
          newCounts    = updateCount updateCount' oldCounts thisMove
      in newCounts `shouldSatisfy` ((== (length oldCounts)) . length)
    prop "should change the played count of the selected record and might change the win count" $ \ (i, k) ->
      let myMoves      = myMovesFrom aState
          thisMove     = myMoves L.!! (i `mod` length myMoves)
          k'           = k `mod` (maxScore + 1)
          updateCount' = incInc k'
          oldCounts    = map (\ move -> (SuccessRecord (Wins 1) (Played 1) move)) myMoves
          newCounts    = updateCount updateCount' oldCounts thisMove
      in newCounts `shouldSatisfy` ((((== (Played $ 1 + maxScore)) . played) .&&.
                                    (((== (Wins   $ 1 + k'))       . wins))) .
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
          k'             = k `mod` (maxScore + 1)
          oldTree        = UnSearchedLevel
                           (MyMoves        $
                            (SuccessRecord (Wins 0) (Played 0) $ L.head myMoves) :
                            (map (\ move -> (SuccessRecord (Wins 1) (Played 1) move)) $ L.tail myMoves))
                           (OpponentsMoves $
                            (SuccessRecord (Wins 0) (Played 0) $ L.head opponentsMoves) :
                            (map (\ move -> (SuccessRecord (Wins 1) (Played 1) move)) $ L.tail opponentsMoves))
          newTree        = updateTree aState
                                      (SearchResult
                                       (Payoff (MyPayoff $ abs k') (OpponentsPayoff $ maxScore - abs k'))
                                       [fromMoves thisMove thatMove])
                                      oldTree
      in ((thisMove, thatMove), newTree) `shouldSatisfy` isSearched . snd
    prop "should produce a tree with one result on it when given a SearchFront" $ \ (i, j, k) ->
      let myMoves        = myMovesFrom aState
          thisMove       = myMoves L.!! (i `mod` length myMoves)
          opponentsMoves = opponentsMovesFrom aState
          thatMove       = opponentsMoves L.!! (j `mod` length opponentsMoves)
          k'             = k `mod` (maxScore + 1)
          newTree        = updateTree aState
                                      (SearchResult
                                       (Payoff (MyPayoff $ abs k') (OpponentsPayoff $ maxScore - abs k'))
                                       [fromMoves thisMove thatMove])
                                      SearchFront
      in newTree `shouldSatisfy` (((== (maxScore - k')) . countOpponentsWins) .&&.
                                  ((== k')              . countMyWins)        .&&.
                                  ((== maxScore)        . countGames'))
    prop "should increment the game count when given an UnSearchedLevel" $ \ (i, j, k) ->
      let myMoves        = myMovesFrom aState
          thisMove       = myMoves L.!! (i `mod` length myMoves)
          opponentsMoves = opponentsMovesFrom aState
          thatMove       = opponentsMoves L.!! (j `mod` length opponentsMoves)
          k'             = k `mod` (maxScore + 1)
          oldTree        = UnSearchedLevel
                           (MyMoves        $ map (\ move -> (SuccessRecord (Wins 1) (Played 1) move)) myMoves)
                           (OpponentsMoves $ map (\ move -> (SuccessRecord (Wins 1) (Played 1) move)) opponentsMoves)
          newTree        = updateTree aState
                                      (SearchResult
                                       (Payoff (MyPayoff k') (OpponentsPayoff $ maxScore - k'))
                                       [fromMoves thisMove thatMove])
                                      oldTree
      in newTree `shouldSatisfy` (((== (maxScore        + countGames' oldTree))        . countGames')         .&&.
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
          newTree        = updateTree aState
                                      (SearchResult
                                       (Payoff (MyPayoff k') (OpponentsPayoff $ maxScore - k'))
                                       [fromMoves thisMove thatMove])
                                      oldTree
      in newTree `shouldSatisfy` (((== (maxScore        + countGames' oldTree))        . countGames')         .&&.
                                  ((== ((maxScore - k') + countOpponentsWins oldTree)) . countOpponentsWins) .&&.
                                  ((== (k'              + countMyWins        oldTree)) . countMyWins))
    prop "should add an unsearched level to the state transitions for this tree" $ \ (i, j, k) ->
      let myMoves        = myMovesFrom aState
          thisMove       = myMoves L.!! (i `mod` length myMoves)
          opponentsMoves = opponentsMovesFrom aState
          thatMove       = opponentsMoves L.!! (j `mod` length opponentsMoves)
          k'             = k `mod` (maxScore + 1)
          oldTree        = SearchedLevel
                           (MyMoves        $ map (\ move -> (SuccessRecord (Wins 1) (Played 1) move)) myMoves)
                           (OpponentsMoves $ map (\ move -> (SuccessRecord (Wins 1) (Played 1) move)) opponentsMoves)
                           []
          newTree        = updateTree aState
                                      (SearchResult
                                       (Payoff (MyPayoff k') (OpponentsPayoff $ maxScore - k'))
                                       [fromMoves thisMove thatMove, fromMoves thisMove thatMove])
                                      oldTree
      in ((thisMove, thatMove), newTree) `shouldSatisfy`
         (((== 1) . length . transitions) .&&.
          ((((== (Wins $ 1 +  k'))         . wins) .&&.
            ((== (Played $ 1 +  maxScore)) . played)) .
           (fromJust . find ((== thisMove) . successRecordMove) . myMovesFromTree)) .&&.
          ((((== (Wins $ 1 + maxScore - k')) . wins) .&&.
            ((== (Played $ 1 + maxScore))   . played)) .
           (fromJust . find ((== thatMove) . successRecordMove) . opponentsMovesFromTree))) . snd
    prop "should add an unsearched level to the state transitions for this tree" $ \ (i, j, k, l, m) ->
      let myMoves        = myMovesFrom aState
          thisMove       = myMoves L.!! (i `mod` length myMoves)
          thisMove'      = myMoves L.!! (m `mod` length myMoves)
          opponentsMoves = opponentsMovesFrom aState
          thatMove       = opponentsMoves L.!! (j `mod` length opponentsMoves)
          thatMove'      = opponentsMoves L.!! (l `mod` length opponentsMoves)
          k'             = k `mod` (maxScore + 1)
          oldTree        = SearchedLevel
                           (MyMoves        $ map (\ move -> (SuccessRecord (Wins 1) (Played 1) move)) myMoves)
                           (OpponentsMoves $ map (\ move -> (SuccessRecord (Wins 1) (Played 1) move)) opponentsMoves)
                           []
          newTree        = updateTree aState
                                      (SearchResult
                                        (Payoff (MyPayoff $ abs k') (OpponentsPayoff $ maxScore - abs k'))
                                        [fromMoves thisMove thatMove,
                                         fromMoves thisMove' thatMove'])
                                      oldTree
      in ((thisMove', thatMove'), newTree) `shouldSatisfy`
         (((((== (Wins k'))         . wins) .&&.
            ((== (Played maxScore)) . played)) .
           (fromJust . find ((== thisMove') . successRecordMove) . myMovesFromTree)) .&&.
          ((((== (Wins $ maxScore - k')) . wins) .&&.
            ((== (Played maxScore))      . played)) .
           (fromJust . find ((== thatMove') . successRecordMove) . opponentsMovesFromTree))) .
         makeMoveInTree (fromMoves thisMove thatMove) . snd
  describe "formatMove" $ do
    prop "should produce the correct type of move for the correct range" $ \ (x, y) ->
      let x'            = abs x `mod` 108
          y'            = shiftL (abs y `mod` 4) 7
          move'         = x' .|. y'
          formattedMove = formatMove (Move move') (toCoord 6 6) aState
      in if move' < 8
         then formattedMove `shouldStartWith` "shoot"
         else if move' < 16
              then formattedMove `shouldStartWith` "move"
              else if move' < 24
                   then formattedMove `shouldStartWith` "dig"
                   else if move' < 107
                        then formattedMove `shouldStartWith` "banana"
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
    it "N  (not on boundry)" $ displaceCoordByMove (toCoord 1 1) (Move 8)  `shouldBe` Just (toCoord 1 0)
    it "NE (not on boundry)" $ displaceCoordByMove (toCoord 1 1) (Move 9)  `shouldBe` Just (toCoord 2 0)
    it "E  (not on boundry)" $ displaceCoordByMove (toCoord 1 1) (Move 10) `shouldBe` Just (toCoord 2 1)
    it "SE (not on boundry)" $ displaceCoordByMove (toCoord 1 1) (Move 11) `shouldBe` Just (toCoord 2 2)
    it "S  (not on boundry)" $ displaceCoordByMove (toCoord 1 1) (Move 12) `shouldBe` Just (toCoord 1 2)
    it "SW (not on boundry)" $ displaceCoordByMove (toCoord 1 1) (Move 13) `shouldBe` Just (toCoord 0 2)
    it "W  (not on boundry)" $ displaceCoordByMove (toCoord 1 1) (Move 14) `shouldBe` Just (toCoord 0 1)
    it "NW (not on boundry)" $ displaceCoordByMove (toCoord 1 1) (Move 15) `shouldBe` Just (toCoord 0 0)
    prop "Move back and forth" $ \ (i, j, k) ->
      let x'          = 1 + (j `mod` (mapDim - 2))
          y'          = 1 + (k `mod` (mapDim - 2))
          coordInMap  = toCoord x' y'
          indexOfMove = ((abs i) `mod` 8)
          randomMove  = Move $ indexOfMove + 8
          moveBack    = Move $ ((indexOfMove + 4) `mod` 8) + 8
      in ((displaceCoordByMove coordInMap randomMove) >>= (\ newCoord -> displaceCoordByMove newCoord moveBack)) `shouldBe` Just coordInMap
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
       aState { wormHealths = removeWormById (WormId 1) $ wormHealths aState,
                wormPositions = removeWormById (WormId 1) $ wormPositions aState }
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
      makeMove True (fromMoves doNothing doNothing) aState `shouldBe` selectNextWormsDefault aState
    it "moving my worm to dirt should dig out that dirt" $
      makeMove True (fromMoves digNorth doNothing) aState `shouldBe`
      (selectNextWormsDefault $ awardPointsToThisPlayerForDigging aStateWithDirtMissingAboveMyWorm)
    it "moving opponents worm to space should not move the worm" $
      makeMove True (fromMoves doNothing moveNorth) aState `shouldBe`
      (setOpponentsLastMove aState moveNorth $
       selectNextWormsDefault         $
       penaliseThatPlayerForAnInvalidCommand aState)
    it "moving my worm into space should not move the worm" $
      makeMove True (fromMoves moveSouth doNothing) aState `shouldBe`
      (selectNextWormsDefault $ penaliseThisPlayerForAnInvalidCommand aState)
    it "moving opponents worm into dirt should dig out the dirt" $
      makeMove True (fromMoves doNothing digSouth) aState `shouldBe`
      (setOpponentsLastMove aState digSouth     $
       selectNextWormsDefault            $
       awardPointsToThatPlayerForDigging $
       removeDirtFromMapAt (toCoord 16 2) aState)
    it "moving my worm into air should move the worm to that spot" $
      makeMove True (fromMoves moveEast doNothing) aState `shouldBe`
      (selectNextWormsDefault $ awardPointsToThisPlayerForMovingToAir $ moveThisWorm (toCoord 16 31) aState)
    it "moving opponents worm into air should move the worm to that spot" $
      makeMove True (fromMoves doNothing moveEast) aState `shouldBe`
      (setOpponentsLastMove aState moveEast         $
       selectNextWormsDefault                $
       awardPointsToThatPlayerForMovingToAir $
       moveThatWorm (toCoord 17 1) aState)
    it "moving to the same square should swap the worms if true and damage both worms" $
      makeMove True (fromMoves moveEast moveWest) aStateWithImpendingCollision `shouldBe`
      (setOpponentsLastMove aStateWithImpendingCollision moveWest         $
       selectNextWormsDefault                         $
       withWormPositions (removeWormById (WormId 1))  $
       withWormPositions (removeWormById (WormId 4))  $
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
       selectNextWormsDefault                $
       awardPointsToThatPlayerForMovingToAir $
       awardPointsToThisPlayerForMovingToAir $
       harmWorm (WormId (-1)) aStateWithImpendingCollision knockBackDamageAmount id id id (toCoord 17 31) $
       harmWorm (WormId (-1)) aStateWithImpendingCollision knockBackDamageAmount id id id (toCoord 15 31)
       aStateWithImpendingCollision)
    it "moving my worm to a square occupied by one of my worms does nothing" $
      makeMove True (fromMoves moveEast doNothing) aStateWithMyWormsNextToEachOther `shouldBe`
      (selectNextWormsDefault $ penaliseThisPlayerForAnInvalidCommand aStateWithMyWormsNextToEachOther)
    it "moving my worm to a square occupied by one of the the opponents worms does nothing " $
      makeMove True (fromMoves moveEast doNothing) aStateWithMyWormNextToAnEnemy `shouldBe`
      (selectNextWormsDefault $ penaliseThisPlayerForAnInvalidCommand aStateWithMyWormNextToAnEnemy)
    it "moving an opponents worm to a square occupied by one of my worms does nothing" $
      makeMove True (fromMoves doNothing moveWest) aStateWithMyWormNextToAnEnemy `shouldBe`
      (setOpponentsLastMove aStateWithMyWormNextToAnEnemy moveWest $
       selectNextWormsDefault        $
       penaliseThatPlayerForAnInvalidCommand aStateWithMyWormNextToAnEnemy)
    it "moving an opponents worm to a square occupied by one of the opponents worms does nothing" $
      makeMove True (fromMoves doNothing moveEast) aStateWithEnemyWormsNextToEachother `shouldBe`
      (setOpponentsLastMove aStateWithEnemyWormsNextToEachother moveEast $
       selectNextWormsDefault        $
       penaliseThatPlayerForAnInvalidCommand aStateWithEnemyWormsNextToEachother)
    it "moving my worm onto the medipack increases my worms health by 10 and changes that square to AIR" $
      makeMove True (fromMoves moveEast doNothing) aStateWithMyWormNextToTheMedipack `shouldBe`
      (selectNextWormsDefault $
       awardPointsToThisPlayerForMovingToAir aStateWithMyWormOnTheMedipack)
    it "moving the opponents worm onto the medipack should increase its health by ten and change that square to AIR" $
      makeMove True (fromMoves doNothing moveSouth) aStateWithOpponentsWormNextToTheMedipack `shouldBe`
      (setOpponentsLastMove aStateWithOpponentsWormNextToTheMedipack moveSouth $
       selectNextWormsDefault         $
       awardPointsToThatPlayerForMovingToAir aStateWithOpponentsWormOnTheMedipack)
    it "moving both worms onto the same medipack results in a swap when the bit is set" $
      makeMove True (fromMoves moveEast moveSouth) aStateWithBothWormsNextToTheMedipack `shouldBe`
      (setOpponentsLastMove aStateWithBothWormsNextToTheMedipack moveSouth        $
       selectNextWormsDefault                $
       awardPointsToThatPlayerForMovingToAir $
       awardPointsToThisPlayerForMovingToAir aStateWhereWeSwappedOverTheMedipack)
    it "moving both worms onto the same medipack results no swap when the bit is set" $
      makeMove False (fromMoves moveEast moveSouth) aStateWithBothWormsNextToTheMedipack `shouldBe`
      (setOpponentsLastMove aStateWithBothWormsNextToTheMedipack moveSouth        $
       selectNextWormsDefault                $
       knockBackDamage                       $
       awardPointsToThatPlayerForMovingToAir $
       awardPointsToThisPlayerForMovingToAir aStateWhereNoSwapHappened)
    -- Top
    it "moving my worm off the top edge of the map changes nothing" $
      makeMove True (fromMoves moveNorth doNothing) aStateWithMyWormOnTop `shouldBe`
      (selectNextWormsDefault $ penaliseThisPlayerForAnInvalidCommand aStateWithMyWormOnTop)
    it "moving opponent worm off the top edge of the map changes nothing" $
      makeMove True (fromMoves doNothing moveNorth) aStateWithOpponentWormOnTop `shouldBe`
      (setOpponentsLastMove aStateWithOpponentWormOnTop moveNorth $
       selectNextWormsDefault         $
       penaliseThatPlayerForAnInvalidCommand aStateWithOpponentWormOnTop)
    it "moving my worm on the top to the east results in the worm moving east" $
      makeMove True (fromMoves moveEast doNothing) aStateWithMyWormOnTop `shouldBe`
      (selectNextWormsDefault $ awardPointsToThisPlayerForMovingToAir aStateWithMyWormOnTopMovedRight)
    it "moving opponent worm on the top to the east results in the worm moving east" $
      makeMove True (fromMoves doNothing moveEast) aStateWithOpponentWormOnTop `shouldBe`
      (setOpponentsLastMove aStateWithOpponentWormOnTop moveEast $
       selectNextWormsDefault        $
       awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormOnTopMovedRight)
    it "moving my worm on the top to the west results in the worm moving west" $
      makeMove True (fromMoves moveWest doNothing) aStateWithMyWormOnTop `shouldBe`
      (selectNextWormsDefault $ awardPointsToThisPlayerForMovingToAir aStateWithMyWormOnTopMovedLeft)
    it "moving opponent worm on the top to the west results in the worm moving west" $
      makeMove True (fromMoves doNothing moveWest) aStateWithOpponentWormOnTop `shouldBe`
      (setOpponentsLastMove aStateWithOpponentWormOnTop moveWest $
       selectNextWormsDefault        $
       awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormOnTopMovedLeft)
    it "moving my worm south from the top of the map results in that worm moving down" $
      makeMove True (fromMoves moveSouth doNothing) aStateWithMyWormOnTop `shouldBe`
      (selectNextWormsDefault $ awardPointsToThisPlayerForMovingToAir aStateWithMyWormOnTopMovedDown)
    it "moving opponent worm south from the top of the map results in that worm moving down" $
      makeMove True (fromMoves doNothing moveSouth) aStateWithOpponentWormOnTop `shouldBe`
      (setOpponentsLastMove aStateWithOpponentWormOnTop moveSouth $
       selectNextWormsDefault         $
       awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormOnTopMovedDown)
    -- Left edge
    it "moving my worm north on the left edge of the map moves that worm north" $
      makeMove True (fromMoves moveNorth doNothing) aStateWithMyWormOnLeftEdge `shouldBe`
      (selectNextWormsDefault $ awardPointsToThisPlayerForMovingToAir aStateWithMyWormUpwardsOnLeftEdge)
    it "moving opponent worm north on the left edge of the map moves that worm north" $
      makeMove True (fromMoves doNothing moveNorth) aStateWithOpponentWormOnLeftEdge `shouldBe`
      (setOpponentsLastMove aStateWithOpponentWormOnLeftEdge moveNorth $
       selectNextWormsDefault         $
       awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormUpwardOnLeftEdge)
    it "moving my worm south on the left edge of the map moves that worm south" $
      makeMove True (fromMoves moveSouth doNothing) aStateWithMyWormOnLeftEdge `shouldBe`
      (selectNextWormsDefault $ awardPointsToThisPlayerForMovingToAir aStateWithMyWormDownwardOnLeftEdge)
    it "moving opponent worm south on the left edge of the map moves that worm south" $
      makeMove True (fromMoves doNothing moveSouth) aStateWithOpponentWormOnLeftEdge `shouldBe`
      (setOpponentsLastMove aStateWithOpponentWormOnLeftEdge moveSouth $
       selectNextWormsDefault         $
       awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormDownwardOnLeftEdge)
    it "moving my worm east on the left edge of the map moves that worm east" $
      makeMove True (fromMoves moveEast doNothing) aStateWithMyWormOnLeftEdge `shouldBe`
      (selectNextWormsDefault $ awardPointsToThisPlayerForMovingToAir aStateWithMyWormRightFromLeftEdge)
    it "moving opponent worm east on the left edge of the map moves that worm east" $
      makeMove True (fromMoves doNothing moveEast) aStateWithOpponentWormOnLeftEdge `shouldBe`
      (setOpponentsLastMove aStateWithOpponentWormOnLeftEdge moveEast $
       selectNextWormsDefault        $
       awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormRightFromLeftEdge)
    it "moving my worm off the edge on the left of the map changes nothing" $
      makeMove True (fromMoves moveWest doNothing) aStateWithMyWormOnLeftEdge `shouldBe`
      (selectNextWormsDefault $ penaliseThisPlayerForAnInvalidCommand aStateWithMyWormOnLeftEdge)
    it "moving opponent worm off the edge on left of the map changes nothing" $
      makeMove True (fromMoves doNothing moveWest) aStateWithOpponentWormOnLeftEdge `shouldBe`
      (setOpponentsLastMove aStateWithOpponentWormOnLeftEdge moveWest $
       selectNextWormsDefault        $
       penaliseThatPlayerForAnInvalidCommand aStateWithOpponentWormOnLeftEdge)
    -- Bottom edge
    it "moving my worm south from the bottom edge results in no change" $
      makeMove True (fromMoves moveSouth doNothing) aStateWithMyWormOnTheBottomEdge `shouldBe`
      (selectNextWormsDefault $ penaliseThisPlayerForAnInvalidCommand aStateWithMyWormOnTheBottomEdge)
    it "moving opponent worm south from the bottom edge results in no change" $
      makeMove True (fromMoves doNothing moveSouth) aStateWithOpponentWormOnTheBottomEdge `shouldBe`
      (setOpponentsLastMove aStateWithOpponentWormOnTheBottomEdge moveSouth $
       selectNextWormsDefault         $
       penaliseThatPlayerForAnInvalidCommand aStateWithOpponentWormOnTheBottomEdge)
    it "moving my worm to the east from the bottom edge results in that worm moving right" $
      makeMove True (fromMoves moveEast doNothing) aStateWithMyWormOnTheBottomEdge `shouldBe`
      (selectNextWormsDefault $ awardPointsToThisPlayerForMovingToAir aStateWithMyWormOnTheBottomEdgeMovedRight)
    it "moving opponent worm to the east from the bottom edge results in that worm moving right" $
      makeMove True (fromMoves doNothing moveEast) aStateWithOpponentWormOnTheBottomEdge `shouldBe`
      (setOpponentsLastMove aStateWithOpponentWormOnTheBottomEdge moveEast $
       selectNextWormsDefault        $
       awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormOnTheBottomEdgeMovedRight)
    it "moving my worm to the west from the bottom edge results in that worm moving left" $
      makeMove True (fromMoves moveWest doNothing) aStateWithMyWormOnTheBottomEdge `shouldBe`
      (selectNextWormsDefault $ awardPointsToThisPlayerForMovingToAir aStateWithMyWormOnTheBottomEdgeMovedLeft)
    it "moving opponent to the west from the bottom edge results in that worm moving left" $
      makeMove True (fromMoves doNothing moveWest) aStateWithOpponentWormOnTheBottomEdge `shouldBe`
      (setOpponentsLastMove aStateWithOpponentWormOnTheBottomEdge moveWest $
       selectNextWormsDefault        $
       awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormOnTheBottomEdgeMovedLeft)
    it "moving my worm to the north from the bottom edge results in that worm moving up" $
      makeMove True (fromMoves moveNorth doNothing) aStateWithMyWormOnTheBottomEdge `shouldBe`
      (selectNextWormsDefault $ awardPointsToThisPlayerForMovingToAir aStateWithMyWormUpFromTheBottomEdge)
    it "moving opponent worm to the north from the bottom edge results in that worm moving up" $
      makeMove True (fromMoves doNothing moveNorth) aStateWithOpponentWormOnTheBottomEdge `shouldBe`
      (setOpponentsLastMove aStateWithOpponentWormOnTheBottomEdge moveNorth $
       selectNextWormsDefault         $
       awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormUpFromTheBottomEdge)
    -- Right edge
    it "moving my worm east from the right edge results in no change" $
      makeMove True (fromMoves moveEast doNothing) aStateWithMyWormOnTheRightEdge `shouldBe`
      (selectNextWormsDefault $ penaliseThisPlayerForAnInvalidCommand aStateWithMyWormOnTheRightEdge)
    it "moving opponent worm east from the right edge results in no change" $
      makeMove True (fromMoves doNothing moveEast) aStateWithOpponentWormOnTheRightEdge `shouldBe`
      (setOpponentsLastMove aStateWithOpponentWormOnTheRightEdge moveEast $
       selectNextWormsDefault        $
       penaliseThatPlayerForAnInvalidCommand aStateWithOpponentWormOnTheRightEdge)
    it "moving my worm north from the right edge results in that worm moving up" $
      makeMove True (fromMoves moveNorth doNothing) aStateWithMyWormOnTheRightEdge `shouldBe`
      (selectNextWormsDefault $ awardPointsToThisPlayerForMovingToAir aStateWithMyWormOnTheRightEdgeMovedUp)
    it "moving opponent worm north from the right edge results in that worm moving up" $
      makeMove True (fromMoves doNothing moveNorth) aStateWithOpponentWormOnTheRightEdge `shouldBe`
      (setOpponentsLastMove aStateWithOpponentWormOnTheRightEdge moveNorth $
       selectNextWormsDefault         $
       awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormOnTheRightEdgeMovedUp)
    it "moving my worm south from the right edge results in that worm moving down" $
      makeMove True (fromMoves moveSouth doNothing) aStateWithMyWormOnTheRightEdge `shouldBe`
      (selectNextWormsDefault $ awardPointsToThisPlayerForMovingToAir aStateWithMyWormOnTheRightEdgeMovedDown)
    it "moving opponent worm south from the right edge results in that worm moving down" $
      makeMove True (fromMoves doNothing moveSouth) aStateWithOpponentWormOnTheRightEdge `shouldBe`
      (setOpponentsLastMove aStateWithOpponentWormOnTheRightEdge moveSouth $
       selectNextWormsDefault         $
       awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormOnTheRightEdgeMovedDown)
    it "moving my worm to the west from the right edge results in that worm moving left" $
      makeMove True (fromMoves moveWest doNothing) aStateWithMyWormOnTheRightEdge `shouldBe`
      (selectNextWormsDefault $ awardPointsToThisPlayerForMovingToAir aStateWithMyWormMovedLeftFromTheRightEdge)
    it "moving opponent worm to the west from the right edge results in that worm moving left" $
      makeMove True (fromMoves doNothing moveWest) aStateWithOpponentWormOnTheRightEdge `shouldBe`
      (setOpponentsLastMove aStateWithOpponentWormOnTheRightEdge moveWest $
       selectNextWormsDefault        $
       awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormMovedLeftFromTheRightEdge)
    -- Digging
    it "should remove dirt when my player digs a dirt block" $
      makeMove True (fromMoves digNorth doNothing) aState `shouldBe`
      (selectNextWormsDefault $ awardPointsToThisPlayerForDigging aStateWithDirtMissingAboveMyWorm)
    it "should remove dirt when opponent digs a dirt block" $
      makeMove True (fromMoves doNothing digNorth) aStateWithOpponentBeneathDirt `shouldBe`
      (setOpponentsLastMove aStateWithOpponentBeneathDirt digNorth $
       selectNextWormsDefault        $
       awardPointsToThatPlayerForDigging aStateWithDirtMissingAboveOpponentWorm)
    it "should penalise my player when I dig air" $
      makeMove True (fromMoves digEast doNothing) aState `shouldBe`
      (selectNextWormsDefault $ penaliseThisPlayerForAnInvalidCommand aState)
    it "should penalise my opponent when he digs air" $
      makeMove True (fromMoves doNothing digEast) aState `shouldBe`
      (setOpponentsLastMove aState digEast $
       selectNextWormsDefault       $
       penaliseThatPlayerForAnInvalidCommand aState)
    it "moving next to dirt should not dig out that dirt when it would be in our way if we continued going that way" $
      makeMove True (fromMoves doNothing moveEast) aStateWithOpponentNearDirtToTheEast `shouldBe`
      (setOpponentsLastMove aStateWithOpponentNearDirtToTheEast moveEast         $ 
       selectNextWormsDefault                $
       awardPointsToThatPlayerForMovingToAir $
       moveThatWorm (toCoord 10 2) aStateWithOpponentNearDirtToTheEast)
    it "should reward both players and remove dirt when both worms dig the same dirt block" $
      makeMove True (fromMoves digSouthEast digSouth) aStateWithBothWormsNearTheSameDirtBlock `shouldBe`
      (setOpponentsLastMove aStateWithBothWormsNearTheSameDirtBlock digSouth     $
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
      it "should cause maximum damage to the worm which it lands on" $
        makeMove False (fromMoves bananaOneToRight doNothing) aStateWithOposingWormsNextToEachother `shouldBe`
        (selectNextWormsDefault $
         harmWorm (WormId 1) aStateWithOposingWormsNextToEachother 20 id id id (toCoord 16 31) $
         harmWorm (WormId 1) aStateWithOposingWormsNextToEachother 13 id id id (toCoord 15 31) $
         -- Decrement banana bombs
         withWormBananas (always $ aListFromList [(1, 2), (4, 3)]) $
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
      let stateWithEnemyOneSquareFromEpicentre = moveThatWorm (toCoord 17 31) aStateWithOposingWormsNextToEachother
      it "should cause damage to the worms in the blast radius" $
        makeMove False (fromMoves bananaOneToRight doNothing) stateWithEnemyOneSquareFromEpicentre `shouldBe`
        (selectNextWormsDefault $
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
        (selectNextWormsDefault $
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
      let stateWithEnemyThreeSquaresFromEpicentre = moveThatWorm (toCoord 19 31) aStateWithOposingWormsNextToEachother
      it "should not cause damage to the worms outside of the blast radius" $
        makeMove False (fromMoves bananaOneToRight doNothing) stateWithEnemyThreeSquaresFromEpicentre `shouldBe`
        (selectNextWormsDefault $
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
        (selectNextWormsDefault $
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
        (selectNextWormsDefault $
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
        (selectNextWormsDefault $ aState)
    context "when the opponent is throwing the bomb" $ do
      it "should cause maximum damage to the worm which it lands on" $
        makeMove False (fromMoves doNothing bananaOneToLeft) aStateWithOposingWormsNextToEachother `shouldBe`
        (setOpponentsLastMove aStateWithOposingWormsNextToEachother bananaOneToLeft $
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
      let stateWithEnemyOneSquareFromEpicentre = moveThisWorm (toCoord 14 31) aStateWithOposingWormsNextToEachother
      it "should cause damage to the worms in the blast radius" $
        makeMove False (fromMoves doNothing bananaOneToLeft) stateWithEnemyOneSquareFromEpicentre `shouldBe`
        (setOpponentsLastMove stateWithEnemyOneSquareFromEpicentre bananaOneToLeft $
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
      let stateWithEnemyThreeSquaresFromEpicentre = moveThisWorm (toCoord 12 31) aStateWithOposingWormsNextToEachother
      it "should not cause damage to the worms outside of the blast radius" $
        makeMove False (fromMoves doNothing bananaOneToLeft) stateWithEnemyThreeSquaresFromEpicentre `shouldBe`
        (setOpponentsLastMove stateWithEnemyThreeSquaresFromEpicentre bananaOneToLeft $
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
         selectNextWormsDefault aState)
    context "when both the opponent and I throw the bomb" $ do
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
         selectNextWormsDefault $
         harmWorm (WormId 1) aStateWithLowHealthOposingWormsNextToEachother 20 id id id (toCoord 16 31) $
         harmWorm (WormId 1) aStateWithLowHealthOposingWormsNextToEachother 13 id id id (toCoord 15 31) $
         -- Decrement banana bombs
         withWormBananas (always $ AList (-1) (-1) (-1) (-1) (-1) (-1)) $
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
    prop "should hit this players first horizontal target in range when it's an opponent worm" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithNonDiagonalPadding
                                                  inBoundsWithNoPadding)
                          (generateCoordDisplacer nonDiagonalDelta addDelta ignoreDelta)
                          (generateShotSwitch     shootEast shootWest)
                          (takeBothWorms          (WormId 1) (WormId 4))
                          (i, j, k)
      in makeMove True (fromMoves shot doNothing) state `shouldBe`
         (selectNextWormsDefault $
          awardPointsToThisPlayerForKillingAnEnemy $
          awardPointsToThisPlayerForHittingAnEnemy $
          state { wormHealths = removeWormById (WormId 4) $ wormHealths state,
                  wormPositions = removeWormById (WormId 4) $ wormPositions state })
    prop "should hit this players first horizontal target in range when it's a friendly worm" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithNonDiagonalPadding
                                                  inBoundsWithNoPadding)
                          (generateCoordDisplacer nonDiagonalDelta addDelta ignoreDelta)
                          (generateShotSwitch     shootEast shootWest)
                          (takeBothWorms          (WormId 1) (WormId 2))
                          (i, j, k)
      in makeMove True (fromMoves shot doNothing) state `shouldBe`
         (selectNextWorms (WormId 3) (WormId 8) $
          penaliseThisPlayerForHittingHisFriendlyWorm $
          state { wormHealths = removeWormById (WormId 2) $ wormHealths state,
                  wormPositions = removeWormById (WormId 2) $ wormPositions state })
    prop "should hit this players first vertical target in range when it's an opponent worm" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithNoPadding
                                                  inBoundsWithNonDiagonalPadding)
                          (generateCoordDisplacer nonDiagonalDelta ignoreDelta addDelta)
                          (generateShotSwitch     shootSouth shootNorth)
                          (takeBothWorms          (WormId 1) (WormId 4))
                          (i, j, k)
      in makeMove True (fromMoves shot doNothing) state `shouldBe`
         (selectNextWormsDefault $
          awardPointsToThisPlayerForKillingAnEnemy $
          awardPointsToThisPlayerForHittingAnEnemy $
          state { wormHealths = removeWormById (WormId 4) $ wormHealths state,
                  wormPositions = removeWormById (WormId 4) $ wormPositions state })
    prop "should hit this players first vertical target in range when it's a friendly worm" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithNoPadding
                                                  inBoundsWithNonDiagonalPadding)
                          (generateCoordDisplacer nonDiagonalDelta ignoreDelta addDelta)
                          (generateShotSwitch     shootSouth shootNorth)
                          (takeBothWorms          (WormId 1) (WormId 2))
                          (i, j, k)
      in makeMove True (fromMoves shot doNothing) state `shouldBe`
         (selectNextWorms (WormId 3) (WormId 8) $
          penaliseThisPlayerForHittingHisFriendlyWorm $
          state { wormHealths = removeWormById (WormId 2) $ wormHealths state,
                  wormPositions = removeWormById (WormId 2) $ wormPositions state })
    prop "should hit this players first NW-SE diagonal target in range when it's an opponent worm" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithDiagonalPadding
                                                  inBoundsWithDiagonalPadding)
                          (generateCoordDisplacer diagonalDelta addDelta addDelta)
                          (generateShotSwitch     shootSouthEast shootNorthWest)
                          (takeBothWorms          (WormId 1) (WormId 4))
                          (i, j, k)
      in makeMove True (fromMoves shot doNothing) state `shouldBe`
         (selectNextWormsDefault $
          awardPointsToThisPlayerForKillingAnEnemy $
          awardPointsToThisPlayerForHittingAnEnemy $
          state { wormHealths = removeWormById (WormId 4) $ wormHealths state,
                  wormPositions = removeWormById (WormId 4) $ wormPositions state })
    prop "should hit this players first NW-SE diagonal target in range when it's a friendly worm" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithDiagonalPadding
                                                  inBoundsWithDiagonalPadding)
                          (generateCoordDisplacer diagonalDelta addDelta addDelta)
                          (generateShotSwitch     shootSouthEast shootNorthWest)
                          (takeBothWorms          (WormId 1) (WormId 2))
                          (i, j, k)
      in makeMove True (fromMoves shot doNothing) state `shouldBe`
         (selectNextWorms (WormId 3) (WormId 8) $
          penaliseThisPlayerForHittingHisFriendlyWorm $
          state { wormHealths = removeWormById (WormId 2) $ wormHealths state,
                  wormPositions = removeWormById (WormId 2) $ wormPositions state })
    prop "should hit this players first NE-SW diagonal target in range when it's an opponent worm" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithDiagonalPadding
                                                  inBoundsWithDiagonalPadding)
                          (generateCoordDisplacer diagonalDelta addDelta subtractDelta)
                          (generateShotSwitch     shootNorthEast shootSouthWest)
                          (takeBothWorms          (WormId 1) (WormId 4))
                          (i, j, k)
      in makeMove True (fromMoves shot doNothing) state `shouldBe`
         (selectNextWormsDefault $
          awardPointsToThisPlayerForKillingAnEnemy $
          awardPointsToThisPlayerForHittingAnEnemy $
          state { wormHealths = removeWormById (WormId 4) $ wormHealths state,
                  wormPositions = removeWormById (WormId 4) $ wormPositions state })
    prop "should hit that players first horizontal target in range when it's my worm" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithNonDiagonalPadding
                                                  inBoundsWithNoPadding)
                          (generateCoordDisplacer nonDiagonalDelta addDelta ignoreDelta)
                          (generateShotSwitch     shootWest shootEast)
                          (takeBothWorms          (WormId 1) (WormId 4))
                          (i, j, k)
      in makeMove True (fromMoves doNothing shot) state `shouldBe`
         (setOpponentsLastMove state shot $
          selectNextWormsDefault $
          awardPointsToThatPlayerForKillingAnEnemy $
          awardPointsToThatPlayerForHittingAnEnemy $
          state { wormHealths = removeWormById (WormId 1) $ wormHealths state,
                  wormPositions = removeWormById (WormId 1) $ wormPositions state })
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
          selectNextWorms (WormId 2) (WormId 12) $
          penaliseThatPlayerForHittingHisFriendlyWorm $
          state { wormHealths = removeWormById (WormId 8) $ wormHealths state,
                  wormPositions = removeWormById (WormId 8) $ wormPositions state })
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
          selectNextWormsDefault $
          awardPointsToThatPlayerForKillingAnEnemy $
          awardPointsToThatPlayerForHittingAnEnemy $
          state { wormHealths = removeWormById (WormId 1) $ wormHealths state,
                  wormPositions = removeWormById (WormId 1) $ wormPositions state })
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
          selectNextWorms (WormId 2) (WormId 12) $
          penaliseThatPlayerForHittingHisFriendlyWorm $
          state { wormHealths = removeWormById (WormId 8) $ wormHealths state,
                  wormPositions = removeWormById (WormId 8) $ wormPositions state })
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
          selectNextWormsDefault $
          awardPointsToThatPlayerForKillingAnEnemy $
          awardPointsToThatPlayerForHittingAnEnemy $
          state { wormHealths = removeWormById (WormId 1) $ wormHealths state,
                  wormPositions = removeWormById (WormId 1) $ wormPositions state })
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
          selectNextWorms (WormId 2) (WormId 12) $
          penaliseThatPlayerForHittingHisFriendlyWorm $
          state { wormHealths = removeWormById (WormId 8) $ wormHealths state,
                  wormPositions = removeWormById (WormId 8) $ wormPositions state })
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
          selectNextWormsDefault $
          awardPointsToThatPlayerForKillingAnEnemy $
          awardPointsToThatPlayerForHittingAnEnemy $
          state { wormHealths = removeWormById (WormId 1) $ wormHealths state,
                  wormPositions = removeWormById (WormId 1) $ wormPositions state })
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
         (selectNextWormsDefault $
          awardPointsToThisPlayerForMissing state)
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
         (setOpponentsLastMove state shot $
          selectNextWormsDefault $
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
         (setOpponentsLastMove state shot $
          selectNextWormsDefault $
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
          selectNextWormsDefault $
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
         (setOpponentsLastMove state shot $
          selectNextWormsDefault $
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
           (setOpponentsLastMove state (oppositeShot shot) $
            selectNextWormsDefault $
            awardPointsToThatPlayerForKillingAnEnemy $
            awardPointsToThatPlayerForHittingAnEnemy $
            awardPointsToThisPlayerForKillingAnEnemy $
            awardPointsToThisPlayerForHittingAnEnemy $
            state { wormHealths =
                      removeWormById (WormId 4) $
                      removeWormById (WormId 1) $
                      wormHealths state,
                    wormPositions =
                      removeWormById (WormId 4) $
                      removeWormById (WormId 1) $
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
          (selectNextWorms (WormId 1) (WormId 4) $
           awardPointsToThisPlayerForHittingAnEnemy $
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
           selectNextWorms (WormId 1) (WormId 4) $
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
              isAPositionOfAWorm (fromJust $ findDataById thisSelection positions)
                                 (wormPositions state)) &&
             (not $
              isAHit $
              isAPositionOfAWorm (fromJust $ findDataById thatSelection positions)
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
              isAPositionOfAWorm (fromJust $
                                  findDataById thisSelection positions)
                                 (wormPositions state)) &&
             (isAHit $
              isAPositionOfAWorm (fromJust $
                                  findDataById thatSelection positions)
                                 (wormPositions state))

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
bananaOneToRight      = Move 65
bananaOneToLeft       = Move 63
bananaIntoDirtFromMe  = Move 24
bananaIntoDirtFromHim = Move 104

hasScore :: Int -> Player -> Bool
hasScore score' (Player score'' _ _) = score' == score''

oppositeShot :: Move -> Move
oppositeShot (Move x) = Move ((x + 4) `mod` 8)

emptyWormHealths = AList (-1) (-1) (-1) (-1) (-1) (-1)

emptyWormPositions = AList (-1) (-1) (-1) (-1) (-1) (-1)

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

addDirtAt :: Coord -> GameMap -> GameMap
addDirtAt = (flip mapSquareAt) (always DIRT)

spaceBetween :: ModifyMap
spaceBetween thisCoord thatCoord=
  addSpaceAt (coordBetween thisCoord thatCoord)

addSpaceAt :: Coord -> GameMap -> GameMap
addSpaceAt = (flip mapSquareAt) (always DEEP_SPACE)

addAirAt :: Coord -> GameMap -> GameMap
addAirAt = (flip mapSquareAt) (always AIR)

addMedipackAt :: Coord -> GameMap -> GameMap
addMedipackAt = (flip mapSquareAt) (always MEDIPACK)

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
            aListConcat someWormHealths (wormHealths state),
          wormPositions =
            aListConcat (
              withAllOtherWormsOffMap $ aListFromList [
                  (thisWormId, thisCoord),
                  (thatWormId, thatCoord)])
              (wormPositions state) }

allWormIds :: [WormId]
allWormIds = [
  (WormId 1),
  (WormId 2),
  (WormId 3),
  (WormId 4),
  (WormId 8),
  (WormId 12)]

withAllOtherWormsOffMap :: WormPositions -> WormPositions
withAllOtherWormsOffMap positions =
  let existingIds  = aListMyIds positions ++ aListOpponentIds positions
      remainingIds = map (\ (WormId wormId') -> wormId') $ filter (not . (flip elem) existingIds) allWormIds
  in aListConcat positions
                 (aListFromList $
                  -- Put the remaining worms all off the map
                  zip remainingIds (repeat (toCoord (4 * mapDim) (4 * mapDim))))

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
                                      emptyWormHealths
                                      emptyWormPositions
                                      emptyBananaBombs
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

-- TODO test
nonDiagonalDeltaOutOfRange :: Int -> Int
nonDiagonalDeltaOutOfRange x =
  if abs x <= horizontalRocketRange
  then ((horizontalRocketRange + 1) * if x < 0 then -1 else 1)
  else x

-- TODO test
diagonalDeltaOutOfRange :: Int -> Int
diagonalDeltaOutOfRange x =
  if abs x <= diagonalRocketRange
  then ((diagonalRocketRange + 1) * if x < 0 then -1 else 1)
  else x

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

inBoundsWithNonDiagonalPadding x = 3 + (x `mod` (mapDim - 6))

inBoundsWithDiagonalPadding x = 2 + (x `mod` (mapDim - 4))

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

emptyBananaBombs = AList (-1) (-1) (-1) (-1) (-1) (-1)

aStateWithoutWorms = State justNothing
                           emptyWormHealths
                           emptyWormPositions
                           emptyBananaBombs
                           aPlayer
                           anOpponent
                           aGameMap

aState = State justNothing
               someWormHealths
               someWormPositions
               emptyBananaBombs
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
  wormPositions = wormPositionsWithMyWormAtTop }

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
  wormPositions = wormPositionsWithOpponentNextToMedipack,
  gameMap       = aGameMapWithAMedipack }

aStateWithOpponentsWormOnTheMedipack = aState {
  wormPositions = wormPositionsWithOpponentOnTheMedipack,
  wormHealths   = wormHealthsWithOpponentHavingReceivedTheMedipack }

aStateWithMyWormNextToTheMedipack = aState {
  wormPositions = wormPositionsWithMyWormNextToMedipack,
  gameMap       = aGameMapWithAMedipack }

aStateWithMyWormOnTheMedipack = aState {
  wormPositions = wormPositionsWithMyWormOnTheMedipack,
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
  (4, (toCoord 17 31)),
  (8, (toCoord 1 1))]

wormHealthsForOneAndFive = aListFromList [
  (1, startingHealth),
  (2, startingHealth),
  (4, startingHealth),
  (8, startingHealth) ]

wormPositionsWithHisNextToMine = AList
  (toCoord 15 31)
  (toCoord 1 31)
  (toCoord 1 30)
  (toCoord 16 31)
  (toCoord 19 1)
  (toCoord 20 1)

wormPositionsWithHisNextToHis = aListFromList [
  (4, (toCoord 15 31)),
  (8, (toCoord 16 31))]

wormPositionsWithOpponentNextToMedipack = aListFromList [
  (4,  (toCoord 31 30)),
  (8,  (toCoord 19 1)),
  (12, (toCoord 20 1))]

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
  (2, (toCoord 16 31))]

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
  (1, (toCoord 15 0))]

startingSelections = (Selections 3)

aPlayer = Player 300 (WormId 1) startingSelections

-- Medipack is at 31 31
aGameMapWithAMedipack = vectorGameMapToHashGameMap $ V.fromList $
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

aGameMap = vectorGameMapToHashGameMap $ V.fromList $
  spaceRow ++
  dirtRow ++
  foldl' (++) [] (take (mapDim - 4) $ repeat middleRow) ++
  dirtRow ++
  spaceRow

airRow = take mapDim $ repeat AIR

aGameMapWithOnlyAir = vectorGameMapToHashGameMap $ V.fromList $
  foldl' (++) [] (take mapDim $ repeat airRow)
