{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module BotSpec (spec) where

import Bot
import Import

import qualified RIO.Vector.Boxed as V
import RIO.List.Partial
import RIO.List

import Test.Hspec
import Test.Hspec.QuickCheck

import System.Random

packThisWorm :: Int -> WormId
packThisWorm 1 = WormId 1
packThisWorm 2 = WormId 2
packThisWorm 3 = WormId 3
packThisWorm x = error $ "packThisWorm with invalid worm id: " ++ show x

packThatWorm :: Int -> WormId
packThatWorm 1 = WormId 4
packThatWorm 2 = WormId 8
packThatWorm 3 = WormId 12
packThatWorm x  = error $ "packThatWorm with invalid worm id: " ++ show x

withId :: WormId -> AListEntry a -> Bool
withId id' = (== id') . idSlot

findWormHealth :: WormId -> State -> Maybe WormHealth
findWormHealth id' = fmap dataSlot . aListFind (withId id') . wormHealths

spec :: Spec
spec = do
  describe "findWormHealth" $ do
    context "given an empty collection of worm health facts" $
      it "produces Nothing" $
      findWormHealth (WormId 1) aStateWithNoFacts `shouldBe` Nothing
    context "given an alist of worm health which doesn't contain the id" $
      it "produces Nothing" $
      findWormHealth (WormId 2) aStateWithTwoWormHealths `shouldBe` Nothing
    context "given an alist of worm health which does contain the id" $
      it "produces Just that cell" $
      findWormHealth (WormId 1) aStateWithTwoWormHealths `shouldBe` (Just $ WormHealth 5)
  describe "packThisWorm" $ do
    context "given a number from 1 to 3" $
      prop "is no different to the WormId constructor" $ \ x ->
      let x' = 1 + x `mod` 3
      in packThisWorm x' `shouldBe` WormId x'
    context "given any number other than 1 to 3" $
      prop "produces an exception" $ \ x ->
      let x' = if x >= 1 && x <= 3 then 0 else x
      in evaluate (packThisWorm x') `shouldThrow` anyErrorCall
    context "given a number from 1 to 3" $
      prop "produces an id for my worm" $ \ x ->
      let x' = 1 + x `mod` 3
      in packThisWorm x' `shouldSatisfy` isMyWorm
    context "given a number from 1 to 3" $
      prop "does not produce an id for an opponents worm" $ \ x ->
      let x' = 1 + x `mod` 3
      in packThisWorm x' `shouldSatisfy` (not . isOpponentWorm)
  describe "packThatWorm" $ do
    context "given a number of either 1, 2 or 3" $
      prop "produces a worm which is an opponent worm" $ \ x ->
      let x' = 1 + x `mod` 3
      in packThatWorm x' `shouldSatisfy` isOpponentWorm
    context "given a number of either 1, 2 or 3" $
      prop "produces a worm which is not my worm" $ \ x ->
      let x' = 1 + x `mod` 3
      in packThatWorm x' `shouldSatisfy` (not . isMyWorm)
    context "given any number other than 1 to 3" $
      prop "produces an exception" $ \ x ->
      let x' = if x >= 1 && x <= 3 then 0 else x
      in evaluate (packThatWorm x') `shouldThrow` anyErrorCall
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
    prop "Can always be extracted" $ \ (i, j) ->
      let iMove = Move $ (abs i) `mod` 16
          jMove = Move $ (abs j) `mod` 16
      in toMoves (fromMoves iMove jMove) `shouldBe` (iMove, jMove)
  describe "coordinates" $ do
    prop "Can always be extracted" $ \ (i, j) ->
      let x' = inBoundsWithNoPadding (abs i)
          y' = inBoundsWithNoPadding (abs j)
      in fromCoord (toCoord x' y') `shouldBe` (x', y')
  describe "penaliseForInvalidCommand" $ do
    it "should reduce the given players score by 4" $
      penaliseForInvalidCommand aPlayer `shouldBe`
      Player 296 (WormId 1)
  describe "penaliseThatPlayerForAnInvalidCommand" $ do
    it "should reduce the points of the opponent by 4" $
      penaliseThatPlayerForAnInvalidCommand aState `shouldBe`
      aState { opponent = Player 296 (WormId 4) }
  describe "penaliseThisPlayerForAnInvalidCommand" $ do
    it "should reduce the points of the player by 4" $
      penaliseThisPlayerForAnInvalidCommand aState `shouldBe`
      aState { myPlayer = Player 296 (WormId 1) }
  describe "awardPointsForMovingToAir" $ do
    it "should increment the points of a player by 5" $
      awardPointsForMovingToAir aPlayer `shouldBe`
      Player 305 (WormId 1)
  describe "awardPointsToThatPlayerForMovingToAir" $ do
    it "should increment the points of opponent by 5" $
      awardPointsToThatPlayerForMovingToAir aState `shouldBe`
      aState { opponent = Player 305 (WormId 4) }
  describe "awardPointsToThisPlayerForMovingToAir" $ do
    it "should increment the points of my player by 5" $
      awardPointsToThisPlayerForMovingToAir aState `shouldBe`
      aState { myPlayer = Player 305 (WormId 1) }
  describe "awardPointsForDigging" $ do
    it "should increment the points of a player by 7" $
      awardPointsForDigging aPlayer `shouldBe`
      Player 307 (WormId 1)
  describe "awardPointsToThisPlayerForDigging" $ do
    it "should increment this players points by 7" $
      awardPointsToThisPlayerForDigging aState `shouldBe`
      aState { myPlayer = Player 307 (WormId 1) }
  describe "awardPointsToThatPlayerForDigging" $ do
    it "should increment that players points by 7" $
      awardPointsToThatPlayerForDigging aState `shouldBe`
      aState { opponent = Player 307 (WormId 4) }
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
      in (countWorms ((\ (WormId x) -> x == 4 || x == 8) . idSlot) $
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
      oneWormHarmed 20 (AList [AListEntry (WormId 1) (WormHealth 20)]) `shouldBe` False
    context "given a collection with two harmed worms" $
      it "should produce false" $
      oneWormHarmed 20 (AList [AListEntry (WormId 1) (WormHealth 10), AListEntry (WormId 2) (WormHealth 10)]) `shouldBe`
      False
    context "given a collection with one worm harmed" $
      it "should produce true" $
      oneWormHarmed 20 (AList [AListEntry (WormId 1) (WormHealth 10), AListEntry (WormId 2) (WormHealth 20)])
  describe "noWormHarmed" $ do
    context "given an empty collection" $
      it "should produce true" $
      noWormHarmed 20 emptyWormHealths
    context "given a collection of one unharmed worm" $
      it "should produce true" $
      noWormHarmed 20 (AList [AListEntry (WormId 1) (WormHealth 20)])
    context "given a collection of more than one unharmed worms" $
      it "should produce true" $
      noWormHarmed 20 (AList [AListEntry (WormId 1) (WormHealth 20), AListEntry (WormId 2) (WormHealth 20)])
    context "given a collection with a harmed worm" $
      it "should produce false" $
      noWormHarmed 20 (AList [AListEntry (WormId 1) startingHealth, AListEntry (WormId 2) (WormHealth 20)]) `shouldBe`
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
      (selectNextWormsDefault $ penaliseThatPlayerForAnInvalidCommand aState)
    it "moving my worm into space should not move the worm" $
      makeMove True (fromMoves moveSouth doNothing) aState `shouldBe`
      (selectNextWormsDefault $ penaliseThisPlayerForAnInvalidCommand aState)
    it "moving opponents worm into dirt should dig out the dirt" $
      makeMove True (fromMoves doNothing digSouth) aState `shouldBe`
      (selectNextWormsDefault $ awardPointsToThatPlayerForDigging $ removeDirtFromMapAt (toCoord 16 2) aState)
    it "moving my worm into air should move the worm to that spot" $
      makeMove True (fromMoves moveEast doNothing) aState `shouldBe`
      (selectNextWormsDefault $ awardPointsToThisPlayerForMovingToAir $ moveThisWorm (toCoord 16 31) aState)
    it "moving opponents worm into air should move the worm to that spot" $
      makeMove True (fromMoves doNothing moveEast) aState `shouldBe`
      (selectNextWormsDefault $ awardPointsToThatPlayerForMovingToAir $ moveThatWorm (toCoord 17 1) aState)
    it "moving to the same square should swap the worms if true and damage both worms" $
      makeMove True (fromMoves moveEast moveWest) aStateWithImpendingCollision `shouldBe`
      (selectNextWormsDefault $
       awardPointsToThatPlayerForMovingToAir $
       awardPointsToThisPlayerForMovingToAir $
       moveThisWorm (toCoord 17 31) $ moveThatWorm (toCoord 15 31) $
       harmWorm (WormId (-1)) aStateWithImpendingCollision knockBackDamageAmount id id id (toCoord 17 31) $
       harmWorm (WormId (-1)) aStateWithImpendingCollision knockBackDamageAmount id id id (toCoord 15 31)
       aStateWithImpendingCollision)
    it "moving to the same square should not swap the worms if false and damage both worms" $
      makeMove False (fromMoves moveEast moveWest) aStateWithImpendingCollision `shouldBe`
      (selectNextWormsDefault $
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
      (selectNextWormsDefault $ penaliseThatPlayerForAnInvalidCommand aStateWithMyWormNextToAnEnemy)
    it "moving an opponents worm to a square occupied by one of the opponents worms does nothing" $
      makeMove True (fromMoves doNothing moveEast) aStateWithEnemyWormsNextToEachother `shouldBe`
      (selectNextWormsDefault $ penaliseThatPlayerForAnInvalidCommand aStateWithEnemyWormsNextToEachother)
    it "moving my worm onto the medipack increases my worms health by 10 and changes that square to AIR" $
      makeMove True (fromMoves moveEast doNothing) aStateWithMyWormNextToTheMedipack `shouldBe`
      (selectNextWormsDefault $
       awardPointsToThisPlayerForMovingToAir aStateWithMyWormOnTheMedipack)
    it "moving the opponents worm onto the medipack should increase its health by ten and change that square to AIR" $
      makeMove True (fromMoves doNothing moveSouth) aStateWithOpponentsWormNextToTheMedipack `shouldBe`
      (selectNextWormsDefault $
       awardPointsToThatPlayerForMovingToAir aStateWithOpponentsWormOnTheMedipack)
    it "moving both worms onto the same medipack results in a swap when the bit is set" $
      makeMove True (fromMoves moveEast moveSouth) aStateWithBothWormsNextToTheMedipack `shouldBe`
      (selectNextWormsDefault $
       awardPointsToThatPlayerForMovingToAir $
       awardPointsToThisPlayerForMovingToAir aStateWhereWeSwappedOverTheMedipack)
    it "moving both worms onto the same medipack results no swap when the bit is set" $
      makeMove False (fromMoves moveEast moveSouth) aStateWithBothWormsNextToTheMedipack `shouldBe`
      (selectNextWormsDefault $
       knockBackDamage $
       awardPointsToThatPlayerForMovingToAir $
       awardPointsToThisPlayerForMovingToAir aStateWhereNoSwapHappened)
    -- Top
    it "moving my worm off the top edge of the map changes nothing" $
      makeMove True (fromMoves moveNorth doNothing) aStateWithMyWormOnTop `shouldBe`
      (selectNextWormsDefault $ penaliseThisPlayerForAnInvalidCommand aStateWithMyWormOnTop)
    it "moving opponent worm off the top edge of the map changes nothing" $
      makeMove True (fromMoves doNothing moveNorth) aStateWithOpponentWormOnTop `shouldBe`
      (selectNextWormsDefault $ penaliseThatPlayerForAnInvalidCommand aStateWithOpponentWormOnTop)
    it "moving my worm on the top to the east results in the worm moving east" $
      makeMove True (fromMoves moveEast doNothing) aStateWithMyWormOnTop `shouldBe`
      (selectNextWormsDefault $ awardPointsToThisPlayerForMovingToAir aStateWithMyWormOnTopMovedRight)
    it "moving opponent worm on the top to the east results in the worm moving east" $
      makeMove True (fromMoves doNothing moveEast) aStateWithOpponentWormOnTop `shouldBe`
      (selectNextWormsDefault $ awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormOnTopMovedRight)
    it "moving my worm on the top to the west results in the worm moving west" $
      makeMove True (fromMoves moveWest doNothing) aStateWithMyWormOnTop `shouldBe`
      (selectNextWormsDefault $ awardPointsToThisPlayerForMovingToAir aStateWithMyWormOnTopMovedLeft)
    it "moving opponent worm on the top to the west results in the worm moving west" $
      makeMove True (fromMoves doNothing moveWest) aStateWithOpponentWormOnTop `shouldBe`
      (selectNextWormsDefault $ awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormOnTopMovedLeft)
    it "moving my worm south from the top of the map results in that worm moving down" $
      makeMove True (fromMoves moveSouth doNothing) aStateWithMyWormOnTop `shouldBe`
      (selectNextWormsDefault $ awardPointsToThisPlayerForMovingToAir aStateWithMyWormOnTopMovedDown)
    it "moving opponent worm south from the top of the map results in that worm moving down" $
      makeMove True (fromMoves doNothing moveSouth) aStateWithOpponentWormOnTop `shouldBe`
      (selectNextWormsDefault $ awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormOnTopMovedDown)
    -- Left edge
    it "moving my worm north on the left edge of the map moves that worm north" $
      makeMove True (fromMoves moveNorth doNothing) aStateWithMyWormOnLeftEdge `shouldBe`
      (selectNextWormsDefault $ awardPointsToThisPlayerForMovingToAir aStateWithMyWormUpwardsOnLeftEdge)
    it "moving opponent worm north on the left edge of the map moves that worm north" $
      makeMove True (fromMoves doNothing moveNorth) aStateWithOpponentWormOnLeftEdge `shouldBe`
      (selectNextWormsDefault $ awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormUpwardOnLeftEdge)
    it "moving my worm south on the left edge of the map moves that worm south" $
      makeMove True (fromMoves moveSouth doNothing) aStateWithMyWormOnLeftEdge `shouldBe`
      (selectNextWormsDefault $ awardPointsToThisPlayerForMovingToAir aStateWithMyWormDownwardOnLeftEdge)
    it "moving opponent worm south on the left edge of the map moves that worm south" $
      makeMove True (fromMoves doNothing moveSouth) aStateWithOpponentWormOnLeftEdge `shouldBe`
      (selectNextWormsDefault $ awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormDownwardOnLeftEdge)
    it "moving my worm east on the left edge of the map moves that worm east" $
      makeMove True (fromMoves moveEast doNothing) aStateWithMyWormOnLeftEdge `shouldBe`
      (selectNextWormsDefault $ awardPointsToThisPlayerForMovingToAir aStateWithMyWormRightFromLeftEdge)
    it "moving opponent worm east on the left edge of the map moves that worm east" $
      makeMove True (fromMoves doNothing moveEast) aStateWithOpponentWormOnLeftEdge `shouldBe`
      (selectNextWormsDefault $ awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormRightFromLeftEdge)
    it "moving my worm off the edge on the left of the map changes nothing" $
      makeMove True (fromMoves moveWest doNothing) aStateWithMyWormOnLeftEdge `shouldBe`
      (selectNextWormsDefault $ penaliseThisPlayerForAnInvalidCommand aStateWithMyWormOnLeftEdge)
    it "moving opponent worm off the edge on left of the map changes nothing" $
      makeMove True (fromMoves doNothing moveWest) aStateWithOpponentWormOnLeftEdge `shouldBe`
      (selectNextWormsDefault $ penaliseThatPlayerForAnInvalidCommand aStateWithOpponentWormOnLeftEdge)
    -- Bottom edge
    it "moving my worm south from the bottom edge results in no change" $
      makeMove True (fromMoves moveSouth doNothing) aStateWithMyWormOnTheBottomEdge `shouldBe`
      (selectNextWormsDefault $ penaliseThisPlayerForAnInvalidCommand aStateWithMyWormOnTheBottomEdge)
    it "moving opponent worm south from the bottom edge results in no change" $
      makeMove True (fromMoves doNothing moveSouth) aStateWithOpponentWormOnTheBottomEdge `shouldBe`
      (selectNextWormsDefault $ penaliseThatPlayerForAnInvalidCommand aStateWithOpponentWormOnTheBottomEdge)
    it "moving my worm to the east from the bottom edge results in that worm moving right" $
      makeMove True (fromMoves moveEast doNothing) aStateWithMyWormOnTheBottomEdge `shouldBe`
      (selectNextWormsDefault $ awardPointsToThisPlayerForMovingToAir aStateWithMyWormOnTheBottomEdgeMovedRight)
    it "moving opponent worm to the east from the bottom edge results in that worm moving right" $
      makeMove True (fromMoves doNothing moveEast) aStateWithOpponentWormOnTheBottomEdge `shouldBe`
      (selectNextWormsDefault $ awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormOnTheBottomEdgeMovedRight)
    it "moving my worm to the west from the bottom edge results in that worm moving left" $
      makeMove True (fromMoves moveWest doNothing) aStateWithMyWormOnTheBottomEdge `shouldBe`
      (selectNextWormsDefault $ awardPointsToThisPlayerForMovingToAir aStateWithMyWormOnTheBottomEdgeMovedLeft)
    it "moving opponent to the west from the bottom edge results in that worm moving left" $
      makeMove True (fromMoves doNothing moveWest) aStateWithOpponentWormOnTheBottomEdge `shouldBe`
      (selectNextWormsDefault $ awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormOnTheBottomEdgeMovedLeft)
    it "moving my worm to the north from the bottom edge results in that worm moving up" $
      makeMove True (fromMoves moveNorth doNothing) aStateWithMyWormOnTheBottomEdge `shouldBe`
      (selectNextWormsDefault $ awardPointsToThisPlayerForMovingToAir aStateWithMyWormUpFromTheBottomEdge)
    it "moving opponent worm to the north from the bottom edge results in that worm moving up" $
      makeMove True (fromMoves doNothing moveNorth) aStateWithOpponentWormOnTheBottomEdge `shouldBe`
      (selectNextWormsDefault $ awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormUpFromTheBottomEdge)
    -- Right edge
    it "moving my worm east from the right edge results in no change" $
      makeMove True (fromMoves moveEast doNothing) aStateWithMyWormOnTheRightEdge `shouldBe`
      (selectNextWormsDefault $ penaliseThisPlayerForAnInvalidCommand aStateWithMyWormOnTheRightEdge)
    it "moving opponent worm east from the right edge results in no change" $
      makeMove True (fromMoves doNothing moveEast) aStateWithOpponentWormOnTheRightEdge `shouldBe`
      (selectNextWormsDefault $ penaliseThatPlayerForAnInvalidCommand aStateWithOpponentWormOnTheRightEdge)
    it "moving my worm north from the right edge results in that worm moving up" $
      makeMove True (fromMoves moveNorth doNothing) aStateWithMyWormOnTheRightEdge `shouldBe`
      (selectNextWormsDefault $ awardPointsToThisPlayerForMovingToAir aStateWithMyWormOnTheRightEdgeMovedUp)
    it "moving opponent worm north from the right edge results in that worm moving up" $
      makeMove True (fromMoves doNothing moveNorth) aStateWithOpponentWormOnTheRightEdge `shouldBe`
      (selectNextWormsDefault $ awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormOnTheRightEdgeMovedUp)
    it "moving my worm south from the right edge results in that worm moving down" $
      makeMove True (fromMoves moveSouth doNothing) aStateWithMyWormOnTheRightEdge `shouldBe`
      (selectNextWormsDefault $ awardPointsToThisPlayerForMovingToAir aStateWithMyWormOnTheRightEdgeMovedDown)
    it "moving opponent worm south from the right edge results in that worm moving down" $
      makeMove True (fromMoves doNothing moveSouth) aStateWithOpponentWormOnTheRightEdge `shouldBe`
      (selectNextWormsDefault $ awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormOnTheRightEdgeMovedDown)
    it "moving my worm to the west from the right edge results in that worm moving left" $
      makeMove True (fromMoves moveWest doNothing) aStateWithMyWormOnTheRightEdge `shouldBe`
      (selectNextWormsDefault $ awardPointsToThisPlayerForMovingToAir aStateWithMyWormMovedLeftFromTheRightEdge)
    it "moving opponent worm to the west from the right edge results in that worm moving left" $
      makeMove True (fromMoves doNothing moveWest) aStateWithOpponentWormOnTheRightEdge `shouldBe`
      (selectNextWormsDefault $ awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormMovedLeftFromTheRightEdge)
    -- Digging
    it "should remove dirt when my player digs a dirt block" $
      makeMove True (fromMoves digNorth doNothing) aState `shouldBe`
      (selectNextWormsDefault $ awardPointsToThisPlayerForDigging aStateWithDirtMissingAboveMyWorm)
    it "should remove dirt when opponent digs a dirt block" $
      makeMove True (fromMoves doNothing digNorth) aStateWithOpponentBeneathDirt `shouldBe`
      (selectNextWormsDefault $ awardPointsToThatPlayerForDigging aStateWithDirtMissingAboveOpponentWorm)
    it "moving next to dirt should not dig out that dirt when it would be in our way if we continued going that way" $
      makeMove True (fromMoves doNothing moveEast) aStateWithOpponentNearDirtToTheEast `shouldBe`
      (selectNextWormsDefault $
       awardPointsToThatPlayerForMovingToAir $
       moveThatWorm (toCoord 10 2) aStateWithOpponentNearDirtToTheEast)
    it "should reward both players and remove dirt when both worms dig the same dirt block" $
      makeMove True (fromMoves digSouthEast digSouth) aStateWithBothWormsNearTheSameDirtBlock `shouldBe`
      (selectNextWormsDefault $
       awardPointsToThatPlayerForDigging $
       awardPointsToThisPlayerForDigging $
       mapGameMap aStateWithBothWormsNearTheSameDirtBlock (removeDirtAt (toCoord 11 2)))
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
         (selectNextWormsDefault $
          awardPointsToThatPlayerForKillingAnEnemy $
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
         (selectNextWorms (WormId 2) (WormId 12) $
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
         (selectNextWormsDefault $
          awardPointsToThatPlayerForKillingAnEnemy $
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
         (selectNextWorms (WormId 2) (WormId 12) $
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
         (selectNextWormsDefault $
          awardPointsToThatPlayerForKillingAnEnemy $
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
         (selectNextWorms (WormId 2) (WormId 12) $
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
         (selectNextWormsDefault $
          awardPointsToThatPlayerForKillingAnEnemy $
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
         (selectNextWormsDefault $
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
         (selectNextWormsDefault $
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
         (selectNextWormsDefault $
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
         (selectNextWormsDefault $
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
           (selectNextWormsDefault $
            awardPointsToThatPlayerForKillingAnEnemy $
            awardPointsToThisPlayerForKillingAnEnemy $
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
                           (takeBothWormsWithHealth (WormHealth 100) (WormId 1) (WormId 4))
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
                           (takeBothWormsWithHealth (WormHealth 100) (WormId 1) (WormId 4))
                           (i, j, k)
       in makeMove True (fromMoves doNothing shot) state `shouldBe`
          (selectNextWorms (WormId 1) (WormId 4) $
           awardPointsToThatPlayerForHittingAnEnemy $
           state { wormHealths = harmWormById rocketDamage (WormId 1)  $ wormHealths state })

hasScore :: Int -> Player -> Bool
hasScore score' (Player score'' _) = score' == score''

oppositeShot :: Move -> Move
oppositeShot (Move x) = Move ((x + 4) `mod` 8)

aStateWithTwoWormHealths =
  aState { wormHealths = AList [
             AListEntry (WormId 1) (WormHealth 5),
             AListEntry (WormId 3) startingHealth] }

aStateWithNoFacts =
  aState { wormHealths   = emptyWormHealths,
           wormPositions = emptyWormPositions }

emptyWormHealths = AList []

emptyWormPositions = AList []

(.&&.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.&&.) p1 p2 x =
  p1 x && p2 x

oneWormHarmed :: Int -> WormHealths -> Bool
oneWormHarmed originalHealth =
  (== 1) . countWorms ((/= originalHealth) . deconstructHealth . dataSlot)

noWormHarmed :: Int -> WormHealths -> Bool
noWormHarmed originalHealth =
  allWormFacts ((== originalHealth) . deconstructHealth . dataSlot)

containsWormOfId :: WormId -> State -> Bool
containsWormOfId wormId' state =
  (isJust $ aListFind ((== wormId') . idSlot) $ wormHealths state) &&
  (isJust $ aListFind ((== wormId') . idSlot) $ wormHealths state)

countWorms :: (AListEntry a -> Bool) -> AList a -> Int
countWorms f = aListFoldl' ( \ acc worm -> acc + if f worm then 1 else 0) 0

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

generateShotSwitch :: Move -> Move -> ShotSwitch
generateShotSwitch a b x =
  if x > 0 then a else b

generateInBoundsCoordinate :: Int -> Int -> Coord
generateInBoundsCoordinate = generateCoordGenerator inBoundsWithNoPadding inBoundsWithNoPadding

startingHealth = WormHealth 8

takeBothWormsWithHealth :: WormHealth ->  WormId -> WormId -> AddToWormFacts
takeBothWormsWithHealth startingHealth' thisWormId thatWormId thisCoord thatCoord state =
  state { wormHealths   =
            aListConcat (
              AList [
                  AListEntry thisWormId startingHealth',
                  AListEntry thatWormId startingHealth'])
              (wormHealths   state),
          wormPositions =
            aListConcat (
              AList [
                  AListEntry thisWormId thisCoord,
                  AListEntry thatWormId thatCoord])
              (wormPositions state) }


takeBothWorms :: WormId -> WormId -> AddToWormFacts
takeBothWorms thisWormId thatWormId thisCoord thatCoord state =
  state { wormHealths   =
            aListConcat someWormHealths (wormHealths state),
          wormPositions =
            aListConcat (
              withAllOtherWormsOffMap $ AList [
                  AListEntry thisWormId thisCoord,
                  AListEntry thatWormId thatCoord])
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
  let existingIds  = aListFoldl' (flip ((:) . idSlot)) [] positions
      remainingIds = filter (not . (flip elem) existingIds) allWormIds
  in aListConcat positions
                 (AList $
                  map (uncurry AListEntry) $
                  -- Put the remaining worms all off the map
                  zip remainingIds (repeat (toCoord (4 * mapDim) (4 * mapDim))))

-- TODO: test
takeBothWormsAndPutAnotherInbetween :: WormId -> WormId -> WormId -> AddToWormFacts
takeBothWormsAndPutAnotherInbetween inbetweenId thisWormId thatWormId thisCoord thatCoord state =
  state { wormHealths   =
            aListConcat (
              AList [
                  AListEntry thisWormId startingHealth,
                  AListEntry inbetweenId startingHealth,
                  AListEntry thatWormId startingHealth])
              (wormHealths   state),
          wormPositions =
            aListConcat (
              AList [
                  AListEntry thisWormId thisCoord,
                  AListEntry inbetweenId (coordBetween thisCoord thatCoord),
                  AListEntry thatWormId thatCoord])
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

generateShotScenarioWithMapModifications :: GenerateCoordinate -> DisplaceFromCoordinate -> ShotSwitch -> AddToWormFacts -> ModifyMap -> (Int, Int, Int) -> (State, Move)
generateShotScenarioWithMapModifications generateCoord displace switchShot addFacts modifyMap (i, j, k) =
  let originatingCoord        = generateCoord i j
      (displacedCoord, delta) = displace      originatingCoord k
      shot                    = switchShot    delta
      modifiedMap             = modifyMap originatingCoord displacedCoord aGameMapWithOnlyAir
      state                   = State 10 10 10 10 emptyWormHealths emptyWormPositions (Player 300 (WormId 1)) (Player 300 (WormId 4)) modifiedMap
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

doNothing = Move 32

moveNorth = Move 8

digNorth = Move 16

digSouth = Move 20

digSouthEast = Move 19

moveSouth = Move 12

moveEast = Move 10

moveWest = Move 14

aStateWithoutWorms = State 10 10 10 10 emptyWormHealths emptyWormPositions aPlayer anOpponent aGameMap

aState = State 10 10 10 10 someWormHealths someWormPositions aPlayer anOpponent aGameMap

thisPlayersHealths = [
  AListEntry (WormId 1)  startingHealth,
  AListEntry (WormId 2)  startingHealth,
  AListEntry (WormId 3)  (WormHealth 20)]

thatPlayersHealths = [
  AListEntry (WormId 4)  startingHealth,
  AListEntry (WormId 8)  startingHealth,
  AListEntry (WormId 12) (WormHealth 20)]

someWormHealths = AList $ thisPlayersHealths ++ thatPlayersHealths

selectNextWorms :: WormId -> WormId -> ModifyState
selectNextWorms thisWormId thatWormId state@(State { myPlayer = myPlayer', opponent = opponent' }) =
  state { myPlayer = withCurrentWormId thisWormId myPlayer',
          opponent = withCurrentWormId thatWormId opponent' }

selectNextWormsDefault :: ModifyState
selectNextWormsDefault = selectNextWorms (WormId 2) (WormId 8)

someWormPositions = AList [
  AListEntry (WormId 1)  (toCoord 15 31),
  AListEntry (WormId 2)  (toCoord 1 31),
  AListEntry (WormId 3)  (toCoord 1 30),
  AListEntry (WormId 4)  (toCoord 16 1),
  AListEntry (WormId 8)  (toCoord 19 1),
  AListEntry (WormId 12) (toCoord 20 1)]

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

anOpponent = Player 300 (WormId 4)

wormPositionsWithImpendingCollision = AList [
  AListEntry (WormId 1) (toCoord 15 31),
  AListEntry (WormId 2) (toCoord 0 0),
  AListEntry (WormId 4) (toCoord 17 31),
  AListEntry (WormId 8) (toCoord 1 1)]

wormHealthsForOneAndFive = AList [
  AListEntry (WormId 1) startingHealth,
  AListEntry (WormId 2) startingHealth,
  AListEntry (WormId 4) startingHealth,
  AListEntry (WormId 8) startingHealth ]

wormPositionsWithHisNextToMine = AList [
  AListEntry (WormId 1) (toCoord 15 31),
  AListEntry (WormId 4) (toCoord 16 31)]

wormPositionsWithHisNextToHis = AList [
  AListEntry (WormId 4) (toCoord 15 31),
  AListEntry (WormId 8) (toCoord 16 31)]

wormPositionsWithOpponentNextToMedipack = AList [
  AListEntry (WormId 4) (toCoord 31 30),
  AListEntry (WormId 8)  (toCoord 19 1),
  AListEntry (WormId 12) (toCoord 20 1)]

wormPositionsWithOpponentOnTheMedipack = AList [
  AListEntry (WormId 4) (toCoord 31 31),
  AListEntry (WormId 8)  (toCoord 19 1),
  AListEntry (WormId 12) (toCoord 20 1)]

wormHealthsWithOpponentHavingReceivedTheMedipack = AList [
  AListEntry (WormId 1)  startingHealth,
  AListEntry (WormId 2)  startingHealth,
  AListEntry (WormId 3)  (WormHealth 20),
  AListEntry (WormId 4)  (WormHealth 18),
  AListEntry (WormId 8)  startingHealth,
  AListEntry (WormId 12) (WormHealth 20)]

wormPositionsWithOpponentAtTop = AList [
  AListEntry (WormId 4) (toCoord 15 0)]

wormPositionsWithMyWormsNextToEachother = AList [
  AListEntry (WormId 1) (toCoord 15 31),
  AListEntry (WormId 2) (toCoord 16 31)]

wormPositionsWithMyWormNextToMedipack = AList [
  AListEntry (WormId 1) (toCoord 30 31),
  AListEntry (WormId 2)  (toCoord 1 31),
  AListEntry (WormId 3)  (toCoord 1 30)]

wormPositionsWithMyWormOnTheMedipack = AList [
  AListEntry (WormId 1) (toCoord 31 31),
  AListEntry (WormId 2)  (toCoord 1 31),
  AListEntry (WormId 3)  (toCoord 1 30)]

wormHealthsWithMyWormHavingReceivedTheMedipack = AList [
  AListEntry (WormId 1)  (WormHealth 18),
  AListEntry (WormId 2)  startingHealth,
  AListEntry (WormId 3)  (WormHealth 20),
  AListEntry (WormId 4)  startingHealth,
  AListEntry (WormId 8)  startingHealth,
  AListEntry (WormId 12) (WormHealth 20)]

wormPositionsWithMyWormAtTop = AList [
  AListEntry (WormId 1) (toCoord 15 0)]

aPlayer = Player 300 (WormId 1)

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
