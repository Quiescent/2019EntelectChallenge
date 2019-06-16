{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module BotSpec (spec) where

import Bot
import Import

import qualified RIO.Vector.Boxed as V
import qualified RIO.HashMap as M
import RIO.List
import RIO.List.Partial

import Test.Hspec
import Test.Hspec.QuickCheck

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

findWormHealth :: WormId -> State -> Maybe WormHealth
findWormHealth = undefined

spec :: Spec
spec = do
  describe "findWormHealth" $ do
    context "given an empty collection of worm health facts" $
      it "produces Nothing" $
      findWormHealth (WormId 1) aState `shouldBe` Nothing
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
  describe "thisCurrentWorm" $ do
    it "shouldn't be found when the index is negative" $
      thisCurrentWorm (withMyCurrentWormIdOf (-1) aState) `shouldBe` Nothing
    it "shouldn't be found when the index is greater than the original number of worms" $
      thisCurrentWorm (withMyCurrentWormIdOf 6 aState) `shouldBe` Nothing
    it "shouldn't be found when searching for a worm in a gap in the sequence" $
      thisCurrentWorm (withMyCurrentWormIdOf 4 aState) `shouldBe` Nothing
    it "should find a worm when it's in the sequence" $
      thisCurrentWorm (withMyCurrentWormIdOf 3 aState) `shouldBe` (Just $ withIdOf 3 thisWorm3)
  describe "thatCurrentWorm" $ do
    it "shouldn't be found when the index is negative" $
      thatCurrentWorm (withOpponentCurrentWormIdOf (-1) aState) `shouldBe` Nothing
    it "shouldn't be found when the index is greater than the original number of worms" $
      thatCurrentWorm (withOpponentCurrentWormIdOf 6 aState) `shouldBe` Nothing
    it "shouldn't be found when searching for a worm in a gap in the sequence" $
      thatCurrentWorm (withOpponentCurrentWormIdOf 2 aState) `shouldBe` Nothing
    it "should find a worm when it's in the sequence" $
      thatCurrentWorm (withOpponentCurrentWormIdOf 3 aState) `shouldBe` (Just $ withIdOf 3 thatWorm3)
  describe "thisPlayersWorms" $ do
    it "should produce this players worms" $
      thisPlayersWorms aState == someWorms
  describe "thatPlayersWorms" $ do
    it "should produce that players worms" $
      thatPlayersWorms aState == someOtherWorms
  describe "penaliseForInvalidCommand" $ do
    it "should reduce the given players score by 4" $
      penaliseForInvalidCommand aPlayer `shouldBe`
      Player 296 1 someWorms
  describe "penaliseThatPlayerForAnInvalidCommand" $ do
    it "should reduce the points of the opponent by 4" $
      penaliseThatPlayerForAnInvalidCommand aState `shouldBe`
      aState { opponent = Player 296 1 someOtherWorms }
  describe "penaliseThisPlayerForAnInvalidCommand" $ do
    it "should reduce the points of the player by 4" $
      penaliseThisPlayerForAnInvalidCommand aState `shouldBe`
      aState { myPlayer = Player 296 1 someWorms }
  describe "awardPointsForMovingToAir" $ do
    it "should increment the points of a player by 5" $
      awardPointsForMovingToAir aPlayer `shouldBe`
      Player 305 1 someWorms
  describe "awardPointsToThatPlayerForMovingToAir" $ do
    it "should increment the points of opponent by 5" $
      awardPointsToThatPlayerForMovingToAir aState `shouldBe`
      aState { opponent = Player 305 1 someOtherWorms }
  describe "awardPointsToThisPlayerForMovingToAir" $ do
    it "should increment the points of my player by 5" $
      awardPointsToThisPlayerForMovingToAir aState `shouldBe`
      aState { myPlayer = Player 305 1 someWorms }
  describe "awardPointsForDigging" $ do
    it "should increment the points of a player by 7" $
      awardPointsForDigging aPlayer `shouldBe`
      Player 307 1 someWorms
  describe "awardPointsToThisPlayerForDigging" $ do
    it "should increment this players points by 7" $
      awardPointsToThisPlayerForDigging aState `shouldBe`
      aState { myPlayer = Player 307 1 someWorms }
  describe "awardPointsToThatPlayerForDigging" $ do
    it "should increment that players points by 7" $
      awardPointsToThatPlayerForDigging aState `shouldBe`
      aState { opponent = Player 307 1 someOtherWorms }
  describe "harmWormWithRocket" $ do
    it "should remove health from the worm" $
      (harmWormWithRocket $ Worm 1 10 $ toCoord 15 31) `shouldBe`
      (Worm 1 0 $ toCoord 15 31)
  describe "mapThoseWorms" $ do
    it "should produce the same state given the identity function" $
      mapThoseWorms id aState `shouldBe` aState
    it "should produce the given state with no worms for that playere when mapping the function always []" $
      mapThoseWorms (always M.empty) aState `shouldBe` aState { opponent = Player 300 1 M.empty }
  describe "mapTheseworms" $ do
    it "should produce the same state given the identity function" $
      mapTheseWorms id aState `shouldBe` aState
    it "should produce the given state with no worms for this playere when mapping the function always []" $
      mapTheseWorms (always M.empty) aState `shouldBe` aState { myPlayer = Player 300 1 M.empty }
  describe "generateShotSwitch" $ do
    prop "produces a function which produces the first given a negative number and the second given a positive number" $
      let switchFunction = generateShotSwitch shootEast shootNorth
      in \ x ->
        switchFunction x `shouldBe` if x > 0 then shootEast else shootNorth
  describe "takeBothWorms" $ do
    prop "creates a map of size two (distinct coordinates) given two (distinct) coordinates" $ \ (i, j) ->
      let thisCoord = generateInBoundsCoordinate i j
          thatCoord = generateInBoundsCoordinate (i + 20) (j + 20) -- Ensure distinct coordinates
      in (M.size $ playersWorms $ takeBothWorms thisCoord thatCoord) `shouldBe` 2
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
  describe "takeThisWorm" $ do
    prop "creates a player with 1 worm" $ \ (i, j) ->
      let thisCoord = generateInBoundsCoordinate i j
          thatCoord = generateInBoundsCoordinate (i + 20) (j + 20)
      in (M.size $ playersWorms $ takeThisWorm thisCoord thatCoord) `shouldBe` 1
    prop "always creates the worm from the first coordinate" $ \ (i, j) ->
      let thisCoord = generateInBoundsCoordinate i j
          thatCoord = generateInBoundsCoordinate (i + 20) (j + 20)
      in (wormPosition $ head $ M.elems $ playersWorms $ takeThisWorm thisCoord thatCoord) `shouldBe` thisCoord
  describe "takeThatWorm" $ do
    prop "creates a player with 1 worm" $ \ (i, j) ->
      let thisCoord = generateInBoundsCoordinate i j
          thatCoord = generateInBoundsCoordinate (i + 20) (j + 20)
      in (M.size $ playersWorms $ takeThatWorm thisCoord thatCoord) `shouldBe` 1
    prop "always creates the worm from the first coordinate" $ \ (i, j) ->
      let thisCoord = generateInBoundsCoordinate i j
          thatCoord = generateInBoundsCoordinate (i + 20) (j + 20)
      in (wormPosition $ head $ M.elems $ playersWorms $ takeThatWorm thisCoord thatCoord) `shouldBe` thatCoord
  describe "takeNoWorms" $ do
    it "should create a player with no worms given two distinct coordinates" $
      let thisCoord = toCoord 10 20
          thatCoord = toCoord 20 30
      in (M.size $ playersWorms $ takeNoWorms thisCoord thatCoord) `shouldBe` 0
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
      isAPositionOfAWorm (toCoord 0 0) someWorms `shouldBe` HitNothing
    it "should produce HitWorm of the worm hit when a coord shares it's position with a worm" $
      isAPositionOfAWorm (toCoord 1 31) someWorms `shouldBe` (HitWorm thisWorm2)
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
      (((\ _ -> False) .&&. (\ _ -> True)) (10::Int)) `shouldBe` False
    context "when the second predicate produces false" $
      it "should produce false" $
      (((\ _ -> True) .&&. (\ _ -> False)) (10::Int)) `shouldBe` False
    context "when both predicates produce true" $
      it "should produce true" $
      (((\ _ -> True) .&&. (\ _ -> True)) (10::Int)) `shouldBe` True
  describe "one worm harmed" $ do
    context "given a collection of no worms" $
      it "should produce false" $
      oneWormHarmed 10 (M.empty) `shouldBe` False
    context "given a collection of one unharmed worm" $
      it "should produce false" $
      oneWormHarmed 10 (wormsToMap $ V.singleton aWorm) `shouldBe` False
    context "given a collection with two harmed worms" $
      it "should produce false" $
      oneWormHarmed 10 (modifyWormWithId 1 (withHealthOf 0) $ modifyWormWithId 2 (withHealthOf 0) someWorms) `shouldBe`
      False
    context "given a collection with one worm harmed" $
      it "should produce true" $
      oneWormHarmed 10 (modifyWormWithId 1 (withHealthOf 0) someWorms)
  describe "noWormHarmed" $ do
    context "given an empty collection" $
      it "should produce true" $
      noWormHarmed 10 (M.empty)
    context "given a collection of one unharmed worm" $
      it "should produce true" $
      noWormHarmed 10 (wormsToMap $ V.singleton aWorm)
    context "given a collection of more than one unharmed worms" $
      it "should produce true" $
      noWormHarmed 10 someWorms
    context "given a collection with a harmed worm" $
      it "should produce false" $
      noWormHarmed 10 (modifyWormWithId 1 (withHealthOf 0) someWorms) `shouldBe` False
  describe "closer" $ do
    context "given three identical coordinates" $
      it "should produce false" $
      closer (toCoord 0 0) (toCoord 0 0) (toCoord 0 0) `shouldBe` False
    context "given a coordinate and then two identical coordinates" $
      it "should produce false" $
      closer (toCoord 0 0) (toCoord 3 4) (toCoord 3 4) `shouldBe` False
    context "given a coordinate a second displaced from it by at most 10 and a third displaced by twice as much more" $
      prop "produces true" $ \ (i, j, k) ->
      let d'     = k `mod` 10
          d      = if d' == 0 then -1 else d'
          pad    = (abs d)
          x'     = pad + (i `mod` (mapDim - (2 * pad)))
          y'     = pad + (j `mod` (mapDim - (2 * pad)))
          origin = toCoord x' y'
          coord1 = toCoord (x' + d) (y' + d)
          coord2 = toCoord (x' + (2 * d)) (y' + (2 * d))
      in closer origin coord1 coord2
  describe "makeMove" $ do
    -- TODO make this a property test...?
    it "should not change anything when it receives two 'nothing's" $
      makeMove True (fromMoves doNothing doNothing) aState `shouldBe` aState
    it "moving my worm to dirt should dig out that dirt" $
      makeMove True (fromMoves moveNorth doNothing) aState `shouldBe`
      (awardPointsToThisPlayerForDigging aStateWithDirtMissingAboveMyWorm)
    it "moving opponents worm to space should not move the worm" $
      makeMove True (fromMoves doNothing moveNorth) aState `shouldBe` penaliseThatPlayerForAnInvalidCommand aState
    it "moving my worm into space should not move the worm" $
      makeMove True (fromMoves moveSouth doNothing) aState `shouldBe` penaliseThisPlayerForAnInvalidCommand aState
    it "moving opponents worm into dirt should dig out the dirt" $
      makeMove True (fromMoves doNothing moveSouth) aState `shouldBe`
      (awardPointsToThatPlayerForDigging $ removeDirtFromMapAt (toCoord 16 2) aState)
    it "moving my worm into air should move the worm to that spot" $
      makeMove True (fromMoves moveEast doNothing) aState `shouldBe`
      (awardPointsToThisPlayerForMovingToAir $ aState { myPlayer = withWorms someWormsWithCurrentMovedEast aPlayer })
    it "moving opponents worm into air should move the worm to that spot" $
      makeMove True (fromMoves doNothing moveEast) aState `shouldBe`
      (awardPointsToThatPlayerForMovingToAir $ aState { opponent = withWorms someOtherWormsWithCurrentMovedEast anOpponent })
    it "moving to the same square should swap the worms if true and damage both worms" $
      makeMove True (fromMoves moveEast moveWest) aStateWithImpendingCollision `shouldBe`
      (awardPointsToThatPlayerForMovingToAir $ awardPointsToThisPlayerForMovingToAir $
       aStateWithImpendingCollision { myPlayer = aPlayerWithCollisionResolvedBySwapping,
                                      opponent = opponentWithCollisionResolvedBySwapping })
    it "moving to the same square should not swap the worms if false and damage both worms" $
      makeMove False (fromMoves moveEast moveWest) aStateWithImpendingCollision `shouldBe`
      (awardPointsToThatPlayerForMovingToAir $ awardPointsToThisPlayerForMovingToAir $
       aStateWithImpendingCollision { myPlayer = aPlayerWithCollisionResolvedByNotMoving,
                                      opponent = opponentWithCollisionResolvedByNotMoving })
    it "moving my worm to a square occupied by one of my worms does nothing" $
      makeMove True (fromMoves moveEast doNothing) aStateWithMyWormsNextToEachOther `shouldBe`
      penaliseThisPlayerForAnInvalidCommand aStateWithMyWormsNextToEachOther
    it "moving my worm to a square occupied by one of the the opponents worms does nothing " $
      makeMove True (fromMoves moveEast doNothing) aStateWithMyWormNextToAnEnemy `shouldBe`
      penaliseThisPlayerForAnInvalidCommand aStateWithMyWormNextToAnEnemy
    it "moving an opponents worm to a square occupied by one of my worms does nothing" $
      makeMove True (fromMoves doNothing moveWest) aStateWithMyWormNextToAnEnemy `shouldBe`
      penaliseThatPlayerForAnInvalidCommand aStateWithMyWormNextToAnEnemy
    it "moving an opponents worm to a square occupied by one of the opponents worms does nothing" $
      makeMove True (fromMoves doNothing moveWest) aStateWithEnemyWormsNextToEachother `shouldBe`
      penaliseThatPlayerForAnInvalidCommand aStateWithEnemyWormsNextToEachother
    it "moving my worm onto the medipack increases my worms health by 10 and changes that square to AIR" $
      makeMove True (fromMoves moveEast doNothing) aStateWithMyWormNextToTheMedipack `shouldBe`
      awardPointsToThisPlayerForMovingToAir aStateWithMyWormOnTheMedipack
    it "moving the opponents worm onto the medipack should increase its health by ten and change that square to AIR" $
      makeMove True (fromMoves doNothing moveSouth) aStateWithOpponentsWormNextToTheMedipack `shouldBe`
      awardPointsToThatPlayerForMovingToAir aStateWithOpponentsWormOnTheMedipack
    it "moving both worms onto the same medipack results in a swap when the bit is set" $
      makeMove True (fromMoves moveEast moveSouth) aStateWithBothWormsNextToTheMedipack `shouldBe`
      (awardPointsToThatPlayerForMovingToAir $ awardPointsToThisPlayerForMovingToAir aStateWhereWeSwappedOverTheMedipack)
    it "moving both worms onto the same medipack results no swap when the bit is set" $
      makeMove False (fromMoves moveEast moveSouth) aStateWithBothWormsNextToTheMedipack `shouldBe`
      (knockBackDamage $
       awardPointsToThatPlayerForMovingToAir $
       awardPointsToThisPlayerForMovingToAir aStateWhereNoSwapHappened)
    -- Top
    it "moving my worm off the top edge of the map changes nothing" $
      makeMove True (fromMoves moveNorth doNothing) aStateWithMyWormOnTop `shouldBe`
      penaliseThisPlayerForAnInvalidCommand aStateWithMyWormOnTop
    it "moving opponent worm off the top edge of the map changes nothing" $
      makeMove True (fromMoves doNothing moveNorth) aStateWithOpponentWormOnTop `shouldBe`
      penaliseThatPlayerForAnInvalidCommand aStateWithOpponentWormOnTop
    it "moving my worm on the top to the east results in the worm moving east" $
      makeMove True (fromMoves moveEast doNothing) aStateWithMyWormOnTop `shouldBe`
      awardPointsToThisPlayerForMovingToAir aStateWithMyWormOnTopMovedRight
    it "moving opponent worm on the top to the east results in the worm moving east" $
      makeMove True (fromMoves doNothing moveEast) aStateWithOpponentWormOnTop `shouldBe`
      awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormOnTopMovedRight
    it "moving my worm on the top to the west results in the worm moving west" $
      makeMove True (fromMoves moveWest doNothing) aStateWithMyWormOnTop `shouldBe`
      awardPointsToThisPlayerForMovingToAir aStateWithMyWormOnTopMovedLeft
    it "moving opponent worm on the top to the west results in the worm moving west" $
      makeMove True (fromMoves doNothing moveWest) aStateWithOpponentWormOnTop `shouldBe`
      awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormOnTopMovedLeft
    it "moving my worm south from the top of the map results in that worm moving down" $
      makeMove True (fromMoves moveSouth doNothing) aStateWithMyWormOnTop `shouldBe`
      awardPointsToThisPlayerForMovingToAir aStateWithMyWormOnTopMovedDown
    it "moving opponent worm south from the top of the map results in that worm moving down" $
      makeMove True (fromMoves doNothing moveSouth) aStateWithOpponentWormOnTop `shouldBe`
      awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormOnTopMovedDown
    -- Left edge
    it "moving my worm north on the left edge of the map moves that worm north" $
      makeMove True (fromMoves moveNorth doNothing) aStateWithMyWormOnLeftEdge `shouldBe`
      awardPointsToThisPlayerForMovingToAir aStateWithMyWormUpwardsOnLeftEdge
    it "moving opponent worm north on the left edge of the map moves that worm north" $
      makeMove True (fromMoves doNothing moveNorth) aStateWithOpponentWormOnLeftEdge `shouldBe`
      awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormUpwardOnLeftEdge
    it "moving my worm south on the left edge of the map moves that worm south" $
      makeMove True (fromMoves moveSouth doNothing) aStateWithMyWormOnLeftEdge `shouldBe`
      awardPointsToThisPlayerForMovingToAir aStateWithMyWormDownwardOnLeftEdge
    it "moving opponent worm south on the left edge of the map moves that worm south" $
      makeMove True (fromMoves doNothing moveSouth) aStateWithOpponentWormOnLeftEdge `shouldBe`
      awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormDownwardOnLeftEdge
    it "moving my worm east on the left edge of the map moves that worm east" $
      makeMove True (fromMoves moveEast doNothing) aStateWithMyWormOnLeftEdge `shouldBe`
      awardPointsToThisPlayerForMovingToAir aStateWithMyWormRightFromLeftEdge
    it "moving opponent worm east on the left edge of the map moves that worm east" $
      makeMove True (fromMoves doNothing moveEast) aStateWithOpponentWormOnLeftEdge `shouldBe`
      awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormRightFromLeftEdge
    it "moving my worm off the edge on the left of the map changes nothing" $
      makeMove True (fromMoves moveWest doNothing) aStateWithMyWormOnLeftEdge `shouldBe`
      penaliseThisPlayerForAnInvalidCommand aStateWithMyWormOnLeftEdge
    it "moving opponent worm off the edge on left of the map changes nothing" $
      makeMove True (fromMoves doNothing moveWest) aStateWithOpponentWormOnLeftEdge `shouldBe`
      penaliseThatPlayerForAnInvalidCommand aStateWithOpponentWormOnLeftEdge
    -- Bottom edge
    it "moving my worm south from the bottom edge results in no change" $
      makeMove True (fromMoves moveSouth doNothing) aStateWithMyWormOnTheBottomEdge `shouldBe`
      penaliseThisPlayerForAnInvalidCommand aStateWithMyWormOnTheBottomEdge
    it "moving opponent worm south from the bottom edge results in no change" $
      makeMove True (fromMoves doNothing moveSouth) aStateWithOpponentWormOnTheBottomEdge `shouldBe`
      penaliseThatPlayerForAnInvalidCommand aStateWithOpponentWormOnTheBottomEdge
    it "moving my worm to the east from the bottom edge results in that worm moving right" $
      makeMove True (fromMoves moveEast doNothing) aStateWithMyWormOnTheBottomEdge `shouldBe`
      awardPointsToThisPlayerForMovingToAir aStateWithMyWormOnTheBottomEdgeMovedRight
    it "moving opponent worm to the east from the bottom edge results in that worm moving right" $
      makeMove True (fromMoves doNothing moveEast) aStateWithOpponentWormOnTheBottomEdge `shouldBe`
      awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormOnTheBottomEdgeMovedRight
    it "moving my worm to the west from the bottom edge results in that worm moving left" $
      makeMove True (fromMoves moveWest doNothing) aStateWithMyWormOnTheBottomEdge `shouldBe`
      awardPointsToThisPlayerForMovingToAir aStateWithMyWormOnTheBottomEdgeMovedLeft
    it "moving opponent to the west from the bottom edge results in that worm moving left" $
      makeMove True (fromMoves doNothing moveWest) aStateWithOpponentWormOnTheBottomEdge `shouldBe`
      awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormOnTheBottomEdgeMovedLeft
    it "moving my worm to the north from the bottom edge results in that worm moving up" $
      makeMove True (fromMoves moveNorth doNothing) aStateWithMyWormOnTheBottomEdge `shouldBe`
      awardPointsToThisPlayerForMovingToAir aStateWithMyWormUpFromTheBottomEdge
    it "moving opponent worm to the north from the bottom edge results in that worm moving up" $
      makeMove True (fromMoves doNothing moveNorth) aStateWithOpponentWormOnTheBottomEdge `shouldBe`
      awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormUpFromTheBottomEdge
    -- Right edge
    it "moving my worm east from the right edge results in no change" $
      makeMove True (fromMoves moveEast doNothing) aStateWithMyWormOnTheRightEdge `shouldBe`
      penaliseThisPlayerForAnInvalidCommand aStateWithMyWormOnTheRightEdge
    it "moving opponent worm east from the right edge results in no change" $
      makeMove True (fromMoves doNothing moveEast) aStateWithOpponentWormOnTheRightEdge `shouldBe`
      penaliseThatPlayerForAnInvalidCommand aStateWithOpponentWormOnTheRightEdge
    it "moving my worm north from the right edge results in that worm moving up" $
      makeMove True (fromMoves moveNorth doNothing) aStateWithMyWormOnTheRightEdge `shouldBe`
      awardPointsToThisPlayerForMovingToAir aStateWithMyWormOnTheRightEdgeMovedUp
    it "moving opponent worm north from the right edge results in that worm moving up" $
      makeMove True (fromMoves doNothing moveNorth) aStateWithOpponentWormOnTheRightEdge `shouldBe`
      awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormOnTheRightEdgeMovedUp
    it "moving my worm south from the right edge results in that worm moving down" $
      makeMove True (fromMoves moveSouth doNothing) aStateWithMyWormOnTheRightEdge `shouldBe`
      awardPointsToThisPlayerForMovingToAir aStateWithMyWormOnTheRightEdgeMovedDown
    it "moving opponent worm south from the right edge results in that worm moving down" $
      makeMove True (fromMoves doNothing moveSouth) aStateWithOpponentWormOnTheRightEdge `shouldBe`
      awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormOnTheRightEdgeMovedDown
    it "moving my worm to the west from the right edge results in that worm moving left" $
      makeMove True (fromMoves moveWest doNothing) aStateWithMyWormOnTheRightEdge `shouldBe`
      awardPointsToThisPlayerForMovingToAir aStateWithMyWormMovedLeftFromTheRightEdge
    it "moving opponent worm to the west from the right edge results in that worm moving left" $
      makeMove True (fromMoves doNothing moveWest) aStateWithOpponentWormOnTheRightEdge `shouldBe`
      awardPointsToThatPlayerForMovingToAir aStateWithOpponentWormMovedLeftFromTheRightEdge
    -- Digging
    it "should remove dirt when my player digs a dirt block" $
      makeMove True (fromMoves digNorth doNothing) aState `shouldBe`
      awardPointsToThisPlayerForDigging aStateWithDirtMissingAboveMyWorm
    it "should remove dirt when opponent digs a dirt block" $
      makeMove True (fromMoves doNothing digNorth) aStateWithOpponentBeneathDirt `shouldBe`
      awardPointsToThatPlayerForDigging aStateWithDirtMissingAboveOpponentWorm
    -- Shooting
    prop "should hit this players first horizontal target in range when it's an opponent worm" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithNonDiagonalPadding
                                                  inBoundsWithNoPadding)
                          (generateCoordDisplacer nonDiagonalDelta addDelta ignoreDelta)
                          (generateShotSwitch shootEast shootWest)
                          takeThisWorm
                          takeThatWorm
                          (i, j, k)
      in makeMove True (fromMoves shot doNothing) state `shouldBe`
         mapThoseWorms (modifyWormWithId 1 (withHealthOf 0)) state
    prop "should hit this players first horizontal target in range when it's a friendly worm" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithNonDiagonalPadding
                                                  inBoundsWithNoPadding)
                          (generateCoordDisplacer nonDiagonalDelta addDelta ignoreDelta)
                          (generateShotSwitch shootEast shootWest)
                          takeBothWorms
                          takeNoWorms
                          (i, j, k)
      in makeMove True (fromMoves shot doNothing) state `shouldBe`
         mapTheseWorms (modifyWormWithId 3 (withHealthOf 0)) state
    prop "should hit this players first vertical target in range when it's an opponent worm" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithNoPadding
                                                  inBoundsWithNonDiagonalPadding)
                          (generateCoordDisplacer nonDiagonalDelta ignoreDelta addDelta)
                          (generateShotSwitch shootSouth shootNorth)
                          takeThisWorm
                          takeThatWorm
                          (i, j, k)
      in makeMove True (fromMoves shot doNothing) state `shouldBe`
         mapThoseWorms (modifyWormWithId 1 (withHealthOf 0)) state
    prop "should hit this players first vertical target in range when it's a friendly worm" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithNoPadding
                                                  inBoundsWithNonDiagonalPadding)
                          (generateCoordDisplacer nonDiagonalDelta ignoreDelta addDelta)
                          (generateShotSwitch shootSouth shootNorth)
                          takeBothWorms
                          takeNoWorms
                          (i, j, k)
      in makeMove True (fromMoves shot doNothing) state `shouldBe`
         mapTheseWorms (modifyWormWithId 3 (withHealthOf 0)) state
    prop "should hit this players first NW-SE diagonal target in range when it's an opponent worm" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithDiagonalPadding
                                                  inBoundsWithDiagonalPadding)
                          (generateCoordDisplacer diagonalDelta addDelta addDelta)
                          (generateShotSwitch shootSouthEast shootNorthWest)
                          takeThisWorm
                          takeThatWorm
                          (i, j, k)
      in makeMove True (fromMoves shot doNothing) state `shouldBe`
         mapThoseWorms (modifyWormWithId 1 (withHealthOf 0)) state
    prop "should hit this players first NW-SE diagonal target in range when it's a friendly worm" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithDiagonalPadding
                                                  inBoundsWithDiagonalPadding)
                          (generateCoordDisplacer diagonalDelta addDelta addDelta)
                          (generateShotSwitch shootSouthEast shootNorthWest)
                          takeBothWorms
                          takeNoWorms
                          (i, j, k)
      in makeMove True (fromMoves shot doNothing) state `shouldBe`
         mapTheseWorms (modifyWormWithId 3 (withHealthOf 0)) state
    prop "should hit this players first NE-SW diagonal target in range when it's an opponent worm" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithDiagonalPadding
                                                  inBoundsWithDiagonalPadding)
                          (generateCoordDisplacer diagonalDelta addDelta subtractDelta)
                          (generateShotSwitch     shootNorthEast shootSouthWest)
                          takeThisWorm
                          takeThatWorm
                          (i, j, k)
      in makeMove True (fromMoves shot doNothing) state `shouldBe`
         mapThoseWorms (modifyWormWithId 1 (withHealthOf 0)) state
    prop "should hit that players first horizontal target in range when it's my worm" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithNonDiagonalPadding
                                                  inBoundsWithNoPadding)
                          (generateCoordDisplacer nonDiagonalDelta addDelta ignoreDelta)
                          (generateShotSwitch     shootEast shootWest)
                          takeThatWorm
                          takeThisWorm
                          (i, j, k)
      in makeMove True (fromMoves doNothing shot) state `shouldBe`
         mapTheseWorms (modifyWormWithId 1 (withHealthOf 0)) state
    prop "should hit that players first horizontal target in range when it's friendly" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithNonDiagonalPadding
                                                  inBoundsWithNoPadding)
                          (generateCoordDisplacer nonDiagonalDelta addDelta ignoreDelta)
                          (generateShotSwitch     shootEast shootWest)
                          takeNoWorms
                          takeBothWorms
                          (i, j, k)
      in makeMove True (fromMoves doNothing shot) state `shouldBe`
         mapThoseWorms (modifyWormWithId 3 (withHealthOf 0)) state
    prop "should hit that players first vertical target in range when it's my worm" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithNoPadding
                                                  inBoundsWithNonDiagonalPadding)
                          (generateCoordDisplacer nonDiagonalDelta ignoreDelta addDelta)
                          (generateShotSwitch     shootSouth shootNorth)
                          takeThatWorm
                          takeThisWorm
                          (i, j, k)
      in makeMove True (fromMoves doNothing shot) state `shouldBe`
         mapTheseWorms (modifyWormWithId 1 (withHealthOf 0)) state
    prop "should hit that players first vertical target in range when it's a friendly worm" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithNoPadding
                                                  inBoundsWithNonDiagonalPadding)
                          (generateCoordDisplacer nonDiagonalDelta ignoreDelta addDelta)
                          (generateShotSwitch     shootSouth shootNorth)
                          takeNoWorms
                          takeBothWorms
                          (i, j, k)
      in makeMove True (fromMoves doNothing shot) state `shouldBe`
         mapThoseWorms (modifyWormWithId 3 (withHealthOf 0)) state
    prop "should hit that players first NW-SE diagonal target in range when it's my worm" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithDiagonalPadding
                                                  inBoundsWithDiagonalPadding)
                          (generateCoordDisplacer diagonalDelta addDelta addDelta)
                          (generateShotSwitch     shootSouthEast shootNorthWest)
                          takeThatWorm
                          takeThisWorm
                          (i, j, k)
      in makeMove True (fromMoves doNothing shot) state `shouldBe`
         mapTheseWorms (modifyWormWithId 1 (withHealthOf 0)) state
    prop "should hit that players first NW-SE diagonal target in range when it's a friendly worm" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithDiagonalPadding
                                                  inBoundsWithDiagonalPadding)
                          (generateCoordDisplacer diagonalDelta addDelta addDelta)
                          (generateShotSwitch     shootSouthEast shootNorthWest)
                          takeNoWorms
                          takeBothWorms
                          (i, j, k)
      in makeMove True (fromMoves doNothing shot) state `shouldBe`
         mapThoseWorms (modifyWormWithId 3 (withHealthOf 0)) state
    prop "should hit that players first NE-SW diagonal target in range when it's my worm" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithDiagonalPadding
                                                  inBoundsWithDiagonalPadding)
                          (generateCoordDisplacer diagonalDelta addDelta subtractDelta)
                          (generateShotSwitch     shootNorthEast shootSouthWest)
                          takeThatWorm
                          takeThisWorm
                          (i, j, k)
      in makeMove True (fromMoves doNothing shot) state `shouldBe`
         mapTheseWorms (modifyWormWithId 1 (withHealthOf 0)) state
    prop "should not hit this players first horizontal target in range when there's dirt or space in the way" $ \ (i, j, k, l) ->
      let (state, shot) = generateShotScenarioWithMapModifications
                          (generateCoordGenerator inBoundsWithNonDiagonalPadding
                                                  inBoundsWithNoPadding)
                          (generateCoordDisplacer nonDiagonalDeltaOfAtLeastTwo addDelta ignoreDelta)
                          (generateShotSwitch     shootEast shootWest)
                          takeThisWorm
                          takeThatWorm
                          (putDirtOrSpaceBetweenWorms l)
                          (i, j, k)
      in makeMove True (fromMoves shot doNothing) state `shouldBe`
         state
    prop "should not hit this players first horizontal target in range when there's a friendly worm in the way" $ \ (i, j, k) ->
      let (state, shot) = generateShotScenario
                          (generateCoordGenerator inBoundsWithNonDiagonalPadding
                                                  inBoundsWithNoPadding)
                          (generateCoordDisplacer nonDiagonalDeltaOfAtLeastTwo addDelta ignoreDelta)
                          (generateShotSwitch     shootEast shootWest)
                          takeThisWormAndPutAnotherInbetween
                          takeThatWorm
                          (i, j, k)
      in makeMove True (fromMoves shot doNothing) state `shouldSatisfy`
        (oneWormHarmed 10 . thisPlayersWorms) .&&. (noWormHarmed 10 . thatPlayersWorms)

emptyWormHealth = AList []

(.&&.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.&&.) p1 p2 x =
  p1 x && p2 x

oneWormHarmed :: Int -> Worms -> Bool
oneWormHarmed originalHealth =
  (== 1) . countWorms ((/= originalHealth) . wormsHealth)

noWormHarmed :: Int -> Worms -> Bool
noWormHarmed originalHealth =
  allWorms (notHarmed originalHealth)

notHarmed :: Int -> Worm -> Bool
notHarmed originalHealth =
  (== originalHealth) . wormsHealth

allWorms :: (Worm -> Bool) -> Worms -> Bool
allWorms f = M.foldl' ( \ acc worm -> acc && f worm) True

countWorms :: (Worm -> Bool) -> Worms -> Int
countWorms f = M.foldl' ( \ acc worm -> acc + if f worm then 1 else 0) 0

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

mapThoseWorms :: (Worms -> Worms) -> State -> State
mapThoseWorms f state@(State { opponent = opponent' }) =
  state { opponent = mapWorms opponent' f }

mapTheseWorms :: (Worms -> Worms) -> State -> State
mapTheseWorms f state@(State { myPlayer = myPlayer' }) =
  state { myPlayer = mapWorms myPlayer' f }

generateShotSwitch :: Move -> Move -> ShotSwitch
generateShotSwitch a b x =
  if x > 0 then a else b

playersWorms :: Player -> Worms
playersWorms (Player _ _ worms') = worms'

generateInBoundsCoordinate :: Int -> Int -> Coord
generateInBoundsCoordinate = generateCoordGenerator inBoundsWithNoPadding inBoundsWithNoPadding

takeBothWorms :: GeneratePlayer
takeBothWorms thisCoord thatCoord =
  Player 300 1 $ wormsToMap $ V.fromList [Worm 1 10 thisCoord, Worm 3 10 thatCoord]

takeThisWormAndPutAnotherInbetween :: GeneratePlayer
takeThisWormAndPutAnotherInbetween thisCoord thatCoord =
  Player 300 1 $ wormsToMap $ V.fromList [Worm 1 10 thisCoord,
                                          Worm 2 10 $ coordBetween thisCoord thatCoord]

takeThisWorm :: GeneratePlayer
takeThisWorm thisCoord _ =
  Player 300 1 $ wormsToMap $ V.fromList [Worm 1 10 thisCoord]

takeThatWorm :: GeneratePlayer
takeThatWorm _ thatCoord =
  Player 300 1 $ wormsToMap $ V.fromList [Worm 1 10 thatCoord]

takeNoWorms :: GeneratePlayer
takeNoWorms _ _ =
  Player 300 1 $ wormsToMap $ V.fromList []

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
type GeneratePlayer         = Coord -> Coord -> Player
type ModifyMap              = Coord -> Coord -> GameMap -> GameMap

generateShotScenarioWithMapModifications :: GenerateCoordinate -> DisplaceFromCoordinate -> ShotSwitch -> GeneratePlayer -> GeneratePlayer -> ModifyMap -> (Int, Int, Int) -> (State, Move)
generateShotScenarioWithMapModifications generateCoord displace switchShot generateThisPlayer generateThatPlayer modifyMap (i, j, k) =
  let originatingCoord        = generateCoord i j
      (displacedCoord, delta) = displace      originatingCoord k
      shot                    = switchShot    delta
      thisPlayer              = generateThisPlayer originatingCoord displacedCoord
      thatPlayer              = generateThatPlayer originatingCoord displacedCoord
      modifiedMap             = modifyMap originatingCoord displacedCoord aGameMapWithOnlyAir
      state                   = State 10 10 10 10 (AList []) thisPlayer thatPlayer modifiedMap
  in (state, shot)

generateShotScenario :: GenerateCoordinate -> DisplaceFromCoordinate -> ShotSwitch -> GeneratePlayer -> GeneratePlayer -> (Int, Int, Int) -> (State, Move)
generateShotScenario generateCoord displace switchShot generateThisPlayer generateThatPlayer (i, j, k) =
  generateShotScenarioWithMapModifications generateCoord
                                           displace
                                           switchShot
                                           generateThisPlayer
                                           generateThatPlayer
                                           identityMapModification
                                           (i, j, k)

identityMapModification :: ModifyMap
identityMapModification _ _ = id

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

doNothing = Move 16

moveNorth = Move 8

digNorth = moveNorth

moveSouth = Move 12

moveEast = Move 10

moveWest = Move 14

aState = State 10 10 10 10 emptyWormHealth aPlayer anOpponent aGameMap

aStateWithOpponentBeneathDirt =
  mapThatWorm aState (withCoordOf (toCoord 14 31))

aStateWithDirtMissingAboveOpponentWorm =
  mapGameMap aStateWithOpponentBeneathDirt (removeDirtAt (toCoord 14 30))

aStateWithDirtMissingAboveMyWorm =
  mapGameMap aState (removeDirtAt (toCoord 15 30))

aStateWithOpponentWormMovedLeftFromTheRightEdge =
  mapThatWorm aStateWithOpponentWormOnTheRightEdge (withCoordOf (toCoord 31 15))

aStateWithOpponentWormOnTheRightEdgeMovedDown =
  mapThatWorm aStateWithOpponentWormOnTheRightEdge (withCoordOf (toCoord 32 16))

aStateWithOpponentWormOnTheRightEdgeMovedUp =
  mapThatWorm aStateWithOpponentWormOnTheRightEdge (withCoordOf (toCoord 32 14))

aStateWithOpponentWormOnTheRightEdge =
  mapThatWorm aStateWithOnlyAirOnMap (withCoordOf (toCoord 32 15))

aStateWithOpponentWormUpFromTheBottomEdge =
  mapThatWorm aStateWithOpponentWormOnTheBottomEdge (withCoordOf (toCoord 4 31))

aStateWithOpponentWormOnTheBottomEdgeMovedLeft =
  mapThatWorm aStateWithOpponentWormOnTheBottomEdge (withCoordOf (toCoord 3 32))

aStateWithOpponentWormOnTheBottomEdgeMovedRight =
  mapThatWorm aStateWithOpponentWormOnTheBottomEdge (withCoordOf (toCoord 5 32))

aStateWithOpponentWormOnTheBottomEdge =
  mapThatWorm aStateWithOnlyAirOnMap (withCoordOf (toCoord 4 32))

aStateWithOpponentWormDownwardOnLeftEdge =
  mapThatWorm aStateWithOnlyAirOnMap (withCoordOf (toCoord 0 16))

aStateWithOpponentWormUpwardOnLeftEdge =
  mapThatWorm aStateWithOnlyAirOnMap (withCoordOf (toCoord 0 14))

aStateWithOpponentWormRightFromLeftEdge =
  mapThatWorm aStateWithOnlyAirOnMap (withCoordOf (toCoord 1 15))

aStateWithOpponentWormOnLeftEdge =
  mapThatWorm aStateWithOnlyAirOnMap (withCoordOf (toCoord 0 15))

aStateWithOpponentWormOnTop = aStateWithOnlyAirOnMap {
  opponent = opponentWithAWormAtTop }

aStateWithOpponentWormOnTopMovedRight =
  mapThatWorm aStateWithOpponentWormOnTop (withCoordOf (toCoord 16 0))

aStateWithOpponentWormOnTopMovedLeft =
  mapThatWorm aStateWithOpponentWormOnTop (withCoordOf (toCoord 14 0))

aStateWithOpponentWormOnTopMovedDown =
  mapThatWorm aStateWithOpponentWormOnTop (withCoordOf (toCoord 15 1))

aStateWithOnlyAirOnMap = aState {
  gameMap = aGameMapWithOnlyAir }

aStateWithMyWormDownwardOnLeftEdge =
  mapThisWorm aStateWithOnlyAirOnMap (withCoordOf (toCoord 0 16))

aStateWithMyWormRightFromLeftEdge =
  mapThisWorm aStateWithOnlyAirOnMap (withCoordOf (toCoord 1 15))

aStateWithMyWormUpwardsOnLeftEdge =
  mapThisWorm aStateWithOnlyAirOnMap (withCoordOf (toCoord 0 14))

aStateWithMyWormOnLeftEdge =
  mapThisWorm aStateWithOnlyAirOnMap (withCoordOf (toCoord 0 15))

aStateWithMyWormOnTop = aStateWithOnlyAirOnMap {
  myPlayer = aPlayerWithAWormAtTop }

aStateWithMyWormOnTopMovedRight =
  mapThisWorm aStateWithMyWormOnTop (withCoordOf (toCoord 16 0))

aStateWithMyWormOnTopMovedLeft =
  mapThisWorm aStateWithMyWormOnTop (withCoordOf (toCoord 14 0))

aStateWithMyWormOnTopMovedDown =
  mapThisWorm aStateWithMyWormOnTop (withCoordOf (toCoord 15 1))

aStateWithMyWormOnTheBottomEdge =
  mapThisWorm aStateWithOnlyAirOnMap (withCoordOf (toCoord 4 32))

aStateWithMyWormOnTheBottomEdgeMovedRight =
  mapThisWorm aStateWithMyWormOnTheBottomEdge (withCoordOf (toCoord 5 32))

aStateWithMyWormOnTheBottomEdgeMovedLeft =
  mapThisWorm aStateWithMyWormOnTheBottomEdge (withCoordOf (toCoord 3 32))

aStateWithMyWormUpFromTheBottomEdge =
  mapThisWorm aStateWithMyWormOnTheBottomEdge (withCoordOf (toCoord 4 31))

aStateWithMyWormOnTheRightEdge =
  mapThisWorm aStateWithOnlyAirOnMap (withCoordOf (toCoord 32 15))

aStateWithMyWormOnTheRightEdgeMovedUp =
  mapThisWorm aStateWithMyWormOnTheRightEdge (withCoordOf (toCoord 32 14))

aStateWithMyWormOnTheRightEdgeMovedDown =
  mapThisWorm aStateWithMyWormOnTheRightEdge (withCoordOf (toCoord 32 16))

aStateWithMyWormMovedLeftFromTheRightEdge =
  mapThisWorm aStateWithMyWormOnTheRightEdge (withCoordOf (toCoord 31 15))

aStateWhereWeSwappedOverTheMedipack =
  knockBackDamage $
  (flip mapThisWorm) (withCoordOf (toCoord 31 30)) $
  (flip mapThatWorm) (withCoordOf (toCoord 30 31)) $
  aState { gameMap = aGameMapWithAMedipack }

aStateWhereNoSwapHappened =
  aStateWithBothWormsNextToTheMedipack

aStateWithBothWormsNextToTheMedipack = aState {
  opponent = opponentWithAWormNextToTheMedipack,
  gameMap  = aGameMapWithAMedipack,
  myPlayer = aPlayerWithAWormNextToTheMedipack }

aStateWithOpponentsWormNextToTheMedipack = aState {
  opponent = opponentWithAWormNextToTheMedipack,
  gameMap = aGameMapWithAMedipack }

aStateWithOpponentsWormOnTheMedipack = aState { opponent = opponentWithAWormOnTheMedipack }

aStateWithMyWormNextToTheMedipack = aState {
  myPlayer = aPlayerWithAWormNextToTheMedipack,
  gameMap = aGameMapWithAMedipack }

aStateWithMyWormOnTheMedipack = aState { myPlayer = aPlayerWithAWormOnTheMedipack }

aStateWithEnemyWormsNextToEachother = aState { opponent = opponentWithHisWormsNextToEachother }

aStateWithMyWormNextToAnEnemy = aState { opponent = opponentWithHisWormNextToMine }

aStateWithMyWormsNextToEachOther = aState { myPlayer = aPlayerWithWormsNextToEachother }

aStateWithImpendingCollision = aState { opponent = anOpponentWithImpendingCollision }

anOpponent = withWorms someOtherWorms aPlayer

anOpponentWithImpendingCollision = withWorms someOtherWormsWithImpendingCollision anOpponent

opponentWithCollisionResolvedByNotMoving =
  withWorms someOtherWormsWithCollisionResolvedInHisFavour anOpponent

opponentWithCollisionResolvedBySwapping =
  withWorms someOtherWormsWithCollisionResolvedBySwapping anOpponent

opponentWithHisWormNextToMine =
  withWorms someOtherWormsWithAWormNextToMine anOpponent

opponentWithHisWormsNextToEachother =
  withWorms someOtherWormsWithTwoNextToEachother anOpponent

opponentWithAWormNextToTheMedipack =
  withWorms someOtherWormsWithOneNextToTheMedipack anOpponent

opponentWithAWormOnTheMedipack =
  withWorms someOtherWormsWithOneOnTheMedipack anOpponent

opponentWithAWormAtTop =
  withWorms someOtherWormsWithOneAtTop anOpponent

withWorms worms' (Player health' wormId' _) = Player health' wormId' worms'

aPlayerWithCollisionResolvedBySwapping =
  withWorms someWormsWithCollisionResolvedBySwapping aPlayer

aPlayerWithCollisionResolvedByNotMoving =
  withWorms someWormsWithCollisionResolvedByNotMoving aPlayer

aPlayerWithWormsNextToEachother =
  withWorms someWormsWithWormsNextToEachother aPlayer

aPlayerWithAWormNextToTheMedipack =
  withWorms someWormsWithOneNextToTheMedipack aPlayer

aPlayerWithAWormOnTheMedipack =
  withWorms someWormsWithOneOnTheMedipack aPlayer

aPlayerWithAWormAtTop =
  withWorms someWormsWithOneAtTop aPlayer

aPlayer = Player 300 1 someWorms

thisWorm1 = aWorm
thisWorm2 = withCoordOf (toCoord 1 31) $ withIdOf 2 aWorm
thisWorm3 = withCoordOf (toCoord 1 30) $ withIdOf 3 aWorm
thisWorm5 = withCoordOf (toCoord 1 16) $ withIdOf 5 aWorm

someWorms = wormsToMap $ V.fromList [thisWorm1, thisWorm2, thisWorm3, thisWorm5]

someWormsWithCurrentMovedEast =
  modifyWormWithId 1 (withCoordOf (toCoord 16 31)) someWorms

someWormsWithCollisionResolvedBySwapping =
  modifyWormWithId 1 (withHealthOf 9 . withCoordOf (toCoord 17 31)) someWorms

someWormsWithCollisionResolvedByNotMoving =
  modifyWormWithId 1 (withHealthOf 9 . withCoordOf (toCoord 15 31)) someWorms

someWormsWithWormsNextToEachother =
  modifyWormWithId 2 (withCoordOf (toCoord 16 31)) someWorms

someWormsWithOneNextToTheMedipack =
  modifyWormWithId 1 (withCoordOf (toCoord 30 31)) someWorms

someWormsWithOneOnTheMedipack =
  modifyWormWithId 1 (withHealthOf 20 . withCoordOf (toCoord 31 31)) someWorms

someWormsWithOneAtTop =
  modifyWormWithId 1 (withCoordOf (toCoord 15 0)) someWorms

thatWorm1 = withCoordOf (toCoord 16 1) aWorm
thatWorm3 = withCoordOf (toCoord 19 1) $ withIdOf 3 aWorm
thatWorm4 = withCoordOf (toCoord 20 1) $ withIdOf 4 aWorm
thatWorm5 = withCoordOf (toCoord 1 20) $ withIdOf 5 aWorm

someOtherWorms = wormsToMap $ V.fromList [
  thatWorm1,
  thatWorm3,
  thatWorm4,
  thatWorm5]

someOtherWormsWithCurrentMovedEast =
  modifyWormWithId 1 (withCoordOf (toCoord 17 1)) someOtherWorms

someOtherWormsWithImpendingCollision =
  modifyWormWithId 1 (withCoordOf (toCoord 17 31)) someOtherWorms

someOtherWormsWithCollisionResolvedInHisFavour =
  modifyWormWithId 1 (withHealthOf 9 . withCoordOf (toCoord 17 31)) someOtherWorms

someOtherWormsWithCollisionResolvedBySwapping =
  modifyWormWithId 1 (withHealthOf 9 . withCoordOf (toCoord 15 31)) someOtherWorms

someOtherWormsWithAWormNextToMine =
  modifyWormWithId 1 (withCoordOf (toCoord 16 31)) someOtherWorms

someOtherWormsWithTwoNextToEachother =
  modifyWormWithId 1 (withCoordOf (toCoord 20 1)) someOtherWorms

someOtherWormsWithOneNextToTheMedipack =
  modifyWormWithId 1 (withCoordOf (toCoord 31 30)) someOtherWorms

someOtherWormsWithOneOnTheMedipack =
  modifyWormWithId 1 (withHealthOf 20 . withCoordOf (toCoord 31 31)) someOtherWorms

someOtherWormsWithOneAtTop =
  modifyWormWithId 1 (withCoordOf (toCoord 15 0)) someOtherWorms

modifyWormWithId = flip M.adjust

aWorm = Worm 1 10 $ toCoord 15 31

withMyCurrentWormIdOf id' =
  mapThisPlayer (withCurrentWormId id')

withOpponentCurrentWormIdOf id' =
  mapThatPlayer (withCurrentWormId id')

withCurrentWormId id' (Player score' _ worms') = Player score' id' worms'

withIdOf id' (Worm _ health' position') = Worm id' health' position'

withHealthOf health' (Worm id' _ position') = Worm id' health' position'

withCoordOf position' (Worm id' health' _) = Worm id' health' position'

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
