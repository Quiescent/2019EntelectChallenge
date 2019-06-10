{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module BotSpec (spec) where

import Bot
import Import

import qualified RIO.Vector.Boxed as V
import qualified RIO.HashMap as M
import RIO.List

import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
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
      thisCurrentWorm (aState { currentWormId = -1 }) `shouldBe` Nothing
    it "shouldn't be found when the index is greater than the original number of worms" $
      thisCurrentWorm (aState { currentWormId = 6 }) `shouldBe` Nothing
    it "shouldn't be found when searching for a worm in a gap in the sequence" $
      thisCurrentWorm (aState { currentWormId = 4 }) `shouldBe` Nothing
    it "should find a worm when it's in the sequence" $
      thisCurrentWorm (aState { currentWormId = 3 }) `shouldBe` (Just $ withIdOf 3 thisWorm3)
  describe "thatCurrentWorm" $ do
    it "shouldn't be found when the index is negative" $
      thatCurrentWorm (aState { currentWormId = -1 }) `shouldBe` Nothing
    it "shouldn't be found when the index is greater than the original number of worms" $
      thatCurrentWorm (aState { currentWormId = 6 }) `shouldBe` Nothing
    it "shouldn't be found when searching for a worm in a gap in the sequence" $
      thatCurrentWorm (aState { currentWormId = 2 }) `shouldBe` Nothing
    it "should find a worm when it's in the sequence" $
      thatCurrentWorm (aState { currentWormId = 3 }) `shouldBe` (Just $ withIdOf 3 thatWorm3)
  describe "thisPlayersWorms" $ do
    it "should produce this players worms" $
      thisPlayersWorms aState == someWorms
  describe "thatPlayersWorms" $ do
    it "should produce that players worms" $
      thatPlayersWorms aState == someOtherWorms
  describe "penaliseForInvalidCommand" $ do
    it "should reduce the given players score by 4" $
      penaliseForInvalidCommand aPlayer `shouldBe`
      Player 296 someWorms
  describe "penaliseThatPlayerForAnInvalidCommand" $ do
    it "should reduce the points of the opponent by 4" $
      penaliseThatPlayerForAnInvalidCommand aState `shouldBe`
      aState { opponent = Player 296 someOtherWorms }
  describe "penaliseThisPlayerForAnInvalidCommand" $ do
    it "should reduce the points of the player by 4" $
      penaliseThisPlayerForAnInvalidCommand aState `shouldBe`
      aState { myPlayer = Player 296 someWorms }
  describe "awardPointsForMovingToAir" $ do
    it "should increment the points of a player by 5" $
      awardPointsForMovingToAir aPlayer `shouldBe`
      Player 305 someWorms
  describe "awardPointsToThatPlayerForMovingToAir" $ do
    it "should increment the points of opponent by 5" $
      awardPointsToThatPlayerForMovingToAir aState `shouldBe`
      aState { opponent = Player 305 someOtherWorms }
  describe "awardPointsToThisPlayerForMovingToAir" $ do
    it "should increment the points of my player by 5" $
      awardPointsToThisPlayerForMovingToAir aState `shouldBe`
      aState { myPlayer = Player 305 someWorms }
  describe "awardPointsForDigging" $ do
    it "should increment the points of a player by 7" $
      awardPointsForDigging aPlayer `shouldBe`
      Player 307 someWorms
  describe "awardPointsToThisPlayerForDigging" $ do
    it "should increment this players points by 7" $
      awardPointsToThisPlayerForDigging aState `shouldBe`
      aState { myPlayer = Player 307 someWorms }
  describe "awardPointsToThatPlayerForDigging" $ do
    it "should increment that players points by 7" $
      awardPointsToThatPlayerForDigging aState `shouldBe`
      aState { opponent = Player 307 someOtherWorms }
  describe "harmWormWithRocket" $ do
    it "should remove health from the worm" $
      (harmWormWithRocket $ Worm 1 10 $ toCoord 15 31) `shouldBe`
      (Worm 1 0 $ toCoord 15 31)
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
      let thisX      = inBoundsWithNonDiagonalPadding i
          thisY      = inBoundsWithNoPadding j
          thisCoord  = toCoord thisX thisY
          delta      = nonDiagonalDelta k
          thatCoord  = toCoord (thisX + delta) thisY
          theseWorms = wormsToMap $ V.fromList $ [Worm 1 10 thisCoord]
          thoseWorms = wormsToMap $ V.fromList $ [Worm 1 10 thatCoord]
          thisPlayer = Player 300 theseWorms
          thatPlayer = Player 300 thoseWorms
          state      = State 1 10 10 10 10 thisPlayer thatPlayer aGameMapWithOnlyAir
          shot       = if delta > 0 then shootEast else shootWest
      in makeMove True (fromMoves shot doNothing) state `shouldBe`
         state { opponent = (Player 300 (modifyWormWithId 1 (withHealthOf 0) thoseWorms)) }
    prop "should hit this players first horizontal target in range when it's a friendly worm" $ \ (i, j, k) ->
      let thisX      = inBoundsWithNonDiagonalPadding i
          thisY      = inBoundsWithNoPadding j
          thisCoord  = toCoord thisX thisY
          delta      = nonDiagonalDelta k
          thatCoord  = toCoord (thisX + delta) thisY
          theseWorms = wormsToMap $ V.fromList $ [Worm 1 10 thisCoord, Worm 3 10 thatCoord]
          thoseWorms = wormsToMap $ V.fromList $ []
          thisPlayer = Player 300 theseWorms
          thatPlayer = Player 300 thoseWorms
          state      = State 1 10 10 10 10 thisPlayer thatPlayer aGameMapWithOnlyAir
          shot       = if delta > 0 then shootEast else shootWest
      in makeMove True (fromMoves shot doNothing) state `shouldBe`
         state { myPlayer = (Player 300 (modifyWormWithId 3 (withHealthOf 0) theseWorms)) }
    prop "should hit this players first vertical target in range when it's an opponent worm" $ \ (i, j, k) ->
      let thisX      = inBoundsWithNoPadding i
          thisY      = inBoundsWithNonDiagonalPadding j
          thisCoord  = toCoord thisX thisY
          delta      = nonDiagonalDelta k
          thatCoord  = toCoord thisX (thisY + delta)
          theseWorms = wormsToMap $ V.fromList $ [Worm 1 10 thisCoord]
          thoseWorms = wormsToMap $ V.fromList $ [Worm 1 10 thatCoord]
          thisPlayer = Player 300 theseWorms
          thatPlayer = Player 300 thoseWorms
          state      = State 1 10 10 10 10 thisPlayer thatPlayer aGameMapWithOnlyAir
          shot       = if delta > 0 then shootSouth else shootNorth
      in makeMove True (fromMoves shot doNothing) state `shouldBe`
         state { opponent = (Player 300 (modifyWormWithId 1 (withHealthOf 0) thoseWorms)) }
    prop "should hit this players first vertical target in range when it's a friendly worm" $ \ (i, j, k) ->
      let thisX      = inBoundsWithNoPadding i
          thisY      = inBoundsWithNonDiagonalPadding j
          thisCoord  = toCoord thisX thisY
          delta      = nonDiagonalDelta k
          thatCoord  = toCoord thisX (thisY + delta)
          theseWorms = wormsToMap $ V.fromList $ [Worm 1 10 thisCoord, Worm 3 10 thatCoord]
          thoseWorms = wormsToMap $ V.fromList $ []
          thisPlayer = Player 300 theseWorms
          thatPlayer = Player 300 thoseWorms
          state      = State 1 10 10 10 10 thisPlayer thatPlayer aGameMapWithOnlyAir
          shot       = if delta > 0 then shootSouth else shootNorth
      in makeMove True (fromMoves shot doNothing) state `shouldBe`
         state { myPlayer = (Player 300 (modifyWormWithId 3 (withHealthOf 0) theseWorms)) }
    prop "should hit this players first NW-SE diagonal target in range when it's an opponent worm" $ \ (i, j, k) ->
      let thisX      = inBoundsWithDiagonalPadding i
          thisY      = inBoundsWithDiagonalPadding j
          thisCoord  = toCoord thisX thisY
          delta      = diagonalDelta k
          thatCoord  = toCoord (thisX + delta) (thisY + delta)
          theseWorms = wormsToMap $ V.fromList $ [Worm 1 10 thisCoord]
          thoseWorms = wormsToMap $ V.fromList $ [Worm 1 10 thatCoord]
          thisPlayer = Player 300 theseWorms
          thatPlayer = Player 300 thoseWorms
          state      = State 1 10 10 10 10 thisPlayer thatPlayer aGameMapWithOnlyAir
          shot       = if delta > 0 then shootSouthEast else shootNorthWest
      in makeMove True (fromMoves shot doNothing) state `shouldBe`
         state { opponent = (Player 300 (modifyWormWithId 1 (withHealthOf 0) thoseWorms)) }
    prop "should hit this players first NW-SE diagonal target in range when it's a friendly worm" $ \ (i, j, k) ->
      let thisX      = inBoundsWithDiagonalPadding i
          thisY      = inBoundsWithDiagonalPadding j
          thisCoord  = toCoord thisX thisY
          delta      = diagonalDelta k
          thatCoord  = toCoord (thisX + delta) (thisY + delta)
          theseWorms = wormsToMap $ V.fromList $ [Worm 1 10 thisCoord, Worm 3 10 thatCoord]
          thoseWorms = wormsToMap $ V.fromList $ []
          thisPlayer = Player 300 theseWorms
          thatPlayer = Player 300 thoseWorms
          state      = State 1 10 10 10 10 thisPlayer thatPlayer aGameMapWithOnlyAir
          shot       = if delta > 0 then shootSouthEast else shootNorthWest
      in makeMove True (fromMoves shot doNothing) state `shouldBe`
         state { myPlayer = (Player 300 (modifyWormWithId 3 (withHealthOf 0) theseWorms)) }
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

mapThoseWorms :: (Worms -> Worms) -> State -> State
mapThoseWorms f state@(State { opponent = opponent' }) =
  state { opponent = mapWorms opponent' f }

mapTheseWorms :: (Worms -> Worms) -> State -> State
mapTheseWorms f state@(State { myPlayer = myPlayer' }) =
  state { myPlayer = mapWorms myPlayer' f }

generateShotSwitch :: Move -> Move -> ShotSwitch
generateShotSwitch a b x =
  if x > 0 then a else b

takeBothWorms :: GeneratePlayer
takeBothWorms thisCoord thatCoord =
  Player 300 $ wormsToMap $ V.fromList [Worm 1 10 thisCoord, Worm 1 10 thatCoord]

takeThisWorm :: GeneratePlayer
takeThisWorm thisCoord _ =
  Player 300 $ wormsToMap $ V.fromList [Worm 1 10 thisCoord]

takeThatWorm :: GeneratePlayer
takeThatWorm _ thatCoord =
  Player 300 $ wormsToMap $ V.fromList [Worm 1 10 thatCoord]

takeNoWorms :: GeneratePlayer
takeNoWorms _ _ =
  Player 300 $ wormsToMap $ V.fromList []

addDelta :: Int -> Int -> Int
addDelta x delta = x + delta

subtractDelta :: Int -> Int -> Int
subtractDelta x delta = x - delta

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

generateShotScenario :: GenerateCoordinate -> DisplaceFromCoordinate -> ShotSwitch -> GeneratePlayer -> GeneratePlayer -> (Int, Int, Int) -> (State, Move)
generateShotScenario generateCoord displace switchShot generateThisPlayer generateThatPlayer (i, j, k) =
  let originatingCoord        = generateCoord i j
      (displacedCoord, delta) = displace      originatingCoord k
      shot                    = switchShot    delta
      thisPlayer              = generateThisPlayer originatingCoord displacedCoord
      thatPlayer              = generateThatPlayer originatingCoord displacedCoord
      state                   = State 1 10 10 10 10 thisPlayer thatPlayer aGameMapWithOnlyAir
  in (state, shot)

nonDiagonalDelta x =
  let y = (x `mod` 7) - 3
  in if y == 0 then -1 else y

diagonalDelta x =
  let y = (x `mod` 5) - 2
  in if y == 0 then -1 else y

inBoundsWithNonDiagonalPadding x = 3 + (x `mod` (mapDim - 6))

inBoundsWithDiagonalPadding x = 3 + (x `mod` (mapDim - 4))

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

aState = State 1 10 10 10 10 aPlayer anOpponent aGameMap

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

withWorms worms' (Player health' _) = Player health' worms'

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

aPlayer = Player 300 someWorms

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
