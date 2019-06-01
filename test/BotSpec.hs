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
    prop "Move back and forth" $ \ (i, j) ->
      let coordInMap  = Coord $ mapDim + ((abs j) `mod` (mapDim * mapDim - 2 * mapDim))
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
      let x' = (abs i) `mod` mapDim
          y' = (abs j) `mod` mapDim
      in fromCoord (toCoord x' y') `shouldBe` (x', y')
  describe "thisCurrentWorm" $ do
    it "shouldn't be found when the index is negative" $
      thisCurrentWorm (aState { currentWormId = -1 }) `shouldBe` Nothing
    it "shouldn't be found when the index is greater than the original number of worms" $
      thisCurrentWorm (aState { currentWormId = 6 }) `shouldBe` Nothing
    it "shouldn't be found when searching for a worm in a gap in the sequence" $
      thisCurrentWorm (aState { currentWormId = 4 }) `shouldBe` Nothing
    it "should find a worm when it's in the sequence" $
      thisCurrentWorm (aState {currentWormId = 3}) `shouldBe` (Just $ withIdOf 3 thisWorm3)
  describe "thatCurrentWorm" $ do
    it "shouldn't be found when the index is negative" $
      thatCurrentWorm (aState { currentWormId = -1 }) `shouldBe` Nothing
    it "shouldn't be found when the index is greater than the original number of worms" $
      thatCurrentWorm (aState { currentWormId = 6 }) `shouldBe` Nothing
    it "shouldn't be found when searching for a worm in a gap in the sequence" $
      thatCurrentWorm (aState { currentWormId = 2 }) `shouldBe` Nothing
    it "should find a worm when it's in the sequence" $
      thatCurrentWorm (aState {currentWormId = 3}) `shouldBe` (Just $ withIdOf 3 thatWorm3)
  describe "thisPlayersWorms" $ do
    it "should produce this players worms" $
      thisPlayersWorms aState == someWorms
  describe "thatPlayersWorms" $ do
    it "should produce that players worms" $
      thatPlayersWorms aState == someOtherWorms
  describe "makeMove" $ do
    -- TODO make this a property test...?
    it "should not change anything when it receives two 'nothing's" $
      makeMove True (fromMoves doNothing doNothing) aState `shouldBe` aState
    it "moving my worm to dirt should not move the worm" $
      makeMove True (fromMoves moveNorth doNothing) aState `shouldBe` aState
    it "moving opponents worm to dirt should not move the worm" $
      makeMove True (fromMoves doNothing moveNorth) aState `shouldBe` aState
    it "moving my worm into space should not move the worm" $
      makeMove True (fromMoves moveSouth doNothing) aState `shouldBe` aState
    it "moving opponents worm into space should not move the worm" $
      makeMove True (fromMoves doNothing moveSouth) aState `shouldBe` aState
    it "moving my worm into air should move the worm to that spot" $
      makeMove True (fromMoves moveEast doNothing) aState `shouldBe`
      aState { myPlayer = withWorms someWormsWithCurrentMovedEast aPlayer }
    it "moving opponents worm into air should move the worm to that spot" $
      makeMove True (fromMoves doNothing moveEast) aState `shouldBe`
      aState { opponent = withWorms someOtherWormsWithCurrentMovedEast anOpponent }
    it "moving to the same square should favour player if true and damage both worms" $
      makeMove True (fromMoves moveEast moveWest) aStateWithImpendingCollision `shouldBe`
      aStateWithImpendingCollision { myPlayer = aPlayerWithCollisionResolvedInMyFavour,
                                     opponent = opponentWithCollisionResolvedInMyFavour }
    it "moving to the same square should favour the opponent if false and damage both worms" $
      makeMove False (fromMoves moveEast moveWest) aStateWithImpendingCollision `shouldBe`
      aStateWithImpendingCollision { myPlayer = aPlayerWithCollisionResolvedInHisFavour,
                                     opponent = opponentWithCollisionResolvedInHisFavour }
    it "moving my worm to a square occupied by one of my worms does nothing" $
      makeMove True (fromMoves moveEast doNothing) aStateWithMyWormsNextToEachOther `shouldBe`
      aStateWithMyWormsNextToEachOther
    it "moving my worm to a square occupied by one of the the opponents worms does nothing " $
      makeMove True (fromMoves moveEast doNothing) aStateWithMyWormNextToAnEnemy `shouldBe`
      aStateWithMyWormNextToAnEnemy
    it "moving an opponents worm to a square occupied by one of my worms does nothing" $
      makeMove True (fromMoves doNothing moveWest) aStateWithMyWormNextToAnEnemy `shouldBe`
      aStateWithMyWormNextToAnEnemy
    it "moving an opponents worm to a square occupied by one of the opponents worms does nothing" $
      makeMove True (fromMoves doNothing moveWest) aStateWithEnemyWormsNextToEachother `shouldBe`
      aStateWithEnemyWormsNextToEachother
    it "moving my worm onto the medipack increases my worms health by 10 and changes that square to AIR" $
      makeMove True (fromMoves moveEast doNothing) aStateWithMyWormNextToTheMedipack `shouldBe`
      aStateWithMyWormOnTheMedipack
    it "moving the opponents worm onto the medipack should increase its health by ten and change that square to AIR" $
      makeMove True (fromMoves doNothing moveSouth) aStateWithOpponentsWormNextToTheMedipack `shouldBe`
      aStateWithOpponentsWormOnTheMedipack
    it "moving both worms onto the same medipack results in this worm getting the medipack when this worm won" $
      makeMove True (fromMoves moveEast moveSouth) aStateWithBothWormNextToTheMedipack `shouldBe`
      aStateWhereIGotTheMedipack
    it "moving both worms onto the same medipack results that worm getting the medipack when that worm won" $
      makeMove False (fromMoves moveEast moveSouth) aStateWithBothWormNextToTheMedipack `shouldBe`
      aStateWhereOpponentGotTheMedipack
    -- Top
    it "moving my worm off the top edge of the map changes nothing" $
      makeMove True (fromMoves moveNorth doNothing) aStateWithMyWormOnTop `shouldBe`
      aStateWithMyWormOnTop
    it "moving opponent worm off the top edge of the map changes nothing" $
      makeMove True (fromMoves doNothing moveNorth) aStateWithOpponentWormOnTop `shouldBe`
      aStateWithOpponentWormOnTop
    it "moving my worm on the top to the east results in the worm moving east" $
      makeMove True (fromMoves moveEast doNothing) aStateWithMyWormOnTop `shouldBe`
      aStateWithMyWormOnTopMovedRight
    it "moving opponent worm on the top to the east results in the worm moving east" $
      makeMove True (fromMoves doNothing moveEast) aStateWithOpponentWormOnTop `shouldBe`
      aStateWithOpponentWormOnTopMovedRight
    it "moving my worm on the top to the west results in the worm moving west" $
      makeMove True (fromMoves moveWest doNothing) aStateWithMyWormOnTop `shouldBe`
      aStateWithMyWormOnTopMovedLeft
    it "moving opponent worm on the top to the west results in the worm moving west" $
      makeMove True (fromMoves doNothing moveWest) aStateWithOpponentWormOnTop `shouldBe`
      aStateWithOpponentWormOnTopMovedLeft
    it "moving my worm south from the top of the map results in that worm moving down" $
      makeMove True (fromMoves moveSouth doNothing) aStateWithMyWormOnTop `shouldBe`
      aStateWithMyWormOnTopMovedDown
    it "moving opponent worm south from the top of the map results in that worm moving down" $
      makeMove True (fromMoves doNothing moveSouth) aStateWithOpponentWormOnTop `shouldBe`
      aStateWithOpponentWormOnTopMovedDown
    -- Left edge
    it "moving my worm north on the left edge of the map moves that worm north" $
      makeMove True (fromMoves moveNorth doNothing) aStateWithMyWormOnLeftEdge `shouldBe`
      aStateWithMyWormUpwardsOnLeftEdge
    it "moving opponent worm north on the left edge of the map moves that worm north" $
      makeMove True (fromMoves doNothing moveNorth) aStateWithOpponentWormOnLeftEdge `shouldBe`
      aStateWithOpponentWormUpwardOnLeftEdge
    it "moving my worm south on the left edge of the map moves that worm south" $
      makeMove True (fromMoves moveSouth doNothing) aStateWithMyWormOnLeftEdge `shouldBe`
      aStateWithMyWormDownwardOnLeftEdge
    it "moving opponent worm south on the left edge of the map moves that worm south" $
      makeMove True (fromMoves doNothing moveSouth) aStateWithOpponentWormOnLeftEdge `shouldBe`
      aStateWithOpponentWormDownwardOnLeftEdge
    it "moving my worm east on the left edge of the map moves that worm east" $
      makeMove True (fromMoves moveEast doNothing) aStateWithMyWormOnLeftEdge `shouldBe`
      aStateWithMyWormRightFromLeftEdge
    it "moving opponent worm east on the left edge of the map moves that worm east" $
      makeMove True (fromMoves doNothing moveEast) aStateWithOpponentWormOnLeftEdge `shouldBe`
      aStateWithOpponentWormRightFromLeftEdge
    it "moving my worm off the edge on the left of the map changes nothing" $
      makeMove True (fromMoves moveWest doNothing) aStateWithMyWormOnLeftEdge `shouldBe`
      aStateWithMyWormOnLeftEdge
    it "moving opponent worm off the edge on left of the map changes nothing" $
      makeMove True (fromMoves doNothing moveWest) aStateWithOpponentWormOnLeftEdge `shouldBe`
      aStateWithOpponentWormOnLeftEdge
    -- Bottom edge
    it "moving my worm south from the bottom edge results in no change" $
      makeMove True (fromMoves moveSouth doNothing) aStateWithMyWormOnTheBottomEdge `shouldBe`
      aStateWithMyWormOnTheBottomEdge
    it "moving opponent worm south from the bottom edge results in no change" $
      makeMove True (fromMoves doNothing moveSouth) aStateWithOpponentWormOnTheBottomEdge `shouldBe`
      aStateWithOpponentWormOnTheBottomEdge
    it "moving my worm to the east from the bottom edge results in that worm moving right" $
      makeMove True (fromMoves moveEast doNothing) aStateWithMyWormOnTheBottomEdge `shouldBe`
      aStateWithMyWormOnTheBottomEdgeMovedRight
    it "moving opponent worm to the east from the bottom edge results in that worm moving right" $
      makeMove True (fromMoves doNothing moveEast) aStateWithOpponentWormOnTheBottomEdge `shouldBe`
      aStateWithOpponentWormOnTheBottomEdgeMovedRight
    it "moving my worm to the west from the bottom edge results in that worm moving left" $
      makeMove True (fromMoves moveWest doNothing) aStateWithMyWormOnTheBottomEdge `shouldBe`
      aStateWithMyWormOnTheBottomEdgeMovedLeft
    it "moving opponent to the west from the bottom edge results in that worm moving left" $
      makeMove True (fromMoves doNothing moveWest) aStateWithOpponentWormOnTheBottomEdge `shouldBe`
      aStateWithOpponentWormOnTheBottomEdgeMovedLeft
    it "moving my worm to the north from the bottom edge results in that worm moving up" $
      makeMove True (fromMoves moveNorth doNothing) aStateWithMyWormOnTheBottomEdge `shouldBe`
      aStateWithMyWormUpFromTheBottomEdge
    it "moving opponent worm to the north from the bottom edge results in that worm moving up" $
      makeMove True (fromMoves doNothing moveNorth) aStateWithOpponentWormOnTheBottomEdge `shouldBe`
      aStateWithOpponentWormUpFromTheBottomEdge
    -- Right edge
    it "moving my worm east from the right edge results in no change" $
      makeMove True (fromMoves moveEast doNothing) aStateWithMyWormOnTheRightEdge `shouldBe`
      aStateWithMyWormOnTheRightEdge
    it "moving opponent worm east from the right edge results in no change" $
      makeMove True (fromMoves doNothing moveEast) aStateWithOpponentWormOnTheRightEdge `shouldBe`
      aStateWithOpponentWormOnTheRightEdge
    it "moving my worm north from the right edge results in that worm moving up" $
      makeMove True (fromMoves moveNorth doNothing) aStateWithMyWormOnTheRightEdge `shouldBe`
      aStateWithMyWormOnTheRightEdgeMovedUp
    it "moving opponent worm north from the right edge results in that worm moving up" $
      makeMove True (fromMoves doNothing moveNorth) aStateWithOpponentWormOnTheRightEdge `shouldBe`
      aStateWithOpponentWormOnTheRightEdgeMovedUp
    it "moving my worm south from the right edge results in that worm moving down" $
      makeMove True (fromMoves moveSouth doNothing) aStateWithMyWormOnTheRightEdge `shouldBe`
      aStateWithMyWormOnTheRightEdgeMovedDown
    it "moving opponent worm south from the right edge results in that worm moving down" $
      makeMove True (fromMoves doNothing moveSouth) aStateWithOpponentWormOnTheRightEdge `shouldBe`
      aStateWithOpponentWormOnTheRightEdgeMovedDown
    it "moving my worm to the west from the right edge results in that worm moving left" $
      makeMove True (fromMoves moveWest doNothing) aStateWithMyWormOnTheRightEdge `shouldBe`
      aStateWithMyWormMovedLeftFromTheRightEdge
    it "moving opponent worm to the west from the right edge results in that worm moving left" $
      makeMove True (fromMoves doNothing moveWest) aStateWithOpponentWormOnTheRightEdge `shouldBe`
      aStateWithOpponentWormMovedLeftFromTheRightEdge

doNothing = Move 16

moveNorth = Move 8

moveSouth = Move 12

moveEast = Move 10

moveWest = Move 14

aState = State 1 10 10 10 10 aPlayer anOpponent aGameMap

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

aStateWhereIGotTheMedipack = knockBackDamage $ aState {
  opponent = opponentWithAWormNextToTheMedipack,
  myPlayer = aPlayerWithAWormOnTheMedipack }

aStateWhereOpponentGotTheMedipack = knockBackDamage $ aState {
  opponent = opponentWithAWormOnTheMedipack,
  myPlayer = aPlayerWithAWormNextToTheMedipack }

aStateWithBothWormNextToTheMedipack = aState {
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

opponentWithCollisionResolvedInHisFavour =
  withWorms someOtherWormsWithCollisionResolvedInHisFavour anOpponent

opponentWithCollisionResolvedInMyFavour =
  withWorms someOtherWormsWithCollisionResolvedInMyFavour anOpponent

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

aPlayerWithCollisionResolvedInMyFavour =
  withWorms someWormsWithCollisionResolvedInMyFavour aPlayer

aPlayerWithCollisionResolvedInHisFavour =
  withWorms someWormsWithCollisionResolvedInHisFavour aPlayer

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
thisWorm5 = withCoordOf (toCoord 1 15) $ withIdOf 5 aWorm

someWorms = wormsToMap $ V.fromList [thisWorm1, thisWorm2, thisWorm3, thisWorm5]

someWormsWithCurrentMovedEast =
  modifyWormWithId 1 (withCoordOf (toCoord 16 31)) someWorms

someWormsWithCollisionResolvedInMyFavour =
  modifyWormWithId 1 (withHealthOf 9 . withCoordOf (toCoord 16 31)) someWorms

someWormsWithCollisionResolvedInHisFavour =
  modifyWormWithId 1 (withHealthOf 9) someWorms

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
  modifyWormWithId 1 (withHealthOf 9 . withCoordOf (toCoord 16 31)) someOtherWorms

someOtherWormsWithCollisionResolvedInMyFavour =
  modifyWormWithId 1 (withHealthOf 9 . withCoordOf (toCoord 17 31)) someOtherWorms

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
