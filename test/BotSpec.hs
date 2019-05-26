{-# LANGUAGE NoImplicitPrelude #-}
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
  describe "desplaceCoordByMove" $ do
    it "N  (not on boundry)" $ displaceCoordByMove (toCoord 1 1) (Move 8)  `shouldBe` (toCoord 1 0)
    it "NE (not on boundry)" $ displaceCoordByMove (toCoord 1 1) (Move 9)  `shouldBe` (toCoord 2 0)
    it "E  (not on boundry)" $ displaceCoordByMove (toCoord 1 1) (Move 10) `shouldBe` (toCoord 2 1)
    it "SE (not on boundry)" $ displaceCoordByMove (toCoord 1 1) (Move 11) `shouldBe` (toCoord 2 2)
    it "S  (not on boundry)" $ displaceCoordByMove (toCoord 1 1) (Move 12) `shouldBe` (toCoord 1 2)
    it "SW (not on boundry)" $ displaceCoordByMove (toCoord 1 1) (Move 13) `shouldBe` (toCoord 0 2)
    it "W  (not on boundry)" $ displaceCoordByMove (toCoord 1 1) (Move 14) `shouldBe` (toCoord 0 1)
    it "NW (not on boundry)" $ displaceCoordByMove (toCoord 1 1) (Move 15) `shouldBe` (toCoord 0 0)
    prop "Move back and forth" $ \ (i, j) ->
      let coordInMap  = Coord $ (abs j) `mod` (mapDim * mapDim)
          indexOfMove = ((abs i) `mod` 8)
          randomMove  = Move $ indexOfMove + 8
          moveBack    = Move $ ((indexOfMove + 4) `mod` 8) + 8
      in displaceCoordByMove (displaceCoordByMove coordInMap randomMove) moveBack `shouldBe` coordInMap
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

doNothing = Move 16

moveNorth = Move 8

moveSouth = Move 12

moveEast = Move 10

moveWest = Move 14

aStateWithImpendingCollision = aState { opponent = anOpponentWithImpendingCollision }

aState = State 1 10 10 10 10 aPlayer anOpponent aGameMap

anOpponent = withWorms someOtherWorms aPlayer

anOpponentWithImpendingCollision = withWorms someOtherWormsWithImpendingCollision anOpponent

opponentWithCollisionResolvedInHisFavour =
  withWorms someOtherWormsWithCollisionResolvedInHisFavour anOpponent

opponentWithCollisionResolvedInMyFavour =
  withWorms someOtherWormsWithCollisionResolvedInMyFavour anOpponent

withWorms worms' (Player health' _) = Player health' worms'

aPlayerWithCollisionResolvedInMyFavour =
  withWorms someWormsWithCollisionResolvedInMyFavour aPlayer

aPlayerWithCollisionResolvedInHisFavour =
  withWorms someWormsWithCollisionResolvedInHisFavour aPlayer

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

modifyWormWithId :: Int -> (Worm -> Worm) -> Worms -> Worms
modifyWormWithId = flip M.adjust

aWorm = Worm 1 10 $ toCoord 15 31

withIdOf id' (Worm _ health' position') = Worm id' health' position'

withHealthOf health' (Worm id' _ position') = Worm id' health' position'

withCoordOf position' (Worm id' health' _) = Worm id' health' position'

aGameMap = GameMap $ V.fromList $
  spaceRow ++
  dirtRow ++
  foldl' (++) [] (take (mapDim - 4) $ repeat middleRow) ++
  dirtRow ++
  spaceRow
  where
    dirtRow = [DEEP_SPACE] ++ (take (mapDim - 2) $ repeat AIR) ++ [DEEP_SPACE]
    spaceRow = take mapDim $ repeat DEEP_SPACE
    middleRow = [DEEP_SPACE] ++ tenAir ++ someDirt ++ tenAir ++ [DEEP_SPACE]
    tenAir = (take 10 $ repeat AIR)
    someDirt = (take (mapDim - 22) $ repeat DIRT)
