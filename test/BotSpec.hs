{-# LANGUAGE NoImplicitPrelude #-}
module BotSpec (spec) where

import Bot
import Import

import qualified RIO.Vector.Boxed as V
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
      thisCurrentWorm (aState {currentWormId = 3}) `shouldBe` (Just $ withIdOf 3 aWorm)
  describe "thatCurrentWorm" $ do
    it "shouldn't be found when the index is negative" $
      thatCurrentWorm (aState { currentWormId = -1 }) `shouldBe` Nothing
    it "shouldn't be found when the index is greater than the original number of worms" $
      thatCurrentWorm (aState { currentWormId = 6 }) `shouldBe` Nothing
    it "shouldn't be found when searching for a worm in a gap in the sequence" $
      thatCurrentWorm (aState { currentWormId = 2 }) `shouldBe` Nothing
    it "should find a worm when it's in the sequence" $
      thatCurrentWorm (aState {currentWormId = 3}) `shouldBe` (Just $ withIdOf 3 aWorm)
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

doNothing = Move 16

moveNorth = Move 8

aState = State 1 10 10 10 10 aPlayer (withWorms someOtherWorms aPlayer) aGameMap

withWorms worms' (Player health' _) = Player health' worms'

aPlayer = Player 300 someWorms

someWorms = wormsToMap $ V.fromList [aWorm, withIdOf 2 aWorm, withIdOf 3 aWorm, withIdOf 5 aWorm]

someOtherWorms = wormsToMap $ V.fromList [aWorm, withIdOf 3 aWorm, withIdOf 4 aWorm, withIdOf 5 aWorm]

aWorm = Worm 1 10 (Coord 1087)

withIdOf :: Int -> Worm -> Worm
withIdOf id' (Worm _ health' position') = Worm id' health' position'

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
