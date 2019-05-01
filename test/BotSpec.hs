{-# LANGUAGE NoImplicitPrelude #-}
module BotSpec (spec) where

import Bot
import Import

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
