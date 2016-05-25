module SumByFactorsSpec (main, spec) where

import Test.Hspec
import Debug.Trace

import Test.Hspec

import SumByFactors

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "Sum by Factors" $ do
    context "When the sum is positive for [15, 12]" $ do
      it "returns the list sorted of all factors and the sum" $ do
        sumByFac [15, 12] `shouldBe` [[2, 12], [3, 27], [5, 15]]

    context "When the sum is negative" $ do
      it "return for 5 and 3 -> 0" $ do
        sumByFac [15, 30, -45] `shouldBe` [[2, 30], [3, 0], [5, 0]]

