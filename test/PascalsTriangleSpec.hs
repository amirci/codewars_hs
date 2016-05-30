module PascalsTriangleSpec (main, spec) where

import Test.Hspec
import Debug.Trace

import Test.Hspec

import PascalsTriangle

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "pascalsTriangle" $ do
    it "should work for 1" $ do
      pascalsTriangle 1 `shouldBe` [1]
    it "should work for 2" $ do
      pascalsTriangle 2 `shouldBe` [1, 1, 1]
    it "should work for 3" $ do
      pascalsTriangle 3 `shouldBe` [1, 1, 1, 1, 2, 1]

