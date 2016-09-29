module SimplifyPolySpec (main, spec) where

import Test.Hspec
import Debug.Trace

import SimplifyPoly
import Text.ParserCombinators.Parsec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "Sample tests" $ do
    it "Test reduction by equivalence" $ do
      simplify "dc+dcba" `shouldBe` "cd+abcd"
      simplify "2xy-yx"  `shouldBe` "xy"
      simplify "-a+5ab+3a-c-2a" `shouldBe` "-c+5ab"
    it "Test monomial length ordering" $ do
      simplify "-abc+3a+2ac" `shouldBe` "3a+2ac-abc"
      simplify "xyz-xz" `shouldBe` "-xz+xyz"
    it "Test lexicographic ordering" $ do
      simplify "a+ca-ab" `shouldBe` "a-ab+ac"
      simplify "xzy+zby" `shouldBe` "byz+xyz"
    it "Test no leading +" $ do
      simplify "-y+x" `shouldBe` "x-y"
      simplify "y-x" `shouldBe` "-x+y"


  describe "Parse multiple terms" $ do
    it "Returns all the numbers and variables" $ do
      parse terms "" "+dc-32dd" `shouldBe` Right [("cd", 1), ("dd", -32)]
      parse terms "" "dc+dcba"  `shouldBe` Right [("cd", 1), ("abcd", 1)]
      parse terms "" "2xy-yx"   `shouldBe` Right [("xy", 2), ("xy", -1)]

  describe "Parse variables" $ do
    it "parses only letters" $ do
      parse var "" "abbb+" `shouldBe` Right "abbb"

  describe "Parsing one term" $ do
    it "parses negative amount" $ do
      parse term "" "-10a" `shouldBe` Right ("a", -10)

    it "parses positive amount" $ do
      parse term "" "51abc" `shouldBe` Right ("abc", 51)

    it "parses no amount by default as +1" $ do
      parse term "" "dc" `shouldBe` Right ("cd", 1)

    it "parses minus variable as -1" $ do
      parse term "" "-abc" `shouldBe` Right ("abc", -1)

