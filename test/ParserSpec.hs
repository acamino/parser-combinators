module ParserSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Parser

spec :: Spec
spec = do
  describe "primitive parsers" $ do
    describe "return'" $
      prop "succeeds without consuming any of the input string" $
        \x xs -> return' x xs `shouldBe` ([(x, xs)] :: [(Char, String)])

    describe "failure" $
      prop "always fails" $
        \xs -> (failure xs :: [(Char, String)]) `shouldBe` []

    describe "item" $ do
      context "when the input string is not empty" $
        it "succeeds" $ do
          item "a"   `shouldBe` [('a', "")]
          item "abc" `shouldBe` [('a', "bc")]

      context "when the input string is not empty" $
        it "fails" $
          item "" `shouldBe` []
