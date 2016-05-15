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

  describe "parse" $
    it "applies a parser to the input string" $ do
      parse item          "abc" `shouldBe` [('a', "bc")]
      parse (return' 'a') "abc" `shouldBe` [('a', "abc")]

  describe "combinators" $ do
    describe "p +++ q" $ do
      context "when p succeeds" $
        it "behaves like p" $
          parse (item +++ return' 'd') "abc" `shouldBe` [('a', "bc")]

      context "when p fails" $
        it "behaves like q" $
          parse (failure +++ return' 'd') "abc" `shouldBe` [('d', "abc")]

    describe "p >>>= f" $ do
      context "when both parsers succeed" $
        it "sequences two parsers" $
          (item >>>= \x -> item >>>= \y -> return' (x, y)) "abc"
            `shouldBe` [(('a', 'b'), "c")]

      context "when one parser fails" $
        it "fails" $
          (item >>>= \x -> item >>>= \y -> return' (x, y)) "a" `shouldBe` []
