module ParserSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import Parser

spec :: Spec
spec = do
  describe "primitive parsers" $ do
    describe "return" $
      prop "succeeds without consuming any of the input string" $
        \x xs -> parse (return x) xs `shouldBe` ([(x, xs)] :: [(Char, String)])

    describe "failure" $
      prop "always fails" $
        \xs -> (parse failure xs :: [(Char, String)]) `shouldBe` []

    describe "item" $ do
      context "when the input string is not empty" $
        it "succeeds" $ do
          parse item "a"   `shouldBe` [('a', "")]
          parse item "abc" `shouldBe` [('a', "bc")]

      context "when the input string is not empty" $
        it "fails" $
          parse item "" `shouldBe` []

  describe "parse" $
    it "applies a parser to the input string" $ do
      parse item         "abc" `shouldBe` [('a', "bc")]
      parse (return 'a') "abc" `shouldBe` [('a', "abc")]

  describe "combinators" $ do
    describe "p +++ q" $ do
      context "when p succeeds" $
        it "behaves like p" $
          parse (item +++ return 'd') "abc" `shouldBe` [('a', "bc")]

      context "when p fails" $
        it "behaves like q" $
          parse (failure +++ return 'd') "abc" `shouldBe` [('d', "abc")]

    describe "p >>= f" $ do
      context "when both parsers succeed" $
        it "sequences two parsers" $
          parse (item >>= \x -> item >>= \y -> return (x, y)) "abc"
            `shouldBe` [(('a', 'b'), "c")]
      context "when one parser fails" $
        it "fails" $
          parse (item >>= \x -> item >>= \y -> return (x, y)) "a" `shouldBe` []

  describe "derived primitives" $ do
    describe "sat" $ do
      context "when the predicate is satisfied" $
        it "parses a character" $
          parse (sat ('a'==)) "abc" `shouldBe` [('a', "bc")]

      context "when the predicate is unsatisfied" $
        it "fails" $
          parse (sat ('a'==)) "bc" `shouldBe` []

    describe "digit" $ do
      context "when the first character is a digit" $
        it "succeeds" $
          parse digit "1ab" `shouldBe` [('1', "ab")]

      context "when the first character is not a digit" $
        it "fails" $
          parse digit "abc" `shouldBe` []

    describe "char" $ do
      context "when the first character is the given char" $
        it "succeeds" $
          parse (char 'a') "abc" `shouldBe` [('a', "bc")]

      context "when the first character is not the given char" $
        it "fails" $
          parse (char 'x') "abc" `shouldBe` []

  describe "many'" $ do
    context "when the parser can succeed" $
      it "succeeds applying the parser many times" $
        parse (many' digit) "123abc" `shouldBe` [("123", "abc")]

    context "when the parser can not succeed" $
      it "succeeds applying the parser zero times" $
        parse (many' digit) "abc"  `shouldBe` [("", "abc")]

  describe "many1" $ do
    context "when the parser can succeed" $
      it "succeeds applying the parser many times" $
        parse (many1 digit) "123abc" `shouldBe` [("123", "abc")]

    context "when the parser can not succeed" $
      it "fails" $
        parse (many1 digit) "abc"  `shouldBe` []

  describe "parser for lists" $
    describe "p" $ do
      context "when the input string contains a list" $
        it "succeeds" $
          parse p "[1,2,3]" `shouldBe` [("123", "")]

      context "when the input string does not contain a list" $
        it "fails" $
          parse p "abc" `shouldBe` []
