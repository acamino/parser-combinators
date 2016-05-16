module ArithmeticExprSpec where

import Control.Exception (evaluate)
import Test.Hspec

import ArithmeticExpr

spec :: Spec
spec =
  describe "eval" $ do
    context "when the input is well-structured" $
      it "calculates the result" $ do
        eval "2+3*4"   `shouldBe` 14
        eval "(2+3)*4" `shouldBe` 20

    context "when the input can not be consumed completely" $
      it "throws an error with the unused input" $
        evaluate (eval "2+3+") `shouldThrow` errorCall "unused input +"

    context "when the input is invalid" $
      it "throws an error with the invalid input" $
        evaluate (eval "abcd2+3") `shouldThrow` errorCall "invalid input"
