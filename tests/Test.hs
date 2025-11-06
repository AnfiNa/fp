module Main where

import Test.Hspec
import Thirty

main :: IO ()
main = hspec $ do
  describe "myDigitToInt" $ do
    it "converts digits correctly" $ do
      myDigitToInt '0' `shouldBe` 0
      myDigitToInt '5' `shouldBe` 5
      myDigitToInt '9' `shouldBe` 9



  describe "fift" $ do
    it "calculates sum of fifth powers of digits" $ do
      fift 123 `shouldBe` (1^5 + 2^5 + 3^5)  -- 1 + 32 + 243 = 276
      fift 0 `shouldBe` 0
      fift 1 `shouldBe` 1

  describe "fifth" $ do
    it "returns number if it equals sum of fifth powers of its digits" $ do
      -- 4150 = 4^5 + 1^5 + 5^5 + 0^5 = 1024 + 1 + 3125 + 0 = 4150
      fifth 4150 `shouldBe` 4150

    it "returns 0 if number doesn't equal sum of fifth powers" $ do
      fifth 123 `shouldBe` 0

  describe "funcTail" $ do
    it "calculates tail-recursive sum" $ do
      -- funcTail n = go 0 n where go acc 1 = acc; go acc a = go (acc + fifth a) (a-1)
      -- For n=1: funcTail 1 = go 0 1 = 0
      -- For n=2: funcTail 2 = go (0 + fifth 2) 1 = 0
      funcTail 1 `shouldBe` 0
      funcTail 2 `shouldBe` 0

  describe "func" $ do
    it "calculates recursive sum of fifth" $ do
      -- func 1 = 0
      -- func a = fifth a + func (a-1)
      func 1 `shouldBe` 0
      func 2 `shouldBe` fifth 2 + func 1

  describe "gener" $ do
    it "sums numbers that equal sum of fifth powers of their digits" $ do
      -- For range [2..4150], only 4150 satisfies the condition
      gener 4150 `shouldBe` 4150
      gener 4151 `shouldBe` 4150 + 4151  -- 4151 also satisfies

    it "returns 0 when no numbers satisfy condition" $ do
      gener 100 `shouldBe` 0

  describe "integration tests" $ do
    it "finds known numbers that equal sum of fifth powers" $ do
      -- Known numbers that equal sum of fifth powers of their digits
      let knownNumbers = [4150, 4151, 54748, 92727, 93084, 194979]
      map fift knownNumbers `shouldBe` knownNumbers
      
    it "verifies gener function with known sequence" $ do
      -- For a=4150, result should be 4150
      -- For a=4151, result should be 4150 + 4151 = 8301
      gener 4150 `shouldBe` 4150
      gener 4151 `shouldBe` 8301