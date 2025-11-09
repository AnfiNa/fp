module Main where

import Test.Hspec
import One
import Thirty

main :: IO ()
main = hspec $ do
  describe "sumTailRec" $ do
    it "возвращает 0 для n=0" $ do
      sumTailRec 0 `shouldBe` 0
    it "возвращает 0 для n=1" $ do
      sumTailRec 1 `shouldBe` 0
    it "возвращает 0 для n=3" $ do
      sumTailRec 3 `shouldBe` 0
    it "возвращает 3 для n=4" $ do
      sumTailRec 4 `shouldBe` 3
    it "возвращает 3 для n=5" $ do
      sumTailRec 5 `shouldBe` 3
    it "возвращает 8 для n=6" $ do
      sumTailRec 6 `shouldBe` 8
    it "возвращает 14 для n=7" $ do
      sumTailRec 7 `shouldBe` 14
    it "возвращает 14 для n=8" $ do
      sumTailRec 8 `shouldBe` 14
    it "возвращает 14 для n=9" $ do
      sumTailRec 9 `shouldBe` 14
    it "возвращает 23 для n=10" $ do
      sumTailRec 10 `shouldBe` 23
    it "возвращает 233168 для n=1000" $ do
      sumTailRec 1000 `shouldBe` 233168
    it "возвращает 0 для отрицательных чисел" $ do
      sumTailRec (-5) `shouldBe` 0

  describe "sumRec" $ do
    it "возвращает 0 для n=0" $ do
      sumRec 0 `shouldBe` 0
    it "возвращает 0 для n=1" $ do
      sumRec 1 `shouldBe` 0
    it "возвращает 0 для n=3" $ do
      sumRec 3 `shouldBe` 0
    it "возвращает 3 для n=4" $ do
      sumRec 4 `shouldBe` 3
    it "возвращает 3 для n=5" $ do
      sumRec 5 `shouldBe` 3
    it "возвращает 8 для n=6" $ do
      sumRec 6 `shouldBe` 8
    it "возвращает 14 для n=7" $ do
      sumRec 7 `shouldBe` 14
    it "возвращает 14 для n=8" $ do
      sumRec 8 `shouldBe` 14
    it "возвращает 14 для n=9" $ do
      sumRec 9 `shouldBe` 14
    it "возвращает 23 для n=10" $ do
      sumRec 10 `shouldBe` 23
    it "возвращает 233168 для n=1000" $ do
      sumRec 1000 `shouldBe` 233168
    it "возвращает 0 для отрицательных чисел" $ do
      sumRec (-5) `shouldBe` 0

  describe "sumModular" $ do
    it "возвращает 0 для n=0" $ do
      sumModular 0 `shouldBe` 0
    it "возвращает 0 для n=1" $ do
      sumModular 1 `shouldBe` 0
    it "возвращает 0 для n=3" $ do
      sumModular 3 `shouldBe` 0
    it "возвращает 3 для n=4" $ do
      sumModular 4 `shouldBe` 3
    it "возвращает 3 для n=5" $ do
      sumModular 5 `shouldBe` 3
    it "возвращает 8 для n=6" $ do
      sumModular 6 `shouldBe` 8
    it "возвращает 14 для n=7" $ do
      sumModular 7 `shouldBe` 14
    it "возвращает 14 для n=8" $ do
      sumModular 8 `shouldBe` 14
    it "возвращает 14 для n=9" $ do
      sumModular 9 `shouldBe` 14
    it "возвращает 23 для n=10" $ do
      sumModular 10 `shouldBe` 23
    it "возвращает 233168 для n=1000" $ do
      sumModular 1000 `shouldBe` 233168
    it "возвращает 0 для отрицательных чисел" $ do
      sumModular (-5) `shouldBe` 0

  describe "sumMap" $ do
    it "возвращает 0 для n=0" $ do
      sumMap 0 `shouldBe` 0
    it "возвращает 0 для n=1" $ do
      sumMap 1 `shouldBe` 0
    it "возвращает 0 для n=3" $ do
      sumMap 3 `shouldBe` 0
    it "возвращает 3 для n=4" $ do
      sumMap 4 `shouldBe` 3
    it "возвращает 3 для n=5" $ do
      sumMap 5 `shouldBe` 3
    it "возвращает 8 для n=6" $ do
      sumMap 6 `shouldBe` 8
    it "возвращает 14 для n=7" $ do
      sumMap 7 `shouldBe` 14
    it "возвращает 14 для n=8" $ do
      sumMap 8 `shouldBe` 14
    it "возвращает 14 для n=9" $ do
      sumMap 9 `shouldBe` 14
    it "возвращает 23 для n=10" $ do
      sumMap 10 `shouldBe` 23
    it "возвращает 233168 для n=1000" $ do
      sumMap 1000 `shouldBe` 233168
    it "возвращает 0 для отрицательных чисел" $ do
      sumMap (-5) `shouldBe` 0

  describe "sumInfinite" $ do
    it "возвращает 0 для n=0" $ do
      sumInfinite 0 `shouldBe` 0
    it "возвращает 0 для n=1" $ do
      sumInfinite 1 `shouldBe` 0
    it "возвращает 0 для n=3" $ do
      sumInfinite 3 `shouldBe` 0
    it "возвращает 3 для n=4" $ do
      sumInfinite 4 `shouldBe` 3
    it "возвращает 3 для n=5" $ do
      sumInfinite 5 `shouldBe` 3
    it "возвращает 8 для n=6" $ do
      sumInfinite 6 `shouldBe` 8
    it "возвращает 14 для n=7" $ do
      sumInfinite 7 `shouldBe` 14
    it "возвращает 14 для n=8" $ do
      sumInfinite 8 `shouldBe` 14
    it "возвращает 14 для n=9" $ do
      sumInfinite 9 `shouldBe` 14
    it "возвращает 23 для n=10" $ do
      sumInfinite 10 `shouldBe` 23
    it "возвращает 233168 для n=1000" $ do
      sumInfinite 1000 `shouldBe` 233168
    it "возвращает 0 для отрицательных чисел" $ do
      sumInfinite (-5) `shouldBe` 0

  describe "problem30Tail" $ do
    it "возвращает правильную сумму чисел, равных сумме пятых степеней своих цифр" $ do
      problem30Tail `shouldBe` 443839

  describe "problem30Normal" $ do
    it "возвращает правильную сумму чисел, равных сумме пятых степеней своих цифр" $ do
      problem30Normal `shouldBe` 443839

  describe "problem30Modular" $ do
    it "возвращает правильную сумму чисел, равных сумме пятых степеней своих цифр" $ do
      problem30Modular `shouldBe` 443839

  describe "problem30Map" $ do
    it "возвращает правильную сумму чисел, равных сумме пятых степеней своих цифр" $ do
      problem30Map `shouldBe` 443839

  describe "problem30Infinite" $ do
    it "возвращает правильную сумму чисел, равных сумме пятых степеней своих цифр" $ do
      problem30Infinite `shouldBe` 443839
