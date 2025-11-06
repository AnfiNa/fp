module Main where

import Test.Hspec
import Thirty

main :: IO ()
main = hspec $ do
    describe "Problem 30" $ do
        it "should return correct sum" $ do
            problem30tail `shouldBe` 443839
