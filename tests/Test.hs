module Main where

import Test.Hspec
import Thirty
import One

main :: IO ()
main = hspec $ do
    describe "Problem 30" $ do
        it "should return correct sum" $ do
            problem30Tail `shouldBe` 443839
