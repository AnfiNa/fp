module Thirty (
    problem30Infinite,
    problem30Map,
    problem30Modular,
    problem30Normal,
    problem30Tail,
) where

import Data.Char (digitToInt)

problem30Tail :: Int
problem30Tail = goTail 2 0
  where
    goTail n acc
        | n > 354294 = acc
        | n == sumOfPowers n = goTail (n + 1) (acc + n)
        | otherwise = goTail (n + 1) acc

    sumOfPowers :: Int -> Int
    sumOfPowers x = go x 0
      where
        go 0 acc = acc
        go n acc = go (n `div` 10) (acc + ((n `mod` 10) ^ (5 :: Int)))

problem30Normal :: Int
problem30Normal =
    let upperBound = 354294
        pow5 = (^ (5 :: Int))
        sumDigits 0 = 0
        sumDigits n = pow5 (n `mod` 10) + sumDigits (n `div` 10)
     in sum [n | n <- [2 .. upperBound], n == sumDigits n]

problem30Modular :: Int
problem30Modular =
    let upperBound = 354294
        pow5 = (^ (5 :: Int))
        digitPowers = map pow5 [0 .. 9]
        sumOfPowers n =
            n == foldl (\acc d -> acc + digitPowers !! d) 0 (map (read . pure) (show n))
     in sum (filter sumOfPowers [2 .. upperBound])

problem30Map :: Int
problem30Map =
    let upperBound = 354294
        pow5 = (^ (5 :: Int))
        sumOfPowers n = sum (map (\c -> pow5 (read [c])) (show n))
     in sum (map fst (filter (uncurry (==)) (map (\n -> (n, sumOfPowers n)) [2 .. upperBound])))

problem30Infinite :: Int
problem30Infinite =
    let pow5 = (^ (5 :: Int))
        sumOfPowers n = sum (map (pow5 . digitToInt) (show n))
     in sum (takeWhile (<= 354294) (filter (\n -> n == sumOfPowers n) [2 ..]))
