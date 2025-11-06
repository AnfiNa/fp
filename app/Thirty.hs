module Thirty (
    problem30infinite,
    problem30map,
    problem30modular,
    problem30normal,
    problem30tail,
) where

import Data.Char (digitToInt)

problem30tail :: Int
problem30tail = goTail 2 0
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

problem30normal :: Int
problem30normal =
    let upperBound = 354294
        pow5 = (^ (5 :: Int))
        sumDigits 0 = 0
        sumDigits n = pow5 (n `mod` 10) + sumDigits (n `div` 10)
     in sum [n | n <- [2 .. upperBound], n == sumDigits n]

problem30modular :: Int
problem30modular =
    let upperBound = 354294
        pow5 = (^ (5 :: Int))
        digitPowers = map pow5 [0 .. 9]
        sumOfPowers n =
            n == foldl (\acc d -> acc + digitPowers !! d) 0 (map (read . pure) (show n))
     in sum (filter sumOfPowers [2 .. upperBound])

problem30map :: Int
problem30map =
    let upperBound = 354294
        pow5 = (^ (5 :: Int))
        sumOfPowers n = sum (map (\c -> pow5 (read [c])) (show n))
     in sum (map fst (filter (uncurry (==)) (map (\n -> (n, sumOfPowers n)) [2 .. upperBound])))

problem30infinite :: Int
problem30infinite =
    let pow5 = (^ (5 :: Int))
        sumOfPowers n = sum (map (pow5 . digitToInt) (show n))
     in sum (takeWhile (<= 354294) (filter (\n -> n == sumOfPowers n) [2 ..]))
