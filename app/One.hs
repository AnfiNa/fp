module One (
    sumTailRec,
    sumRec,
    sumModular,
    sumMap,
    sumListComp,
    sumInfinite
) where

import Data.List (foldl')

sumTailRec :: Int -> Int
sumTailRec n = go 0 0
  where
    go acc i
        | i >= n = acc
        | i `mod` 3 == 0 || i `mod` 5 == 0 = go (acc + i) (i + 1)
        | otherwise = go acc (i + 1)

sumRec :: Int -> Int
sumRec n
    | n <= 0 = 0
    | (n - 1) `mod` 3 == 0 || (n - 1) `mod` 5 == 0 = (n - 1) + sumRec (n - 1)
    | otherwise = sumRec (n - 1)

sumModular :: Int -> Int
sumModular n =
    foldl' (+) 0 $
        filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0) [1 .. n - 1]

sumMap :: Int -> Int
sumMap n = sum $ map (\x -> if x `mod` 3 == 0 || x `mod` 5 == 0 then x else 0) [1 .. n - 1]

sumInfinite :: Int -> Int
sumInfinite n = sum $ takeWhile (< n) $ filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0) [1 ..]
