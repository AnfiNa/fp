module Thirty (
    fifth,
    fift,
    gener,
    func,
    funcTail,
    myDigitToInt,
) where

myDigitToInt :: Char -> Int
myDigitToInt '0' = 0
myDigitToInt '1' = 1
myDigitToInt '2' = 2
myDigitToInt '3' = 3
myDigitToInt '4' = 4
myDigitToInt '5' = 5
myDigitToInt '6' = 6
myDigitToInt '7' = 7
myDigitToInt '8' = 8
myDigitToInt '9' = 9
myDigitToInt _ = error "Not a digit"

fifth :: Int -> Int
fifth a = if a == s then a else 0
  where
    s = sum $ map ((^ 5) . myDigitToInt) (show a)

funcTail :: Int -> Int
funcTail = go 0
  where
    go acc 1 = acc
    go acc a = go (acc + fifth a) (a - 1)

func :: Int -> Int
func 1 = 0
func a = fifth a + func (a - 1)

fift :: Int -> Int
fift n = sum $ map ((^ 5) . myDigitToInt) (show n)

gener :: Int -> Int
gener a = sum $ filter (\i -> i == fift i) [2 .. a]
