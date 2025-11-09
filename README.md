# Лабораторная работа 1
## Проект Эйлера №1, №30

---

  * Студент: `Анфилатова Надежда Геннадьевна`
  * Группа: `P3321`
  * ИСУ: `408162`
  * Функциональный язык: `Haskell`

---

## Проблема №1

  * **Название**: `Multiples of $3$ or $5$`
  * **Описание**: If we list all the natural numbers below $10$ that are multiples of $3$ or $5$, we get $3, $5$, $6$ and $9$. The sum of these multiples is $23$.
  * **Задание**: Find the sum of all the multiples of $3$ or $5$ below $1000$.

---

### Основная идея решения

Так как 1000 - число не особо большое, не будем использовать математические оптимизации решения (сложить каждое 3 с каждым 5 числом и вычесть каждое 15), кроме последней реализации. Также не будем хардкодить 1000, а передадим её в качестве параметра n во все реализации.

---

### Решение через хвостовую рекурсию

```haskell
sumTailRec :: Int -> Int
sumTailRec n = go 0 0
  where
    go acc i
        | i >= n = acc
        | i `mod` 3 == 0 || i `mod` 5 == 0 = go (acc + i) (i + 1)
        | otherwise = go acc (i + 1)
```

### Решение через рекурсию 

```haskell
sumRec :: Int -> Int
sumRec n
    | n <= 0 = 0
    | (n - 1) `mod` 3 == 0 || (n - 1) `mod` 5 == 0 = (n - 1) + sumRec (n - 1)
    | otherwise = sumRec (n - 1)
```

### Решение через модульность

```haskell
sumModular :: Int -> Int
sumModular n =
    foldl' (+) 0 $
        filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0) [1 .. n - 1]
```

### Генерация последовательности при помощи отображения (map)

```haskell
sumMap :: Int -> Int
sumMap n = sum $ map (\x -> if x `mod` 3 == 0 || x `mod` 5 == 0 then x else 0) [1 .. n - 1]
```

### Решение через бесконечные структуры и ленивые исполнения

```haskell
sumInfinite :: Int -> Int
sumInfinite n = sum $ takeWhile (< n) $ filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0) [1 ..]
```

### Решение через другой язык

```Python
k=0
for i in range(1, 1000):
    if i%3 == 0 or i%5==0:
        k += i
print(k)
```

---

## Проблема №30

  * **Название**: `Digit Fifth Powers`
  * **Описание**: There are only three numbers that can be written as the sum of fourth powers of their digits:

    ```
    1634 = 1⁴ + 6⁴ + 3⁴ + 4⁴
    8208 = 8⁴ + 2⁴ + 0⁴ + 8⁴  
    9474 = 9⁴ + 4⁴ + 7⁴ + 4⁴
    ```

    Note that **1 = 1⁴** is not included because it is not considered a sum.
    The sum of these numbers is **1634 + 8208 + 9474 = 19316**.
  * **Задание**: Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.

---

### Основная идея решения

Для начала определимся с диапазоном, в котором мы можем встретить подходящие числа. 
Заметим, что пятая степень девятки равна $59049$. Если мы умножим это число на 7, то получим шестизначное число, как и при умножении на 6. 
А значит числа после $9^5*6$ никак не смогут подходить решению, так как сумма их пятых степеней цифр будет короче по длине цифр, а значит точно не будет равной изначальному числу.

---

### Решение через хвостовую рекурсию

```haskell
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
```

### Решение через рекурсию 

```haskell
problem30Normal :: Int
problem30Normal =
    let upperBound = 354294
        pow5 = (^ (5 :: Int))
        sumDigits 0 = 0
        sumDigits n = pow5 (n `mod` 10) + sumDigits (n `div` 10)
     in sum [n | n <- [2 .. upperBound], n == sumDigits n]
```

### Решение через модульность

```haskell
problem30Modular :: Int
problem30Modular =
    let upperBound = 354294
        pow5 = (^ (5 :: Int))
        digitPowers = map pow5 [0 .. 9]
        sumOfPowers n =
            n == foldl (\acc d -> acc + digitPowers !! d) 0 (map (read . pure) (show n))
     in sum (filter sumOfPowers [2 .. upperBound])
```

### Генерация последовательности при помощи отображения (map)

```haskell
problem30Map :: Int
problem30Map =
    let upperBound = 354294
        pow5 = (^ (5 :: Int))
        sumOfPowers n = sum (map (\c -> pow5 (read [c])) (show n))
     in sum (map fst (filter (uncurry (==)) (map (\n -> (n, sumOfPowers n)) [2 .. upperBound])))
```

### Решение через бесконечные структуры и ленивые исполнения

```haskell
problem30Infinite :: Int
problem30Infinite =
    let pow5 = (^ (5 :: Int))
        sumOfPowers n = sum (map (pow5 . digitToInt) (show n))
     in sum (filter (\n -> n == sumOfPowers n) (takeWhile (<= 354294) [2 ..]))
```

### Решение через другой язык


```Python
a = 360000
b = 0
for i in range(2, a):
    k = 0
    for j in str(i):
        k += int(j)**5
    if k == i:
        b += i

print(b)
```

---

## Выводы

В ходе выполнения лабораторной работы я познакомилась с новым для себя стилем программирования и языком. 
Писать код было максимально непривычно, очень хотелось написать реализации через циклы, но их нет в хаскеле.
