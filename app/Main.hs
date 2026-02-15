-- Пример использования PrefixTreeMultiset
module Main where

import Structure.Internal
import qualified Structure.Internal as M
import Data.List (sort)       -- для упорядоченного вывода

main :: IO ()
main = do
    -- 1. Пустое мультимножество
    let empty = emptyMultiset :: PrefixTreeMultiset

    -- 2. Вставка нескольких слов (некоторые повторяются)
    let wordsList = ["apple", "banana", "apple", "orange", "banana", "apple"]
    let multiset = foldr insertMultiset empty wordsList
    -- Результат: "apple" -> 3, "banana" -> 2, "orange" -> 1

    -- 3. Получение количества вхождений
    putStrLn "=== Количество вхождений ==="
    print $ lookupMultiset "apple" multiset   -- 3
    print $ lookupMultiset "grape" multiset   -- 0
    print $ memberMultiset "banana" multiset  -- True

    -- 4. Удаление одного "apple"
    let afterDelete = deleteMultiset "apple" multiset
    putStrLn "\n=== После удаления одного 'apple' ==="
    print $ lookupMultiset "apple" afterDelete   -- 2

    -- 5. Фильтрация: оставить только слова длиной больше 5 символов
    let filtered = filterMultiset (\key cnt -> length key > 5) afterDelete
    putStrLn "\n=== Фильтр (длина > 5) ==="
    -- Превращаем результат в список пар для наглядности
    let pairsAfterFilter = foldlWithKey (\acc k c -> (k, c) : acc) [] filtered
    print $ sort pairsAfterFilter   -- [("banana",2), ("orange",1)]

    -- 6. Объединение двух мультимножеств
    let setA = foldr insertMultiset empty ["apple", "apple", "kiwi"]
    let setB = foldr insertMultiset empty ["banana", "kiwi", "kiwi"]
    let merged = mergeMultiset setA setB
    putStrLn "\n=== Объединение setA и setB ==="
    print $ sort $ foldlWithKey (\acc k c -> (k, c) : acc) [] merged
    -- ожидаем: [("apple",2), ("banana",1), ("kiwi",3)]

    -- 7. Свёртка с ключами: суммарная длина всех слов с учётом повторений
    let totalChars = foldlWithKey (\acc key cnt -> acc + length key * cnt) 0 multiset
    putStrLn $ "\n=== Суммарное количество символов (с повторениями) ==="
    print totalChars   -- (5*3 + 6*2 + 6*1) = 15+12+6 = 33

    -- 8. Общее количество слов (сумма счётчиков) через foldlWithKey
    let totalCount = foldlWithKey (\acc _ cnt -> acc + cnt) 0 multiset
    putStrLn "\n=== Общее количество слов (сумма счётчиков) ==="
    print totalCount   -- 3+2+1 = 6

    -- 9. Правая свёртка с ключами для построения списка
    let pairsRight = foldrWithKey (\key cnt rest -> (key, cnt) : rest) [] multiset
    putStrLn "\n=== Список (ключ, количество) через foldrWithKey ==="
    print $ sort pairsRight

    -- 10. Сравнение на равенство
    let m1 = foldr insertMultiset empty ["a", "b"]
    let m2 = foldr insertMultiset empty ["b", "a"]
    putStrLn "\n=== Равенство мультимножеств ==="
    print $ m1 == m2   -- True (порядок вставки не важен)

    -- 11. Использование моноида (<> = mergeMultiset)
    let mergedMonoid = m1 <> m2
    putStrLn "\n=== Объединение через <> ==="
    print $ sort $ foldlWithKey (\acc k c -> (k, c) : acc) [] mergedMonoid
    -- [("a",2), ("b",2)]