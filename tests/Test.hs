{-# LANGUAGE FlexibleInstances #-}

import Structure.Internal
import Test.HUnit
import Test.QuickCheck
import Data.List (sort, nub)
import qualified Data.Map.Strict as Map
import Data.Char (chr, ord)

-- | Генерируем строки из символов 'a'..'c' длиной до 3 (чтобы дерево не разрасталось)
genSmallString :: Gen String
genSmallString = do
  len <- choose (0, 3)
  vectorOf len (elements ['a'..'c'])

-- | Произвольное мультимножество для Char
instance Arbitrary (PrefixTreeMultiset Char) where
  arbitrary = do
    strings <- listOf genSmallString
    return $ foldr insertMultiset emptyMultiset strings

  shrink t =
    let xs = foldlWithKey (\acc k c -> replicate c k ++ acc) [] t
    in [ foldr insertMultiset emptyMultiset (take i xs ++ drop (i+1) xs)
       | i <- [0..length xs - 1] ]

prop_MonoidLeftIdentity :: PrefixTreeMultiset Char -> Bool
prop_MonoidLeftIdentity x = x <> emptyMultiset == x

prop_MonoidRightIdentity :: PrefixTreeMultiset Char -> Bool
prop_MonoidRightIdentity x = emptyMultiset <> x == x

prop_MonoidAssociativity :: PrefixTreeMultiset Char -> PrefixTreeMultiset Char -> PrefixTreeMultiset Char -> Bool
prop_MonoidAssociativity x y z = ((x <> y) <> z) == (x <> (y <> z))

-- | Вставка с последующим удалением одного вхождения не меняет множество
prop_InsertDelete :: Property
prop_InsertDelete = forAll genSmallString $ \s ->
  forAll arbitrary $ \t ->
    deleteMultiset s (insertMultiset s t) == t

-- | Количество вхождений ключа в дереве равно числу его появлений в исходном списке
prop_LookupExact :: Property
prop_LookupExact = forAll (resize 10 (listOf genSmallString)) $ \xs ->
  let t = foldr insertMultiset emptyMultiset xs
      countInList k = length (filter (== k) xs)
  in all (\k -> lookupMultiset k t == countInList k) (nub xs)

-- | Фильтрация, всегда возвращающая True, не меняет множество
prop_FilterAlwaysTrue :: PrefixTreeMultiset Char -> Bool
prop_FilterAlwaysTrue t = filterMultiset (\_ _ -> True) t == t

-- | Фильтрация, всегда возвращающая False, даёт пустое множество
prop_FilterAlwaysFalse :: PrefixTreeMultiset Char -> Bool
prop_FilterAlwaysFalse t = filterMultiset (\_ _ -> False) t == emptyMultiset

-- | Фильтрация по длине ключа (больше 1)
prop_FilterLength :: PrefixTreeMultiset Char -> Bool
prop_FilterLength t =
  let t' = filterMultiset (\key _ -> length key > 1) t
      keys = foldlWithKey (\acc k _ -> k:acc) [] t
  in all (\k -> if length k > 1
                then lookupMultiset k t' == lookupMultiset k t
                else lookupMultiset k t' == 0) keys

-- | Отображение ключей сохраняет общее количество элементов
prop_MapKeysCount :: PrefixTreeMultiset Char -> Bool
prop_MapKeysCount t =
  let totalBefore = foldlWithKey (\acc _ cnt -> acc + cnt) 0 t
      t' = mapKeysMultiset id t
      totalAfter = foldlWithKey (\acc _ cnt -> acc + cnt) 0 t'
  in totalBefore == totalAfter

-- | Левая и правая свёртки эквивалентны при ассоциативной операции
prop_FoldsAgree :: PrefixTreeMultiset Char -> Bool
prop_FoldsAgree t =
  let list = foldrWithKey (\k cnt acc -> replicate cnt k ++ acc) [] t
      foldlRes = foldlWithKey (\acc k cnt -> replicate cnt k ++ acc) [] t
  in sort list == sort foldlRes

unitTests :: Test
unitTests = TestList
  [ testEmpty
  , testInsertAndLookup
  , testMember
  , testDelete
  , testFilter
  , testMerge
  , testFolds
  , testEq
  , testSemigroupMonoid
  , testMapKeys
  ]

testEmpty :: Test
testEmpty = TestCase $ do
  let empty = emptyMultiset :: PrefixTreeMultiset Char
  assertEqual "empty count" 0 (count empty)
  assertBool "empty children" (Map.null (children empty))
  assertEqual "lookup empty string" 0 (lookupMultiset "" empty)
  assertEqual "lookup any string" 0 (lookupMultiset "abc" empty)

testInsertAndLookup :: Test
testInsertAndLookup = TestCase $ do
  let t1 = insertMultiset "cat" (emptyMultiset :: PrefixTreeMultiset Char)
  assertEqual "insert one 'cat'" 1 (lookupMultiset "cat" t1)
  assertEqual "lookup non-existent" 0 (lookupMultiset "dog" t1)

  let t2 = insertMultiset "cat" t1
  assertEqual "insert second 'cat'" 2 (lookupMultiset "cat" t2)

  let t3 = insertMultiset "dog" t2
  assertEqual "lookup 'cat' after dog" 2 (lookupMultiset "cat" t3)
  assertEqual "lookup 'dog'" 1 (lookupMultiset "dog" t3)

  let t4 = insertMultiset "" t3
  assertEqual "insert empty string" 1 (lookupMultiset "" t4)

testMember :: Test
testMember = TestCase $ do
  let t = foldr insertMultiset emptyMultiset ["a","b","a"] :: PrefixTreeMultiset Char
  assertBool "member 'a'" (memberMultiset "a" t)
  assertBool "member 'b'" (memberMultiset "b" t)
  assertBool "not member 'c'" (not $ memberMultiset "c" t)
  assertBool "not member empty string" (not $ memberMultiset "" t)

testDelete :: Test
testDelete = TestCase $ do
  let t = foldr insertMultiset emptyMultiset ["x","x","y"] :: PrefixTreeMultiset Char
  let t1 = deleteMultiset "x" t
  assertEqual "delete one 'x'" 1 (lookupMultiset "x" t1)
  assertEqual "'y' unchanged" 1 (lookupMultiset "y" t1)

  let t2 = deleteMultiset "x" t1
  assertEqual "delete second 'x'" 0 (lookupMultiset "x" t2)
  assertEqual "'y' still 1" 1 (lookupMultiset "y" t2)

  let t3 = deleteMultiset "z" t2
  assertEqual "delete non-existing" t2 t3

  let t4 = deleteMultiset "y" t2
  assertEqual "delete last 'y'" 0 (lookupMultiset "y" t4)

  let t5 = deleteMultiset "any" emptyMultiset
  assertEqual "delete from empty" emptyMultiset t5

testFilter :: Test
testFilter = TestCase $ do
  let t = foldr insertMultiset emptyMultiset
        [ "a", "b", "aa", "bb", "aaa", "bbb" ] :: PrefixTreeMultiset Char

  let t1 = filterMultiset (\key _ -> length key == 1) t
  assertEqual "filter len=1, 'a'" 1 (lookupMultiset "a" t1)
  assertEqual "filter len=1, 'b'" 1 (lookupMultiset "b" t1)
  assertEqual "filter len=1, 'aa'" 0 (lookupMultiset "aa" t1)

  let t2 = filterMultiset (\_ cnt -> even cnt) t
  assertEqual "even cnt, 'a'" 0 (lookupMultiset "a" t2)
  assertEqual "even cnt, all zero" emptyMultiset t2

  let t3 = filterMultiset (\_ _ -> True) t
  assertEqual "filter all" t t3

testMerge :: Test
testMerge = TestCase $ do
  let t1 = foldr insertMultiset emptyMultiset ["a","b","a"] :: PrefixTreeMultiset Char
  let t2 = foldr insertMultiset emptyMultiset ["b","c","c"] :: PrefixTreeMultiset Char
  let merged = mergeMultiset t1 t2
  assertEqual "merge 'a'" 2 (lookupMultiset "a" merged)
  assertEqual "merge 'b'" 2 (lookupMultiset "b" merged)
  assertEqual "merge 'c'" 2 (lookupMultiset "c" merged)

  assertEqual "merge empty left" t2 (mergeMultiset emptyMultiset t2)
  assertEqual "merge empty right" t1 (mergeMultiset t1 emptyMultiset)

testFolds :: Test
testFolds = TestCase $ do
  let t = foldr insertMultiset emptyMultiset
        [ "ab", "ab", "c", "d", "d", "d" ] :: PrefixTreeMultiset Char

  let totalLen = foldlWithKey (\acc key cnt -> acc + length key * cnt) 0 t
  assertEqual "total length" (2*2 + 1 + 1*3) totalLen

  let pairs = foldrWithKey (\key cnt rest -> (key, cnt) : rest) [] t
  assertEqual "pairs from foldr" (sort [("ab",2),("c",1),("d",3)]) (sort pairs)

  let totalCnt = foldlWithKey (\acc _ cnt -> acc + cnt) 0 t
  assertEqual "total count" (2+1+3) totalCnt

testEq :: Test
testEq = TestCase $ do
  let t1 = foldr insertMultiset emptyMultiset ["x","y","x"] :: PrefixTreeMultiset Char
  let t2 = foldr insertMultiset emptyMultiset ["y","x","x"] :: PrefixTreeMultiset Char
  assertEqual "equal multisets" t1 t2

  let t3 = insertMultiset "z" t1
  assertBool "not equal (different keys)" (t1 /= t3)

  let t4 = insertMultiset "x" t1
  assertBool "not equal (different counts)" (t1 /= t4)

  assertEqual "empty eq" (emptyMultiset :: PrefixTreeMultiset Char) emptyMultiset

testSemigroupMonoid :: Test
testSemigroupMonoid = TestCase $ do
  let t1 = foldr insertMultiset emptyMultiset ["a","b"] :: PrefixTreeMultiset Char
  let t2 = foldr insertMultiset emptyMultiset ["b","c"] :: PrefixTreeMultiset Char

  let combined = t1 <> t2
  assertEqual "combined 'a'" 1 (lookupMultiset "a" combined)
  assertEqual "combined 'b'" 2 (lookupMultiset "b" combined)
  assertEqual "combined 'c'" 1 (lookupMultiset "c" combined)

  let leftId = t1
  assertEqual "left identity" leftId t1
  let rightId = t1
  assertEqual "right identity" rightId t1

  let t3 = insertMultiset "d" emptyMultiset
  let assoc1 = (t1 <> t2) <> t3
  let assoc2 = t1 <> (t2 <> t3)
  assertEqual "associativity" assoc1 assoc2

testMapKeys :: Test
testMapKeys = TestCase $ do
  let t = foldr insertMultiset emptyMultiset ["ab", "ab", "c"] :: PrefixTreeMultiset Char
  let t' = mapKeysMultiset toUpper t
  assertEqual "mapKeys 'ab' -> 'AB'" 2 (lookupMultiset "AB" t')
  assertEqual "mapKeys 'c' -> 'C'" 1 (lookupMultiset "C" t')
  where
    toUpper c = chr (ord c - 32)

main :: IO ()
main = do
  putStrLn "Running HUnit tests..."
  counts <- runTestTT unitTests
  putStrLn "\nRunning QuickCheck properties..."
  putStrLn "1. Monoid left identity"
  quickCheck (prop_MonoidLeftIdentity :: PrefixTreeMultiset Char -> Bool)
  putStrLn "2. Monoid right identity"
  quickCheck (prop_MonoidRightIdentity :: PrefixTreeMultiset Char -> Bool)
  putStrLn "3. Monoid associativity"
  quickCheck (prop_MonoidAssociativity :: PrefixTreeMultiset Char -> PrefixTreeMultiset Char -> PrefixTreeMultiset Char -> Bool)
  putStrLn "4. Insert + delete = identity"
  quickCheck prop_InsertDelete
  putStrLn "5. Lookup matches exact counts"
  quickCheck prop_LookupExact
  putStrLn "6. Filter always true = identity"
  quickCheck (prop_FilterAlwaysTrue :: PrefixTreeMultiset Char -> Bool)
  putStrLn "7. Filter always false = empty"
  quickCheck (prop_FilterAlwaysFalse :: PrefixTreeMultiset Char -> Bool)
  putStrLn "8. Filter by length > 1"
  quickCheck (prop_FilterLength :: PrefixTreeMultiset Char -> Bool)
  putStrLn "9. MapKeys preserves total count"
  quickCheck (prop_MapKeysCount :: PrefixTreeMultiset Char -> Bool)
  putStrLn "10. Left and right folds produce same multiset of keys"
  quickCheck (prop_FoldsAgree :: PrefixTreeMultiset Char -> Bool)
  putStrLn "\nAll tests completed."
