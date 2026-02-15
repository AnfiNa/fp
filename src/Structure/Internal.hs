module Structure.Internal
  ( PrefixTreeMultiset (..),
    emptyMultiset,
    insertMultiset,
    deleteMultiset,
    lookupMultiset,
    memberMultiset,
    filterMultiset,
    mapKeysMultiset,
    mergeMultiset,
    foldlWithKey,
    foldrWithKey,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (foldl')

-- | Полиморфное префиксное дерево для мультимножества списков элементов типа k.
data PrefixTreeMultiset k = Node
  { count :: !Int,
    children :: !(Map k (PrefixTreeMultiset k))
  }
  deriving (Show)

-- | Пустое мультимножество.
emptyMultiset :: PrefixTreeMultiset k
emptyMultiset = Node {count = 0, children = Map.empty}

-- | Вставка списка (ключа) в мультимножество.
insertMultiset :: Ord k => [k] -> PrefixTreeMultiset k -> PrefixTreeMultiset k
insertMultiset [] node = node {count = count node + 1}
insertMultiset (c : cs) node =
  let child = Map.findWithDefault emptyMultiset c (children node)
      child' = insertMultiset cs child
   in node {children = Map.insert c child' (children node)}

-- | Удаление одного вхождения списка.
deleteMultiset :: Ord k => [k] -> PrefixTreeMultiset k -> PrefixTreeMultiset k
deleteMultiset [] node =
  let newCount = max 0 (count node - 1)
   in node {count = newCount}
deleteMultiset (c : cs) node =
  case Map.lookup c (children node) of
    Nothing -> node
    Just child ->
      let child' = deleteMultiset cs child
          children' =
            if isEmptyNode child'
              then Map.delete c (children node)
              else Map.insert c child' (children node)
       in node {children = children'}

-- | Количество вхождений списка.
lookupMultiset :: Ord k => [k] -> PrefixTreeMultiset k -> Int
lookupMultiset [] node = count node
lookupMultiset (c : cs) node =
  case Map.lookup c (children node) of
    Nothing -> 0
    Just child -> lookupMultiset cs child

-- | Проверка наличия хотя бы одного вхождения.
memberMultiset :: Ord k => [k] -> PrefixTreeMultiset k -> Bool
memberMultiset key node = lookupMultiset key node > 0

-- | Фильтрация по предикату, принимающему ключ и количество.
filterMultiset :: Ord k => ([k] -> Int -> Bool) -> PrefixTreeMultiset k -> PrefixTreeMultiset k
filterMultiset p = filterNode p []
  where
    filterNode predicate prefix node =
      let newCount = if predicate (reverse prefix) (count node) then count node else 0
          children' = Map.mapWithKey (\c child -> filterNode predicate (c : prefix) child) (children node)
          children'' = Map.filter (not . isEmptyNode) children'
       in Node {count = newCount, children = children''}

-- | Отображение ключей: применяет функцию к каждому элементу каждого ключа,
--   сохраняя количество вхождений. Новые ключи могут сливаться, счётчики складываются.
mapKeysMultiset :: (Ord k1, Ord k2) => (k1 -> k2) -> PrefixTreeMultiset k1 -> PrefixTreeMultiset k2
mapKeysMultiset f = foldlWithKey (\acc key cnt -> foldl' (flip insertMultiset) acc (replicate cnt (map f key))) emptyMultiset

-- | Объединение двух мультимножеств (сложение счётчиков для одинаковых ключей).
mergeMultiset :: Ord k => PrefixTreeMultiset k -> PrefixTreeMultiset k -> PrefixTreeMultiset k
mergeMultiset lhs rhs =
  let insertFn acc key cnt = foldl' (flip insertMultiset) acc (replicate cnt key)
   in foldlWithKey insertFn lhs rhs

-- | Левая свёртка с ключами.
foldlWithKey :: Ord k => (b -> [k] -> Int -> b) -> b -> PrefixTreeMultiset k -> b
foldlWithKey = foldlWithPrefix []
  where
    foldlWithPrefix prefix func z n =
      let z' = if count n > 0 then func z (reverse prefix) (count n) else z
       in Map.foldlWithKey (\acc' c child -> foldlWithPrefix (c : prefix) func acc' child) z' (children n)

-- | Правая свёртка с ключами.
foldrWithKey :: Ord k => ([k] -> Int -> b -> b) -> b -> PrefixTreeMultiset k -> b
foldrWithKey = foldrWithPrefix []
  where
    foldrWithPrefix prefix func z n =
      let z' = Map.foldrWithKey (\c child acc' -> foldrWithPrefix (c : prefix) func acc' child) z (children n)
       in if count n > 0 then func (reverse prefix) (count n) z' else z'

-- | Проверка пустого узла.
isEmptyNode :: PrefixTreeMultiset k -> Bool
isEmptyNode node = count node == 0 && Map.null (children node)

-- | Сравнение: структурное, без промежуточных списков.
instance Ord k => Eq (PrefixTreeMultiset k) where
  Node c1 m1 == Node c2 m2 = c1 == c2 && m1 == m2

-- | Объединение как полугрупповая операция.
instance Ord k => Semigroup (PrefixTreeMultiset k) where
  (<>) = mergeMultiset

-- | Нейтральный элемент и объединение.
instance Ord k => Monoid (PrefixTreeMultiset k) where
  mempty = emptyMultiset
  mappend = (<>)
