## Лабораторная работа 2: Реализация префиксного дерева как мультимножества


**Дисциплина:** Функциональное программирование  
**Студент:** Анфилатова Надежда Геннадьевна  
**Группа:** P3321

---

## Требования к разработанному ПО

### Функциональные требования
- **Добавление элемента** – функция `insertMultiset :: Ord k => [k] -> PrefixTreeMultiset k -> PrefixTreeMultiset k`
- **Удаление одного вхождения элемента** – `deleteMultiset :: Ord k => [k] -> PrefixTreeMultiset k -> PrefixTreeMultiset k`
- **Фильтрация по предикату, зависящему от ключа и количества вхождений** – `filterMultiset :: Ord k => ([k] -> Int -> Bool) -> PrefixTreeMultiset k -> PrefixTreeMultiset k`
- **Отображение ключей** (map) – `mapKeysMultiset :: (Ord k1, Ord k2) => (k1 -> k2) -> PrefixTreeMultiset k1 -> PrefixTreeMultiset k2`
- **Левая и правая свёртки с ключами** – `foldlWithKey` и `foldrWithKey`
- **Реализация моноида** – экземпляры классов `Semigroup` и `Monoid` (объединение с помощью `mergeMultiset`, нейтральный элемент `emptyMultiset`)
- **Проверка наличия** – `memberMultiset`
- **Получение количества вхождений** – `lookupMultiset`

### Нефункциональные требования
- **Неизменяемость** – все операции возвращают новое дерево, не модифицируя исходное.
- **Полиморфизм** – структура параметризована типом элемента ключа (`k`), что позволяет хранить списки любого типа, для которого определён порядок (`Ord`).
- **Идиоматичность кода** – использование строгих полей, рекурсивных свёрток, классов типов, скрытие реализации (экспорт только безопасных функций).
- **Эффективное сравнение** – экземпляр `Eq` реализован структурно, без приведения к спискам.
- **Тестирование**:
  - Unit-тесты (HUnit) для каждой публичной функции.
  - Property-based тестирование (QuickCheck) – минимум 3 свойства, включая законы моноида.

---

## Ключевые элементы реализации

### Структура данных
```haskell
data PrefixTreeMultiset k = Node
  { count    :: !Int,                     -- количество строк, заканчивающихся в этом узле
    children :: !(Map k (PrefixTreeMultiset k)) -- отображение из элемента в дочерний узел
  }
```
Узел хранит счётчик и детей. Благодаря использованию `Map` поиск и вставка выполняются за O(1) относительно длины ключа.

### Основные операции

**Вставка** – рекурсивно спускается по ключу, создавая недостающие узлы, и увеличивает счётчик в конечном узле.
```haskell
insertMultiset [] node = node {count = count node + 1}
insertMultiset (c:cs) node =
  let child = Map.findWithDefault emptyMultiset c (children node)
      child' = insertMultiset cs child
  in node {children = Map.insert c child' (children node)}
```

**Удаление** – аналогично спуску, уменьшает счётчик и удаляет пустые узлы.
```haskell
deleteMultiset [] node = node {count = max 0 (count node - 1)}
deleteMultiset (c:cs) node = case Map.lookup c (children node) of
  Nothing -> node
  Just child ->
    let child' = deleteMultiset cs child
        children' = if isEmptyNode child'
                    then Map.delete c (children node)
                    else Map.insert c child' (children node)
    in node {children = children'}
```

**Фильтрация** – обходит дерево, применяет предикат к каждому узлу, обнуляет счётчик, если условие ложно, и рекурсивно фильтрует детей.
```haskell
filterMultiset p = filterNode p []
  where
    filterNode predicate prefix node =
      let newCount = if predicate (reverse prefix) (count node) then count node else 0
          children' = Map.mapWithKey (\c child -> filterNode predicate (c:prefix) child) (children node)
          children'' = Map.filter (not . isEmptyNode) children'
      in Node newCount children''
```

**Отображение ключей** – использует свёртку для преобразования каждого ключа с помощью заданной функции и повторной вставки.
```haskell
mapKeysMultiset f t =
  foldlWithKey (\acc key cnt -> foldl' (flip insertMultiset) acc (replicate cnt (map f key))) emptyMultiset t
```

**Свёртки** – реализованы с аккумулятором, проходящим по дереву в глубину.
```haskell
foldlWithKey = foldlWithPrefix []
  where
    foldlWithPrefix prefix func z n =
      let z' = if count n > 0 then func z (reverse prefix) (count n) else z
      in Map.foldlWithKey (\acc c child -> foldlWithPrefix (c:prefix) func acc child) z' (children n)
```

**Моноид** – объединение через `mergeMultiset`, которое складывает счётчики одинаковых ключей. Нейтральный элемент – `emptyMultiset`.

---

## Тестирование

Для тестирования использовались библиотеки **HUnit** (unit-тесты) и **QuickCheck** (property-based тестирование).

### Unit-тесты
Покрывают все публичные функции:
- `testEmpty` – проверка пустого мультимножества.
- `testInsertAndLookup` – вставка и поиск.
- `testMember` – проверка принадлежности.
- `testDelete` – удаление элементов.
- `testFilter` – фильтрация по разным условиям.
- `testMerge` – объединение двух мультимножеств.
- `testFolds` – корректность левой и правой свёрток.
- `testEq` – сравнение деревьев.
- `testSemigroupMonoid` – законы полугруппы и моноида.
- `testMapKeys` – отображение ключей.

Всего **10 unit-тестов**, каждый включает несколько проверок.

### Property-based тестирование
Было реализовано **10 свойств**, среди которых три закона моноида обязательны:
1. **Левая единица**: `mempty <> x == x`
2. **Правая единица**: `x <> mempty == x`
3. **Ассоциативность**: `(x <> y) <> z == x <> (y <> z)`
4. Вставка с последующим удалением не изменяет множество.
5. Количество вхождений ключа в дереве совпадает с числом его появлений в исходном списке.
6. Фильтрация с `\_ _ -> True` возвращает то же дерево.
7. Фильтрация с `\_ _ -> False` возвращает пустое дерево.
8. Фильтрация по длине ключа корректна.
9. Отображение ключей сохраняет общее количество элементов.
10. Левая и правая свёртки при ассоциативной операции дают одинаковый мультинабор ключей.

### Отчёт о прохождении тестов
При запуске `cabal test` получен следующий вывод:

```            
Build profile: -w ghc-9.8.2 -O1
In order, the following will be built (use -v for more details):
 - fp-lab2-0.1.0.0 (test:fp-lab2-test) (first run)
Preprocessing test suite 'fp-lab2-test' for fp-lab2-0.1.0.0..
Building test suite 'fp-lab2-test' for fp-lab2-0.1.0.0..
Running 1 test suites...
Test suite fp-lab2-test: RUNNING...
Test suite fp-lab2-test: PASS
Test suite logged to:
C:\Users\Sheep\Desktop\hs2\dist-newstyle\build\x86_64-windows\ghc-9.8.2\fp-lab2-0.1.0.0\t\fp-lab2-test\test\fp-lab2-0.1.0.0-fp-lab2-test.log
1 of 1 test suites (1 of 1 test cases) passed.
```

Все тесты пройдены успешно.

## Выводы

В ходе лабораторной работы была разработана полиморфная неизменяемая структура данных «префиксное дерево», реализующая интерфейс мультимножества (bag). Структура полностью соответствует функциональным требованиям: поддерживает добавление, удаление, фильтрацию, отображение ключей, свёртки и является моноидом.

**Использованные приёмы программирования:**
- **Алгебраические типы данных** – для представления узла дерева.
- **Рекурсия** – для обхода и модификации дерева.
- **Параметрический полиморфизм** – структура работает с ключами любого типа, для которого определён `Ord`.
- **Классы типов** – реализованы экземпляры `Eq`, `Semigroup`, `Monoid`, что обеспечивает естественную интеграцию с Haskell.
- **Строгие поля** – для предотвращения накопления отложенных вычислений.
- **Использование `Map` из `containers`** – эффективное хранение дочерних узлов.
- **Свёртки** – `foldlWithKey` и `foldrWithKey` предоставляют гибкий механизм агрегации данных.
- **Тестирование** – комбинация модульных (HUnit) и свойственных (QuickCheck) тестов позволила убедиться в корректности реализации и соблюдении законов моноида.
