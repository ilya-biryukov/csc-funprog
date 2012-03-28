{-- Домашнее задание № 4. Задача 1. Бирюков Илья, CS Center, отделение SE
 -
 - Дерево задано следующим описанием структуры данных:
 -  data Tree a = Empty | Node (Tree a) a (Tree a)
 - Написать функцию optimalTree :: Ord a => [a] -> Tree a , которая строит оптимальное
 - двоичное дерево поиска из элементов заданного списка.
 - Двоичное дерево называется оптимальным, если на каждом его уровне с номером k
 - содержится ровно 2k узлов, кроме, возможно, последнего уровня,
 - на котором может находиться и меньшее число узлов.
 -}

import List (sort)

data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Show, Eq)

{--
 -  Строит оптимальное дерево поиска из элементов списка
 -
 -  Аргументы:
 -    list Список, из которого будет строиться дерево
 -
 -  Возвращает:
 -    Оптимальное дерево поиска из элементов переданного списка
 -}
optimalTree :: Ord a => [a] -> Tree a
-- Дерево строим только от отсортированного списка
optimalTree = optFromSorted . sort
    where
      -- Вспомогательная функция, которая строит оптимальное дерево поиска из
      -- отстортированного списка
      optFromSorted [] = Empty
      -- Чтобы дерево поиска было оптимальным в корне должен находиться средний элемент,
      -- а дальше запускаемся рекурсивно
      optFromSorted list = Node (optFromSorted less) pivot (optFromSorted greater)
            where
                (less, (pivot: greater)) = splitAt (length list `div` 2) list


-- Входные данные для тестов
testInput = [
    [1, 3, 1], -- С повторяющимися элементами
    [3, 3, 3], -- Из одинаковых элементов
    [1, 2, 3, 4, 5, 6, 7], -- Отсортированный список
    [2, 3, 1, 4, 7, 6, 5], -- Неотсортированный
    [] -- Пустой список
    -- Больше тестов составить вручную достаточно сложно
    ]

-- Ожидаемые результаты тестов
results = [
  Node (Node Empty 1 Empty) 1 (Node Empty 3 Empty),
  Node (Node Empty 3 Empty) 3 (Node Empty 3 Empty),
  Node (Node (Node Empty 1 Empty) 2 (Node Empty 3 Empty)) 4 (Node (Node Empty 5 Empty ) 6 (Node Empty 7 Empty)),
  Node (Node (Node Empty 1 Empty) 2 (Node Empty 3 Empty)) 4 (Node (Node Empty 5 Empty ) 6 (Node Empty 7 Empty)),
  Empty
  ]

-- True, если все тесты пройдены и False иначе
test = results == map optimalTree testInput

-- Более детальная информация по каждому тесту
testDetail = zipWith (==) results $ map optimalTree testInput
