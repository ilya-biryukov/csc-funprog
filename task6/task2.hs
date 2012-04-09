{-- Домашнее задание № 6. Задача 2. Бирюков Илья, CS Center, отделение SE
 -
 -
 -}


type Graph = (Int, Int -> Int -> Bool)

{-- Вспомогательная функция, вычисляет максимальную из кратчайших расстояний от
 - переданной вершины до всех остальных
 -}
maxDist graph n = 0

diameter graph@(n, _) = maximum $ map (maxDist graph) [1..n]


-- Входные данные для тестов
testInput = [
  (0, \x y -> x /= y),
  (1, \x y -> x /= y),
  (2, \x y -> x /= y),
  ]

-- Ожидаемые результаты тестов
results = [
  0,
  0,
  2
  ]

-- Результат всех тестов
test = results == map diameter testInput


-- Результаты отдельных тестов
testDetail = zipWith (==) results $ map diameter testInput
--
