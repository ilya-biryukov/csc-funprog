{-- Домашнее задание № 4. Задача 3. Бирюков Илья, CS Center, отделение SE.
 -
 -  Написать функцию для перемножения двух числовых матриц.
 -}
import Ratio(Rational)

type Matrix a = [[a]]

{--
 - Вычислить произведение двух матриц.
 -
 - Аргументы:
 -    left - левая часть произведения
 -    right - правая часть произведения
 -
 - Возвращает:
 -    Произведение матриц left * right
 -}
matrixProduct :: Num a => Matrix a -> Matrix a -> Matrix a
-- Умножаем каждую строку матрицы left на транспонированную матрицу right, таким образом
-- получаю строки произведения матриц left * right
matrixProduct left right = map (matrixVectorProduct $ transpose right) left
    where
      -- Функция для транспонирования матрицы
      transpose ([]: _) = []
      transpose m = map head m : transpose (map tail m)
      -- Вычисляет произведение матрицы m на вектор x
      matrixVectorProduct m x = map (scalarProduct x) m
      -- Вычисляет скалярное произведение векторов x и y
      scalarProduct x y = foldl1 (+) (zipWith (*) x y)

-- Матрицы для тестов
nullMatrix = [[0, 0, 0], [0, 0, 0], [0, 0, 0]]
unitMatrix = [[1, 0, 0], [0, 1, 0], [0, 0, 1]]
someMatrix = [[1, 2, 3], [4, 9, 8], [5, 5, 5]]
-- Обратная к someMatrix. Посчитана с помощью WolframAlpha
-- Здесь удобно использовать рациональные числа, чтобы сравнения проводились точно
invSomeMatrix :: Matrix Rational
invSomeMatrix =[[-1 / 6, -1 / 6, 11/30], [-2/3, 1/3, -2/15], [5/6, -1/6, -1/30]]
-- Не квадратные матрицы(фактически вектора)
nonRectMatrix = [[1], [1], [1]]
nonRectMatrix2 = [[1, 1, 1]]


testInput = [
  -- Произведение единичной на всякие матрицы не должно их изменять. Причем не смотря
  -- на порядок перемножения.
  (unitMatrix, someMatrix),
  (someMatrix, unitMatrix),
  (unitMatrix, unitMatrix),
  (unitMatrix, nullMatrix),
  (nullMatrix, unitMatrix),
  -- Произведение матрицы и обратной к ней должно дать единичную матрицу
  (someMatrix, invSomeMatrix),
  (invSomeMatrix, someMatrix),
  -- Произведение матрицы на нулевую должно давать нулевую
  (nullMatrix, someMatrix),
  (someMatrix, nullMatrix),
  -- Произведение неквадратных матрицы
  (nonRectMatrix2, unitMatrix),
  (unitMatrix, nonRectMatrix)
  ]

results = [
  -- Единичная матрица
  someMatrix,
  someMatrix,
  unitMatrix,
  nullMatrix,
  nullMatrix,
  -- Обратная
  unitMatrix,
  unitMatrix,
  -- На нулевую
  nullMatrix,
  nullMatrix,
  -- Неквадратные
  nonRectMatrix2,
  nonRectMatrix
  ]


-- Результат теста (True или False)
test = results == map (uncurry matrixProduct) testInput

-- Более детальная информация по каждому тесту
testDetail = zipWith (==) results $ map (uncurry matrixProduct) testInput
