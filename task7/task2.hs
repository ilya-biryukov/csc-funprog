{-- Домашнее задание № 7. Задача 2. Бирюков Илья, CS Center, отделение SE
 -  Структура простой императивной программы задана следующим описанием типов:
 -    data Expression = Constant Integer |
 -      Variable String |
 -      Unary Char Expression |
 -      Binary Expression Char Expression
 -      data Operator = Assignment String Expression |
 -      Sequence [Operator]
 -   Написать функцию doPlus :: Operator -> Operator, которая преобразует в заданной
 -   программе все выражения типа "a+b", где a и b - константы, в константу,
 -   содержащую сумму значений (то есть выражение Binary (Constant 3) '+' (Constant 5)
 -   должно быть заменено на Constant 8).
 -
 -   Обратите внимание, что в результате таких преобразований выражений,
 -   содержащих сумму констант, должно в программе не остаться совсем!
 -
 -}

data Expression = Constant Integer |
                  Variable String |
                  Unary Char Expression |
                  Binary Expression Char Expression
                  deriving Eq -- Для тестов

data Operator = Assignment String Expression |
                Sequence [Operator]
                deriving Eq -- Для тестов

{--
 -  Заменяет все выражения вида Constant + Constant на их значения в переданной программе.
 -
 -  Аргументы:
 -    prog Программа, которую необходимо преобразовать
 -
 -  Возвращает:
 -    Новую программу, в которой все выражения вида Constant + Constant заменены своими
 -    значениями
 -}
doPlus :: Operator -> Operator
-- Преобразуем каждый Operator из Sequence по отдельности
doPlus (Sequence opList) = Sequence $ map doPlus opList
-- Для преобразования Assignment необходимо обработать Expression, стоящий в правой части
-- присваивания
doPlus (Assignment v expr) = Assignment v $ doPlus' expr
  where
  -- doPlus' обрабатывает необходимым образом тип данных Expression
  -- Выражения Constant + Constant заменяем их значением
  doPlus' (Binary (Constant c1) '+' (Constant c2)) = Constant (c1 + c2)
  -- Если есть сумма двух выражений - сначала обработаем левый и правый операнды суммы,
  -- после чего обработаем сумму получившихся выражений. Таким образом мы сначала
  -- вычислим значения левого и правого операдна, если это возможно.
  doPlus' (Binary expr1 '+' expr2) = doPlus' (Binary (doPlus' expr1) '+' (doPlus' expr2))
  -- В сложных выражениях другого типа обрабатываем их составные части
  doPlus' (Binary expr1 s expr2 ) = Binary (doPlus' expr1) s (doPlus' expr2)
  doPlus' (Unary s expr) = Unary s (doPlus' expr)
  -- Для простых выражений(без составных частей) просто вернём их значение
  doPlus' x = x

-- Входные данные для тестов
testInput = [
  Assignment "x" (Binary (Constant 3) '+' (Constant 2)),
  Assignment "x" (Binary (Binary (Constant 4) '+' (Constant 5)) '+' (Constant 7)),
  Assignment "x" (Unary '-' (Binary (Constant 4) '+' (Constant 4)))
  ]

-- Результаты тестов
results = [
  Assignment "x" (Constant 5),
  Assignment "x" (Constant 16),
  Assignment "x" (Unary '-' (Constant 8)),
  ]

-- Детальная информация по результатам тестов
testDetail = zipWith (==) results $ map doPlus testInput

-- Результат всех тестов - прошли или нет
test = results == map doPlus testInput
