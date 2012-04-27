{-- Домашнее задание № 7. Задача 1. Бирюков Илья, CS Center, отделение SE
 -    Структура выражения расширенного лямбда-исчисления задано следующим описанием типа:\
 -      data Expression =
 -        Integral Integer | Function String | Variable String |
 -        Lambda String Expression |
 -        Apply Expression Expressio
 -
 -    Написать функцию hasBeta :: Expression -> Bool, которая проверяет,
 -    есть ли в заданном выражении хоть один $\beta$-редекс.
 -}

data Expression = Integral Integer
  | Function String
  | Variable String
  | Lambda String Expression
  | Apply Expression Expression

{-- Функция, определяющая содержится ли в переданном выражениие $\beta$-редекс.
 -
 -  Аргументы:
 -    expr Выражение
 -
 -  Результат:
 -    True, если в выражении содержится $\beta$-редекс
 -    False, если нет
 -}
hasBeta :: Expression -> Bool
-- Если есть $\beta$-редекс в явном виде, то возвращаем True
hasBeta (Apply (Lambda _ _) _) = True
-- Для Lambda и Apply(левым аргументом котрого явл-ся не Lambda) проверим их составные
-- части
hasBeta (Lambda _ expr) = hasBeta expr
hasBeta (Apply funExpr argExpr) = hasBeta funExpr || hasBeta argExpr
-- Остальные типы выражений не могут содержать $\beta$-редекс
hasBeta _ = False


-- Входные данные для тестов
testInput = [
  Integral 10,
  Lambda "x" (Integral 10),
  Apply (Function "sin") (Integral 10),
  Apply (Lambda "x" (Integral 10)) (Integral 10)
  ]

-- Ожидаемые результаты тестов
results = [
  False,
  False,
  False,
  True
  ]

-- Детальная информация о результатах тестов
testDetail = zipWith (==) results $ map hasBeta testInput

-- Результат всех тестов - прошли или нет
test = results == map hasBeta testInput
