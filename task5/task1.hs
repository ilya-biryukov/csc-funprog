{-- Домашнее задание № 5. Задача 1. Бирюков Илья, CS Center, отделение SE
 -
 - Определить, можно ли расставить знаки "+", "-", "*" и круглые скобки между числами
 - 1, 2,..., 10 (именно в этом порядке, без перестановок) так, чтобы в результате
 - выполнения всех действий получилось заданное число. Функция должна получать на вход
 - число и выдавать строку, изображающую правильную расстановку знаков и скобок,
 - или строку "impossible", если заданное число получить невозможно.
 -
 - Например, для числа 2011 возможный вариант мог бы выглядеть так:
 - (1+((2*3)+(4+(5*((6*((7*8)+9))+10)))))
 - (конечно, в этой записи некоторые скобки - лишние).
 -}

import Data.List(find)

-- Оператор
data Operator = Plus | Minus | Mult deriving (Eq, Show)
-- Элемент обратной польской записи - оператор или операнд
data RpnItem = OperatorItem Operator | OperandItem Integer deriving (Eq, Show)

{--
 -  Вспомогательная функция. Генерирует все возможные выражения в обратной польской
 -  записи, состоящий из чисел [1..n](стоящих именно в таком порядке) и операторов
 -  Plus, Minus, Mult.
 -
 -  Аргументы:
 -    n Количество операндов в обратной польской записи
 -
 -  Возвращает:
 -    Список из всех возможных выражений в обратной польской записи, в которых
 -}
generateRpns :: Integer -> [[RpnItem]]
-- План такой:
-- 1) генерируем все возможные последовательности только с оператором +,
-- 2) генерируем все возможные последовательности из (n - 1) операторов
-- 3) подставляем каждую возможную последовательность операторов из пункта 2 на
--    соответствующие позиции операторов + в пункте 1.
generateRpns n
  = emplaceAllOperators n $ map fst $ filterCorrect $ genPartialRpns n
    where
    -- Генерирует все посл-ти с n операндами и <= n-1 операторами Plus.
    -- Сгенерированные последовательности получаются в обратном порядке.
    -- Возвращает список Tuple'ов вида:
    --    (посл-ть, сколько операторов надо добавить чтобы получилась корреткная запись
    --     в ОПН)
    genPartialRpns 1 = [([OperandItem 1], 0)]
    genPartialRpns n = addOpers $ map addItem prevRpns
        where
        prevRpns = genPartialRpns (n - 1)
        addItem (list, count) = (OperandItem n:list, count + 1)
    -- Отфильтровывает те пары, в которых содержатся некорректные ОПН, в которых не
    -- хватает операторов
    filterCorrect = filter (\(_, y) -> y == 0)
    -- Добавляет к списку пар (посл-ть, сколько операторов надо добавить) всевозможные
    -- пары, которые можно получить корректным добавлением дополнительных операторов
    -- (чтобы ``сколько надо добавить`` оставалось >= 0)
    addOpers = foldl addOpers' []
        where
        -- Обрабатывает один элемент списка
        addOpers' res item@(list, 0) = item:res
        addOpers' res item@(list, opCount) = addOpers' (item:res) withPlus
            where
            withPlus = (OperatorItem Plus:list, opCount - 1)
    -- Генерирует все возможные списки из n операторов
    makeOpersList 0 = [[]]
    makeOpersList n = foldl makeOpersList' [] prevLists
        where
        prevLists = makeOpersList (n - 1)
        makeOpersList' res list = map (:list) allOperItems ++ res
        allOperItems = map OperatorItem [Plus, Minus, Mult]
    -- Принимает на вход корректную ОПН и список из операторов и на место
    -- каждого оператора в ОПН вставляет соотв-ий оператор из второго списка
    replaceOpers items opers = fst $ foldl changeOp ([], opers) items
        where
        changeOp (res, opers) x@(OperandItem _) = (x:res, opers)
        changeOp (res, op:ops) x@(OperatorItem _) = (op:res, ops)
    -- Принимает на вход число операндов, список корректных ОПН с таким количеством
    -- операндов выдаёт все возможные комбинации принятых ОПН с другими операторами.
    emplaceAllOperators n items
      = [replaceOpers x y | x <- items, y <- makeOpersList (n - 1)];

{--
 -  Вычисляет значение выражения, записанного в обратной польской нотации.
 -
 -  Аргументы:
 -      items Последовательность в ОПН
 -
 -  Возвращает:
 -      Результат вычисления переданного выражения
 -}
evalRpn :: [RpnItem] -> Integer
evalRpn items = head $ foldl evalItem [] $ items
    where
    -- Принимает список, в котором хранится текущее состояние стека операндов
    -- и следующее значение в цепочке выражения.
    evalItem ops (OperandItem value) = value:ops
    -- NB! операторы вынимаются из стэка в обратном порядке!
    evalItem (x:y:ops) (OperatorItem operator) = (applyOper operator y x):ops
    -- Принимает оператор, левый аргумент, правый аргумент.
    -- Возвращает результат применения соответствующего оператора
    applyOper Plus x y = x + y
    applyOper Minus x y = x - y
    applyOper Mult x y = x * y

{--
 -  Преобразует выражением в ОПН в строку, представляющую собой инфиксную запись
 -  такого выражения с соответствующими скобками.
 -
 -  Аргументы:
 -      items Последовательность в ОПН
 -
 -  Возвращает:
 -      Строку, представляющую запись ОПН в инфиксной форме.
 -}

printRpn :: [RpnItem] -> String
printRpn [] = ""
printRpn items = head $ foldl printItem [] $ items
    where
    -- Принимает список, в котором хранится текущее состояние стека операндов
    -- и следующее значение в цепочке выражения.
    printItem ops (OperandItem value) = (show value):ops
    -- NB! операторы вынимаются из стэка в обратном порядке!
    printItem (x:y:ops) (OperatorItem operator) = ("(" ++ y ++ opStr ++ x ++ ")"):ops
        where
        opStr = case operator of
          Plus -> "+"
          Minus -> "-"
          Mult -> "*"

{--
 -  Если данное число можно получить способом, описанным в задании выдаёт
 -  соответствующее выражение, иначе "impossible".
 -
 -  Аргументы:
 -      n Число, которое необходимо получить
 -
 -  Возвращает:
 -      Строку, содержащую выражение, равное n или "impossible", если способом,
 -      описанным в задании такое число получить невозможно
 -}
exprFor :: Integer -> String
-- Будем вычислять все возможные выражения пока не получим нужное число. Если ничего не
-- вышло, выдадим impossible
exprFor n = sayResult $ find (\(x,_) -> x == n) $ evalPrintPairs
    where
    evalPrintPairs = map (\rpn -> (evalRpn rpn, printRpn rpn)) $ generateRpns 10
    sayResult (Just (_, printedRpn)) = printedRpn
    sayResult Nothing = "impossible"

main = do
  print $ exprFor $ product [1..10]
  print $ exprFor $ 1 + product[1..10]

