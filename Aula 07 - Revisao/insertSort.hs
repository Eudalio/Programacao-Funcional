--Usando a função insert, escreva a função insertSort :: Ord a => [a] -> [a] tal que (insertSort xs)
--recebe uma lista possivelmente não ordenada xs e devolve uma lista ordenada implementando ordenação
--pelo método de inserção:

--Se a lista é vazia, então  já está ordenada;

--Se a lista é não vazia (x : xs), então inserimos x na lista ordenada obtida pela ordenação de xs.

--insert a xs = [x | x <- filter (<=a) xs] ++ [a] ++ [x | x <- filter (>a) xs]

insertSort [] = []
insertSort (x:xs) = insertSort menores ++ [x] ++ insertSort maiores
    where menores = [y | y <- xs, y <= x]
          maiores = [y | y <- xs, y > x]