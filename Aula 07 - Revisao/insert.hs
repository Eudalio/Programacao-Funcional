--A função insert :: Ord a => a -> [a] -> [a] tal que (insert x xs) devolve uma lista ordenada obtida
--pela inserção de x na lista ordenada xs.

--Exemplo: insert 2 [0, 1, 3, 5] == [0, 1, 2, 3, 5]

insert a xs = [x | x <- filter (<=a) xs] ++ [a] ++ [x | x <- filter (>a) xs]