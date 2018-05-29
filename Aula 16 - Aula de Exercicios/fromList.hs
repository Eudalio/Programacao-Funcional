--Escreva a função (fromList :: [a] -> LinkedList a) tal que (fromList xs) devolve  uma lista encadeada com os elementos da lista xs seguindo a ordem dos elementos da lista.

--fromList [1,2,3] == No 1 (No 2 (No 3 Vazia))

--fromList [1,2,3,4] == No 1 (No 2 (No 3 (No 4 Vazia)))

--fromList [1,2,3,4,2] == No 1 (No 2 (No 3 (No 4 (No 2 Vazia))))

--data LinkedList a = Vazia | No a (LinkedList a) deriving (Eq, Show)
data MConj a = Vazia | No a Int (MConj a) (MConj a) deriving(Show)
--fromList [] = Vazia
--fromList (x:xs) = No x (fromList xs)

contaRepetidos y [] = 0
contaRepetidos y (x:xs)
    | x == y = 1 + contaRepetidos y xs
    | otherwise = contaRepetidos y xs

fromList [] = Vazia
fromList (x:xs)
    | elem x xs = (No x ( 1 + contaRepetidos x xs) (fromList (filter (/=x) xs)) (fromList (filter (/=x) xs)))
    | otherwise = fromList xs