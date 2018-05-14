--Escreva  a função toList :: Arv  a -> [a]  tal que (toList arv ) devolve uma lista dos  elementos de uma árvore de pesquisa em ordem crescente.

--toList No 4 (No 2 (No 1 Vazia Vazia) (No 3 Vazia Vazia)) (No 9 (No 8 Vazia Vazia) Vazia) == [1,2,3,4,8,9]

--toList No 5 (No 2 (No 1 (No 1 Vazia Vazia) Vazia) (No 5 Vazia Vazia)) (No 6 Vazia (No 7 Vazia Vazia)) == [1,1,2,5,5,6,7]

data Arv a = Vazia | No a ( Arv a ) ( Arv a ) deriving (Eq , Show)
    
toList Vazia = []
toList (No y esq dir) = (toList esq) ++ [y] ++ (toList dir)