-- Usando a função foldr, defina a função removeLista tal que (removeLista xs ys) remove todo elemento 
-- de ys que ocorre na lista xs. Por exemplo,

-- removeLista [1,2] [1,1,3,2,2,4,5] == [3,4,5]

-- Dica: A definição de removeLista usando a função foldr tem a seguinte estrutura:  

-- removeLista xs ys = foldr (\x z-> ) [] ys

-- no corpo da função pode ser usada a lista xs.


-- Resolução Recursiva

removeListaRec xs [] = []
removeListaRec xs (y:ys) | elem y xs = removeListaRec xs ys
                      | otherwise = y : removeListaRec xs ys

-- Resolução com Compreção de lista
removeListaComp xs ys = [y | y <- ys, not (elem y xs)]

-- Resolução com foldr
removeLista xs ys = foldr (\x z -> if elem x xs then z else x:z) [] ys