--Defina uma função filtrandoListas :: [[a]]->[[a]] tal que (filtrandoListas xss) devolve uma lista 
--contendo o maior prefixo do mesmo tamanho de cada lista de xss.

--filtrandoListas [[3,2,1],[3,4],[4,3,2,1]] == [[3,2],[3,4],[4,3]]

--filtrandoListas usando funções do prelude

--Ordeno a lista
insertSort [] = []
insertSort (x:xs) = insertSort menores ++ [x] ++ insertSort maiores
    where menores = [y | y <- xs, y <= x]
          maiores = [y | y <- xs, y > x]

--Pego o primeiro elemento da lista ordenada
menorLista xss = head (insertSort [length xs | xs <- xss])

--filtrandoListas mode prelude or gambiarra
filtrandoListas xss = [ take (menorLista xss) x | x <- xss]