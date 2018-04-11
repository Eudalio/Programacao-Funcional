--Escreva a função maiorIndice tal que (maiorIndice xs) devolve uma tupla (z, id) onde z é o maior
--elemento da lista xs e id é a posição do maior elemento na lista xs.

--maiorIndice [1,3,4,6]

--(6,3)

--maiorIndice [1,3,7,4,6]

--(7,2)

--maiorIndice [1,3,7,4,6,3]

--(7,2)

--maiorIndice [1,3,7,4,6,3,9]

--(9,6)

maiorIndice [x] = (x, 0)
maiorIndice (x:xs) | x >= m = (x,0)
				   | otherwise = (m, id + 1)
		where (m, id) = maiorIndice xs