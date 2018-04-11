--Desenvolva a função splitints tal que (splitints xs) devolve uma tupla de duas listas, (A,B), 
--onde A e B são respectivamente compostas pelos inteiros ímpares e pares de xs. Por exemplo,

--splitints [1,2,3,4,5,6,7] == ([1,3,5,7],[2,4,6])

--splitints xs = [(x),(y) | x <- filter odd xs, y <- filter even xs]

--pares xs = [x | x <- filter even xs]
--impares xs = [x | x <- filter odd xs]
--splitints xs = (impares xs, pares xs)

splitints [] = ([],[])
splitints (x:xs) | mod x 2 == 0 = (fst (splitints xs),x:snd (splitints xs))
                 | otherwise = (x:fst (splitints xs), snd (splitints xs))

splitintsBonito [] = ([],[])
splitintsBonito (x:xs) | mod x 2 == 0 = (f, x:s)
                 | otherwise = (x:f, s)
    where (f,s) = splitints xs