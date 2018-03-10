-- Defina a função somaConsecutivos tal que (somaConsecutivos xs) é a soma dos pares de elementos
-- consecutivos de uma lista xs. Por exemplo, 

-- somaConsecutivos [3,1,5,2] == [4,6,7]

-- somaConsecutivos [3] == []

pares xs = zip xs (tail xs)

somaConsecutivos xs = [x+y | (x,y) <- pares xs]