--Defina a função neglist xs que computa o número de elementos negativos em uma lista xs.

--neglist [ 1 , 2 , 3 , 4 , 5 ] == 0
--neglist [1 , −3 , −4 ,3 ,4 , −5] == 3

--Dica: usa a função filter e length.

-- Funciona se usar direto no terminal
-- neglist xs = length ([[x| x <- xs], x < 0])

neglist xs = length (filter (<0) xs)