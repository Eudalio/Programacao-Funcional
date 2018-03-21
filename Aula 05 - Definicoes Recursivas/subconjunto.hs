-- Defina uma função subconjunto :: Eq a => [a] -> [a] -> Bool tal que (subconjunto xs ys)
-- verifica xs é um subconjunto de ys. Por exemplo,

-- subconjunto [ 3 , 2 , 3 ] [ 2 , 5 , 3 , 5 ] == True

-- subconjunto [ 3 , 2 , 3 ] [ 2 , 5 , 6 , 5 ] == F a l s e

-- Defina os seguintes casos:

-- Dica: use a função elem do Prelude.

-- Passa em quatro teste
--subconjunto [] ys = True
--subconjunto (x:xs) ys 
--    | elem x ys == True = True
--    | elem x ys == False = False
--    | otherwise = subconjunto xs ys

-- Método que foi entregue
subconjunto [] ys = True
subconjunto (x:xs) ys = if (elem x ys == True) then (subconjunto xs ys) else False