--Escreva a  função fatoracao :: Int -> [Int] tal que (fatoracaoPrimos n) retorna a lista de todos os 
--fatores primos de um número n . Exemplos:

--fatoracao 8 == [2,2,2]

--fatoracao 42 == [2,3,7]

--fatoracao 50 == [2,5,5]

-- Dica: Desenvolva uma função fatoracaoAux n xs que devolve a lista dos fatores primos de n na lista xs.

fatoracaoAux 1 _ = []
fatoracaoAux n (x:xs) | mod n x == 0 = x:fatoracaoAux (div n x) (x:xs)
                      | otherwise = fatoracaoAux n xs

fatoracao n = fatoracaoAux n [2..n]