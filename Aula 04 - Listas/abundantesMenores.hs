-- Defina a função abundantesMenores tal que (abundantesMenores n) devolve uma lista de números
-- abundantes menores ou igual a n.

-- abundantesMenores 50 == [12,18,20,24,30,36,40,42,48]

-- abundantesMenores 100 == [12,18,20,24,30,36,40,42,48,54,56,60,66,70,72,78,80,84,88,90,96,100]

divisores n = [ x | x <- [1..n], (mod n x) == 0]

abundante n
    | sum (init (divisores n)) > n = True
    | otherwise = False

abundantesMenores n = [x | x <- [1..n] , (abundante x) == True]