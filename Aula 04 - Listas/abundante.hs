-- Números abundantes são números natural maiores que a soma dos seus divisores próprios.
-- Por exemplo, os divisores próprios de 12 são 1, 2, 3, 4 e 6. A soma dos divisores é 16. 
-- Logo, 12 é um número abundante. Por outro lado, o número 5 não é abudante uma vez que ele 
-- possui apenas um divisor próprio que é 1.

-- abundante 12 == True

-- abundante   5 == False

divisores n = [ x | x <- [1..n], (mod n x) == 0]

abundante n
    | sum (init (divisores n)) > n = True
    | otherwise = False