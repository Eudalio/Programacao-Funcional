--A conjetura de Goldbach, proposta pelo matemático prussiano Christian Goldbach, é um dos problemas mais antigos não resolvidos da matemática, mais precisamente da teoria dos números.

--Ela diz que todo número par maior que 2 pode ser representado pela soma de dois números primos.

--Por exemplo: 4 = 2 + 2; 6 = 3 + 3; 8 = 5 + 3; 10 = 3 + 7 = 5 + 5; 12 = 5 + 7; etc.

--Verificações por computador já confirmaram a conjetura de Goldbach para vários números. No entanto, a efetiva demonstração matemática ainda não ocorreu.

--Defina uma função goldbach :: Int -> [(Int,Int,Int)] tal que (goldbach n) devolve uma lista contendo  triplas (x,y,z) , onde x é um número par maior que 2 e menor que n e y e z são dois primos tal que x = y+z.

crivo [] = []
crivo (x:xs) = x : crivo [y | y <- xs, mod y x /= 0]
primos n = crivo [2..n]
goldbach n = [ head [(par, x, y) | x <- primos n, y <- primos n, par == x+y]| par <- [4,6..n] ] 