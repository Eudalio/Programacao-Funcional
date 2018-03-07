-- Defina a função somaQuadrados que recebe um inteiro n como argumento e devolve a soma dos quadrados dos primeiros n inteiros, ou seja,

-- somaQuadrados n == 1^2 + 2^2 + .. + n^2

-- Exemplos

-- somaQuadrados 1 == 1

-- somaQuadrados 2 == 5

-- somaQuadrados 3 == 14

-- somaQuadrados 150 == 1136275

-- Essa forma funciona, porém não é esperado que se use recursão neste ponto da disciplina
-- quadrado:: [Int] -> Int
-- quadrado xs = [x*x | x <- xs]

-- somaQuadrados 0 = 0
-- somaQuadrados n = quadrado n + somaQuadrados(n-1)

-- A forma que é mais correta para este ponto da disciplina seria essa

somaQuadrados n = sum ([n*n | n <- [0..n]])