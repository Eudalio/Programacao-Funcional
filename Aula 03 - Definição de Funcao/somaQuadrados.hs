-- Defina a função somaQuadrados que recebe um inteiro n como argumento e devolve a soma dos quadrados dos primeiros n inteiros, ou seja,

-- somaQuadrados n == 1^2 + 2^2 + .. + n^2

-- Exemplos

-- somaQuadrados 1 == 1

-- somaQuadrados 2 == 5

-- somaQuadrados 3 == 14

-- somaQuadrados 150 == 1136275

quadrado:: Int -> Int
quadrado x = x*x

somaQuadrados 0 = 0
somaQuadrados n = quadrado n + somaQuadrados(n-1)