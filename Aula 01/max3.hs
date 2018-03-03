-- Defina a função maxTres tal que (maxTres x y z) é o máximo entre x, y e z. Por exemplo,
-- maxTres 6 2 4 == 6
-- maxTres 6 7 4 == 7
-- maxTres 6 7 9 == 9

max3 :: Int -> Int -> Int -> Int

max3 a b c
    | a >= b && a > c = a
    | b > c           = b
    |otherwise        = c