-- Defina uma função remove :: Eq a => a -> [a] -> [a] tal que (remove x xs) 
-- devolve uma lista obtida removendo uma ocorrências de x em xs, caso ela exista. Por exemplo,

-- remove 2 [2,3,4,5] == [3,4,5]

-- remove 2 [1,3,2,4] == [1,3,4]

remove x [] = []
remove x (y:xs)
    | x == y = xs
    | otherwise = y: remove x xs