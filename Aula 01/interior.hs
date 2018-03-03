-- Defina a função interior tal que (interior xs) é uma lista obtida eliminando os extremos da lista xs. Por exemplo,
-- interior [2,5,3,7,3] == [5,3,7]

interior:: [a] -> [a]
interior xs = take ((length (tail xs))-1) (tail xs)