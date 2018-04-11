-- Questão01 - Iten a
--Defina a função final tal que (final n xs) é a lista formada pelos n elementos finais de xs. Por exemplo,
--final 3 [2,5,4,7,9,6] == [7,9,6]

final n xs = reverse(take n (reverse xs))

-- Questão01 - Iten b
-- Defina a função rota tal que (rota n xs) é uma lista formada colocando n primeiros elementos de xs no
--final da lista. Por exemplo,
--rota 1 [3,2,5,7] == [2,5,7,3]
--rota 2 [3,2,5,7] == [5,7,3,2]
--rota 3 [3,2,5,7] == [7,3,2,5]

-- rota n xs = tail xs ++ [head xs]
rota n xs = drop n (xs ++ take n xs)

--Questão02 - Iten a
-- interseccao :: Eq a => [a] -> [a] -> [a] tal que (interseccao xs ys) ´e a interseccao dos
-- conjuntos xs e ys. Por exemplo,

-- interseccao [3,2,5] [5,7,3,4] == [3,5]

interseccao xs ys = [ x | x <- xs, y <- ys, x == y]

-- Questão02 - Iten b
-- diferencia :: Eq a => [a] -> [a] -> [a] tal que (diferencia xs ys) ´e a diferen¸ca entre os conjuntos
-- xs e ys. Por exemplo

-- diferencia [3,2,5,6] [5,7,3,4] == [2,6]
-- diferencia [3,2,5] [5,7,3,2] == []

diferencia xs ys = [ x | x <- xs, not (elem x ys)]

-- Questão03 - Iten a
--A funçãao duplicar :: String -> String repete duas vezes cada vogal (letras ’a’, ’e’, ’i’, ’o’, ’u’
--minúsculas ou maiúsculas) numa cadeia de carateres; os outros carateres devem ficar inalterados.

--Exemplo: duplicar "Ola, mundo!" == "OOlaa, muundoo!"
--Dica: Crie uma lista com as vogais minúsculas e maiúsculas

minúsculas = ['a','e','i','o','u']
maiúsculas = ['A','E','I','O','U']

duplicar [] = []
duplicar (s:str)
    | (elem s maiúsculas) || (elem s minúsculas) = s:s:duplicar str
    | otherwise = s:duplicar str


--Questão03 - Iten b
--A função aplica :: [a -> a] -> a -> [a] recebe uma lista de funções e um valor retornando
--uma lista com os resultados das aplicações das funnções. Por exemplo,

--aplica [(*4), (+2), (-4)] 2 == [8,4,-2]
aplica [] n = []
--aplica (x:xs) n = if x then x n else aplica xs

--Questão04 - Iten a(Recursivo) e b(Foldr)
--Escreva uma fun¸c˜ao paridade :: [Bool] -> Bool que calcule a paridade de uma sequˆencia de bits
--(representados como uma lista de boleanos): se o n´umero de bits de valor True for ´ımpar ent˜ao a paridade ´e
--True, caso contr´ario ´e False.

--Exemplo: paridade [True,True, False,True] = True

--Paridade usando funções do prelude
paridade xs | mod (length (filter (==True) xs)) 2 == 0 = False 
            | otherwise = True

paridadeRec [] = False
paridadeRec (x:xs) | x /= not (paridadeRec xs) = False
                   | otherwise = True

paridadeFold xs = foldr(\x z -> if x /= not z then False else True) False xs

--intersperse 0 [ 1 . . 4 ]
--intersperse ’ , ’ ” abcd ”
--”a , b , c , d”
--[ ]
--intersperse ’,’ ”a”
--”a”

intersperse a [] = []
intersperse a [x] = [x]
intersperse a (x:xs) = x:a:intersperse a xs

--prefixos [] = []
--prefixos [x] = [x]
--prefixos (x:y:xs) = [x] :  prefixos xs

frequencia a [] = 0
frequencia a (x:xs) | x == a    = 1 + frequencia a xs
                    | otherwise = frequencia a xs

--frequencia' [] = 1
--frequencia' (x:xs) | x 

--group xs = frequencia' xs