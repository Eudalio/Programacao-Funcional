--neglist xs
-- a quantidade de numero negativos

neglist []     = 0
neglist (x:xs)	| x < 0 = 1 + neglist xs
								| otherwise = neglist xs

somaImpares [] = 0
somaImpares (x:xs)	| mod x 2 == 1 = x + somaImpares xs
										| otherwise = somaImpares xs

--interseccao xs ys 

interseccao [] _     = []
--interseccao _  []    = []
interseccao (x:xs) ys | elem x ys = x : interseccao xs ys
											| otherwise =  interseccao xs ys

--somaConsecutivos [2,3,4] = [2+3, 3+4]

somaConsecutivos [] = []
somaConsecutivos [_] = []
somaConsecutivos (x:y:xs) = x+y : somaConsecutivos (y:xs)

--segmento 3 4 [3,4,1,2,7,9,0]
--segmento 2 3 [4,1,2,7,9,0]
--segmento 1 2 [1,2,7,9,0]
--[1,2]

--segmento 3 3 [3,4,1,2,7,9,0]
--segmento 2 2 [4,1,2,7,9,0]
--segmento 1 1 [1,2,7,9,0]
--[1]

--segmento 1 2 [3,4,1,2,7,9,0] == [3,4]
--segmento 1 3 [3,4,1,2,7,9,0] == [3,4,1]
--segmento 1 1 [3,4,1,2,7,9,0] == [3]

--segmento 4 5 [1,2,3]
--segmento 3 4 [2,3]
--segmento 2 3 [3]
--segmento 1 2 []
--[]

segmento 1 m xs = take m xs
segmento n m []     = []
segmento n m (x:xs) | n > m = []
										| n < 0 || m < 0 = []
										| otherwise = segmento (n-1) (m-1) xs
  

par 0 = True
par n = impar(n-1)

impar 0 = False
impar n = par(n-1)

--menores = [1,2,2,3]
--maiores = [9]

--qsort [4,1,2,9,2,3] = qsort [1,2,2,3,4] ++ [4] ++ qsort [9]

-- init [1,2,3,4,5]

--[1,2,3,4]
-- init [1]
--[]
-- init []

--inicio [2,3,4]
--2: inicio [3,4]
--2: 3 : inicio [4]
--2:3:[]
--[2,3]

frequencia a [] = 0
frequencia a (x:xs) | x == a = 1 + frequencia a xs
										| otherwise = frequencia a xs 


--merge [1,3,4] [2,3,4] 
--1 : merge [3,4] [2,3,4]
--1 : 2 : merge [3,4] [3,4]
--1 : 2 : 3 : merge [4] [3,4]
--1 : 2 : 3 : 3 : merge [4] [4]
--1 : 2 : 3 : 3 : 4 : merge [] [4]
--1 : 2 : 3 : 3 : 4 : [4]

--todosPrefixos [2,3,5,6,7,8] = [[],[2],[2,3],[2,3,5],[2,3,5,6],[2,3,5,6,7],[2,3,5,6,7,8]]

--todosPrefixos xs = [ take i xs | i <- [0..n]]
--	where n = length xs

todosPrefixos [] = []
todosPrefixos xs = todosPrefixos (tail xs) ++ [xs]


  

merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y =  x : merge xs (y:ys)
										| otherwise = y : merge (x:xs) ys





inicio []  = error "Lista Vazia"
inicio [x] = []
inicio (x:xs) = x: inicio xs   










































 










