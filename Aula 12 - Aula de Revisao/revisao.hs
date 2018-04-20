import Prelude hiding (elem)

removeListaRec xs [] = []
removeListaRec xs (y:ys) | elem y xs = removeListaRec xs ys  
						 | otherwise = y : removeListaRec xs ys

removeListaComp xs ys = [ y | y <- ys, not (elem y xs)]

removeListaFold xs ys = foldr (\x z-> if elem x xs then z else x:z) [] ys

elem x [] = False
elem x (y:ys) | x == y = True
			  | otherwise = elem x ys	

remdupsRec [] = []
remdupsRec (x:xs) | elem x xs = remdupsRec xs 
				  |	otherwise = x : remdupsRec xs

remdups xs = foldr (\x z-> if elem x z then z else x:z) [] 

metadePares xs = map (\x-> div x 2) (filter even xs)

noIntervalo a b xs = [ y | y <- xs, y >= a && y <= b]

intercala a [] = []
intercala a [x] = [x]
intercala a (x:xs) = x:a:(intercala a xs)

filtrandoCaudas xss = [ tail xs| xs <- xss, not (null xs)]


filtrandoCaudas2 xss = [ tail xs| xs <- xss, not (null xs), (head xs) > 5]

temVazia xss = elem [] xss
--temVazia xss = or (map (\xs -> length xs == 0) xss)

agrupa xss | elem [] xss = [] 
		   | otherwise = (map head xss) : agrupa (map tail xss)	

fat n = product [1..n]

approxn n = foldl (\z x-> z + 1/ (fromIntegral (fat x) ) ) 0 [0..n]



























