
merge2 [] [] = []
merge2 [] ys = ys
merge2 xs [] = xs
merge2 (x:xs) (y:ys) | x < y = x:merge2 xs (y:ys)
										 | y < x = y:merge2 (x:xs) ys
										 | otherwise = x : merge2 xs ys

merge3 xs ys zs = merge2 (merge2 xs ys) zs

hamming = 1 : merge3 xs ys zs 
	where 
	xs = [ 2*x| x <- hamming]
	ys = [ 3*x| x <- hamming]
	zs = [ 5*x| x <- hamming]

teste x h | mod x 2 == 0 = busca (div x 2) h
					| mod x 3 == 0 = busca (div x 3) h
					| mod x 5 == 0 = busca (div x 5) h
					| otherwise = False	

busca x (y:ys) | x < y = False 
							 | x > y = busca x ys 
							 | otherwise = True	 



hamming2 = 1 : [ x | x <- [2..], teste x hamming2]

ehamming :: Int -> Bool
ehamming x | mod x 2 == 0 = ehamming (div x 2)
           | mod x 3 == 0 = ehamming (div x 3)
           | mod x 5 == 0 = ehamming (div x 5)
           | x == 1       = True
           | otherwise    = False

hamming3 :: [Int]
hamming3 = [ x | x <- [1..], ehamming x]



kolakoski = 1 : 2 : 2 : concat [ replicate n y | (y,n) <- zip (cycle [1,2]) (drop 2 kolakoski) ]

fechoKleene xs = "" : [ w ++ [v] | w <- (fechoKleene xs), v <- xs]

crivo [] = []
crivo (x:xs) = x : crivo [y | y <- xs, mod y x /= 0]
primos n = crivo [2..n]
goldbach n = [ head [(par, x, y) | x <- primos n, y <- primos n, par == x+y]| par <- [4,6..n] ] 


fatoracaoAux 1 _      = []
fatoracaoAux n (x:xs) | mod n x  == 0 = x : fatoracaoAux (div n x) (x:xs)
											| otherwise = fatoracaoAux n xs
fatoracaoAux3 1 _      z  = z
fatoracaoAux3 n (x:xs) z  | mod n x  == 0 = fatoracaoAux3 (div n x) (x:xs) (z ++ [x])
											    | otherwise = fatoracaoAux3 n xs z



fatoracao1 n = fatoracaoAux3 n ([2..n]) []
fatoracao2 n = fatoracaoAux3 n (primos n) []


splitints [] = ([],[])
splitints (x:xs) | mod x 2 == 0 = (i, x:p) 
								 | otherwise = (x:i, p)
	where (i,p) = splitints xs

--splitints (x:xs) | mod x 2 == 0 = ( fst (splitints xs), x: snd (splintints xs)) 
--								 | otherwise = (x:i, p)















