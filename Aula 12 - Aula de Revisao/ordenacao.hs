
remove x [] = []
remove x (y:ys) | x == y    = ys
	| otherwise = y : remove x ys  

ordsel [] = []
ordsel xs = m : ordsel (remove m xs)
	where m = minimum xs

inserir x xs = takeWhile (<x) xs ++ [x] ++ dropWhile (<x) xs

ordins []     = []
ordins (x:xs) = inserir x (ordins xs)

merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
										| otherwise = y : merge (x:xs) ys

metades xs = (take m xs,drop m xs)
	where 
	m = div (length xs) 2
	
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort l)( mergesort r)
	where (l,r) = metades xs

quicksort [] = []
quicksort (x:xs) = quicksort esq ++ [x] ++ quicksort dir
	where 
	esq = [y | y <- xs, y < x]
	dir = [y | y <- xs, y >= x]


trocas [] = []
trocas [x] = [x]
trocas (x:y:xs) | x <= y = x : trocas (y:xs)
								| otherwise = y : trocas (x:xs)




bubblesort [] = []
bubblesort xs = bubblesort (init ys) ++ [last ys]
	where ys = trocas xs

trocas2 [] = ([], 0)
trocas2 [x] = ([x], 0)
trocas2 (x:y:xs) | x <= y = (x:l1, z1)  
								 | otherwise = (y:l2, z2 + 1) 
	where 
	(l1, z1) = trocas2 (y:xs)
	(l2, z2) = trocas2 (x:xs)


bubblesort2 [] = ([],0)
bubblesort2 xs  = (zs ++ [last ys], n1+n2)
	where 
	(ys, n1) = trocas2 xs
	(zs, n2) = bubblesort2 (init ys)

contaTrocas xs = z
	where (ys, z) = bubblesort2 xs
				

incrementa p z = [ if i == p then x+1 else x| (i,x) <- zip  [0..] z]

contagem ::  [Int] -> [Int]
contagem xs = foldr (\x z-> incrementa (x-min) z ) (replicate  (max-min+1) 0) xs
	where 
	min = minimum xs
	max = maximum xs

mostraContagem min [] = []
mostraContagem min (x:xs) = replicate x min ++ mostraContagem (min+1) xs 

ordenaContagem ::  [Int] -> [Int]
ordenaContagem [] = []
ordenaContagem xs = mostraContagem (minimum xs) (contagem xs)



	







