    remdupsRec [] = []
    remdupsRec (x:xs) | elem x xs = remdupsRec xs 
     				  |	otherwise = x : remdupsRec xs
     
    fstDominio xs = [fst x | x <- xs]
    
    quicksort [] = []
    quicksort (x:xs) = quicksort esq ++ [x] ++ quicksort dir
    	where 
    	esq = [y | y <- xs, y < x]
    	dir = [y | y <- xs, y >= x]
    
    dominio xs = quicksort (remdupsRec (fstDominio xs))