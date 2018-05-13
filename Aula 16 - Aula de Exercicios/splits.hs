--Defina a função splits :: [a] -> [([a],[a])], que retorna a lista de todas as possı́veis partições, em duas listas, da lista dada como argumento . Por exemplo:

--splits [1,2,3] == [ ([1,2,3],[]),  ([1,2],[3]),([1,3],[2]),([1],[2,3]),([2,3],[1]),([2],[1,3]),([3],[1,2]),([],[1,2,3])]

--splits [1,2,3,4] == [([1,2,3,4],[]),([1,2,3],[4]),([1,2,4],[3]),([1,2],[3,4]),([1,3,4],[2]),([1,3],[2,4]),([1,4],[2,3]),([1],[2,3,4]),([2,3,4],[1]),([2,3],[1,4]),([2,4],[1,3]),([2],[1,3,4]),([3,4],[1,2]),([3],[1,2,4]),([4],[1,2,3]),([],[1,2,3,4])]

splits [] = [([],[])]
splits (x:xs) = [ (x:a,b) | (a,b) <- splits xs] ++ [ (a,x:b) | (a,b) <- splits xs]
--splits (x:xs) = zip (reverse (subsets xs) ) (subsets xs)

--[ (x:a,b) ++ (a,b:x) | (a,b) <- splits xs] 

-- [ (a,x:b) | (a,b) <- splits xs]

subsets []     = [[]]
--subsets (x:xs) = subsets xs ++ [ ys++[x] | ys <- subsets xs]
subsets xs = subsets (init xs) ++ [ ys ++ [last xs] | ys <- subsets (init xs)]

--subsets [1] = [[],[1]]
--subsets [1,2] = [[],[1],[2],[1,2]]
--subsets [1,2,3] = [[],[1],[2],[1,2],[3],[1,3],[2,3],[1,2,3]]
