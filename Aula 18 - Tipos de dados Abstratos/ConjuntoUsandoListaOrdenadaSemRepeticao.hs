module Conjunto (Conj, 
								 vazio,
								 insere,
                 elimina,
                 pertence,
                 uniao,
                 uniaoLista,
								 tamanho,
								 diferenca,
                 fromList    	
) where

--Conjunto como uma lista sem elementos repetidos com
--os elementos ordenados

data Conj a = Cj [a] deriving (Eq)

instance (Show a) => Show (Conj a) where
	show conj1 = showConjunto conj1

showLista []     = ""
showLista [x]    = show x
showLista (x:xs) = show x ++ "," ++showLista xs

showConjunto (Cj []) = "{}"
showConjunto (Cj xs) = "{" ++ showLista xs ++ "}"

vazio :: (Conj a)
vazio = Cj []
                       
insereOrd x [] = [x]
insereOrd x (y:ys) 
 | x < y 			= x:y:ys
 | x > y 		 = y:(insereOrd x ys)
 | otherwise = (y:ys)

insere x (Cj xs) = Cj (insereOrd x xs) 

elimina x (Cj xs) = Cj [ y | y <- xs, y /= x]

pertenceOrd x [] = False
pertenceOrd x (y:ys) 
	| x == y = True
  | x > y  = pertenceOrd x ys
  | x < y  = False 


pertence x (Cj xs) = pertenceOrd x xs

ehVazio (Cj []) 		= True
ehVazio (Cj xs)     = False

merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) 
 | x < y  = x : merge xs (y:ys)
 | y < x  = y : merge (x:xs) ys
 | x == y = x : merge xs ys
     
uniao (Cj xs) (Cj ys) = Cj (merge xs ys)

interseccao (Cj xs) (Cj ys) = Cj [ x | x <- xs, pertenceOrd x ys]

disjuntos conj1 conj2 = ehVazio ( interseccao conj1 conj2 )

diferenca (Cj xs) (Cj ys) = Cj [ x | x <- xs, notElem x ys ]

fromList xs = foldr insere vazio xs

subconjunto c1 c2 = ehVazio (diferenca c1 c2)

igual c1 c2 = subconjunto c1 c2 && subconjunto c2 c1 
          
uniaoLista xs = foldr uniao vazio xs
  
interseccaoLista xs = foldr interseccao (head xs) (tail xs)

tamanho (Cj xs) = length xs


