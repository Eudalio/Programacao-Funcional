
module Conjunto (Conj, 
								 vazio,
								 insere,
                 elimina,
                 pertence,
                 uniao,
                 uniaoLista,    	
) where

--Conjunto como uma lista permitindo elementos repetidos
data Conj a = Cj [a] 

instance (Show a) => Show (Conj a) where
	show conj1 = showConjunto conj1

instance Eq a => Eq (Conj a) where
	(==) c1 c2 = igual c1 c2

showLista []     = ""
showLista [x]    = show x
showLista (x:xs) = show x ++ "," ++showLista xs

showConjunto (Cj []) = "{}"
showConjunto (Cj xs) = "{" ++ showLista xs ++ "}"

vazio :: (Conj a)
vazio = Cj []
          
             
--insere  ::  a -> (Conj a) ‐> (Conj a)
insere x (Cj xs) = Cj (x:xs)

--elimina     :: Eq a => a ‐> (Conj a) ‐> (Conj a)
elimina x (Cj xs) = Cj [ y | y <- xs, y /= x]

--pertence    :: Eq a => a ‐> Conj a ‐> Bool  
pertence x (Cj xs) = elem x xs

--ehVazio     :: Conj a ‐> Bool 
ehVazio (Cj []) 		= True
ehVazio (Cj xs) = False

     
--uniao       :: Conj a -> Conj a -> Conj a
uniao (Cj xs) (Cj ys) = Cj (xs++ys)


--interseccao ::  Eq a => Conj a -> Conj a -> Conj a
interseccao (Cj xs) (Cj ys) = Cj [ x | x <- xs, elem x ys]


--disjuntos   :: Eq a => Conj a -> Conj a -> Bool
disjuntos conj1 conj2 = ehVazio ( interseccao conj1 conj2 )


--diferenca   :: Eq a => Conj a -> Conj a -> Conj a
diferenca (Cj xs) (Cj ys) = Cj [ x | x <- xs, notElem x ys ]

fromList xs = Cj xs

subconjunto c1 c2 = ehVazio (diferenca c1 c2)

igual c1 c2 = subconjunto c1 c2 && subconjunto c2 c1 

--uniaoLista  :: [Conj a] -> Conj a

--              foldr (\conj1 z-> uniao conj1 z) (vazio) xs
--uniaoLista xs = foldr (\c z -> uniao c z) vazio xs
          
uniaoLista xs = foldr uniao vazio xs
  
--interseccaoLista :: [Conj a] -> Conj a

interseccaoLista xs = foldr interseccao (head xs) (tail xs)




