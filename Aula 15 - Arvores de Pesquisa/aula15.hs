data Arv a = No a (Arv a) (Arv a) | Vazia deriving (Show, Eq)

listar :: Arv a -> [a]
listar Vazia = []
listar (No x esq dir) = listar esq ++ [x] ++ listar dir

pertence :: Ord a => a -> Arv a -> Bool
pertence x Vazia = False -- não ocorre
pertence x (No y esq dir)
	| x==y = True -- encontrou
	| x<y = pertence x esq -- procura à esquerda
	| x>y = pertence x dir -- procura à direita


inserir :: Ord a => a -> Arv a -> Arv a
inserir x Vazia = No x Vazia Vazia
inserir x (No y esq dir)
	| x==y = No y esq dir -- já ocorre
	| x<y = No y (inserir x esq) dir -- insere à esquerda
	| x>y = No y esq (inserir x dir ) -- insere à direita


fromList :: Ord a => [a] -> Arv a
fromList xs = foldr inserir Vazia xs

construir :: [a] -> Arv a
construir [] = Vazia
construir xs = No x (construir xs') (construir xs'')
	where 
	n = div (length xs) 2 -- ponto médio
	xs' = take n xs -- valores à esquerda
	x:xs'' = drop n xs -- valores central e à direita


mais_esq :: Arv a -> a
mais_esq (No x Vazia _) = x
mais_esq (No _ esq _) = mais_esq esq

remover :: Ord a => a -> Arv a -> Arv a

remover x Vazia = Vazia -- não ocorre

remover x (No y Vazia dir) -- um descendente
	| x == y = dir

remover x (No y esq Vazia) -- um descendente
	| x==y = esq

remover x (No y esq dir) -- dois descendentes
	| x<y  = No y (remover x esq) dir
	| x>y  = No y esq (remover x dir)
	| x==y = No z esq (remover z dir)
	where z = mais_esq dir


removeList :: Ord a =>  [a] -> Arv a-> Arv a
removeList xs arv = foldr remover arv xs


