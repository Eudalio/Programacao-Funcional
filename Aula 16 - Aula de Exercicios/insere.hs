--Considere a definição em Haskell dum tipo de dados para multiconjuntos (i.e. coleções sem ordem mas com repetições) representado como árvore de pesquisa:

--data MConj a = Vazio | No a Int (MConj a) (MConj a)

--Cada no contém um valor e a sua multiplicidade (i.e. o numero de repetições); para facilitar a pesquisa, a arvore deve estar ordenada pelos valores. ´

--Por exemplo:

--No 'A' 2 Vazio (No 'B' 1 Vazio Vazio)

--representa o multi-conjunto {A, A, B} com dois carateres 'A' e um 'B'.

--Escreva a funcão insere :: Ord a => a -> MConj a -> MConj a que insere um valor num multiconjunto mantendo a arvore de pesquisa ordenada.

--insere 1 Vazia

--No 1 1 Vazia Vazia

--insere 1 $ insere 1 Vazia

--No 1 2 Vazia Vazia

--insere 2 $ insere 1 $ insere 1 Vazia

--No 1 2 Vazia (No 2 1 Vazia Vazia)

--insere 2 $ insere 2 $ insere 1 $ insere 1 Vazia

--No 1 2 Vazia (No 2 2 Vazia Vazia)

data MConj a = Vazia | No a Int (MConj a) (MConj a) deriving (Show)

insere x Vazia = No x 1 Vazia Vazia
insere x (No y n esq dir)
    | x == y = No y (n + 1) esq dir
    | x < y = No y n (insere x esq) dir
    | x > y = No y n esq (insere x dir)