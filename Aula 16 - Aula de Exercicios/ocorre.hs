--Considere a definição em Haskell dum tipo de dados para multiconjuntos (i.e. coleções sem ordem mas com repetições) representado como árvore de pesquisa:

--data MConj a = Vazio | No a Int (MConj a) (MConj a)

--Cada no contém um valor e a sua multiplicidade (i.e. o numero de repetições); para facilitar a pesquisa, a arvore deve estar ordenada pelos valores. ´

--Por exemplo:

--No 'A' 2 Vazio (No 'B' 1 Vazio Vazio)

--representa o multi-conjunto {A, A, B} com dois carateres 'A' e um 'B'.

--Escreva função ocorre :: Ord a => a -> MConj a -> Int que procura o numero de  ocorrências de um valor num multiconjunto; o resultado deve ser 0 se o valor nao pertencer ao multiconjunto.

--ocorre 1 No 1 3 Vazia (No 2 3 Vazia (No 3 2 Vazia Vazia)) == 3

--ocorre 2 No 1 3 Vazia (No 2 3 Vazia (No 3 2 Vazia Vazia)) == 3

--ocorre 3 No 1 3 Vazia (No 2 3 Vazia (No 3 2 Vazia Vazia)) == 2

--ocorre 4 No 1 3 Vazia (No 2 3 Vazia (No 3 2 Vazia Vazia)) == 0

data MConj a = Vazia | No a Int (MConj a) (MConj a)

ocorre x Vazia = 0
ocorre x (No a n esq dir) = if (x /= a) then 0 + (ocorre x esq) + (ocorre x dir) else (n + (ocorre x esq) + (ocorre x dir))