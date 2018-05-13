--Considere a definição em Haskell dum tipo de dados para multiconjuntos (i.e. coleções sem ordem mas com repetições) representado como árvore de pesquisa:

--data MConj a = Vazio | No a Int (MConj a) (MConj a)

--Cada no contém um valor e a sua multiplicidade (i.e. o numero de repetições); para facilitar a pesquisa, a arvore deve estar ordenada pelos valores. ´

--Por exemplo:

--No 'A' 2 Vazio (No 'B' 1 Vazio Vazio)

--representa o multi-conjunto {A, A, B} com dois carateres 'A' e um 'B'.

--Escreva a funcão tamanho :: MConj a -> Int para calcular o numero de elementos de um multiconjunto. 

--tamanho ( No 1 3 Vazia (No 2 3 Vazia (No 3 3 Vazia Vazia)) ) == 9

--tamanho (No 1 2 Vazia (No 2 3 Vazia (No 4 1 Vazia (No 5 2 Vazia Vazia))) ) == 8

data MConj a = Vazia | No a Int (MConj a) (MConj a)

tamanho Vazia = 0
tamanho (No x n esq dir) = n + (tamanho esq) + (tamanho dir)