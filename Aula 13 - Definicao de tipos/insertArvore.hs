--Considere a seguinte definição de árvore binária do tipo a

--data Arv a = Vazia | No a ( Arv a ) ( Arv a ) deriving (Eq , Show)

--Escreva a função insertArvore :: Ord a => a -> Arv a -> Arv a tal que (insertArvore x arv) 
--insere um valor x numa árvore de pesquisa ordenada arv, inserindo os valores menores ou 
--iguais a raiz na subárvore da esquerda e os valores maiores na subárvore da direita.

--insertArvore 2 Vazia == No 2 Vazia Vazia

data Arv a = Vazia | No a ( Arv a ) ( Arv a ) deriving (Eq , Show)

insertArvore x Vazia = No x Vazia Vazia
insertArvore x (No y esq dir)
    | x <= y = No y (insertArvore x esq) dir
    | x > y = No y esq (insertArvore x dir)