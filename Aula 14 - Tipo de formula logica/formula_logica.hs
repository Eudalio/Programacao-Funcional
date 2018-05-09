import Data.List hiding (find)

data Prop = Const Bool -- constantes
	| Var Char -- variáveis
	| Neg Prop -- negação
	| Conj Prop Prop -- conjunção
	| Disj Prop Prop -- disjunção
	| Impl Prop Prop -- implicação
	deriving (Eq,Show)




removeRepetidos [] = []
removeRepetidos (x:xs)
 | elem x xs = removeRepetidos xs
 | otherwise = x:removeRepetidos xs  	
 

variaveis :: Prop -> [Char]
variaveis (Const _) = []
variaveis (Var c) = [c]
variaveis (Neg p) = sort.nub $ variaveis p
variaveis (Conj p1 p2) = sort.nub $ variaveis p1 ++ variaveis p2
variaveis (Disj p1 p2) = sort.nub $ variaveis p1 ++ variaveis p2
variaveis (Impl p1 p2) = sort.nub $ variaveis p1 ++ variaveis p2

type Assoc ch v = [(ch,v)]


find :: Eq ch => ch -> Assoc ch v -> v
find ch assocs = head [v | (ch',v)<-assocs, ch==ch']


type Atrib = Assoc Char Bool

valor :: Atrib -> Prop -> Bool
valor s (Const b) = b
valor s (Var x) = find x s
valor s (Neg p) = not (valor s p)
valor s (Conj p q) = valor s p && valor s q
valor s (Disj p q) = valor s p || valor s q
valor s (Impl p q) = not (valor s p) || valor s q

atr1 = [('A',True), ('B', False), ('C', False)]
atr2 = [('A',True), ('B', True), ('C', False)]

prop1 = Impl (Var 'A') (Var 'B')
prop2 = Disj (Var 'A') (Var 'C')
prop3 = Conj (Neg prop1) prop2

bits :: Int -> [[Bool]]
bits 0 = [[]]
bits n = [b:bs | bs<-bits (n-1), b<-[False,True]]


atribs :: Prop -> [Atrib]
atribs p = map (zip vs) (bits (length vs))
	where vs = nub (variaveis p)


tautologia :: Prop -> Bool
tautologia p = and [valor s p | s<-atribs p]



--Escreva uma função para retornar as atribuições que tornam uma proposição falsa
--Escreva uma função para retornar as atribuições que tornam uma proposição falsa
--Escreva uma função que decide se duas proposições são equivalentes
--Escreva uma função que decide se uma proposição implica logicamente em outra

--Escreva uma função contar :: Prop -> [(Char, Int)] cujo resultado é uma lista de associações entre cada variável e o número de vezes que ocorre na proposição.

--contar (Conj (Var 'a') (Disj (Var 'b') (Var 'a')) = [('a', 2), ('b', 1)]


--Escreva uma definição duma função \texttt{showProp :: Prop -> String} para converter uma proposição em texto;

--showProp (Neg (Var 'a'))
--"(~a)"
--showProp (Disj (Var 'a') (Conj (Var 'a') (Var 'b')))
--(a || (a && b))
--showProp (Impl (Var 'a') (Impl (Neg (Var 'a')) (Const False))
--"(a -> ((~a) -> F))"



