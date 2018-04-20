data Figura = Circ Float | Rect Float Float deriving (Show, Eq)

data Alunos = Samuel | Leticia | Alcides deriving (Show, Eq)

quadrado :: Float -> Figura
quadrado h = Rect h h

area :: Figura -> Float
area (Circ r) = pi*r^2
area (Rect w h) = w*h


safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n`div`m)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)


data Nat = Zero | Succ Nat deriving (Show, Eq)

--data Inteiro = Zero | Succ Nat | Pred Nat deriving (Show, Eq)


add Zero m = m
add (Succ n) m = Succ ( add n m)

prod Zero m        = Zero
prod (Succ Zero) m = m
prod (Succ n)    m = add m (prod n m)
 
data Expr = Val Int | Soma Expr Expr | Mult Expr Expr | Mod Expr Expr | Div Expr Expr deriving (Show, Eq)

valor :: Expr -> Int
valor (Val n) = n
valor (Soma e1 e2) = valor e1 + valor e2
valor (Mult e1 e2) = valor e1 * valor e2
valor (Mod  e1 e2) = mod (valor e1) (valor e2)

divisao :: Expr -> Expr -> Maybe Int
divisao expr1 expr2
 | valor expr2 == 0 = Nothing
 | otherwise = Just ( div (valor expr1) (valor expr2) )


data Arv = Vazia | No Int Arv Arv  deriving (Show, Eq)


insere :: Int -> Arv -> Arv
insere x Vazia          = No x Vazia Vazia
insere x (No y esq dir) 
 | x == y = (No y esq dir)
 | x < y  = (No y (insere x esq) dir)
 | otherwise = (No y esq (insere x dir) )  


sumArv :: Arv -> Int
sumArv Vazia          = 0
sumArv (No x esq dir) = x + sumArv esq + sumArv dir 


alturaArv :: Arv -> Int
alturaArv Vazia = 0
alturaArv (No x esq dir) = 1 + max (alturaArv esq)  (alturaArv dir)


nivelArv :: Arv -> Int -> [Int]
nivelArv Vazia _          = []
nivelArv (No x esq dir) 0 = [x]
nivelArv (No x esq dir) n = nivelArv esq (n-1) ++ nivelArv dir (n-1)   



























 


























