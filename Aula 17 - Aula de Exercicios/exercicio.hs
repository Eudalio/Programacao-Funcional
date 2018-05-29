type Ponto = (Float,Float)
type Regiao = Ponto -> Bool

retang::Ponto->Ponto->Regiao

dist (x1,y1) (x2,y2) = sqrt ( (x2-x1)*(x2-x1) + (y2-y1)*(y2-y1) )
retang (x1,y1) (x2,y2) = \(x,y) -> x >= x1 && x <= x2 && y >= y1 && y <= y2
circ (xc,yc) r = \(x,y) -> (dist (x,y) (xc,yc)) <= r


uniao::Regiao->Regiao->Regiao
uniao r1 r2 = (\(x,y) -> r1 (x,y) || r2 (x,y))

interseccao r1 r2 = (\(x,y) -> r1 (x,y) && r2 (x,y)) 

data Mobile = Barra Int Mobile Int Mobile | Objeto Int

m1 = Barra 6 ( Objeto 1) 2 ( Objeto 3) 
m2 = Barra 4 ( Objeto 2) 2 ( Objeto 4) 
m3 = Barra 1 ( Objeto 1) 1 ( Objeto 1) 
m4 = Barra 3 (m3) 1 (m2) 
m5 = Barra 2 (m4) 6 (m1) 


peso :: Mobile -> Int 
peso (Objeto x) = x
peso (Barra d1 m1 d2 m2) = peso m1 + peso m2


equilibrio :: Mobile -> Bool
equilibrio (Objeto _) = True
equilibrio (Barra d1 m1 d2 m2) = d1*(peso m1) == d2*(peso m2) && equilibrio m1 && equilibrio m2

data Fracao = Fracao Int Int deriving (Show)

numerador :: Fracao -> Int
numerador (Fracao x y) = x

denominador :: Fracao -> Int
denominador (Fracao x y) = y

valor :: Fracao -> Double
valor (Fracao x y) = fromIntegral x / fromIntegral y

soma :: Fracao -> Fracao -> Fracao
soma (Fracao a b) (Fracao c d) = simplificar ( Fracao (a*d+c*b) (b*d) ) 
sub (Fracao a b) (Fracao c d) = simplificar ( Fracao (a*d-c*b) (b*d) ) 
mult (Fracao a b) (Fracao c d) = simplificar ( Fracao (a*c) (b*d) )
inverso (Fracao a b) = Fracao b a 
divisao f1 f2 = mult f1 (inverso f2)


mdc a 0 = a
mdc a b = mdc b (mod a b)

simplificar :: Fracao -> Fracao
simplificar (Fracao a b) = Fracao (div a d) (div b d)
	where d = mdc a b


splits [] = [([],[])]
--splits (x:xs) = [ (x:a,b) | (a,b) <- splits xs] ++ [ (a,x:b) | (a,b) <- splits xs]
splits (x:xs) = zip (reverse (subsets xs) ) (subsets xs)

--[ (x:a,b) ++ (a,b:x) | (a,b) <- splits xs] 

-- [ (a,x:b) | (a,b) <- splits xs]
	




subsets []     = [[]]
subsets (x:xs) = subsets xs ++ [ ys++[x] | ys <- subsets xs]
--subsets xs = subsets (init xs) ++ [ ys ++ [last xs] | ys <- subsets (init xs)]



--subsets [1] = [[],[1]]
--subsets [1,2] = [[],[1],[2],[1,2]]
--subsets [1,2,3] = [[],[1],[2],[1,2],[3],[1,3],[2,3],[1,2,3]]







































