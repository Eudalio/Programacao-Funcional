
-- sucessão infinita de aproximações à raiz quadrada
raiz :: Double -> [Double]
raiz q = iterate (\x->0.5*(x+q/x)) q

-- critérios de paragem
absolute :: [Double] -> Double -> Double
absolute xs eps = head [x | (x,x')<-zip xs (tail xs),
abs(x-x')<eps]

relative :: [Double] -> Double -> Double
relative xs eps = head [x | (x,x')<-zip xs (tail xs),
abs((x-x')/x')<eps]

fibs :: [Integer]
fibs = 0 : 1 : [n+m | (n,m)<-zip fibs (tail fibs)]

primos :: [Integer]
primos = crivo [2..]

crivo :: [Integer] -> [Integer]
crivo (p:xs) = p : crivo [x | x<-xs, x`mod`p/=0]
