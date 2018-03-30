--Defina a função filtraAplicaFold :: (a->b) -> (a->Bool)->[a]->[b] tal que (filtraAplicaFold f p xs) 
--é uma lista obtida aplicando a função f aos elementos de xs que satisfazem o predicado p usando a 
--função foldr. Por exemplo,

--filtraAplicaFold (4+) (<3) [1..7] == [5,6]

filtraAplicaRec f p [] = []
filtraAplicaRec f p (x:xs) | p x = f x: filtraAplicaRec f p xs
                           | otherwise = filtraAplicaRec f p xs

filtraAplicaFold f p xs = foldr(\x z -> if p x then f x:z else z) [] xs