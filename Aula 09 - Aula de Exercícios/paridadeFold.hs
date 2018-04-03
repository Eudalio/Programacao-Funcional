--Escreva uma função paridadeFold :: [Bool] -> Bool que calcule a paridade de uma lista de boleanos: 
--se o número de valores True for par então a paridade é True, caso contrário é False. Por exemplo, 

--paridadeFold [True,True, False,True] = False

--paridadeFold [True,True, False,True, True] = True

--Paridade usando funções do prelude
paridade xs = if mod (length (filter (==True) xs)) 2 == 0 then True else False

--Paridade Recursivo
paridadeRec [] = True
paridadeRec (x:xs) | x == not (paridadeRec xs) = True
                   | otherwise = False

--Paridade usando foldr
paridadeFold xs = foldr(\x z -> if x == not z then True else False) True xs