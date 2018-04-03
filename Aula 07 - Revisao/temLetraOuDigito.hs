--Defina a função temLetraOuDigito :: String -> Bool, usando foldr, que recebe um argumento do tipo String 
--e devolve True, se a string contém algum letra (minúscula ou maiúscula) ou algum dı́gito, e False, caso 
--contrário.

--Dica: Use as funções isLetter :: Char -> Bool e isDigit:: Char -> Bool importando o módulo Data.Char 
--adicionando a seguinte instrução import Data.Char.

import Data.Char

--Resposta usando Recursao
temLetraOuDigitoRec [] = False
temLetraOuDigitoRec (s:str) | isLetter s || isDigit s = True
                         | otherwise = temLetraOuDigitoRec str

--Resposta usando foldr
temLetraOuDigito str = foldr (\x z -> if isLetter x || isDigit x then True else z) False str