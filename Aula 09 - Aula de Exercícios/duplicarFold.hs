--A função duplicarFold :: String -> String repete duas vezes cada vogal 
--(letras 'a', 'e', 'i', 'o', 'u' minúsculas ou maiúsculas) numa cadeia de carateres; os outros carateres 
--devem ficar inalterados. Por exemplo, 

--duplicar "Ola, mundo!"== "OOlaa, muundoo!"

--Dica: Crie uma lista com as vogais minúsculas e maiúsculas.

vogais_minusculas = ['a','e','i','o','u']
vogais_maiusculas = ['A','E','I','O','U']

duplicarRec [] = []
duplicarRec (x:xs) | elem x vogais_maiusculas || elem x vogais_minusculas = x:x:duplicarRec xs  
                   | otherwise = x:duplicarRec xs

duplicarFold xs = foldr(\x z -> if (elem x vogais_maiusculas || elem x vogais_minusculas) then x:x:z else x:z) [] xs