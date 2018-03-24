--Defina a função intercala :: a -> [a] -> [a] que intercala um elemento entre valores consecutivos

--numa lista; se a lista tiver menos de dois valores deve ficar inalterada. Exemplos:

{-> intercala 0 [ 1 . . 4 ]

--[1 ,0 ,2 ,0 ,3 ,0 ,4]

--> i n t e r c a l a ' , ' ” abcd ”

--”a , b , c , d”

--> i n t e r c a l a ' , ' ”a”

--”a”

--> intercala 1 [ ]

--[]-}

intercala a xs 
    | (length xs) < 2 = []
    | otherwise = (head xs):a:intercala a (tail xs)
