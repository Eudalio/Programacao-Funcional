import Stack

processa [] p = isEmpty p
processa (x:xs) p
 | x == '(' || x == '[' = processa xs (push x p)
 | isEmpty p = False
 | x == ')' && (top p) == '(' = processa xs (pop p)
 | x == ']' && (top p) == '[' = processa xs (pop p)
 | otherwise = False

--Recebe uma string formada apenas ()
balanceado :: String -> Bool
balanceado xs = processa xs empty


fromList :: [a] -> Stack a
fromList xs = makeStack (reverse xs)

processaPalindromo [] p = isEmpty p
processaPalindromo (x:xs) p 
 | x == top p = processaPalindromo xs (pop p)
 | otherwise = False

palindrome xs
 | mod (length xs) 2 == 0 = processaPalindromo  (b)      (fromList a) 
 | otherwise              = processaPalindromo  (tail b) (fromList a)
 where 
 m  = div (length xs) 2
 (a,b) = (take m xs, drop m xs)  		




