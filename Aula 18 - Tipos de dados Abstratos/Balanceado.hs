import Stack

processa [] p  = isEmpty p
processa (x:xs) p
 	| x == '('  = processa xs (push x p) 
  | x == '['  = processa xs (push x p)
  | x == '{'  = processa xs (push x p)
  | isEmpty p = False  
  | x == ')' && top p == '(' = processa xs (pop p)
  | x == ']' && top p == '[' = processa xs (pop p)
  | x == '}' && top p == '{' = processa xs (pop p)

balanceado xs = processa xs empty

