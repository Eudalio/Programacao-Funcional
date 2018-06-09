diagonalPrincipal [[]]       = []
diagonalPrincipal (xs:[])    = [head xs]
diagonalPrincipal (x:xs)     = head x : diagonalPrincipal (map tail xs)