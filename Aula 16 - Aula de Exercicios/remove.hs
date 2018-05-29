data MConj a = Vazia | No a Int (MConj a) (MConj a) deriving (Show, Eq)
remove :: Ord a => a -> Int -> MConj a -> MConj a

mais_esq (No y n Vazia Vazia) = (y,n)
mais_esq (No y n esq dir) = mais_esq esq

remove x q Vazia = Vazia

remove x q (No y n Vazia dir)
    | x == y    = dir
    | otherwise = (No y n Vazia (remove x q dir))

remove x q (No y n esq Vazia)
    | x == y    = esq
    | otherwise = (No y n (remove x q esq) Vazia)

remove x q (No y n esq dir)
    | x == y    = (No fst2 snd2 esq (remove fst2 snd2 dir))
    | x > y     = (No y n esq (remove x n dir))
    | otherwise = (No y n (remove x n esq) dir)
    where fst2 = fst (mais_esq dir)
          snd2 = snd (mais_esq dir)