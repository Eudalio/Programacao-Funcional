--Os números de Hamming formam uma seqüência estritamente crescente de números
--que atendem às seguintes condições:

--1. O número 1 está na sequência.

--2. Se x estiver na sequência, então, 2x, 3x e 5x são também.

--3. Nenhum outro número está na sequência.

--Defina a seguinte lista hamming :: [Int] de modo que hamming é a sequência de Hamming. Por exemplo,

--take 12 hamming == [1,2,3,4,5,6,8,9,10,12,15,16]

--Dica: Defina a função mescla3 tal que (mescla3 xs ys zs) é a lista obtida mesclando as listas ordenadas xy, ys e zs eliminando os elementos duplicados. Por exemplo,

--mescla3 [2,4,6,8,10] [3,6,9,12] [5,10] == [2,3,4,5,6,8,9,10,12]


hamming = 1 : merge3 xs ys zs 
	where 
	xs = [ 2*x| x <- hamming]
	ys = [ 3*x| x <- hamming]
	zs = [ 5*x| x <- hamming]

hamming2 = 1 : [ x | x <- [2..], teste x hamming2]

hamming3 :: [Int]
hamming3 = [ x | x <- [1..], ehamming x]

ehamming :: Int -> Bool
ehamming x | mod x 2 == 0 = ehamming (div x 2)
           | mod x 3 == 0 = ehamming (div x 3)
           | mod x 5 == 0 = ehamming (div x 5)
           | x == 1       = True
           | otherwise    = False