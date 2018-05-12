--A sequência Kolakoski (seqüência OEIS A000002) é a seqüência infinita  a_1,a_2,a_3,\ldots  
--de uns e dois consistindo de um bloco de  a_1  de uns, seguido de um bloco de  a_2  de dois,
--seguido de um bloco  a_3  de uns, e assim por diante. 

--Defina a seguinte lista kolakoski :: [Int] de modo que kolakoski é a sequência de Kolakoski 
--considerando  a_1 = 1  = 1 ,  a_2 = 2 . e  a_3 = 2  Por exemplo,

--take 5 kolakoski == [1,2,2,1,1]

--take 10 kolakoski == [1,2,2,1,1,2,1,2,2,1]

--take 24 kolakoski == [1,2,2,1,1,2,1,2,2,1,2,2,1,1,2,1,1,2,2,1,2,1,1,2]

kolakoski = 1 : 2 : 2 : concat [ replicate n y | (y,n) <- zip (cycle [1,2]) (drop 2 kolakoski) ]
