--Você foi contratado pela Coordenação da OBI (Olimpíada Brasileira de Informática) para 
--determina quantos competidores serão convocados para um curso de  programação avançado.
--São dados o número mínimo de pontos para ser convocado e uma lista com os nomes dos competidores 
--juntamente com a pontuação obtida pelo competidor. 
--Você precisa devolver uma lista com os nomes dos competidores convocados. Por exemplo,

--convocados 100 [("Leticia", 100), ("Iana", 30),("Roy", 200) ] == ["Leticia","Roy"]

convocados n xs = [ fst x | x <- xs, (snd x) >= n]