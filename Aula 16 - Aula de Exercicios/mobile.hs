--Um móbile é constituído de pendentes, fios e barras. Em cada uma das extremidades de uma barra de um mobile é preso um fio, no qual pode estar pendurado um pendente ou uma nova barra. Barras e fios são considerados elementos sem peso e pendentes possuem um peso (representado por um valor inteiro). O seguinte tipo de dado pode ser usado para representar um móbile:

--data Mobile = Pendente Int | Barra Mobile Mobile deriving (Eq, Show)

--O peso de um móbile é igual à soma dos pesos de todos os seus pendentes. Um móbile é balanceado se ele consiste de um único pendente ou se os pesos dos móbiles pendurados nas duas extremidades da sua barra são iguais e esses móbiles são balanceados. 

--Defina a função balanceado :: Mobile -> Bool que determina se o móbile dado como argumento é ou não balanceado ou não.

--Dica: Defina a função auxiliar peso :: Mobile -> Int que retorna o peso do móbile dado como argumento.


--balanceado  $ Barra (Barra (Pendente 2) (Barra (Pendente 4) (Pendente 1))) (Barra (Pendente 3) (Barra (Pendente 2) (Pendente 1))) == False

--balanceado  $ Barra (Barra (Pendente 2) (Barra (Pendente 4) (Pendente 3))) (Barra (Pendente 3) (Barra (Pendente 4) (Pendente 2))) == True


data Mobile = Pendente Int | Barra Mobile Mobile deriving (Eq, Show)
 
    peso (Pendente x) = x
    peso (Barra m1 m2) = peso m1 + peso m2 

    balanceado (Pendente a) = True
    balanceado (Barra esq dir) = if (peso(esq)) == (peso(dir)) then True else False
