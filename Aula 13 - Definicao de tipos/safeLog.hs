--A função log :: Float -> Float não está definida para números negativos.

--Prelude> log 1000

--6.907755278982137

--Prelude> log (-1000)

--''ERROR'' -- runtime error 

--Defina uma versão segura que evite runtime error usando Maybe.

--safeLog ::  Float -> Maybe Float

--safeLog x
--    | x > 0     =
--    | otherwise =

--safeLog 56 == Just 4.025

--safeLog 78 == Just 4.357

--Ps: O seu resultado  precisa ser mostrado com apenas 3 casas decimais. Para realizar esta tarefa utilize a seguinte função

--import Numeric

--setprecision :: Int -> Float -> Float

--setprecision prec a = read $ showFFloat (Just prec) a ""
--setprecision 2 12.3545
--12.35

import Numeric

setprecision prec a = read $ showFFloat (Just prec) a ""
    
safeLog ::  Float -> Maybe Float
safeLog x   | x > 0     = Just (setprecision 3 (log x))
            | otherwise = Nothing