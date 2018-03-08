-- Defina a função capitalises que recebe uma lista de caracteres e devolve a uma lista substituindo as letras minúsculas por maiúsculas.

-- capitalises "Minority Report" == "MINORITY REPORT"

-- Dica 1: use a função toUpper

-- Dica 2: Não esqueça de colocar import Data.Char na primeira linha do arquivo .hs para utilizar a função toUpper do módulo Data.Char
import Data.Char

capitalises xs = [toUpper x | x <- xs]