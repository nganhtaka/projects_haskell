module Exemples where

-- L’ordre des définitions de fonction n’est pas important

quadruple x = double (double x)
double    x = x + x

factorial n = product [1..n]

moyenne  ns = sum ns `div` length ns
moyenne' ns = div (sum ns) (length ns)
