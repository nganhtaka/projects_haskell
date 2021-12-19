module Somme where

somme  n = sum [1..n]
-- La notation [ .. ] est un raccourci pour enumFromTo
somme' n = sum (enumFromTo 1 n)

sommePairs  n = sum [0,2..2*n]
sommePairs' n = sum (enumFromThenTo 0 2 (2*n))

sommePairs'' n = sum (map (* 2) [1..n])

sommeCarres   n = sum (map (\i -> i * i) [1..n])
sommeCarres'  n = sum (map (^ 2) [1..n])
sommeCarres'' n = sum (map (\i -> i ^ 2) [1..n])
