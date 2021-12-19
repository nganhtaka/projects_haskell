module Main where

-- Parallélisme

import           Control.Parallel
import           Control.Parallel.Strategies

import           Debug.Trace                 (traceEvent)


-- Essayer successivement tous les main

main = print test1
-- main = print test2
-- main = print test3
-- main = print test4
-- main = print test5
-- main = print test6
-- main = print test7

-- Calcul de deux fonctions qui prennent du temps
-- (peu importe ici ce qu’elles calculent)

-- Les valeurs sur lesquelles on va tester les fonctions
fiboN = 44
fiboR = 701408733
eulN  = 17000
eulR  = 87850453

-- | 1re version

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

euler :: Int -> Int
euler n = length [ x | x <- [1..n-1], gcd x n == 1 ]

sumEuler :: Int -> Int
sumEuler n = sum [ euler x | x <- [1..n] ]

test1 :: Bool
test1 = fibok && eulok
    where fibok = fib fiboN == fiboR
          eulok = sumEuler eulN == eulR

-- | 2e version
-- Exécuter le programme avec les options « +RTS -lu -N2 -s »

test2 :: Bool
test2 = fibok && eulok
    where fibok = traceEvent "-------fib--------" $ fib fiboN == fiboR
          eulok = traceEvent "-------eul--------" $ sumEuler eulN == eulR

-- | 3e version
-- Voir https://wiki.haskell.org/ThreadScope_Tour/SparkOverview

test3 :: Bool
test3 = fibok `par` fibok && eulok
    where fibok = traceEvent "-------fib--------" $ fib fiboN == fiboR
          eulok = traceEvent "-------eul--------" $ sumEuler eulN == eulR

-- | 4e version
test4 :: Bool
test4 = eulok `par` fibok && eulok
    where fibok = traceEvent "-------fib--------" $ fib fiboN == fiboR
          eulok = traceEvent "-------eul--------" $ sumEuler eulN == eulR

-- | 5e version, un peu plus résistante
test5 :: Bool
test5 = fibok `par` (eulok `pseq` fibok && eulok)
    where fibok = traceEvent "-------fib--------" $ fib fiboN == fiboR
          eulok = traceEvent "-------eul--------" $ sumEuler eulN == eulR

-- | Un peu plus de calculs
multifib = map (traceEvent "-------fib--------" . fib)   [ 40, 41, 42 ]
multieul = map (traceEvent "-------eul--------" . sumEuler) [ 12345, 12346 ]
test6 = multifib `par` (multieul `pseq` sum multifib + sum multieul)

-- | Enquête
plus1 = map (+1) [0..100]
force xs = xs `pseq` ()

force' []     = ()
force' (x:xs) = x `pseq` force' xs

test7 = force' multifib `par`
    (force' multieul `pseq` sum multifib + sum multieul)
