module Combinateurs where
-- Aussi appelés Fonctions d’ordre supérieur, c’est-à-dire des
-- fonctions qui retournent des fonctions ou prennent des fonctions en
-- argument

import           Data.Char (isLower)
import           Data.List (nub, sort)

-- Retourner une fonction : on en a l’habitude, c’est l’application partielle

add :: Int -> Int -> Int
add x y = x + y

add' :: Int -> (Int -> Int)
add' = add

ex1 = add 1 2
ex2 = (add' 1) 2

-- Prendre des fonctions en argument

deuxFois :: (a -> a) -> a -> a
deuxFois f x = f (f x)

ex3 = deuxFois (*2) 3

ex4 = deuxFois reverse [1..10]

stupide = deuxFois reverse

map' :: (a -> b) -> [a] -> [b]
map' _     [] = []
map' f (x:xs) = f x : map' f xs

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = [ f x | x <- xs ]

ex5 = reverse [ "abcde", "ghi" ]
ex6 = map reverse [ "abcde", "ghi" ]
ex7 = map (1+) [0..10]

ex8' :: Num a => [a] -> [a]
ex8' = map (1+)
ex8 = map (map (1+)) [[0..10],[12..20]]

ex9 = sum (map sum [[0..10],[12..20]])

et :: [Bool] -> Bool
et        [] = True
et (True:xs) = et xs
et         _ = False

ex10 = and (map isLower "abcde")
ex11 = and (map isLower "Abcde")

filtre :: (a -> Bool) -> [a] -> [a]
filtre _    []              = []
filtre f (x:xs) | f x       = x : filtre f xs
                | otherwise = filtre f xs

ex12 = filtre isLower "Abcde"
ex13 = filtre even [1..20]

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort petits
            ++ [x]
            ++ qsort grands
    where petits = [ y | y <- xs, y < x ]
          grands = [ z | z <- xs, z >= x ]

qsortGen :: (a -> a -> Bool) -> [a] -> [a]
qsortGen _ [] = []
qsortGen f (x:xs) = qsortGen f petits
                 ++ [x]
                 ++ qsortGen f grands
    where petits = [ y | y <- xs, f y x ]
          grands = [ z | z <- xs, not (f z x) ]

ex14 = qsortGen (>) [0..10]
ex15 = qsortGen (<) [1,4,2]
ex16 :: Ord a => [a] -> [a]
ex16 = qsortGen (<)

-- Aller voir les différentes fonctions « ...By » du module Data.List

-- Fold : Repli

somme :: Num a => [a] -> a
somme [] = 0
somme (x:xs) = x + somme xs

produit :: Num a => [a] -> a
produit [] = 1
produit (x:xs) = x * produit xs

et' :: [Bool] -> Bool
et'     [] = True
et' (x:xs) = x && et xs

generique :: (b -> a -> a) -> a -> [b] -> a
generique  _ v     [] = v
generique op v (x:xs) = x `op` generique op v xs

somme' :: Num a => [a] -> a
somme' = generique (+) 0

produit' :: Num a => [a] -> a
produit' = generique (*) 1

et'' :: [Bool] -> Bool
et'' = generique (&&) True

ex17 = somme' [1..3]

-- generique s’appelle foldr

-- La version qui replie la liste par la gauche

-- v peut être vu comme un accumulateur
generiqueGauche :: (a -> b -> a) -> a -> [b] -> a
generiqueGauche op v     [] = v
generiqueGauche op v (x:xs) = generiqueGauche op (v `op` x) xs

longueur :: [a] -> Int
longueur = foldr (\_ n -> n + 1) 0

longueur' :: [a] -> Int
longueur' = foldr (const (+1)) 0

ex18 :: Integer
ex18 = foldr (^) 2 [2,2,2]
ex19 :: Integer
ex19 = foldl (^) 2 [2,2,2]


-- Composition de fonction
-- Cette fonction s’appelle (.) dans la bibliothèque standard

o :: (b -> c) -> (a -> b) -> a -> c
o f g x = f (g x)

-- even :: Int -> Bool

-- impair x = not (even x)
impair = not . even

deuxFois' f = f . f

longueur'' xs = sum (map (const 1) xs)
longueur''' = sum . map (const 1)

-- La commande shell « sort | uniq | head » correspond à peu près à
-- l’expression haskell suivante
ex20 :: (Eq a, Ord a) => [a] -> [a]
ex20 = take 10 . nub . sort
