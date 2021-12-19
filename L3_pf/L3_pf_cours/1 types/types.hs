module Types where

-- Types de fonctions

add :: (Int, Int) -> Int
add (n, n') = n + n'

zeroTo :: Int -> [Int]
zeroTo n = [0..n]

duplique :: Int -> (Int, Int)
duplique n = (n, n)


--- Fonctions curryfiées

add' :: Int -> Int -> Int
-- signifie : add' :: Int -> (Int -> Int)
add' n n' = n + n'
-- Penser à la différence avec le type (Int -> Int) -> Int

add3 :: Int -> Int -> Int -> Int
add3 n n' n'' = n + n' + n''

-- Cas d’applications partielles
ex1 :: Int -> Int
ex1 = add' 12
-- réutilisation du code de l’addition pour éviter de devoir définir
-- une fonction juste pour ajouter 12
-- exemple de fermeture


--- Fonctions polymorphes
--
-- On parle parfois de polymorphisme paramétré

ex2 :: [a] -> [a]
ex2 = take 5

ex3 :: [a] -> Int
ex3 = length

ex4 :: [a] -> a
ex4 = head

-- fonction id
ex5 :: a -> a
ex5 x = x

-- Quelles sont toutes les fonctions ayant le type : [a] -> a ?
-- head, retourne un élément particulier
-- Toutes les fonctions de ce type doivent retourner un élément de la
-- liste : elles ne peuvent pas inventer une valeur de type a
-- quelconque
ex6 :: [a] -> a
ex6 xs = xs !! 12

-- Quelles sont toutes les fonctions ayant le type : [a] -> [a] ?
-- tail, take 5, drop 5, reverse
-- De façon générale, elles ne peuvent que retourner une liste dont
-- les éléments viennent de la liste argument (elles peuvent en faire
-- ce qu’elles veulent : mélange, sous-ensemble, etc.)


--- Surcharge
--
-- Contraintes de classes de type
-- « Num a » signifie pour tout type a de la classe Num
-- Exemple
-- (+) :: Num a => a -> a -> a
-- Num a : contrainte de classe de type
-- a le type des arguments et du résultat
--
-- Classe Eq pour tester l’égalité
-- Classe Num pour les nombres
-- Classe Fractional pour les « nombres à virgule »
-- Classe Show pour les valeurs que l’on sait afficher
-- Classe Ord pour les types de valeurs ordonnées (comparables)
-- ...

a :: Integer
a = 12

b :: Num a => a
b = 12

-- L’expression suivante est mal typée
-- ex7 = a + 1.2

-- ex8 est bien typée, avec plusieurs types possibles
-- par exemple : ex8 :: Double
ex8 = b + 1.2
