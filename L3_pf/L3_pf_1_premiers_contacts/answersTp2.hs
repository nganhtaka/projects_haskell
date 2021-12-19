-- Q1. prend un élément sur deux dans une liste
alterne :: [a] -> [a]
alterne [x] = [x]
alterne (x:_:xs) = (x:alterne xs)

-- Q2. Retourne la liste des [f x1 y1, f x2 y2, ...]
combine :: (a -> b -> c) -> [a] -> [b] -> [c]
combine f [] _ = []
combine f _ [] = []
combine f (x:xs) (y:ys) = [f x y] ++ combine f xs ys 


--------- Triangle de Pascal -----------

-- Q3. Calcule une ligne du triangle de Pascal en fonction de la ligne précédente.
pasPascal :: [Integer] -> [Integer]
pasPascal xs = zipWith (+) (0:xs) (xs++[0])

-- Q4. Définissez le triangle de Pascal en utilisant la fonction iterate.
pascal :: [[Integer]]
pascal = iterate pasPascal [1]


------------------- Courbe du dragon --------------------

------- Première version --------
-- Q5 -> Q7 sont dans le fichier dragon.hs
{-
    Compiler le fichier
    > make dragon

    Lancer le fichier :
    > ./dragon
-}


------- Version alternative --------
-- Q8 -> Q9 sont dans le fichier dragon2.hs
{-
    Compiler le fichier
    > make dragon2

    Lancer le fichier :
    > ./dragon2
-}