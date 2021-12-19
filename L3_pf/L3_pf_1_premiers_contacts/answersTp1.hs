-- Q1
somme1 :: Int -> Int
somme1 n = sum[1..n]

-- Q3
sommeDeXaY :: Int -> Int -> Int
sommeDeXaY x y = sum[x..y]

-- Q4
somme2 :: [Int] -> Int
somme2 [] = 0
somme2 (x:xs) = x + somme2 xs

-- Q5
m_last :: [a] -> a
m_last xs = xs !! (length xs -1)

m_init :: [a] -> [a]
m_init xs = take (length xs -1) xs

-- Q6
(!!!) :: [a] -> Int -> a 
(!!!) [] _ = error "n too big"
(!!!) (x:_) 0 = x
(!!!) (x:xs) n = xs !!! (n-1)

(+++) :: [a] -> [a] -> [a]
(+++) [] ys = ys
(+++) (x:xs) ys = x: (xs +++ ys)

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x +++ (concat' xs)

map' :: (a->b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = (f x) : (map' f xs)

    {- ancienne version en utilisant les fonctions de base
    m_recup :: [a] -> Int -> a
    m_recup xs n = head (drop n xs)

    m_append :: [a] -> [a] -> [a]
    m_append (x:xs) ys = (x : m_append xs ys)

    m_concat :: [[a]] -> [a]
    m_concat(xs:[]) = xs
    m_concat (xs:ys) = m_append xs (m_concat ys)

    m_map :: (a->b) -> [a] -> [b]
    m_map f [] = []
    m_map f (x:xs) = (f x : m_map f xs)
    -}


-- Q7
-- x est une fonction qui envoie le enième élément de la liste l, 
-- fonction (!!) prend une liste en paramètre
-- c'est une définition partielle
-- x        = (!!) l

-- Q8
m_len :: [a] -> Int 
m_len xs = somme2 (map (\i -> 1) xs)

-- Q9
function9 :: (a->a) -> a -> Int -> [a]
function9 f x n = take n (iterate f x)

-- Q10
function10 :: Int -> [Int]
function10 n = function9 (\i->i+1) 1 n