module DefinitionsFonctions where

absolue :: (Ord a, Num a) => a -> a
absolue n = if n >= 0 then n else -n

signe :: (Num t, Num a, Ord a) => a -> t
signe n = if n >= 0
          then if n > 0
               then 1
               else 0
          else -1

pairSuivant :: Integral a => a -> a
pairSuivant n = n + 1 + if n `mod` 2 == 0 then 0 else -1

-- Définitions avec des gardes
-- Les gardes sont des conditions (expressions booléennes)
signe' :: (Num t, Num a, Ord a) => a -> t
signe' n | n < 0     = -1
         | n == 0    = 0
         | otherwise = 1

-- otherwise est défini comme True

-- On n’est pas obligé d’avoir une dernière garde avec otherwise, tout
-- comme on n’est pas obligé de couvrir tous les cas dans le filtrage
-- de motifs


-- Filtrage de motifs

non :: Bool -> Bool
non True  = False
non False = True

-- True, False (avec une majuscule) sont les « constructeurs » du type Bool

(&&&) :: Bool -> Bool -> Bool
True  &&& True  = True
False &&& True  = False
True  &&& False = False
False &&& False = False

-- L’ordre des équations de définitions est important
(&&&&) :: Bool -> Bool -> Bool
True &&&& True = True
_    &&&& _    = False

(&&&&&) :: Bool -> Bool -> Bool
b &&&&& True = b
_ &&&&& _    = False

(&&&&&&) :: Bool -> Bool -> Bool
-- Impossible d’avoir deux arguments ayant le même nom
-- b &&&&&& b = b
-- _ &&&&&& _ = False
b &&&&&& b' | b == b'   = b
            | otherwise = False
-- _ &&&&&& _ = False

tete :: [a] -> a
-- (:) et [] sont les constructeurs de liste
tete (x:_) = x

decompose :: [a] -> (a,[a])
decompose (x:xs) = (x, xs)

extraitTete :: [a] -> (a,[a])
extraitTete tous@(x:_) = (x,tous)
-- équivalent à :
-- extraitTete (x:xs) = (x,x:xs)

somme :: (Num a, Eq a) => [a] -> a
somme (0:xs) = sum xs
somme    xs  = sum xs

troisieme :: [a] -> a
-- troisieme (x:(y:(z:uss))) = z
-- équivalent à : troisieme (x:y:z:uss) = z
-- ((x:ys):zss):usss
troisieme (_:_:z:_) = z

cas :: [a] -> String
cas []        = "zéro"
cas [_]       = "un"
cas [_,_]     = "deux"
-- équivalent à :
-- cas (_:_:[])  = "deux"
cas (_:_:_:_) = "beaucoup"

eclair :: [a] -> [b] -> [(a,b)]
eclair []     _      = []
eclair _      []     = []
eclair (x:xs) (y:ys) = (x,y) : eclair xs ys

teteImbriquee :: [[a]] -> a
teteImbriquee ((h:_):_) = h


-- Fonctions anonymes
-- \ vient de la lettre grecque λ (lambda)

impairs :: [Integer]
impairs = map (\n -> n * 2 + 1) [0..]

impairs' :: [Integer]
impairs' = let f n = n * 2 + 1
           in  map f [0..]

impairs'' :: [Integer]
impairs'' = map f [0..]
    where f n = n * 2 + 1

fun :: Int -> String
fun n = replicate n 'a' ++ "h !"

g :: Int -> String
g = \n -> replicate n 'a' ++ "h !"

mult :: Integer -> [Integer] -> [Integer]
mult n ns = map (\n' -> n * n') ns
-- (\n' -> n * n') est une « fermeture »
--
mult' :: Integer -> ([Integer] -> [Integer])
mult' n = map (\n' -> n * n')


-- Sections
-- Le nom d’un opérateur est une suite de symboles
-- L’opérateur s’utilise en position infixe, entre ses deux arguments
-- On utilise des parenthèses pour désigner la fonction sans
-- l’appliquer à ses arguments : (+)
-- On peut aussi appliquer l’opérateur partiellement, à son premier ou
-- son second argument :

carres :: [Integer]
carres = map (^2) [0..]

puissances2 :: [Integer]
puissances2 = map (2^) [0..]

mult'' :: Integer -> [Integer] -> [Integer]
mult'' n = map (n *)
