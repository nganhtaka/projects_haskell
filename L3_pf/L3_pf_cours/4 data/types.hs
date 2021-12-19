module DéclarationTypes where

-- Synonymes de types

type String' = [Char]

type Point = (Float, Float)
-- Si on décide par la suite de changer de type Point (passage en 3D),
-- le compilateur nous aide à corriger
-- type Point = (Float, Float, Float)

bougeAGauche :: Point -> Point
bougeAGauche (x,y) = (x+1,y)

bougeAGauche' :: (Float, Float) -> (Float, Float)
bougeAGauche' = bougeAGauche

type Paire a = (a, a)

bougeAGauche'' :: Paire Float -> Paire Float
bougeAGauche'' = bougeAGauche'

duplique :: a -> Paire a
duplique x = (x, x)

type Rayon = Float
type Cercle = (Point, Rayon)

-- Impossible de créer un cycle
-- type Tree a = (a, [Tree a])


-- Types de données algébriques

data Bool' = Vrai
           | Faux
    deriving Show

show' :: Bool' -> String
show' Vrai = "Vrai"
show' Faux = "Faux"

-- Vrai et Faux sont les _constructeurs_ du type
-- Ils commencent toujours par une majuscule, ce qui permet de les
-- utiliser dans le filtrage de motifs
-- Ici c’est ± un type énuméré

ex1 = Vrai

non :: Bool' -> Bool'
non Vrai = Faux
non Faux = Vrai

-- Essayer avec :
-- non vrai = Faux
-- non faux = Vrai

data Réponse = Oui
             | Non
             | PtetBenQuOuiPtetBenQuNon
    deriving (Show, Enum)

contredit :: Réponse -> Réponse
contredit Oui                      = Non
contredit Non                      = Oui
contredit PtetBenQuOuiPtetBenQuNon = PtetBenQuOuiPtetBenQuNon

ex2 = [ Oui .. ]


data TupleEntiers = Paire Int Int
                  | Triplet Int Int Int
    deriving (Show, Eq, Ord, Read)

réduit :: TupleEntiers -> Int
réduit (Paire m n) = m + n
réduit (Triplet m n o) = m + n + o

-- Constructeurs ne sont pas forcément paramétrés par les mêmes types
--
-- « Type somme »
data Message = Chaine String
             | Entier Integer
    deriving (Show, Eq, Ord, Read)

ex3 = [Chaine "abcd", Entier 12]

-- Permet de faire du code sûr !

data PeutEtre a = Rien
                | Juste a
    deriving (Show, Eq, Ord, Read)

-- C’est une redéfinition du type Maybe

-- Contrairement à la fonction head standard, tete n’échoue jamais
-- vraiment : elle a toujours un résultat possible à fournir, Nothing
-- pour signaler qu’elle a « échoué »
-- Au lieu d’une exception (qui casse l’exécution du programme), on
-- utilise des valeurs pour signaler qu’il y a eu un problème
tete :: [a] -> Maybe a
tete    [] = Nothing
tete (x:_) = Just x

commenceParA :: String -> Bool
commenceParA cs = head cs == 'A'

commenceParA' :: String -> Bool
commenceParA' cs = not (null cs) && head cs == 'A'

-- L’utilisateur de tete ne peut pas ignorer le cas d’erreur

commenceParA'' :: String -> Bool
commenceParA'' cs = case tete cs of
                      Just 'A' -> True
                      _        -> False

-- Structures de données récursives

data Liste a = Vide
             | Cons a (Liste a)
    deriving (Show, Eq, Ord, Read)

ex4 = Vide
ex5 = Cons 1 Vide
ex6 = Cons 2 ex5

data Liste' a = Vide'
              | a :- Liste' a
    deriving (Show, Eq, Ord, Read)

infixr 5 :-

ex7 = 1 :- Vide'
ex8 = 1 : []
ex9 = 1 :- 2 :- Vide'
ex10 = 1 : (2 : [])

-- Une structure de données un peu étrange, parce que ces listes sont toutes « infinies »
data Liste'' a = Cons' a (Liste'' a)
    deriving (Show, Eq, Ord, Read)

-- La façon la plus naturelle d’en faire une, est de créer une liste
-- circulaire
ex11 :: Liste'' Int
ex11 = Cons' 1 ex11


-- Revenons au type Liste : il est équivalent au type de liste
-- standard, on peut donc convertir une liste standard en Liste

convertit :: [a] -> Liste a
convertit     [] = Vide
convertit (x:xs) = Cons x (convertit xs)

-- hlint propose (en deux étapes) de simplifier ce code en :

convertit' :: [a] -> Liste a
convertit' = foldr Cons Vide


data Arbre a = Noeud a [Arbre a]
    deriving (Show, Eq, Ord, Read)

ex12 = Noeud 1 []
ex13 = Noeud 12 [ex12, ex12, ex13]
ex14 = Noeud 12 [ex12, ex12]

mapArbre :: (a -> b) -> Arbre a -> Arbre b
mapArbre f (Noeud x as) = Noeud (f x) (map (mapArbre f) as)

(===) :: Eq a => Arbre a -> Arbre a -> Bool
Noeud x as === Noeud y bs = x == y
                         && length as == length bs
                         && and (zipWith (===) as bs)

data Point'  = Point' (Float, Float)
data Point'' = Point'' Float Float

bougeAGauche''' :: Point' -> Point'
-- Impossible de confondre avec Point
-- bougeAGauche''' = bougeAGauche''
bougeAGauche''' (Point' (x,y)) = Point' (x+1,y)

newtype Point''' = Point''' (Float, Float)
