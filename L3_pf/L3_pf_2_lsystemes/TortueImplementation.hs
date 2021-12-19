module TortueImplementation where

------- Implémentation ------
type Symbole  = Char
type Mot      = [Symbole]
type Axiome   = Mot
type Regles   = Symbole -> Mot
type LSysteme = [Mot]

-- Q1.
-- Version récursive
motSuivant :: Regles -> Mot -> Mot
motSuivant _ [] = []
motSuivant r (x:xs) = (r x) ++ (motSuivant r xs)

-- Fonction bien choisie du Prelude
motSuivant' :: Regles -> Mot -> Mot
motSuivant' r xs = concatMap r xs

-- Liste en compréhension
motSuivant'' :: Regles -> Mot -> Mot
motSuivant'' r xs = [s | x <- xs, s <- r x]


-- Q2.
regleFlocon :: Symbole -> Mot
regleFlocon 'F' = "F-F++F-F"
regleFlocon '+' = "+"
regleFlocon '-' = "-"
regleFlocon _ = ""


-- Q3.
lsysteme :: Axiome -> Regles -> LSysteme
lsysteme a r = iterate (motSuivant r) a