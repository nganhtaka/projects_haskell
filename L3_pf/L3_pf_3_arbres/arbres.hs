import Data.String
import Test.QuickCheck
import Control.Concurrent (threadDelay)

main :: IO ()
main = mapM_ ecrit arbres
    where ecrit a = do writeFile "arbre.dot" a
                       threadDelay 1000000
          arbres  = arbresDot "gcfxieqzrujlmdoywnbakhpvst"

-- -- -- -- -- -- -- --
-- Arbres Binaires   --
-- -- -- -- -- -- -- --

-- Q1.

data Arbre coul val = Feuille
                    | Noeud coul val (Arbre coul val) (Arbre coul val)
                      deriving (Eq, Ord, Show)

-- Exemples des arbres
tree0 = Feuille
tree1 = Noeud 'B' 1 (Noeud 'V' 2 (Noeud 'R' 3 Feuille Feuille) Feuille) Feuille
tree2 = Noeud 'B' 'a' Feuille Feuille
tree3 = Noeud 'B' 'b' tree2 (Noeud 'B' 'c' Feuille Feuille)
tree4 = Noeud 'B' 'd' tree3 (Noeud 'B' 'f' Feuille Feuille)
tree5 = Noeud 'B' 'd' tree3 (Noeud 'B' 'f' (Noeud 'B' 'e' Feuille Feuille) (Noeud 'B' 'g' Feuille Feuille))

-- Q2.

mapArbre :: (coul -> coul) -> (val -> val)  -> Arbre coul val -> Arbre coul val
mapArbre fc fv Feuille = Feuille
mapArbre fc fv (Noeud c v ag ad) = Noeud (fc c) (fv v) (mapArbre fc fv ag) (mapArbre fc fv ad)

-- Q3.

hauteur :: Arbre coul val -> Int
hauteur Feuille = 0
hauteur (Noeud _ _ ag ad) = 1 + max (hauteur ag) (hauteur ad)

taille :: Arbre coul val -> Int
taille Feuille = 0
taille (Noeud _ _ ag ad) = 1 + taille ag + taille ad

-- Q4.

dimension :: (c -> v -> n -> n -> n) -> n -> Arbre c v -> n
dimension _ b Feuille = b
dimension f b (Noeud c v ag ad) = f c v (dimension f b ag) (dimension f b ad)

hauteur' :: Arbre coul val -> Int
hauteur' = dimension (\_ _ ag ad -> 1 + max ag ad) 0

taille' :: Arbre coul val -> Int
taille' = dimension (\_ _ ag ad -> 1 + ag + ad) 0

-- Q5.

peigneGauche :: [(coul, val)] -> Arbre coul val
peigneGauche [] = Feuille
peigneGauche ((coul, val):xs) = Noeud coul val (peigneGauche xs) Feuille

-- Q6.

-- Cette fonction vérifie si l'hauteur de l'arbre créé par la fonction
-- peigneGauche est égale à la longeur de la liste (coul, val).

prop_hauteurPeigne :: [(coul, val)] -> Bool
prop_hauteurPeigne xs = length xs == hauteur (peigneGauche xs)

prop_hauteurPeigne' :: [(coul, val)] -> Bool
prop_hauteurPeigne' xs = length xs == hauteur' (peigneGauche xs)

-- Q7

prop_taillePeigne :: [(coul, val)] -> Bool
prop_taillePeigne xs = length xs == taille (peigneGauche xs)

prop_taillePeigne' :: [(coul, val)] -> Bool
prop_taillePeigne' xs = length xs == taille' (peigneGauche xs)


-- prop_mapArbre
compareArbre :: (Eq coul, Eq val) => Arbre coul val -> Arbre coul val -> Bool
compareArbre Feuille Feuille = True
compareArbre (Noeud c1 v1 ag1 ad1) (Noeud c2 v2 ag2 ad2) =
        c1 == c2 && v1 == v2 && (compareArbre ag1 ag2) && (compareArbre ad1 ad2)

mapListe :: (a -> a) -> (b -> b) -> [(a, b)] -> [(a, b)]
mapListe f g xs = [(f x, g y) | (x, y) <- xs]

-- to be corrected
prop_mapArbre :: (Eq coul, Eq val) => (coul -> coul) -> (val -> val) -> [(coul, val)] -> Bool
prop_mapArbre fc fv xs = compareArbre (mapArbre fc fv (peigneGauche xs)) (peigneGauche (mapListe fc fv xs))

-- Q8.

estComplet :: Arbre c v -> Bool
estComplet Feuille = True
estComplet (Noeud _ _ ag ad) = hauteur ag == hauteur ad
                                && estComplet ag
                                && estComplet ad

-- Q9.

estComplet' :: Arbre c v -> Bool
estComplet' = fst . dimension (\_ _ (ag_complet, hauteur_ag) (ad_complet, hauteur_ad)
                -> (ag_complet && ad_complet && hauteur_ad == hauteur_ag, 1 + max hauteur_ag hauteur_ad))
                (True, 0)

-- Q10.

-- {-
-- -- Il y a un seul peigne à gauche étant complet : le cas [ ].
-- -- Il y a un cas vrai mais les autres sont faux.
-- -- On ne peut pas utiliser QuickCheck parce que il ne donne pas de même résultat pour tous les cas.
-- -}

-- Q11.

complet :: Int -> [(c, a)] -> Arbre c a
complet 0 [] = Feuille
complet h l = let middle   = length l `quot` 2
                  (c, v)   = l !! middle
                  l_gauche = take middle l
                  l_droite = drop (middle + 1) l in
                  Noeud c v (complet (h - 1) l_gauche) (complet (h - 1) l_droite)


-- {-
-- Pour un arbre complet de hauteur n :
-- level  h  =  0    1     2              n
-- nb_noeuds = (1 + 2^1 + 2^2 + ... + 2^(n)) = 2^n - 1
-- Nombre de noeuds   = 2^n - 1
-- Nombre de feuilles = 2^n
-- Pas raisonnable en effet de construire des arbres complets de hauteur de 20 ou plus car le nombre des
-- feuilles crées sera environ (10 ^ 6)
-- -}

prop_complet1 :: [(c,a)] -> Bool
prop_complet1 xs = estComplet (complet (length xs) xs)

-- Q12.

infiniteL :: a -> [a]
infiniteL x = [x] ++ infiniteL x

infiniteL' :: a -> [a]
infiniteL' = iterate id

-- Q13.

listElemUnicode :: [((), Char)]
listElemUnicode = [((), x) | x <- ['a'..]]

-- To get a complete tree of height n, the list must contain (2^n - 1) elements to create (2^n - 1) nodes
completeList :: Int -> [((), Char)]
completeList h = take (2^h - 1) listElemUnicode


-- Q14.

aplatit :: Arbre c a -> [(c, a)]
aplatit = dimension (\c v ag ad -> ag ++ [(c, v)] ++ ad) []

prop_aplatit :: Bool
prop_aplatit = let complet4 = complet 4 (completeList 4)
               in  map snd (aplatit complet4) == "abcdefghijklmno"


-- Q15.

element :: Eq a => a -> Arbre c a -> Bool
element el = dimension (\_ v ag ad -> el == v || ag || ad ) False

prop_element1 :: Bool
prop_element1 = let complet3 = complet 3 (completeList 3)
                in  all (\x -> element x complet3) "abcdefg"

prop_element2 :: Bool
prop_element2 = let complet3 = complet 3 (completeList 3)
                in  all (\x -> not $ element x complet3) "xyz()::"


-- -- -- -- -- -- -- -- --
-- Affichage des arbres --
-- -- -- -- -- -- -- -- --

-- Q16.

noeud :: (c -> String) -> (a -> String) -> (c, a) -> String
noeud fc fa (c, a) = fa a ++ " [color=" ++ fc c ++ ", fontcolor=" ++ fc c ++ "]"

-- Q17.

arcs :: Arbre c a -> [(a, a)]
arcs Feuille = []
arcs (Noeud _ _ Feuille Feuille) = []
arcs (Noeud _ a ag@(Noeud _ vg _ _) ad@(Noeud _ vd _ _)) =  (a, vg) : (a, vd) : arcs ag ++ arcs ad
arcs (Noeud _ a Feuille ad@(Noeud _ vd _ _)) = (a, vd) : arcs ad
arcs (Noeud _ a ag@(Noeud _ vg _ _) Feuille) = (a, vg) : arcs ag

-- Q18.

arc :: (a -> String) -> (a, a) -> String
arc f (x, y) = f x ++ " -> " ++ f y

-- Q19.

dotise :: String -> (c -> String) -> (a -> String) -> Arbre c a -> String
dotise nom fc fv arbre@(Noeud _ _ _ _) =
        let entete  = "digraph \"" ++ nom ++ "\" {"
            font    = "node [fontname=\"DejaVu-Sans\", shape=circle]"
            l_nodes = map (noeud fc fv) (aplatit arbre)
            ls_arcs = map (arc fv) (arcs arbre)
            fin     = "}"
        in entete ++ font ++ unlines l_nodes ++ unlines ls_arcs ++ fin


-- -- -- -- -- -- -- -- --
-- Enfin de la couleur  --
-- -- -- -- -- -- -- -- --

-- Q20.

elementR :: Ord(a) => Arbre c a -> a -> Bool
elementR Feuille _ = False
elementR (Noeud _ v ag ad) x
        | x == v = True
        | x < v  = elementR ag x
        | x > v  = elementR ad x

-- Q21.

-- Rouge et Noir
data Couleur = R
             | N deriving (Eq, Show)

type ArbreRN = Arbre Couleur

-- Q22.

equilibre :: Couleur -> a -> ArbreRN a -> ArbreRN a -> ArbreRN a
equilibre N z (Noeud R y (Noeud R x a b) c) d = Noeud R y (Noeud N x a b) (Noeud N z c d)

equilibre N z (Noeud R x a (Noeud R y b c)) d = Noeud R y (Noeud N x a b) (Noeud N z c d)

equilibre N x a (Noeud R z (Noeud R y b c) d) = Noeud R y (Noeud N x a b) (Noeud N z c d)

equilibre N x a (Noeud R y b (Noeud R z c d)) = Noeud R y (Noeud N x a b) (Noeud N z c d)

equilibre color x ag ad = Noeud color x ag ad

-- Q23.

colorNodeBlack :: ArbreRN a -> ArbreRN a
colorNodeBlack (Noeud _ v ag ad) = Noeud N v ag ad
colorNodeBlack Feuille = Feuille

insert :: Ord a => a -> ArbreRN a -> ArbreRN a
insert x tree = colorNodeBlack (ins tree)
                where ins Feuille = Noeud R x Feuille Feuille
                      ins arbre@(Noeud color v ag ad) | x < v  = equilibre color v (ins ag) ad
                                                      | x == v = arbre
                                                      | x > v  = equilibre color v ag (ins ad)

-- Q24.

-- racine est noir
prop_insert1 :: ArbreRN a -> Bool
prop_insert1 abr = head (map fst (aplatit abr)) == N

-- nœud rouge n’a pas de fils rouge.
colorR :: ArbreRN a -> Couleur
colorR Feuille = N
colorR (Noeud c _ _ _) = c

prop_insert2 :: ArbreRN a -> Bool
prop_insert2 tree = aux tree
        where aux (Noeud col _ ag ad) = aux ag && aux ad
                && if col == R
                   then (colorR ag == N && colorR ad == N)
                   else True
              aux Feuille = True


-- -- -- -- -- -- -- -- -- --
--  et un peu d’animation  --
-- -- -- -- -- -- -- -- -- --

-- Q25.

trees :: (Eq a, Ord a) => ArbreRN a -> [a] ->  [ArbreRN a]
trees _ []         = []
trees arbre (x:xs) = newTree : trees newTree xs
        where newTree = insert x arbre

trees' :: (Eq a, Ord a) => ArbreRN a -> [a] -> [ArbreRN a]
trees' arbre xs = tail $ scanl (flip (insert)) arbre xs


dotiseRN :: ArbreRN Char -> String
dotiseRN = dotise "arbre"
                        (\c -> case c of R -> "red"
                                         N -> "black")
                        (\x -> [x])

arbresDot :: String -> [String]
arbresDot xs = [ dotiseRN x | x <- trees Feuille xs]

-- -- -- --
-- Test  --
-- -- -- --
testTree :: ArbreRN Char
testTree = foldl (flip $ insert) Feuille ['a'..'z']

test :: IO ()
test = do
        quickCheck prop_aplatit
        quickCheck prop_element1
        quickCheck prop_element2
        quickCheck (prop_insert1 testTree)
        quickCheck (prop_insert2 testTree)


-- quickCheck prop_hauteurPeigne
-- quickCheck prop_taillePeigne
-- quickCheckWith (stdArgs {maxSize=17}) prop_complet1
